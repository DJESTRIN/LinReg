from __future__ import annotations

import json
from pathlib import Path
from typing import List, Optional

import click
import pandas as pd

from .decision import DecisionInputs, resolve_decision
from .eda import summarize_dependents
from .io_data import load_dataframe
from .rbridge import run_r_analysis
from .report import render_reports
from .reshape import detect_shape_details, to_long, to_wide
from .spec import AnalysisSpec, NonlinearSpec, PosthocSpec


def _parse_csv_list(value: Optional[str]) -> List[str]:
    if not value:
        return []
    return [item.strip() for item in value.split(",") if item.strip()]


def _ensure_columns(df: pd.DataFrame, columns: List[str], label: str) -> None:
    missing = [column for column in columns if column and column not in df.columns]
    if missing:
        raise click.ClickException(f"{label} column(s) not found in data: {', '.join(missing)}")


def _positive_right_skewed(summary: dict) -> bool:
    stats = summary.get("summary", {})
    normality = summary.get("normality", {})
    return bool(
        stats.get("min") is not None
        and stats.get("min") > 0
        and abs(normality.get("skew") or 0.0) > 1
        and (normality.get("skew") or 0.0) > 0
    )


def _zero_inflated_continuous(summary: dict) -> bool:
    stats = summary.get("summary", {})
    normality = summary.get("normality", {})
    return bool(
        summary.get("distribution") == "continuous"
        and stats.get("non_negative")
        and (stats.get("zero_fraction") or 0.0) >= 0.1
        and (normality.get("skew") or 0.0) > 1
    )


def _all_transforms_failed(summary: dict) -> bool:
    transform = summary.get("transform", {})
    return bool(transform.get("all_failed"))


@click.group()
def main() -> None:
    """infeR statistics automation CLI."""


@main.command()
@click.option("--input", "input_path", type=click.Path(path_type=Path))
@click.option("--sheet")
@click.option("--sql-uri")
@click.option("--sql-query")
@click.option("--output-dir", type=click.Path(path_type=Path), default=Path(".\\infer_results"))
@click.option("--dv", required=True)
@click.option("--iv")
@click.option("--random-effects")
@click.option("--id-col")
@click.option("--time-col")
@click.option("--shape", type=click.Choice(["wide", "long", "auto"]), default="auto")
@click.option("--wide-to-long", is_flag=True, default=False)
@click.option("--long-to-wide", is_flag=True, default=False)
@click.option("--model-family", type=click.Choice(["auto", "lm", "glm", "lmm", "glmm", "tweedie", "nonparametric", "nonlinear", "manova"]), default="auto")
@click.option("--nonlinear-form", type=click.Choice(["logistic", "exponential", "michaelis_menten", "custom"]))
@click.option("--nonlinear-formula")
@click.option("--distribution", type=click.Choice(["auto", "gaussian", "poisson", "binomial", "gamma", "negbinom", "tweedie"]), default="auto")
@click.option("--transform", "transform_name", type=click.Choice(["auto", "none", "log", "sqrt", "boxcox", "yeojohnson"]), default="auto")
@click.option("--formula")
@click.option("--alpha", type=float, default=0.05, show_default=True)
@click.option("--posthoc-correction", type=click.Choice(["bonferroni", "tukey", "holm", "none"]), default="bonferroni", show_default=True)
@click.option("--posthoc-factors", help="Comma-separated factor(s)/interaction(s) to always post-hoc test regardless of significance (e.g. 'group,group:time'). By default post-hoc tests are only auto-triggered for ANOVA terms significant at --alpha.")
@click.option("--no-auto-posthoc", is_flag=True, default=False, help="Only run post-hoc tests for factors listed in --posthoc-factors; skip auto-triggering on significant ANOVA terms.")
@click.option("--no-term-comparison", is_flag=True, default=False, help="Skip the automatic single-term-deletion AIC comparison (useful to save time on very large models).")
@click.option("--report", "report_format", type=click.Choice(["html", "md", "both"]), default="both", show_default=True)
@click.option("--no-plots", is_flag=True, default=False)
@click.option("--r-path")
@click.option("--dry-run", is_flag=True, default=False)
def run(
    input_path: Optional[Path],
    sheet: Optional[str],
    sql_uri: Optional[str],
    sql_query: Optional[str],
    output_dir: Path,
    dv: str,
    iv: Optional[str],
    random_effects: Optional[str],
    id_col: Optional[str],
    time_col: Optional[str],
    shape: str,
    wide_to_long: bool,
    long_to_wide: bool,
    model_family: str,
    nonlinear_form: Optional[str],
    nonlinear_formula: Optional[str],
    distribution: str,
    transform_name: str,
    formula: Optional[str],
    alpha: float,
    posthoc_correction: str,
    posthoc_factors: Optional[str],
    no_auto_posthoc: bool,
    no_term_comparison: bool,
    report_format: str,
    no_plots: bool,
    r_path: Optional[str],
    dry_run: bool,
) -> None:
    """Ingest data, decide a model family, invoke R, and render reports."""
    if wide_to_long and long_to_wide:
        raise click.ClickException("--wide-to-long and --long-to-wide are mutually exclusive.")

    dependent_vars = _parse_csv_list(dv)
    independent_vars = _parse_csv_list(iv)
    random_effects_list = _parse_csv_list(random_effects)

    output_dir.mkdir(parents=True, exist_ok=True)
    try:
        df = load_dataframe(
            str(input_path) if input_path else None,
            sheet=sheet,
            sql_uri=sql_uri,
            sql_query=sql_query,
        )
    except Exception as exc:
        raise click.ClickException(str(exc)) from exc

    _ensure_columns(df, independent_vars + random_effects_list, "Requested")
    if id_col and id_col not in df.columns:
        raise click.ClickException(f"--id-col '{id_col}' not found in data.")
    if time_col and time_col not in df.columns and (shape == "long" or long_to_wide):
        raise click.ClickException(f"--time-col '{time_col}' not found in data.")

    shape_details = detect_shape_details(df, id_col=id_col, time_col=time_col, dependent_vars=dependent_vars)
    detected_shape = shape if shape != "auto" else shape_details["shape"]
    shape_reason = shape_details["reason"] if shape == "auto" else f"Shape forced by user: {shape}."

    working_df = df.copy()
    working_dvs = list(dependent_vars)

    if wide_to_long or detected_shape == "wide":
        try:
            working_df = to_long(working_df, id_col=id_col, time_col=time_col, dependent_vars=dependent_vars)
        except Exception as exc:
            raise click.ClickException(f"Failed to reshape wide data to long: {exc}") from exc

        if len(working_dvs) > 1:
            matching_dvs = [column for column in dependent_vars if column in working_df.columns]
            protected = set(independent_vars + random_effects_list + [id_col, time_col])
            working_dvs = matching_dvs[-1:] if matching_dvs else [column for column in working_df.columns if column not in protected][:1]
        elif len(working_dvs) == 1 and working_dvs[0] not in working_df.columns:
            protected = set(independent_vars + random_effects_list + [id_col, time_col])
            candidates = [column for column in working_df.columns if column not in protected]
            working_dvs = candidates[-1:] if candidates else working_dvs

    if long_to_wide:
        if not id_col or not time_col or not working_dvs:
            raise click.ClickException("--long-to-wide requires --id-col, --time-col, and at least one --dv.")
        try:
            working_df = to_wide(working_df, id_col=id_col, time_col=time_col, value_col=working_dvs[0])
        except Exception as exc:
            raise click.ClickException(f"Failed to reshape long data to wide: {exc}") from exc

    _ensure_columns(working_df, working_dvs, "Dependent variable")

    repeated_id = bool(id_col and id_col in working_df.columns and working_df[id_col].duplicated().any())
    eda_summary = summarize_dependents(working_df, working_dvs)
    first_summary = eda_summary[working_dvs[0]]

    decision = resolve_decision(
        DecisionInputs(
            dependent_vars=working_dvs,
            independent_vars=independent_vars,
            random_effects=random_effects_list,
            id_col=id_col,
            repeated_id=repeated_id,
            distribution_hint=first_summary["distribution"],
            normality_ok=bool(first_summary["normality"].get("normal")),
            suggested_transform=first_summary["transform"]["selected"],
            positive_right_skewed=_positive_right_skewed(first_summary),
            zero_inflated=_zero_inflated_continuous(first_summary),
            ordinal_like=bool(first_summary["summary"].get("ordinal_like")),
            transforms_failed=_all_transforms_failed(first_summary),
            sample_size=int(first_summary["summary"].get("n") or 0),
            nonlinear_form=nonlinear_form,
            nonlinear_formula=nonlinear_formula,
            nonlinear_flag=model_family == "nonlinear",
            model_family_override=model_family,
            distribution_override=distribution,
            transform_override=transform_name,
            formula_override=formula,
            time_col=time_col,
            dv_summary=first_summary["summary"],
        )
    )

    data_for_r = output_dir / "data_for_r.csv"
    spec_path = output_dir / "analysis_spec.json"
    working_df.to_csv(data_for_r, index=False)

    spec = AnalysisSpec(
        input_csv=str(data_for_r),
        output_dir=str(output_dir),
        dependent_vars=working_dvs,
        independent_vars=independent_vars,
        random_effects=random_effects_list,
        id_col=id_col,
        model_family=decision.model_family,
        distribution=decision.distribution,
        transformation=decision.transformation,
        formula=decision.formula,
        nonlinear=NonlinearSpec(form=nonlinear_form, formula=nonlinear_formula, start=None),
        posthoc=PosthocSpec(factors=_parse_csv_list(posthoc_factors), correction=posthoc_correction, auto=not no_auto_posthoc),
        alpha=alpha,
        make_plots=not no_plots,
        candidate_families=decision.candidate_families,
        compare_terms=not no_term_comparison,
    )
    spec.to_json(spec_path)

    decision_info = {
        "shape_used": detected_shape,
        "shape_rationale": shape_reason,
        "resolved_model_family": decision.model_family,
        "resolved_distribution": decision.distribution,
        "resolved_transformation": decision.transformation,
        "candidate_families": decision.candidate_families,
        "rationale": list(decision.rationale),
    }

    click.echo("Decision summary:")
    click.echo(f"- Shape used: {detected_shape}")
    click.echo(f"- Shape rationale: {shape_reason}")
    click.echo(f"- Resolved model family: {decision.model_family}")
    click.echo(f"- Resolved distribution: {decision.distribution}")
    click.echo(f"- Resolved transformation: {decision.transformation}")
    click.echo(f"- Formula: {decision.formula}")
    click.echo(f"- Candidate families: {', '.join(decision.candidate_families)}")
    click.echo("- Decision rationale:")
    for item in decision.rationale:
        click.echo(f"  * {item}")
    click.echo(f"- Wrote analysis spec: {spec_path}")
    click.echo(f"- Wrote data for R: {data_for_r}")
    click.echo("- EDA summary:")
    click.echo(json.dumps(eda_summary, indent=2, default=str))

    if dry_run:
        click.echo("Dry run requested; skipping R invocation and report rendering.")
        return

    results = run_r_analysis(spec_path, r_path=r_path)
    results["decision"] = decision_info
    results["eda_summary"] = eda_summary
    (output_dir / "results.json").write_text(json.dumps(results, indent=2, default=str), encoding="utf-8")
    render_reports(results, output_dir, report_format=report_format)
    click.echo(f"Results written to {output_dir}")


if __name__ == "__main__":
    main()
