import shutil
import uuid
from pathlib import Path

from infer_auto.report import render_reports


def _make_work_dir() -> Path:
    base = Path(__file__).resolve().parent / "_work"
    work_dir = base / str(uuid.uuid4())
    work_dir.mkdir(parents=True, exist_ok=True)
    return work_dir


def test_render_reports_includes_decisions_eda_and_plots() -> None:
    work_dir = _make_work_dir()
    try:
        results = {
            "model_family_used": "lm",
            "formula": "mpg ~ hp * wt",
            "backbone_package": "stats::lm",
            "convergence_ok": True,
            "aic": -43.9,
            "bic": -40.1,
            "logLik": 25.9,
            "r_squared": 0.83,
            "anova_table": [{"term": "hp", "p": 0.002}],
            "coefficients": [{"term": "hp", "estimate": -0.03}],
            "model_comparison": [{"family": "lm", "aic": -43.9, "delta_aic": 0}],
            "term_comparison": [{"term": "hp", "aic": -36.8}],
            "significant_terms": [{"term": "hp", "p": 0.002}],
            "posthoc": [{"factor": "hp", "contrast": "a - b", "trigger": "significant"}],
            "effect_sizes": [],
            "diagnostic_plots": ["plots/resid_vs_fitted.png", "plots/qq.png"],
            "eda_plots": ["plots/eda_density_mpg.png", "plots/eda_scatter_mpg_vs_hp.png"],
            "warnings": ["a warning"],
            "errors": [],
            "decision": {
                "shape_used": "wide",
                "shape_rationale": "No id/time columns detected.",
                "resolved_model_family": "lm",
                "resolved_distribution": "gaussian",
                "resolved_transformation": "none",
                "candidate_families": ["lm", "glm"],
                "rationale": ["Normality is acceptable for an LM-style model."],
            },
            "eda_summary": {
                "mpg": {
                    "distribution": "continuous",
                    "normality": {"normal": True, "p": 0.4},
                    "transform": {"selected": "none"},
                    "summary": {"n": 32, "mean": 20.1},
                }
            },
        }

        outputs = render_reports(results, work_dir, report_format="both")
        html_text = Path(outputs["html"]).read_text(encoding="utf-8")
        md_text = Path(outputs["md"]).read_text(encoding="utf-8")

        for text in (html_text, md_text):
            assert "Modeling decisions" in text
            assert "No id/time columns detected." in text
            assert "Normality is acceptable for an LM-style model." in text
            assert "Exploratory data analysis" in text
            assert "mpg" in text

        assert "plot-grid" in html_text
        assert "resid_vs_fitted.png" in html_text
        assert "eda_density_mpg.png" in html_text
        assert "eda_scatter_mpg_vs_hp.png" in html_text
    finally:
        shutil.rmtree(work_dir, ignore_errors=True)


def test_render_reports_handles_missing_decision_and_eda() -> None:
    work_dir = _make_work_dir()
    try:
        results = {
            "model_family_used": "lm",
            "formula": "y ~ x",
            "diagnostic_plots": [],
            "warnings": [],
            "errors": [],
        }
        outputs = render_reports(results, work_dir, report_format="both")
        html_text = Path(outputs["html"]).read_text(encoding="utf-8")
        assert "No decision log available." in html_text
        assert "No EDA summary available." in html_text
    finally:
        shutil.rmtree(work_dir, ignore_errors=True)
