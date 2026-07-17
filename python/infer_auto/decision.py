from __future__ import annotations

from dataclasses import dataclass, field
from typing import Any, Dict, List, Optional, Sequence


@dataclass
class DecisionInputs:
    dependent_vars: List[str]
    independent_vars: List[str] = field(default_factory=list)
    random_effects: List[str] = field(default_factory=list)
    id_col: Optional[str] = None
    repeated_id: bool = False
    distribution_hint: str = "continuous"
    normality_ok: bool = True
    suggested_transform: str = "none"
    positive_right_skewed: bool = False
    zero_inflated: bool = False
    ordinal_like: bool = False
    transforms_failed: bool = False
    sample_size: int = 0
    nonlinear_form: Optional[str] = None
    nonlinear_formula: Optional[str] = None
    nonlinear_flag: bool = False
    model_family_override: str = "auto"
    distribution_override: str = "auto"
    transform_override: str = "auto"
    formula_override: Optional[str] = None
    time_col: Optional[str] = None
    dv_summary: Dict[str, Any] = field(default_factory=dict)


@dataclass
class DecisionResult:
    model_family: str
    distribution: str
    transformation: str
    candidate_families: List[str]
    formula: str
    rationale: List[str]

    def to_dict(self) -> Dict[str, Any]:
        return {
            "model_family": self.model_family,
            "distribution": self.distribution,
            "transformation": self.transformation,
            "candidate_families": list(self.candidate_families),
            "formula": self.formula,
            "rationale": list(self.rationale),
        }


def _dedupe(values: Sequence[str]) -> List[str]:
    seen = set()
    result = []
    for value in values:
        if value and value not in seen:
            seen.add(value)
            result.append(value)
    return result


def build_formula(
    dependent_vars: Sequence[str],
    independent_vars: Sequence[str],
    *,
    random_effects: Optional[Sequence[str]] = None,
    repeated_id: bool = False,
    id_col: Optional[str] = None,
    formula_override: Optional[str] = None,
) -> str:
    if formula_override:
        return formula_override

    lhs = dependent_vars[0] if len(dependent_vars) == 1 else f"cbind({', '.join(dependent_vars)})"
    rhs = " * ".join(independent_vars) if independent_vars else "1"
    model_terms = [rhs]

    re_terms = _dedupe(list(random_effects or []) + ([id_col] if repeated_id and id_col else []))
    for term in re_terms:
        model_terms.append(f"(1 | {term})")
    return f"{lhs} ~ {' + '.join(model_terms)}"


def resolve_decision(inputs: DecisionInputs) -> DecisionResult:
    rationale: List[str] = []
    mixed = bool(inputs.random_effects) or (bool(inputs.id_col) and inputs.repeated_id)
    transformation = "none"
    distribution = "gaussian"
    small_sample = 0 < inputs.sample_size < 30
    non_negative = bool(inputs.dv_summary.get("non_negative", False))
    zero_fraction = float(inputs.dv_summary.get("zero_fraction") or 0.0)

    if inputs.transform_override != "auto":
        transformation = inputs.transform_override
        rationale.append(f"Transformation overridden by user: {transformation}.")
    elif inputs.suggested_transform and inputs.suggested_transform != "none":
        transformation = inputs.suggested_transform
        rationale.append(f"EDA suggested '{transformation}' to improve normality.")
    else:
        rationale.append("No transformation override or useful transform detected; using none.")

    if inputs.distribution_override != "auto":
        distribution = inputs.distribution_override
        rationale.append(f"Distribution overridden by user: {distribution}.")

    if inputs.model_family_override not in {"", "auto"}:
        model_family = inputs.model_family_override
        if inputs.distribution_override == "tweedie" and model_family in {"glm", "glmm"}:
            model_family = "tweedie"
            rationale.append("Tweedie override requires the dedicated glmmTMB path; promoting model family to tweedie.")
        rationale.append(f"Model family overridden by user: {model_family}.")
        if inputs.distribution_override == "auto":
            if model_family == "tweedie":
                distribution = "tweedie"
                rationale.append("Tweedie family override implies the Tweedie distribution.")
                if inputs.transform_override == "auto":
                    transformation = "none"
                    rationale.append("Tweedie models use the raw non-negative outcome, so the automatic transform suggestion was suppressed.")
            elif model_family in {"glm", "glmm"} and inputs.distribution_hint in {"binary", "count", "proportion"}:
                distribution = "binomial" if inputs.distribution_hint in {"binary", "proportion"} else "poisson"
        formula = build_formula(
            inputs.dependent_vars,
            inputs.independent_vars,
            random_effects=inputs.random_effects,
            repeated_id=inputs.repeated_id,
            id_col=inputs.id_col,
            formula_override=inputs.formula_override,
        )
        return DecisionResult(model_family, distribution, transformation, [model_family], formula, rationale)

    if inputs.nonlinear_form or inputs.nonlinear_formula or inputs.nonlinear_flag:
        rationale.append("Nonlinear form/formula was provided, so the nonlinear path takes precedence.")
        model_family = "nonlinear"
        formula = inputs.nonlinear_formula or build_formula(
            inputs.dependent_vars,
            inputs.independent_vars,
            random_effects=inputs.random_effects,
            repeated_id=inputs.repeated_id,
            id_col=inputs.id_col,
            formula_override=inputs.formula_override,
        )
        return DecisionResult(model_family, distribution, transformation, ["nonlinear"], formula, rationale)

    if len(inputs.dependent_vars) > 1:
        rationale.append("Multiple dependent variables were supplied; selecting the multivariate path.")
        formula = build_formula(
            inputs.dependent_vars,
            inputs.independent_vars,
            random_effects=inputs.random_effects,
            repeated_id=inputs.repeated_id,
            id_col=inputs.id_col,
            formula_override=inputs.formula_override,
        )
        return DecisionResult("manova", distribution, transformation, ["manova"], formula, rationale)

    if mixed:
        rationale.append("Random effects or repeated ids were detected; considering mixed-effects families.")

    prefer_tweedie = inputs.distribution_override == "tweedie" or (
        inputs.distribution_hint == "continuous"
        and non_negative
        and inputs.zero_inflated
        and (inputs.positive_right_skewed or inputs.transforms_failed or not inputs.normality_ok)
    )
    prefer_nonparametric = (
        inputs.transforms_failed
        and (small_sample or inputs.ordinal_like)
        and inputs.distribution_hint not in {"binary", "proportion"}
    )

    if prefer_tweedie:
        transformation = inputs.transform_override if inputs.transform_override != "auto" else "none"
        rationale.append(
            f"Detected a non-negative continuous outcome with {zero_fraction:.1%} exact zeros and persistent right skew; selecting Tweedie."
        )
        if inputs.transform_override == "auto":
            rationale.append("Suppressing the automatic transform suggestion because Tweedie modeling should use the original outcome scale.")
        formula = build_formula(
            inputs.dependent_vars,
            inputs.independent_vars,
            random_effects=inputs.random_effects,
            repeated_id=inputs.repeated_id,
            id_col=inputs.id_col,
            formula_override=inputs.formula_override,
        )
        return DecisionResult("tweedie", "tweedie", transformation, ["tweedie"], formula, rationale)

    if prefer_nonparametric:
        if inputs.ordinal_like:
            rationale.append("The dependent variable looks ordinal/rank-like and no transform restored normality; selecting a nonparametric test.")
        else:
            rationale.append("No tested transform achieved approximate normality for a small sample; selecting a nonparametric fallback.")
        formula = build_formula(
            inputs.dependent_vars,
            inputs.independent_vars,
            random_effects=inputs.random_effects,
            repeated_id=inputs.repeated_id,
            id_col=inputs.id_col,
            formula_override=inputs.formula_override,
        )
        return DecisionResult("nonparametric", distribution, transformation, ["nonparametric"], formula, rationale)

    if inputs.distribution_override == "auto":
        if inputs.distribution_hint in {"binary", "proportion"}:
            distribution = "binomial"
            rationale.append("Dependent variable looks binary/proportional, so binomial is appropriate.")
        elif inputs.distribution_hint == "count":
            mean_value = float(inputs.dv_summary.get("mean") or 0.0)
            variance = float(inputs.dv_summary.get("variance") or 0.0)
            distribution = "negbinom" if mean_value > 0 and variance > mean_value * 1.5 else "poisson"
            rationale.append(f"Dependent variable looks count-like; selected {distribution} based on mean/variance.")
        elif inputs.positive_right_skewed:
            distribution = "gamma"
            rationale.append("Positive right-skewed continuous outcome detected; gamma/log path is a good candidate.")
        else:
            distribution = "gaussian"
            rationale.append("Continuous approximately normal outcome detected; gaussian is appropriate.")

    if inputs.distribution_hint in {"binary", "proportion"}:
        model_family = "glmm" if mixed else "glm"
        candidate_families = [model_family]
    elif inputs.distribution_hint == "count":
        model_family = "glmm" if mixed else "glm"
        candidate_families = [model_family, "lmm" if mixed else "lm"]
    elif inputs.positive_right_skewed and transformation in {"none", "auto"}:
        model_family = "glmm" if mixed else "glm"
        candidate_families = [model_family, "lmm" if mixed else "lm"]
    else:
        model_family = "lmm" if mixed else "lm"
        candidate_families = [model_family, "lm"] if mixed else ["lm", "glm"]
        if inputs.normality_ok:
            rationale.append("Normality is acceptable for an LM-style model.")
        elif transformation != "none":
            rationale.append("Outcome is not normal, but a transform may make LM-style modeling viable.")
        else:
            rationale.append("Outcome is not clearly normalizable; retaining gaussian LM/LMM as the primary candidate.")

    formula = build_formula(
        inputs.dependent_vars,
        inputs.independent_vars,
        random_effects=inputs.random_effects,
        repeated_id=inputs.repeated_id,
        id_col=inputs.id_col,
        formula_override=inputs.formula_override,
    )

    return DecisionResult(model_family, distribution, transformation, _dedupe(candidate_families), formula, rationale)
