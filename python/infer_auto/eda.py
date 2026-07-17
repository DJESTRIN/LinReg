from __future__ import annotations

from dataclasses import dataclass
from typing import Any, Dict, Iterable, List, Optional

import numpy as np
import pandas as pd
from scipy import stats
from sklearn.preprocessing import PowerTransformer


@dataclass
class TransformEvaluation:
    transformation: str
    score: float
    normality: Dict[str, Any]
    metadata: Dict[str, Any]


def _clean_numeric(series: pd.Series) -> pd.Series:
    numeric = pd.to_numeric(series, errors="coerce").dropna()
    return numeric.astype(float)


def is_integer_like(series: pd.Series, tolerance: float = 1e-8) -> bool:
    numeric = _clean_numeric(series)
    if numeric.empty:
        return False
    return bool(np.all(np.isclose(numeric, np.round(numeric), atol=tolerance)))


def zero_fraction(series: pd.Series) -> float:
    numeric = _clean_numeric(series)
    if numeric.empty:
        return 0.0
    return float((numeric == 0).mean())


def is_ordinal_like(series: pd.Series, max_unique: int = 7) -> bool:
    numeric = _clean_numeric(series)
    if numeric.empty or not is_integer_like(numeric):
        return False
    unique_count = len(np.unique(numeric))
    unique_ratio = unique_count / max(len(numeric), 1)
    return bool(3 <= unique_count <= max_unique and unique_ratio <= 0.5)


def detect_distribution(series: pd.Series) -> str:
    numeric = _clean_numeric(series)
    if numeric.empty:
        return "continuous"

    unique_values = np.sort(numeric.unique())
    unique_ratio = len(unique_values) / max(len(numeric), 1)
    integer_dtype = pd.api.types.is_integer_dtype(series.dropna())
    if len(unique_values) <= 2:
        return "binary"
    if numeric.between(0, 1).all() and len(unique_values) > 2:
        return "proportion"
    if numeric.min() >= 0 and is_integer_like(numeric) and (integer_dtype or unique_ratio < 0.8):
        return "count"
    return "continuous"


def normality_test(series: pd.Series) -> Dict[str, Any]:
    numeric = _clean_numeric(series)
    n = len(numeric)
    if n < 3:
        return {
            "test": "insufficient_data",
            "n": n,
            "normal": False,
            "p": None,
            "statistic": None,
            "skew": None,
            "kurtosis": None,
        }

    skew = float(stats.skew(numeric, bias=False))
    kurtosis = float(stats.kurtosis(numeric, bias=False))
    if n < 5000:
        statistic, p_value = stats.shapiro(numeric)
        return {
            "test": "shapiro",
            "n": n,
            "statistic": float(statistic),
            "p": float(p_value),
            "normal": bool(p_value >= 0.05),
            "skew": skew,
            "kurtosis": kurtosis,
        }

    anderson = stats.anderson(numeric, dist="norm")
    significance_map = {level: crit for level, crit in zip(anderson.significance_level, anderson.critical_values)}
    critical_5 = float(significance_map.get(5.0, anderson.critical_values[2]))
    statistic = float(anderson.statistic)
    return {
        "test": "anderson",
        "n": n,
        "statistic": statistic,
        "p": None,
        "critical_value_5pct": critical_5,
        "normal": bool(statistic <= critical_5),
        "skew": skew,
        "kurtosis": kurtosis,
    }


def _normality_score(result: Dict[str, Any]) -> float:
    if result["test"] == "shapiro":
        return float(result.get("p") or 0.0)
    if result["test"] == "anderson":
        statistic = float(result.get("statistic") or np.inf)
        critical = float(result.get("critical_value_5pct") or 1.0)
        return -statistic / critical
    return float("-inf")


def _evaluate_transform(name: str, transformed: Iterable[float], metadata: Optional[Dict[str, Any]] = None) -> TransformEvaluation:
    normality = normality_test(pd.Series(list(transformed)))
    return TransformEvaluation(
        transformation=name,
        score=_normality_score(normality),
        normality=normality,
        metadata=metadata or {},
    )


def suggest_transform(series: pd.Series) -> Dict[str, Any]:
    numeric = _clean_numeric(series)
    if len(numeric) < 3:
        baseline = normality_test(numeric)
        return {"selected": "none", "before": baseline, "after": baseline, "evaluations": [{"transformation": "none"}]}

    evaluations: List[TransformEvaluation] = [_evaluate_transform("none", numeric)]

    if (numeric > 0).all():
        evaluations.append(_evaluate_transform("log", np.log(numeric)))
        evaluations.append(_evaluate_transform("sqrt", np.sqrt(numeric)))
        boxcox_values, lam = stats.boxcox(numeric)
        evaluations.append(_evaluate_transform("boxcox", boxcox_values, {"lambda": float(lam)}))

    yeo = PowerTransformer(method="yeo-johnson", standardize=False)
    yeo_values = yeo.fit_transform(numeric.to_numpy().reshape(-1, 1)).ravel()
    evaluations.append(_evaluate_transform("yeojohnson", yeo_values, {"lambda": float(yeo.lambdas_[0])}))

    baseline = evaluations[0]
    best = max(
        evaluations,
        key=lambda evaluation: (evaluation.score, evaluation.normality.get("normal", False), -abs(evaluation.normality.get("skew") or 0.0)),
    )

    selected = best.transformation
    baseline_skew = abs(baseline.normality.get("skew") or 0.0)
    best_skew = abs(best.normality.get("skew") or 0.0)
    improved = best.score > baseline.score + 0.02 or best_skew < baseline_skew - 0.1
    if best.transformation != "none" and not improved:
        best = baseline
        selected = "none"

    return {
        "selected": selected,
        "before": baseline.normality,
        "after": best.normality,
        "any_normal": any(evaluation.normality.get("normal", False) for evaluation in evaluations),
        "all_failed": not any(evaluation.normality.get("normal", False) for evaluation in evaluations),
        "evaluations": [
            {
                "transformation": evaluation.transformation,
                "score": evaluation.score,
                "normality": evaluation.normality,
                **({"metadata": evaluation.metadata} if evaluation.metadata else {}),
            }
            for evaluation in evaluations
        ],
    }


def summarize_variable(series: pd.Series) -> Dict[str, Any]:
    numeric = _clean_numeric(series)
    integer_like = is_integer_like(series)
    unique_count = int(len(np.unique(numeric))) if not numeric.empty else 0
    return {
        "distribution": detect_distribution(series),
        "normality": normality_test(series),
        "transform": suggest_transform(series),
        "summary": {
            "n": int(len(numeric)),
            "n_unique": unique_count,
            "mean": float(numeric.mean()) if not numeric.empty else None,
            "variance": float(numeric.var(ddof=1)) if len(numeric) > 1 else 0.0,
            "min": float(numeric.min()) if not numeric.empty else None,
            "max": float(numeric.max()) if not numeric.empty else None,
            "zero_fraction": zero_fraction(series),
            "non_negative": bool(not numeric.empty and numeric.min() >= 0),
            "integer_like": integer_like,
            "ordinal_like": is_ordinal_like(series),
        },
    }


def summarize_dependents(df: pd.DataFrame, dependent_vars: List[str]) -> Dict[str, Dict[str, Any]]:
    summaries = {}
    for dv in dependent_vars:
        if dv not in df.columns:
            raise ValueError(f"Dependent variable '{dv}' not found in data.")
        summaries[dv] = summarize_variable(df[dv])
    return summaries
