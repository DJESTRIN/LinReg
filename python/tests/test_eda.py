import numpy as np
import pandas as pd

from infer_auto.eda import detect_distribution, normality_test, suggest_transform, summarize_variable


def test_distribution_detection_variants() -> None:
    assert detect_distribution(pd.Series([0, 1, 0, 1])) == "binary"
    assert detect_distribution(pd.Series([0, 1, 2, 3, 4])) == "count"
    assert detect_distribution(pd.Series([0.1, 0.3, 0.7, 0.9])) == "proportion"
    assert detect_distribution(pd.Series([1.2, 2.5, 3.1, 4.8])) == "continuous"


def test_normality_uses_shapiro_for_small_samples() -> None:
    rng = np.random.default_rng(42)
    result = normality_test(pd.Series(rng.normal(size=100)))
    assert result["test"] == "shapiro"
    assert "skew" in result


def test_transform_suggestion_improves_right_skewed_series() -> None:
    rng = np.random.default_rng(7)
    skewed = pd.Series(rng.lognormal(mean=1.0, sigma=1.2, size=250))
    suggestion = suggest_transform(skewed)

    assert suggestion["selected"] in {"log", "sqrt", "boxcox", "yeojohnson", "none"}
    assert len(suggestion["evaluations"]) >= 2
    if suggestion["selected"] != "none":
        assert suggestion["after"]["normal"] or abs(suggestion["after"]["skew"]) <= abs(suggestion["before"]["skew"])


def test_summary_flags_zero_inflated_continuous_series() -> None:
    rng = np.random.default_rng(123)
    values = rng.gamma(shape=1.2, scale=4.0, size=200)
    values[:50] = 0
    summary = summarize_variable(pd.Series(values))

    assert summary["distribution"] == "continuous"
    assert summary["summary"]["non_negative"] is True
    assert summary["summary"]["zero_fraction"] >= 0.25


def test_summary_flags_ordinal_like_series() -> None:
    ordinal = pd.Series([1, 2, 3, 4, 5, 3, 2, 4, 5, 1, 2, 3])
    summary = summarize_variable(ordinal)

    assert summary["distribution"] == "count"
    assert summary["summary"]["ordinal_like"] is True
