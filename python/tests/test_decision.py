from linreg_auto.decision import DecisionInputs, build_formula, resolve_decision


def test_build_formula_includes_random_intercept() -> None:
    formula = build_formula(["y"], ["group", "time"], random_effects=["subject"])
    assert formula == "y ~ group * time + (1 | subject)"


def test_resolve_decision_prefers_lm_for_normal_continuous_data() -> None:
    decision = resolve_decision(
        DecisionInputs(
            dependent_vars=["y"],
            independent_vars=["x1", "x2"],
            distribution_hint="continuous",
            normality_ok=True,
            suggested_transform="none",
            dv_summary={"mean": 10, "variance": 2},
        )
    )

    assert decision.model_family == "lm"
    assert decision.distribution == "gaussian"
    assert decision.candidate_families[0] == "lm"


def test_resolve_decision_uses_lmm_for_repeated_measure_data() -> None:
    decision = resolve_decision(
        DecisionInputs(
            dependent_vars=["score"],
            independent_vars=["group", "time"],
            id_col="subject",
            repeated_id=True,
            distribution_hint="continuous",
            normality_ok=True,
            dv_summary={"mean": 10, "variance": 2},
        )
    )

    assert decision.model_family == "lmm"
    assert "(1 | subject)" in decision.formula


def test_resolve_decision_uses_glm_for_binary_outcome() -> None:
    decision = resolve_decision(
        DecisionInputs(
            dependent_vars=["response"],
            independent_vars=["dose"],
            distribution_hint="binary",
            normality_ok=False,
            dv_summary={"mean": 0.4, "variance": 0.2},
        )
    )

    assert decision.model_family == "glm"
    assert decision.distribution == "binomial"


def test_resolve_decision_prefers_tweedie_for_zero_inflated_continuous_data() -> None:
    decision = resolve_decision(
        DecisionInputs(
            dependent_vars=["rainfall"],
            independent_vars=["group"],
            distribution_hint="continuous",
            normality_ok=False,
            suggested_transform="yeojohnson",
            positive_right_skewed=True,
            zero_inflated=True,
            transforms_failed=True,
            sample_size=80,
            dv_summary={"mean": 3.5, "variance": 15.0, "non_negative": True, "zero_fraction": 0.24},
        )
    )

    assert decision.model_family == "tweedie"
    assert decision.distribution == "tweedie"
    assert decision.transformation == "none"
    assert decision.candidate_families == ["tweedie"]


def test_resolve_decision_prefers_nonparametric_when_transforms_fail_for_small_sample() -> None:
    decision = resolve_decision(
        DecisionInputs(
            dependent_vars=["score"],
            independent_vars=["group"],
            distribution_hint="continuous",
            normality_ok=False,
            suggested_transform="yeojohnson",
            transforms_failed=True,
            sample_size=18,
            dv_summary={"mean": 12, "variance": 10, "non_negative": True},
        )
    )

    assert decision.model_family == "nonparametric"
    assert decision.candidate_families == ["nonparametric"]
