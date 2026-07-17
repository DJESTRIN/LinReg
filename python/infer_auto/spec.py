from __future__ import annotations

import json
from dataclasses import asdict, dataclass, field
from pathlib import Path
from typing import Any, Dict, List, Optional, Union


@dataclass
class NonlinearSpec:
    form: Optional[str] = None
    formula: Optional[str] = None
    start: Optional[Dict[str, Any]] = None

    @classmethod
    def from_dict(cls, data: Optional[Dict[str, Any]]) -> "NonlinearSpec":
        data = data or {}
        return cls(form=data.get("form"), formula=data.get("formula"), start=data.get("start"))


@dataclass
class PosthocSpec:
    factors: List[str] = field(default_factory=list)
    correction: str = "bonferroni"
    auto: bool = True

    @classmethod
    def from_dict(cls, data: Optional[Dict[str, Any]]) -> "PosthocSpec":
        data = data or {}
        return cls(
            factors=list(data.get("factors") or []),
            correction=data.get("correction", "bonferroni"),
            auto=bool(data.get("auto", True)),
        )


@dataclass
class AnalysisSpec:
    schema_version: str = "1.0"
    input_csv: str = ""
    output_dir: str = ""
    dependent_vars: List[str] = field(default_factory=list)
    independent_vars: List[str] = field(default_factory=list)
    random_effects: List[str] = field(default_factory=list)
    id_col: Optional[str] = None
    model_family: str = "lm"
    distribution: str = "gaussian"
    transformation: str = "none"
    formula: str = ""
    nonlinear: NonlinearSpec = field(default_factory=NonlinearSpec)
    posthoc: PosthocSpec = field(default_factory=PosthocSpec)
    alpha: float = 0.05
    make_plots: bool = True
    candidate_families: List[str] = field(default_factory=list)
    compare_terms: bool = True

    def to_dict(self) -> Dict[str, Any]:
        payload = asdict(self)
        payload["nonlinear"] = asdict(self.nonlinear)
        payload["posthoc"] = asdict(self.posthoc)
        return payload

    def to_json(self, path: Optional[Union[str, Path]] = None) -> str:
        payload = json.dumps(self.to_dict(), indent=2)
        if path is not None:
            Path(path).write_text(payload, encoding="utf-8")
        return payload

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "AnalysisSpec":
        data = data or {}
        return cls(
            schema_version=data.get("schema_version", "1.0"),
            input_csv=data.get("input_csv", ""),
            output_dir=data.get("output_dir", ""),
            dependent_vars=list(data.get("dependent_vars") or []),
            independent_vars=list(data.get("independent_vars") or []),
            random_effects=list(data.get("random_effects") or []),
            id_col=data.get("id_col"),
            model_family=data.get("model_family", "lm"),
            distribution=data.get("distribution", "gaussian"),
            transformation=data.get("transformation", "none"),
            formula=data.get("formula", ""),
            nonlinear=NonlinearSpec.from_dict(data.get("nonlinear")),
            posthoc=PosthocSpec.from_dict(data.get("posthoc")),
            alpha=float(data.get("alpha", 0.05)),
            make_plots=bool(data.get("make_plots", True)),
            candidate_families=list(data.get("candidate_families") or []),
            compare_terms=bool(data.get("compare_terms", True)),
        )

    @classmethod
    def from_json(cls, path_or_text: Union[str, Path]) -> "AnalysisSpec":
        try:
            candidate = Path(path_or_text)
            if candidate.exists():
                return cls.from_dict(json.loads(candidate.read_text(encoding="utf-8")))
        except OSError:
            pass
        return cls.from_dict(json.loads(str(path_or_text)))


@dataclass
class Results:
    schema_version: str = "1.0"
    model_family_used: str = ""
    backbone_package: str = ""
    formula: str = ""
    convergence_ok: bool = False
    distribution_parameters: Dict[str, Any] = field(default_factory=dict)
    residual_normality: Dict[str, Any] = field(default_factory=dict)
    anova_table: List[Dict[str, Any]] = field(default_factory=list)
    coefficients: List[Dict[str, Any]] = field(default_factory=list)
    r_squared: Dict[str, Any] = field(default_factory=dict)
    aic: Optional[float] = None
    bic: Optional[float] = None
    logLik: Optional[float] = None
    model_comparison: List[Dict[str, Any]] = field(default_factory=list)
    term_comparison: List[Dict[str, Any]] = field(default_factory=list)
    significant_terms: List[Dict[str, Any]] = field(default_factory=list)
    nonparametric_test: Dict[str, Any] = field(default_factory=dict)
    posthoc: List[Dict[str, Any]] = field(default_factory=list)
    effect_sizes: List[Dict[str, Any]] = field(default_factory=list)
    diagnostic_plots: List[str] = field(default_factory=list)
    eda_plots: List[str] = field(default_factory=list)
    warnings: List[str] = field(default_factory=list)
    errors: List[str] = field(default_factory=list)

    def to_dict(self) -> Dict[str, Any]:
        return asdict(self)

    def to_json(self, path: Optional[Union[str, Path]] = None) -> str:
        payload = json.dumps(self.to_dict(), indent=2)
        if path is not None:
            Path(path).write_text(payload, encoding="utf-8")
        return payload

    @classmethod
    def from_dict(cls, data: Dict[str, Any]) -> "Results":
        data = data or {}
        return cls(
            schema_version=data.get("schema_version", "1.0"),
            model_family_used=data.get("model_family_used", ""),
            backbone_package=data.get("backbone_package", ""),
            formula=data.get("formula", ""),
            convergence_ok=bool(data.get("convergence_ok", False)),
            distribution_parameters=dict(data.get("distribution_parameters") or {}),
            residual_normality=dict(data.get("residual_normality") or {}),
            anova_table=list(data.get("anova_table") or []),
            coefficients=list(data.get("coefficients") or []),
            r_squared=dict(data.get("r_squared") or {}),
            aic=data.get("aic"),
            bic=data.get("bic"),
            logLik=data.get("logLik"),
            model_comparison=list(data.get("model_comparison") or []),
            term_comparison=list(data.get("term_comparison") or []),
            significant_terms=list(data.get("significant_terms") or []),
            nonparametric_test=dict(data.get("nonparametric_test") or {}),
            posthoc=list(data.get("posthoc") or []),
            effect_sizes=list(data.get("effect_sizes") or []),
            diagnostic_plots=list(data.get("diagnostic_plots") or []),
            eda_plots=list(data.get("eda_plots") or []),
            warnings=list(data.get("warnings") or []),
            errors=list(data.get("errors") or []),
        )

    @classmethod
    def from_json(cls, path_or_text: Union[str, Path]) -> "Results":
        try:
            candidate = Path(path_or_text)
            if candidate.exists():
                return cls.from_dict(json.loads(candidate.read_text(encoding="utf-8")))
        except OSError:
            pass
        return cls.from_dict(json.loads(str(path_or_text)))
