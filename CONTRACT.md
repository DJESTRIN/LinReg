# LinReg Python↔R Contract

LinReg is a CLI tool: `linreg run --input data.csv [flags...]`. Python is the
orchestrator (CLI, data ingestion, reshaping, EDA, model-family decision,
report rendering). R is the statistics engine (model fitting, diagnostics,
post-hoc tests, plots). They communicate through two JSON files on disk —
no rpy2, no shared process. This keeps both sides independently testable and
Docker-friendly (`Rscript` is just invoked as a subprocess).

```
Python CLI  --writes-->  <outdir>/analysis_spec.json
Python CLI  --invokes-->  Rscript R/run_analysis.R --config <outdir>/analysis_spec.json
R engine    --writes-->  <outdir>/results.json  + <outdir>/plots/*.png + <outdir>/data_for_r.csv (echo of data used)
Python CLI  --reads-->   <outdir>/results.json --> renders <outdir>/report.html and report.md
```

## Repo layout

```
LinReg/
  python/
    pyproject.toml
    linreg_auto/
      __init__.py
      cli.py            # click-based CLI, entry point `linreg`
      io_data.py         # load csv/xlsx/sql/parquet -> pandas DataFrame
      reshape.py          # wide<->long, detect id/time/measure columns
      eda.py              # normality tests, transform suggestion, distribution detection
      decision.py         # model family decision engine
      spec.py             # AnalysisSpec dataclass + JSON (de)serialization (the schema below)
      rbridge.py           # subprocess invocation of Rscript, timeout/error handling
      report.py            # renders results.json -> markdown/html report w/ embedded plots
    tests/
      test_reshape.py, test_eda.py, test_decision.py, test_cli_smoke.py
  R/
    run_analysis.R        # entrypoint: reads --config, dispatches to fit_*, writes results.json
    R/io_json.R            # read spec, write results, helpers to base64/copy plots
    R/model_fit.R          # lm, glm, lmer, glmer, glmmTMB dispatch
    R/nonlinear.R           # nls / SSlogis / SSmicmen / nlme
    R/multivariate.R        # manova, mvabund-style multiple-DV mixed models
    R/diagnostics.R         # residual/QQ/leverage/Cook's D/VIF plots, shapiro on residuals
    R/posthoc.R             # emmeans, pairwise contrasts, effect sizes (cohen's d, eta^2)
    install_packages.R      # installs pinned CRAN packages (idempotent, used by Docker build)
    tests/testthat.R + tests/testthat/*   # testthat unit tests for pure helper functions
  docker/
    Dockerfile
    docker-compose.yml
  examples/
    mtcars.csv, repeated_measures_synthetic.csv, counts_synthetic.csv,
    zero_inflated_tweedie_synthetic.csv, nonparametric_synthetic.csv
  CONTRACT.md
  README.md
```

## CLI flags (Python, `linreg run`)

| flag | description |
|---|---|
| `--input PATH` | csv/xlsx/parquet path, or `--sql-uri` for a DB connection |
| `--sheet NAME` | xlsx sheet name |
| `--sql-uri` / `--sql-query` | alternative to `--input` |
| `--output-dir DIR` | default `./linreg_results` |
| `--dv NAME[,NAME...]` | dependent variable(s); >1 triggers multivariate path |
| `--iv NAME[,NAME...]` | independent/fixed-effect variables |
| `--random-effects NAME[,NAME...]` | grouping variables for mixed models (e.g. subject) |
| `--id-col NAME` | subject/unit identifier, used for repeated-measures detection |
| `--time-col NAME` | time/within-subject variable, for wide<->long reshape |
| `--shape {wide,long,auto}` | input shape; auto-detected if omitted |
| `--wide-to-long` / `--long-to-wide` | force a reshape and emit the reshaped csv |
| `--model-family {auto,lm,glm,lmm,glmm,tweedie,nonparametric,nonlinear,manova}` | default `auto` |
| `--nonlinear-form {logistic,exponential,michaelis_menten,custom}` + `--nonlinear-formula` | for nonlinear path |
| `--distribution {auto,gaussian,poisson,binomial,gamma,negbinom,tweedie}` | glm family override |
| `--transform {auto,none,log,sqrt,boxcox,yeojohnson}` | DV transform override |
| `--formula "y ~ x1*x2 + (1|subject)"` | full override of auto-built formula |
| `--alpha FLOAT` | significance threshold, default 0.05 |
| `--posthoc-correction {bonferroni,tukey,holm,none}` | default `bonferroni` |
| `--report {html,md,both}` | default `both` |
| `--no-plots` | skip diagnostic plot generation |
| `--r-path PATH` | override `Rscript` executable location |
| `--dry-run` | build spec + formula, print decision rationale, skip R invocation |

## analysis_spec.json (Python -> R)

```jsonc
{
  "schema_version": "1.0",
  "input_csv": "<outdir>/data_for_r.csv",   // always CSV, always the shape R should fit on (long format)
  "output_dir": "<outdir>",
  "dependent_vars": ["y"],                   // list; len>1 => multivariate
  "independent_vars": ["group", "time"],
  "random_effects": ["subject"],              // empty => no mixed model
  "id_col": "subject",
  "model_family": "lmm",                      // resolved; enum: lm/glm/lmm/glmm/tweedie/nonparametric/nonlinear/manova
  "distribution": "gaussian",                 // resolved; enum: gaussian/poisson/binomial/gamma/negbinom/tweedie
  "transformation": "none",                   // resolved; R applies it defensively too
  "formula": "y ~ group * time + (1 | subject)",
  "nonlinear": { "form": "michaelis_menten", "formula": null, "start": null },
  "posthoc": { "factors": ["group"], "correction": "bonferroni" },
  "alpha": 0.05,
  "make_plots": true,
  "candidate_families": ["lmm", "lm"]         // for auto mode: R fits all, compares AIC, reports winner
}
```

## results.json (R -> Python)

```jsonc
{
  "schema_version": "1.0",
  "model_family_used": "lmm",
  "backbone_package": "lme4::lmer",
  "formula": "y ~ group * time + (1 | subject)",
  "convergence_ok": true,
  "distribution_parameters": { "tweedie_power": 1.43, "dispersion": 0.82 }, // optional, Tweedie/glmmTMB-specific
  "residual_normality": { "test": "shapiro.test", "W": 0.98, "p": 0.42 },
  "anova_table": [ { "term": "group", "df": 1, "F": 4.2, "p": 0.03 }, ... ],
  "coefficients": [ { "term": "(Intercept)", "estimate": 1.2, "se": 0.3, "p": 0.001 }, ... ],
  "r_squared": { "marginal": 0.31, "conditional": 0.55 },
  "aic": 245.6, "bic": 251.2, "logLik": -116.8,
  "model_comparison": [ { "family": "lmm", "aic": 245.6 }, { "family": "lm", "aic": 260.1 } ],
  "nonparametric_test": { "method": "stats::kruskal.test", "statistic": 7.4, "df": 2, "p": 0.025, "effect_size_metric": "epsilon_squared", "effect_size_value": 0.19 }, // optional
  "posthoc": [ { "contrast": "A - B", "estimate": 0.5, "p.adj": 0.02 }, ... ],
  "effect_sizes": [ { "term": "group", "metric": "cohen_d", "value": 0.6 }, ... ],
  "diagnostic_plots": ["plots/resid_vs_fitted.png", "plots/qq.png", "plots/cooksd.png", "plots/vif.png"],
  "warnings": [], "errors": []
}
```

Both sides must treat unknown/missing JSON fields leniently (forward-compatible).
Python's `rbridge.py` must capture R stdout/stderr and surface them if
`results.json` is missing or `errors` is non-empty. R scripts must never
`stop()` without first writing whatever partial `results.json` + warnings they
can, so the Python side always has something to report.

### Nonparametric results shape

For `model_family == "nonparametric"`, `results.json` may intentionally omit
`aic`, `bic`, `logLik`, `coefficients`, and `r_squared`. Instead it should
populate an optional `nonparametric_test` object:

```jsonc
{
  "nonparametric_test": {
    "method": "stats::kruskal.test",
    "statistic": 7.4,
    "df": 2,
    "p": 0.025,
    "effect_size_metric": "epsilon_squared",
    "effect_size_value": 0.19
  }
}
```

`posthoc` may still be present for multi-level factor tests, using pairwise
Wilcoxon comparisons with p-value adjustment.

## Model-family decision heuristics (Python `decision.py`)

1. `len(dependent_vars) > 1` -> `manova` (or mixed multivariate) candidate.
2. `random_effects` non-empty OR (`id_col` present AND repeated rows per id) -> mixed model (`lmm`/`glmm`).
3. DV numeric + roughly continuous + normal (or normalizable via transform) -> `lm`/`lmm`.
4. DV integer counts (non-negative, mostly whole numbers) -> `glm`/`glmm` with `poisson`/`negbinom`.
5. DV binary (0/1 or two-level factor) -> `glm`/`glmm` with `binomial`.
6. DV positive continuous & right-skewed -> try `gamma` glm or log-transform lm.
7. DV continuous, non-negative, zero-inflated, and still strongly right-skewed after transform attempts -> `tweedie`.
8. DV fails approximate normality after every transform and `(small n OR ordinal/rank-like)` -> `nonparametric`.
9. If `--nonlinear-form` given or user marks relationship nonlinear -> `nonlinear` (`nls`/`nlme`).
10. `auto` mode always populates `candidate_families` so R can fit >1 and compare AIC when that concept applies.

This file is the single source of truth for the JSON schema — both the
Python and R implementations, and their tests, must conform to it.
