<h1><b>LinReg: An automatic statistics pipeline (Python + R)</b></h1>

LinReg turns a raw dataset into a full statistical analysis with a single CLI
command. Hand it a CSV/XLSX/SQL table and a set of flags describing your
variables, and it will:

1. **Ingest** the data (CSV, XLSX, SQL query, or Parquet).
2. **Reshape** it between wide and long format, auto-detecting the current
   shape from repeated IDs / time columns / suffixed value columns.
3. **Run EDA** — normality testing (Shapiro-Wilk / Anderson-Darling),
   skew/kurtosis, distribution typing (continuous / count / binary /
   proportion), and transform selection (log, sqrt, Box-Cox, Yeo-Johnson),
   each scored against the resulting normality.
4. **Decide a model family** — `lm`, `glm` (gaussian/poisson/binomial/gamma/
   negbinom), `lmm`/`glmm` (mixed models when repeated measures or explicit
   random effects are present), nonlinear regression (`nls`/`nlme`), or
   MANOVA/multivariate mixed models for multiple dependent variables — using
   an explainable rule-based decision engine (see rationale printed to
   stdout, and `CONTRACT.md`).
5. **Fit the model(s) in R**, run diagnostics (residuals-vs-fitted, Q-Q,
   Cook's distance, scale-location, VIF), post-hoc tests (`emmeans` pairwise
   contrasts with bonferroni/tukey/holm correction), and effect sizes
   (Cohen's d, eta-squared).
6. **Render a report** (`report.html` + `report.md`) with the formula used,
   ANOVA/coefficient tables, R², AIC/BIC, post-hoc results, effect sizes, and
   embedded diagnostic plots.

Python is the orchestrator (CLI, data wrangling, decision logic, reporting).
R is the statistics engine (model fitting, diagnostics, plots) — the two
communicate through a small JSON contract on disk, documented in full in
[`CONTRACT.md`](CONTRACT.md). That separation means each side is testable
independently and the whole thing runs identically on your laptop or in
Docker.

<h2><b>Quick start (Docker — recommended)</b></h2>

```powershell
docker compose -f docker/docker-compose.yml build
docker compose -f docker/docker-compose.yml run linreg run `
  --input /data/mtcars.csv --dv mpg --iv hp,wt --output-dir /results/mtcars
```

Mount your own dataset directory in place of `examples/` in
`docker/docker-compose.yml`, or run directly:

```powershell
docker build -f docker/Dockerfile -t linreg:latest .
docker run --rm -v ${PWD}/examples:/data -v ${PWD}/out:/results linreg:latest `
  run --input /data/mtcars.csv --dv mpg --iv hp,wt --output-dir /results/mtcars
```

<h2><b>Quick start (local Python + R)</b></h2>

Requires Python >= 3.9 and R >= 4.1 (`Rscript` on PATH, or pass `--r-path`).

```powershell
cd python
pip install -e .
Rscript ../R/install_packages.R   # one-time: installs lme4, glmmTMB, emmeans, car, etc.

linreg run --input ..\examples\mtcars.csv --dv mpg --iv hp,wt --output-dir ..\out\mtcars
```

Use `--dry-run` to see the decision engine's reasoning (shape detection,
resolved model family/distribution/transform, auto-built formula) without
invoking R at all — handy for sanity-checking a new dataset first.

<h2><b>Example: repeated-measures mixed model</b></h2>

```powershell
linreg run --input examples\repeated_measures_synthetic.csv `
  --dv y --iv group,time --id-col subject --random-effects subject `
  --output-dir out\repeated_measures
```

This resolves automatically to `lmm` (`lme4::lmer`, formula
`y ~ group * time + (1 | subject)`) because repeated rows per `subject` are
detected. See `CONTRACT.md` for the full decision-heuristics table.

<h2><b>Example: count data (GLM)</b></h2>

```powershell
linreg run --input examples\counts_synthetic.csv `
  --dv count --iv group,x --output-dir out\counts
```

The EDA step detects non-negative integer data and resolves to a Poisson
`glm` (falling back to negative-binomial via `MASS::glm.nb` if the data are
overdispersed).

<h2><b>Bonus capabilities</b></h2>

* **Nonlinear models**: `--model-family nonlinear --nonlinear-form
  {logistic,exponential,michaelis_menten,custom}` fits via `nls()`
  (or `nlme::nlme` when random effects are supplied).
* **Tweedie auto-detection**: `linreg run --input examples\zero_inflated_tweedie_synthetic.csv --dv y --iv group`
  routes zero-inflated, non-negative continuous outcomes to
  `glmmTMB::glmmTMB(family = tweedie())`.
* **Nonparametric fallback**: `linreg run --input examples\nonparametric_synthetic.csv --dv y --iv group`
  falls back to rank-based tests when small-sample outcomes stay non-normal after every transform.
* **Multiple dependent variables**: pass `--dv y1,y2,...` to trigger the
  MANOVA / per-DV mixed-model multivariate path in `R/R/multivariate.R`.
* **Auto model comparison**: in `--model-family auto` mode, R fits every
  candidate family the decision engine proposes and reports an AIC-ranked
  `model_comparison` table alongside the winning model.

<h2><b>Using LinReg from another repository</b></h2>

Since LinReg is invoked purely as a CLI (or a container), you can call it
from any other project without importing it as a library — either
`pip install -e path\to\LinReg\python` and call `linreg run ...`, or invoke
the Docker image with your data directory mounted, as shown above. Point
`--input` at whatever CSV/XLSX your other repo produces.

<h2><b>Repository layout</b></h2>

```
LinReg/
  python/        Python orchestrator package (linreg_auto) + tests
  R/              R statistics engine (model fitting, diagnostics, posthoc) + tests
  docker/         Dockerfile + docker-compose.yml
  examples/       Sample datasets (mtcars, repeated-measures, count data)
  CONTRACT.md     Full JSON schema + CLI flag reference + decision heuristics
```

<h2><b>Development</b></h2>

```powershell
# Python tests
cd python; pip install -e .[dev]; pytest tests -q

# R tests (fast, no model fitting)
Rscript R/tests/testthat.R
```
