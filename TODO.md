# LinReg TODO

This file mirrors the working task list used to build and extend LinReg. It
is updated as work is requested and completed. "Done" items are kept for
history; new asks are added under "In progress / Pending" as they come in.

## In progress / Pending

_(none currently — all requested features implemented and validated)_

## Done

- [x] **Auto-trigger post-hoc tests on significant effects/interactions**
      R's `posthoc.R` now inspects the ANOVA table and automatically runs
      `emmeans` pairwise comparisons for any main effect or interaction
      significant at `alpha` (`detect_significant_terms`), including proper
      handling of interaction terms via `posthoc_factors_for_term` (pairwise
      contrasts across factor-combination cells). Explicit factors can still
      be forced via `--posthoc-factors`, independent of auto-detection
      (`--no-auto-posthoc` to disable). `results.json` gains a
      `significant_terms` field, and each post-hoc row records a `trigger`
      (`explicit`, `significant`, or `explicit_and_significant`). Documented
      in `CONTRACT.md`. Validated end-to-end against the repeated-measures
      example (`group:time` interaction, p=9.4e-10, correctly auto-triggers
      an interaction-aware post-hoc).
- [x] **Robust AIC-based model comparison for best-fit selection**
      Extended the family-vs-family AIC comparison with delta-AIC and Akaike
      weights (`add_aic_weights`), and added an efficient single-term-deletion
      AIC comparison (`term_drop_comparison`, conceptually like `drop1`,
      implemented via `update()` so it works uniformly across
      `lm`/`glm`/`lmer`/`glmer`/`glmmTMB`). Scales linearly with the number of
      fixed-effect terms (capped at `max_terms`, default 15, with a graceful
      skip + warning beyond that) rather than combinatorially, so it stays
      practical for large/complex models. `--no-term-comparison` disables it.
      Surfaced as a new "Term-drop AIC comparison" report section and
      documented in `CONTRACT.md`. Fixed a real R gotcha along the way:
      `update()` re-evaluates a model's stored call in the *caller's* frame,
      not the model's original fit environment, so `data`/`family` must be
      forwarded explicitly — covered by new regression tests in
      `test_helpers.R`.
- [x] **Write and maintain this TODO.md**
      Keep this file in sync with the session todo tracker so the task list
      is visible directly in the repo, not just in the working session.
- [x] Define the Python↔R JSON contract (`CONTRACT.md`): schema for
      `analysis_spec.json` / `results.json`, CLI flag reference, model-family
      decision heuristics.
- [x] Build the Python orchestrator (`python/linreg_auto`): CLI, data
      ingestion (csv/xlsx/sql/parquet), wide/long reshaping with shape
      auto-detection, EDA (normality tests, transform selection), the
      model-family decision engine, the R subprocess bridge, and HTML/
      Markdown report rendering.
- [x] Build the R statistics engine (`R/`): model fitting (lm, glm, lmm,
      glmm, nonlinear via nls/nlme, MANOVA/multivariate), diagnostics plots
      (residuals, Q-Q, Cook's distance, scale-location, VIF), `emmeans`
      post-hoc, effect sizes, AIC-based family comparison, JSON IO.
- [x] Dockerize the stack: `docker/Dockerfile` + `docker/docker-compose.yml`,
      built and verified end-to-end against `mtcars.csv`.
- [x] Clean up the legacy prototype: original `.r` files were superseded by
      the new `python/` + `R/` packages, then removed entirely from the repo
      per request.
- [x] Integration-test the pipeline end-to-end (local + Docker) against
      `mtcars.csv` and a synthetic repeated-measures dataset (linear + mixed
      model paths).
- [x] Write top-level `README.md`: usage, CLI examples, Docker instructions,
      architecture, bonus-capability docs.
- [x] Expand automatic distribution/model-family support beyond
      gaussian/poisson/binomial/gamma: added **Tweedie** (zero-inflated,
      right-skewed continuous data via `glmmTMB`) and a **non-parametric**
      fallback family (Wilcoxon/Kruskal-Wallis/Friedman/permutation tests)
      for data that can't be normalized or fit well parametrically, with new
      example datasets and end-to-end validation for both.

## How this list is maintained

- Requests are logged as rows in the session's SQL `todos` table as soon as
  they're made (so nothing is forgotten mid-task), then implemented.
- This `TODO.md` is refreshed to reflect that table whenever tasks are added
  or completed, so the plan is visible in the repo itself, not just in an
  ephemeral session.
