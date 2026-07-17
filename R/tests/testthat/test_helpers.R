test_that("new_results matches the contract shape", {
  res <- new_results(list(schema_version = "1.0", formula = "y ~ x"))
  expect_equal(res$schema_version, "1.0")
  expect_equal(res$formula, "y ~ x")
  expect_true(all(c("anova_table", "coefficients", "diagnostic_plots", "warnings", "errors") %in% names(res)))
  expect_equal(length(res$diagnostic_plots), 0)
})

test_that("append_result_message appends messages in order", {
  res <- new_results()
  res <- append_result_message(res, "warnings", "first")
  res <- append_result_message(res, "warnings", "second")
  expect_equal(unlist(res$warnings), c("first", "second"))
})

test_that("strip_random_effects removes mixed terms cleanly", {
  input <- "y ~ group * time + (1 | subject) + (1|batch)"
  expect_equal(strip_random_effects(input), "y ~ group * time")
})

test_that("replace_formula_lhs swaps the dependent variable", {
  expect_equal(replace_formula_lhs("y ~ group + time", "z"), "z ~ group + time")
})

test_that("build_manova_formula uses cbind on dependent vars", {
  spec <- list(
    dependent_vars = c("y1", "y2"),
    formula = "y1 ~ group * time"
  )
  expect_equal(format(build_manova_formula(spec)), "cbind(y1, y2) ~ group * time")
})

test_that("manual_cohens_d falls back from sigma to se", {
  expect_equal(manual_cohens_d(estimate = 4, sigma_value = 2, se_value = 0.5), 2)
  expect_equal(manual_cohens_d(estimate = 4, sigma_value = NA_real_, se_value = 2), 2)
})

test_that("kendalls_w_from_friedman matches the standard formula", {
  expect_equal(kendalls_w_from_friedman(statistic = 6, n_blocks = 6, k_levels = 3), 0.5)
})

test_that("kruskal_epsilon_squared returns a bounded effect size", {
  expect_equal(kruskal_epsilon_squared(statistic = 8.5, n_obs = 24, k_levels = 3), (8.5 - 3 + 1) / (24 - 3))
})

test_that("rank_biserial_effect_size is defined for two groups", {
  values <- c(1, 2, 3, 7, 8, 9)
  groups <- factor(c("A", "A", "A", "B", "B", "B"))
  expect_true(is.finite(rank_biserial_effect_size(values, groups)))
})

test_that("detect_significant_terms excludes intercept and honors alpha", {
  anova_table <- list(
    list(term = "(Intercept)", p = 0.0001),
    list(term = "group", p = 0.03),
    list(term = "time", p = 0.2),
    list(term = "group:time", p = 0.001)
  )
  sig <- detect_significant_terms(anova_table, alpha = 0.05)
  labels <- vapply(sig, function(x) x$term, character(1L))
  expect_setequal(labels, c("group", "group:time"))
})

test_that("detect_significant_terms returns nothing for an empty anova table", {
  expect_equal(detect_significant_terms(list(), alpha = 0.05), list())
})

test_that("posthoc_factors_for_term splits interaction terms and drops non-factor/absent columns", {
  data <- data.frame(group = factor(c("A", "B")), time = factor(c("0", "1")), x = c(1.0, 2.0))
  expect_equal(posthoc_factors_for_term("group:time", data), c("group", "time"))
  expect_equal(posthoc_factors_for_term("group:x", data), "group")
  expect_equal(posthoc_factors_for_term("missing_col", data), character())
})

test_that("add_aic_weights computes delta_aic and weights relative to the best model", {
  comparison <- list(list(family = "a", aic = 100), list(family = "b", aic = 102), list(family = "c", aic = NULL))
  out <- add_aic_weights(comparison)
  expect_equal(out[[1]]$delta_aic, 0)
  expect_equal(out[[2]]$delta_aic, 2)
  expect_true(out[[1]]$aic_weight > out[[2]]$aic_weight)
  expect_null(out[[3]]$delta_aic)
})

test_that("fixed_effect_terms excludes random-effect groups", {
  expect_equal(fixed_effect_terms("y ~ group * time + (1 | subject)"), c("group", "time", "group:time"))
})

test_that("fixed_effect_terms returns empty for a formula with no fixed terms", {
  expect_equal(fixed_effect_terms(""), character())
})

test_that("term_drop_comparison refits a glm fitted inside a function without 'object not found' errors", {
  # Regression test: stats::update() re-evaluates the model's stored call in
  # the caller's frame, not the frame it was originally fitted in. If the
  # model was built inside a helper function using local variables named
  # `data`/`distribution` (as fit_glm_model does), update() will fail to
  # resolve those symbols unless term_drop_comparison forwards `data` and
  # `family` explicitly.
  fit_like_pipeline <- function(data) {
    distribution <- "gaussian"
    stats::glm(mpg ~ hp * wt, data = data, family = stats::gaussian())
  }
  model <- fit_like_pipeline(mtcars)
  spec <- list(formula = "mpg ~ hp * wt")

  out <- term_drop_comparison(model, spec, data = mtcars)
  expect_null(out$warning)
  expect_equal(length(out$records), 3L)
  terms_seen <- vapply(out$records, function(x) x$term, character(1L))
  expect_setequal(terms_seen, c("hp", "wt", "hp:wt"))
  for (record in out$records) {
    expect_null(record$error)
    expect_true(is.numeric(record$aic))
  }
})

test_that("term_drop_comparison does not forward family= to a plain lm call", {
  fit_like_pipeline <- function(data) {
    stats::lm(mpg ~ hp * wt, data = data)
  }
  model <- fit_like_pipeline(mtcars)
  spec <- list(formula = "mpg ~ hp * wt")

  out <- term_drop_comparison(model, spec, data = mtcars)
  expect_null(out$warning)
  for (record in out$records) {
    expect_null(record$error)
  }
})

test_that("term_drop_comparison respects max_terms and returns a warning instead of erroring", {
  model <- stats::lm(mpg ~ hp * wt, data = mtcars)
  spec <- list(formula = "mpg ~ hp * wt")

  out <- term_drop_comparison(model, spec, data = mtcars, max_terms = 1L)
  expect_equal(length(out$records), 0L)
  expect_true(grepl("Skipped", out$warning))
})

test_that("is_categorical_column identifies factor/character/logical columns", {
  expect_true(is_categorical_column(factor(c("a", "b"))))
  expect_true(is_categorical_column(c("a", "b")))
  expect_true(is_categorical_column(c(TRUE, FALSE)))
  expect_false(is_categorical_column(c(1.0, 2.0)))
})

test_that("generate_eda_plots registers overall density, grouped density/violin, and scatter plots", {
  data <- data.frame(
    mpg = mtcars$mpg,
    hp = mtcars$hp,
    cyl_group = factor(ifelse(mtcars$cyl >= 6, "high", "low"))
  )
  spec <- list(dependent_vars = "mpg", independent_vars = c("cyl_group", "hp"))
  out_dir <- tempfile()

  results <- generate_eda_plots(spec, data, new_results(), out_dir)

  registered <- unlist(results$eda_plots)
  expect_true(any(grepl("eda_density_mpg\\.png$", registered)))
  expect_true(any(grepl("eda_density_mpg_by_cyl_group\\.png$", registered)))
  expect_true(any(grepl("eda_violin_mpg_by_cyl_group\\.png$", registered)))
  expect_true(any(grepl("eda_scatter_mpg_vs_hp\\.png$", registered)))
  expect_true(any(grepl("eda_panel\\.png$", registered)))
  for (path in registered) {
    expect_true(file.exists(file.path(out_dir, path)))
  }
})

test_that("generate_eda_plots skips grouping variables with too many or too few levels", {
  data <- data.frame(
    mpg = mtcars$mpg,
    id_like = factor(seq_len(nrow(mtcars))),
    constant_group = factor(rep("only_level", nrow(mtcars)))
  )
  spec <- list(dependent_vars = "mpg", independent_vars = c("id_like", "constant_group"))
  out_dir <- tempfile()

  results <- generate_eda_plots(spec, data, new_results(), out_dir)

  registered <- unlist(results$eda_plots)
  expect_false(any(grepl("id_like", registered)))
  expect_false(any(grepl("constant_group", registered)))
  expect_true(any(grepl("Skipped grouped EDA plots", unlist(results$warnings))))
})
