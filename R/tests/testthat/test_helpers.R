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
