extract_residuals <- function(model) {
  stats::residuals(model)
}

extract_fitted <- function(model) {
  stats::fitted(model)
}

standardized_residuals <- function(model) {
  tryCatch(stats::rstandard(model), error = function(...) {
    res <- extract_residuals(model)
    res / stats::sd(res, na.rm = TRUE)
  })
}

residual_normality_test <- function(model) {
  res <- stats::na.omit(as.numeric(extract_residuals(model)))
  if (length(res) < 3L) {
    return(list(test = "skipped", reason = "Too few residuals"))
  }
  if (length(res) > 5000L) {
    out <- nortest::ad.test(res)
    return(list(test = "nortest::ad.test", statistic = unname(out$statistic), p = out$p.value))
  }
  out <- stats::shapiro.test(res)
  list(test = "shapiro.test", W = unname(out$statistic), p = out$p.value)
}

has_multiple_predictors <- function(model) {
  labels <- tryCatch(attr(stats::terms(model), "term.labels"), error = function(...) character())
  length(labels) > 1L
}

safe_register_diagnostic <- function(results, output_dir, name, plot_fn) {
  tryCatch(
    register_plot(results, output_dir, name, plot_expr = plot_fn),
    error = function(err) append_result_message(results, "warnings", sprintf("Skipped diagnostic plot '%s': %s", name, conditionMessage(err)))
  )
}

plot_residuals_vs_fitted <- function(model) {
  fitted_vals <- extract_fitted(model)
  residuals_vals <- extract_residuals(model)
  plot(fitted_vals, residuals_vals, xlab = "Fitted values", ylab = "Residuals", main = "Residuals vs Fitted")
  abline(h = 0, lty = 2, col = "red")
}

plot_qq <- function(model) {
  res <- extract_residuals(model)
  qqnorm(res, main = "Normal Q-Q")
  qqline(res, col = "red")
}

plot_scale_location <- function(model) {
  fitted_vals <- extract_fitted(model)
  std_res <- standardized_residuals(model)
  plot(fitted_vals, sqrt(abs(std_res)), xlab = "Fitted values", ylab = expression(sqrt("|Standardized residuals|")), main = "Scale-Location")
}

plot_influence <- function(model) {
  par(mfrow = c(1, 2))
  cooks <- stats::cooks.distance(model)
  lev <- stats::hatvalues(model)
  plot(cooks, type = "h", main = "Cook's Distance", ylab = "Cook's D")
  plot(lev, type = "h", main = "Leverage", ylab = "Hat values")
}

plot_vif_base <- function(vif_values) {
  barplot(vif_values, las = 2, ylab = "VIF", main = "Variance Inflation Factors")
  abline(h = 5, col = "red", lty = 2)
}

generate_vif_plot <- function(model, spec, data, results, output_dir) {
  if (!has_multiple_predictors(model)) {
    return(results)
  }

  if (inherits(model, "merMod") || inherits(model, "glmmTMB")) {
    if (!requireNamespace("performance", quietly = TRUE)) {
      return(results)
    }
    collinearity <- tryCatch(performance::check_collinearity(model), error = function(...) NULL)
    if (is.null(collinearity) || !"VIF" %in% names(collinearity)) {
      return(results)
    }
    return(register_plot(
      results,
      output_dir,
      "vif",
      plot_expr = function() plot_vif_base(stats::setNames(collinearity$VIF, collinearity$Term))
    ))
  }

  vif_values <- tryCatch(car::vif(model), error = function(...) NULL)
  if (is.null(vif_values)) {
    return(results)
  }
  if (is.matrix(vif_values)) {
    vif_values <- vif_values[, 1L]
  }
  register_plot(
    results,
    output_dir,
    "vif",
    plot_expr = function() plot_vif_base(vif_values)
  )
}

generate_diagnostics <- function(model, spec, data, results, output_dir) {
  normality <- tryCatch(residual_normality_test(model), error = function(err) err)
  if (inherits(normality, "error")) {
    results <- append_result_message(results, "warnings", sprintf("Residual normality test skipped: %s", conditionMessage(normality)))
  } else {
    results$residual_normality <- normality
  }

  results <- safe_register_diagnostic(results, output_dir, "resid_vs_fitted", function() plot_residuals_vs_fitted(model))
  results <- safe_register_diagnostic(results, output_dir, "qq", function() plot_qq(model))
  results <- safe_register_diagnostic(results, output_dir, "scale_location", function() plot_scale_location(model))

  if (inherits(model, "lm") || inherits(model, "glm")) {
    results <- safe_register_diagnostic(results, output_dir, "influence", function() plot_influence(model))
  }

  generate_vif_plot(model, spec, data, results, output_dir)
}
