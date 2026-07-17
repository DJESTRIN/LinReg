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
    register_plot(results, output_dir, name, plot = plot_fn()),
    error = function(err) append_result_message(results, "warnings", sprintf("Skipped diagnostic plot '%s': %s", name, conditionMessage(err)))
  )
}

# All diagnostic figures are built with ggplot2 and the shared
# theme_publication() styling (see theme.R) so they are consistent and ready
# to drop directly into a manuscript/report without further editing.

plot_residuals_vs_fitted <- function(model) {
  df <- data.frame(fitted = as.numeric(extract_fitted(model)), resid = as.numeric(extract_residuals(model)))
  ggplot2::ggplot(df, ggplot2::aes(x = fitted, y = resid)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "firebrick", linewidth = 0.6) +
    ggplot2::geom_point(shape = 21, size = 2.2, fill = "#2C7FB8", color = "black", alpha = 0.85, stroke = 0.3) +
    ggplot2::geom_smooth(method = "loess", formula = y ~ x, se = FALSE, color = "grey30", linewidth = 0.6) +
    ggplot2::labs(title = "Residuals vs Fitted", x = "Fitted values", y = "Residuals") +
    theme_publication()
}

plot_qq <- function(model) {
  res <- stats::na.omit(as.numeric(extract_residuals(model)))
  df <- data.frame(sample = res)
  ggplot2::ggplot(df, ggplot2::aes(sample = sample)) +
    ggplot2::stat_qq_line(color = "firebrick", linetype = "dashed", linewidth = 0.6) +
    ggplot2::stat_qq(shape = 21, size = 2.2, fill = "#2C7FB8", color = "black", alpha = 0.85, stroke = 0.3) +
    ggplot2::labs(title = "Normal Q-Q", x = "Theoretical quantiles", y = "Sample quantiles") +
    theme_publication()
}

plot_scale_location <- function(model) {
  df <- data.frame(
    fitted = as.numeric(extract_fitted(model)),
    sqrt_std_resid = sqrt(abs(as.numeric(standardized_residuals(model))))
  )
  ggplot2::ggplot(df, ggplot2::aes(x = fitted, y = sqrt_std_resid)) +
    ggplot2::geom_point(shape = 21, size = 2.2, fill = "#2C7FB8", color = "black", alpha = 0.85, stroke = 0.3) +
    ggplot2::geom_smooth(method = "loess", formula = y ~ x, se = FALSE, color = "grey30", linewidth = 0.6) +
    ggplot2::labs(
      title = "Scale-Location",
      x = "Fitted values",
      y = expression(sqrt("|Standardized residuals|"))
    ) +
    theme_publication()
}

plot_cooks_distance <- function(model) {
  cooks <- as.numeric(stats::cooks.distance(model))
  idx <- seq_along(cooks)
  ggplot2::ggplot(data.frame(idx = idx, cooks = cooks), ggplot2::aes(x = idx, y = cooks)) +
    ggplot2::geom_segment(ggplot2::aes(xend = idx, y = 0, yend = cooks), color = "#2C7FB8", linewidth = 0.5) +
    ggplot2::labs(title = "Cook's Distance", x = "Observation index", y = "Cook's D") +
    theme_publication()
}

plot_leverage <- function(model) {
  lev <- as.numeric(stats::hatvalues(model))
  idx <- seq_along(lev)
  ggplot2::ggplot(data.frame(idx = idx, lev = lev), ggplot2::aes(x = idx, y = lev)) +
    ggplot2::geom_segment(ggplot2::aes(xend = idx, y = 0, yend = lev), color = "#D95F02", linewidth = 0.5) +
    ggplot2::labs(title = "Leverage", x = "Observation index", y = "Hat values") +
    theme_publication()
}

plot_influence <- function(model) {
  cooks_plot <- plot_cooks_distance(model)
  leverage_plot <- plot_leverage(model)

  if (requireNamespace("patchwork", quietly = TRUE)) {
    cooks_plot + leverage_plot
  } else {
    cooks_plot
  }
}

plot_vif <- function(vif_values) {
  df <- data.frame(term = names(vif_values), vif = as.numeric(vif_values))
  df$term <- factor(df$term, levels = df$term[order(df$vif)])
  ggplot2::ggplot(df, ggplot2::aes(x = term, y = vif)) +
    ggplot2::geom_col(fill = "#2C7FB8", color = "black", width = 0.6) +
    ggplot2::geom_hline(yintercept = 5, linetype = "dashed", color = "firebrick", linewidth = 0.6) +
    ggplot2::coord_flip() +
    ggplot2::labs(title = "Variance Inflation Factors", x = NULL, y = "VIF") +
    theme_publication()
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
    return(safe_register_diagnostic(
      results, output_dir, "vif",
      function() plot_vif(stats::setNames(collinearity$VIF, collinearity$Term))
    ))
  }

  vif_values <- tryCatch(car::vif(model), error = function(...) NULL)
  if (is.null(vif_values)) {
    return(results)
  }
  if (is.matrix(vif_values)) {
    vif_values <- vif_values[, 1L]
  }
  safe_register_diagnostic(results, output_dir, "vif", function() plot_vif(vif_values))
}

# Combine the core diagnostic panels (residuals vs fitted, Q-Q, scale-
# location, and influence when available) into a single multi-panel figure
# via patchwork, in addition to registering each panel individually. This
# gives a one-glance publication-ready diagnostic figure alongside the
# per-plot images used in the report.
generate_combined_panel <- function(model, results, output_dir) {
  if (!requireNamespace("patchwork", quietly = TRUE)) {
    return(results)
  }
  panels <- list(
    tryCatch(plot_residuals_vs_fitted(model), error = function(...) NULL),
    tryCatch(plot_qq(model), error = function(...) NULL),
    tryCatch(plot_scale_location(model), error = function(...) NULL)
  )
  if (inherits(model, "lm") || inherits(model, "glm")) {
    panels <- c(
      panels,
      list(
        tryCatch(plot_cooks_distance(model), error = function(...) NULL),
        tryCatch(plot_leverage(model), error = function(...) NULL)
      )
    )
  }
  panels <- Filter(Negate(is.null), panels)
  if (length(panels) == 0L) {
    return(results)
  }
  combined <- tryCatch(
    patchwork::wrap_plots(panels, ncol = 2) +
      patchwork::plot_annotation(title = "Model diagnostics"),
    error = function(...) NULL
  )
  if (is.null(combined)) {
    return(results)
  }
  safe_register_diagnostic(results, output_dir, "diagnostic_panel", function() combined)
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

  results <- generate_vif_plot(model, spec, data, results, output_dir)
  generate_combined_panel(model, results, output_dir)
}
