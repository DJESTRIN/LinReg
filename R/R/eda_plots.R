# Exploratory data analysis (EDA) plots. Unlike diagnostics.R (which
# visualizes a fitted model's residuals/influence), these functions
# visualize the *raw* data before/independent of model fitting: overall
# kernel density of each dependent variable, density/violin plots split by
# each categorical main effect, and scatter plots against each continuous
# main effect. All built with ggplot2 + theme_publication() for consistency
# with the rest of the report.

is_categorical_column <- function(column) {
  is.factor(column) || is.character(column) || is.logical(column)
}

plot_density_overall <- function(data, dv) {
  df <- data.frame(value = as.numeric(data[[dv]]))
  ggplot2::ggplot(df, ggplot2::aes(x = value)) +
    ggplot2::geom_density(fill = "#2C7FB8", alpha = 0.55, color = "black", linewidth = 0.6) +
    ggplot2::geom_rug(alpha = 0.4, color = "#2C7FB8") +
    ggplot2::labs(title = sprintf("Distribution of %s", dv), x = dv, y = "Density") +
    theme_publication()
}

plot_density_by_group <- function(data, dv, group) {
  df <- data.frame(value = as.numeric(data[[dv]]), group = as.factor(data[[group]]))
  n_levels <- nlevels(df$group)
  palette <- rep_len(publication_palette, n_levels)
  ggplot2::ggplot(df, ggplot2::aes(x = value, fill = group, color = group)) +
    ggplot2::geom_density(alpha = 0.45, linewidth = 0.6) +
    ggplot2::scale_fill_manual(values = palette) +
    ggplot2::scale_color_manual(values = palette) +
    ggplot2::labs(
      title = sprintf("Distribution of %s by %s", dv, group),
      x = dv, y = "Density", fill = group, color = group
    ) +
    theme_publication()
}

plot_violin_by_group <- function(data, dv, group) {
  df <- data.frame(value = as.numeric(data[[dv]]), group = as.factor(data[[group]]))
  n_levels <- nlevels(df$group)
  palette <- rep_len(publication_palette, n_levels)
  ggplot2::ggplot(df, ggplot2::aes(x = group, y = value, fill = group)) +
    ggplot2::geom_violin(alpha = 0.55, color = "black", linewidth = 0.5, trim = TRUE) +
    ggplot2::geom_boxplot(width = 0.12, fill = "white", outlier.shape = NA, linewidth = 0.5) +
    ggplot2::geom_jitter(width = 0.06, alpha = 0.5, size = 1.4, color = "black") +
    ggplot2::scale_fill_manual(values = palette) +
    ggplot2::labs(title = sprintf("%s by %s", dv, group), x = group, y = dv) +
    theme_publication() +
    ggplot2::theme(legend.position = "none")
}

plot_scatter_by_covariate <- function(data, dv, covariate) {
  df <- data.frame(x = as.numeric(data[[covariate]]), y = as.numeric(data[[dv]]))
  ggplot2::ggplot(df, ggplot2::aes(x = x, y = y)) +
    ggplot2::geom_point(shape = 21, size = 2.2, fill = "#2C7FB8", color = "black", alpha = 0.8, stroke = 0.3) +
    ggplot2::geom_smooth(method = "loess", formula = y ~ x, se = TRUE, color = "grey30", fill = "grey70", linewidth = 0.7) +
    ggplot2::labs(title = sprintf("%s vs %s", dv, covariate), x = covariate, y = dv) +
    theme_publication()
}

# Registers a plot into results$eda_plots (kept separate from
# results$diagnostic_plots, which is reserved for post-fit model diagnostics).
safe_register_eda_plot <- function(results, output_dir, name, plot_fn) {
  tryCatch(
    register_plot(results, output_dir, name, plot = plot_fn(), field = "eda_plots"),
    error = function(err) append_result_message(results, "warnings", sprintf("Skipped EDA plot '%s': %s", name, conditionMessage(err)))
  )
}

# Generate kernel density (overall + by each categorical main effect),
# violin/boxplot-by-group, and scatter-vs-continuous-covariate plots for
# every dependent variable in the spec, using the raw (untransformed) data.
# Runs independently of model fitting so it still produces useful exploratory
# output even if the model itself fails to fit.
generate_eda_plots <- function(spec, data, results, output_dir) {
  dvs <- default_if_null(spec$dependent_vars, character())
  ivs <- default_if_null(spec$independent_vars, character())

  max_group_levels <- 10L
  panels <- list()

  for (dv in dvs) {
    if (!dv %in% names(data) || !is.numeric(data[[dv]])) {
      next
    }

    overall_plot <- tryCatch(plot_density_overall(data, dv), error = function(...) NULL)
    if (!is.null(overall_plot)) {
      results <- safe_register_eda_plot(results, output_dir, sprintf("eda_density_%s", dv), function() overall_plot)
      panels[[length(panels) + 1L]] <- overall_plot
    }

    for (iv in ivs) {
      if (!iv %in% names(data)) {
        next
      }

      if (is_categorical_column(data[[iv]])) {
        n_levels <- length(unique(stats::na.omit(data[[iv]])))
        if (n_levels < 2L || n_levels > max_group_levels) {
          results <- append_result_message(
            results, "warnings",
            sprintf("Skipped grouped EDA plots for '%s' by '%s': %d factor levels (expected 2-%d).", dv, iv, n_levels, max_group_levels)
          )
          next
        }
        density_plot <- tryCatch(plot_density_by_group(data, dv, iv), error = function(...) NULL)
        if (!is.null(density_plot)) {
          results <- safe_register_eda_plot(results, output_dir, sprintf("eda_density_%s_by_%s", dv, iv), function() density_plot)
          panels[[length(panels) + 1L]] <- density_plot
        }
        violin_plot <- tryCatch(plot_violin_by_group(data, dv, iv), error = function(...) NULL)
        if (!is.null(violin_plot)) {
          results <- safe_register_eda_plot(results, output_dir, sprintf("eda_violin_%s_by_%s", dv, iv), function() violin_plot)
          panels[[length(panels) + 1L]] <- violin_plot
        }
      } else if (is.numeric(data[[iv]])) {
        scatter_plot <- tryCatch(plot_scatter_by_covariate(data, dv, iv), error = function(...) NULL)
        if (!is.null(scatter_plot)) {
          results <- safe_register_eda_plot(results, output_dir, sprintf("eda_scatter_%s_vs_%s", dv, iv), function() scatter_plot)
          panels[[length(panels) + 1L]] <- scatter_plot
        }
      }
    }
  }

  if (length(panels) > 1L && requireNamespace("patchwork", quietly = TRUE)) {
    combined <- tryCatch(
      patchwork::wrap_plots(panels, ncol = 2) +
        patchwork::plot_annotation(title = "Exploratory data analysis"),
      error = function(...) NULL
    )
    if (!is.null(combined)) {
      results <- safe_register_eda_plot(results, output_dir, "eda_panel", function() combined)
    }
  }

  results
}
