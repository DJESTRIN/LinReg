default_if_null <- function(x, default) {
  if (is.null(x)) {
    default
  } else {
    x
  }
}

ensure_output_structure <- function(output_dir) {
  if (is.null(output_dir) || !nzchar(output_dir)) {
    stop("output_dir is required to write results.")
  }
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(file.path(output_dir, "plots"), recursive = TRUE, showWarnings = FALSE)
}

normalize_json_path <- function(path) {
  gsub("\\\\", "/", path)
}

relative_to_output_dir <- function(path, output_dir) {
  normalized_path <- normalizePath(path, winslash = "\\", mustWork = FALSE)
  normalized_output <- normalizePath(output_dir, winslash = "\\", mustWork = FALSE)
  rel <- sub(
    paste0("^", gsub("\\\\", "\\\\\\\\", normalized_output), "\\\\?"),
    "",
    normalized_path
  )
  normalize_json_path(rel)
}

new_results <- function(spec = NULL) {
  list(
    schema_version = default_if_null(spec$schema_version, "1.0"),
    model_family_used = NULL,
    backbone_package = NULL,
    formula = default_if_null(spec$formula, NULL),
    convergence_ok = NULL,
    distribution_parameters = NULL,
    residual_normality = NULL,
    anova_table = list(),
    coefficients = list(),
    r_squared = NULL,
    aic = NULL,
    bic = NULL,
    logLik = NULL,
    model_comparison = list(),
    term_comparison = list(),
    significant_terms = list(),
    nonparametric_test = NULL,
    posthoc = list(),
    effect_sizes = list(),
    diagnostic_plots = list(),
    warnings = list(),
    errors = list()
  )
}

append_result_message <- function(results, field, message) {
  if (!field %in% c("warnings", "errors")) {
    stop("Only warnings and errors message fields are supported.")
  }
  current <- default_if_null(results[[field]], list())
  current[[length(current) + 1L]] <- as.character(message)
  results[[field]] <- current
  results
}

merge_results <- function(results, updates) {
  if (is.null(updates)) {
    return(results)
  }
  for (name in names(updates)) {
    if (name %in% c("warnings", "errors", "diagnostic_plots", "posthoc", "effect_sizes", "model_comparison", "term_comparison", "significant_terms") &&
        length(default_if_null(updates[[name]], list())) > 0L) {
      results[[name]] <- c(default_if_null(results[[name]], list()), updates[[name]])
    } else {
      results[[name]] <- updates[[name]]
    }
  }
  results
}

read_spec <- function(path) {
  jsonlite::fromJSON(path, simplifyVector = TRUE)
}

write_results <- function(results, output_dir) {
  ensure_output_structure(output_dir)
  results_path <- file.path(output_dir, "results.json")
  jsonlite::write_json(
    x = results,
    path = results_path,
    pretty = TRUE,
    auto_unbox = TRUE,
    null = "null",
    na = "null"
  )
  invisible(results_path)
}

register_plot <- function(results,
                          output_dir,
                          name,
                          plot = NULL,
                          plot_expr = NULL,
                          src_path = NULL,
                          width = 7,
                          height = 5,
                          dpi = 120) {
  ensure_output_structure(output_dir)
  target_path <- file.path(output_dir, "plots", paste0(name, ".png"))

  if (!is.null(plot)) {
    ggplot2::ggsave(
      filename = target_path,
      plot = plot,
      width = width,
      height = height,
      dpi = dpi,
      units = "in"
    )
  } else if (!is.null(plot_expr)) {
    grDevices::png(filename = target_path, width = width, height = height, units = "in", res = dpi)
    on.exit(grDevices::dev.off(), add = TRUE)
    plot_expr()
  } else if (!is.null(src_path)) {
    file.copy(src_path, target_path, overwrite = TRUE)
  } else {
    stop("register_plot requires plot, plot_expr, or src_path.")
  }

  rel_path <- relative_to_output_dir(target_path, output_dir)
  results$diagnostic_plots <- unique(c(default_if_null(results$diagnostic_plots, list()), rel_path))
  results
}
