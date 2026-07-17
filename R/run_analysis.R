#!/usr/bin/env Rscript

get_script_dir <- function() {
  args <- commandArgs(trailingOnly = FALSE)
  file_arg <- grep("^--file=", args, value = TRUE)
  if (length(file_arg) > 0) {
    return(dirname(normalizePath(sub("^--file=", "", file_arg[1L]), winslash = "\\")))
  }
  if (!is.null(sys.frames()[[1L]]$ofile)) {
    return(dirname(normalizePath(sys.frames()[[1L]]$ofile, winslash = "\\")))
  }
  normalizePath(getwd(), winslash = "\\")
}

script_dir <- get_script_dir()
source(file.path(script_dir, "R", "io_json.R"))
source(file.path(script_dir, "R", "theme.R"))
source(file.path(script_dir, "R", "model_fit.R"))
source(file.path(script_dir, "R", "nonparametric.R"))
source(file.path(script_dir, "R", "nonlinear.R"))
source(file.path(script_dir, "R", "multivariate.R"))
source(file.path(script_dir, "R", "diagnostics.R"))
source(file.path(script_dir, "R", "eda_plots.R"))
source(file.path(script_dir, "R", "posthoc.R"))

suppressPackageStartupMessages(library(optparse))

option_list <- list(
  make_option(c("--config"), type = "character", help = "Path to analysis_spec.json")
)

parse_cli_args <- function() {
  parser <- OptionParser(option_list = option_list)
  parse_args(parser)
}

run_main <- function(config_path) {
  state <- new.env(parent = emptyenv())
  state$config_path <- normalizePath(config_path, winslash = "\\", mustWork = FALSE)
  state$output_dir <- dirname(state$config_path)
  state$results <- new_results(list(output_dir = state$output_dir))
  state$spec <- NULL

  withCallingHandlers(
    {
      tryCatch(
        {
          state$spec <- read_spec(state$config_path)
          if (!is.null(state$spec$output_dir) && nzchar(state$spec$output_dir)) {
            state$output_dir <- state$spec$output_dir
          }
          state$results <- new_results(state$spec)

          data <- read_analysis_data(state$spec)

          if (isTRUE(default_if_null(state$spec$make_plots, TRUE))) {
            raw_data <- tryCatch(read_raw_data(state$spec), error = function(...) NULL)
            if (!is.null(raw_data)) {
              state$results <- generate_eda_plots(
                spec = state$spec,
                data = raw_data,
                results = state$results,
                output_dir = state$output_dir
              )
            }
          }

          fit_result <- if (length(default_if_null(state$spec$dependent_vars, character())) > 1L ||
              identical(state$spec$model_family, "manova")) {
            fit_multivariate_model(state$spec, data)
          } else if (identical(state$spec$model_family, "nonlinear")) {
            fit_nonlinear_model(state$spec, data)
          } else {
            fit_analysis_model(state$spec, data)
          }

          state$results <- merge_results(state$results, fit_result$results)

          if (isTRUE(default_if_null(state$spec$make_plots, TRUE)) &&
              !is.null(fit_result$diagnostic_model)) {
            state$results <- generate_diagnostics(
              model = fit_result$diagnostic_model,
              spec = state$spec,
              data = fit_result$data_used,
              results = state$results,
              output_dir = state$output_dir
            )
          }

          if (!is.null(fit_result$posthoc_model)) {
            state$results <- run_posthoc(
              spec = state$spec,
              model = fit_result$posthoc_model,
              data = fit_result$data_used,
              results = state$results
            )
          }
        },
        error = function(err) {
          state$results <- append_result_message(
            state$results,
            "errors",
            paste("Analysis failed:", conditionMessage(err))
          )
        }
      )
    },
    warning = function(w) {
      state$results <- append_result_message(
        state$results,
        "warnings",
        conditionMessage(w)
      )
      invokeRestart("muffleWarning")
    }
  )

  if (length(default_if_null(state$results$errors, character())) > 0L &&
      is.null(state$results$convergence_ok)) {
    state$results$convergence_ok <- FALSE
  }

  write_results(state$results, state$output_dir)
}

args <- parse_cli_args()
if (is.null(args$config) || !nzchar(args$config)) {
  fallback_dir <- normalizePath(getwd(), winslash = "\\", mustWork = FALSE)
  write_results(
    append_result_message(
      new_results(list(output_dir = fallback_dir)),
      "errors",
      "Missing required --config argument."
    ),
    fallback_dir
  )
  quit(save = "no", status = 1L)
}

run_main(args$config)
