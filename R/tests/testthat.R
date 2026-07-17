suppressPackageStartupMessages(library(testthat))

args <- commandArgs(trailingOnly = FALSE)
file_arg <- grep("^--file=", args, value = TRUE)
runner_path <- if (length(file_arg) > 0) {
  sub("^--file=", "", file_arg[1L])
} else {
  "R/tests/testthat.R"
}
repo_root <- normalizePath(file.path(dirname(runner_path), "..", ".."), winslash = "\\")
source(file.path(repo_root, "R", "R", "io_json.R"))
source(file.path(repo_root, "R", "R", "theme.R"))
source(file.path(repo_root, "R", "R", "model_fit.R"))
source(file.path(repo_root, "R", "R", "nonparametric.R"))
source(file.path(repo_root, "R", "R", "multivariate.R"))
source(file.path(repo_root, "R", "R", "posthoc.R"))
source(file.path(repo_root, "R", "R", "diagnostics.R"))
source(file.path(repo_root, "R", "R", "eda_plots.R"))

test_dir(file.path(repo_root, "R", "tests", "testthat"))
