required_packages <- c(
  "optparse",
  "jsonlite",
  "lme4",
  "glmmTMB",
  "MuMIn",
  "car",
  "emmeans",
  "effectsize",
  "broom",
  "nlme",
  "MASS",
  "ggplot2",
  "patchwork",
  "performance",
  "nortest",
  "coin",
  "R6",
  "testthat",
  "lsr"
)

for (pkg in required_packages) {
  message(sprintf("Checking package: %s", pkg))
  if (!requireNamespace(pkg, quietly = TRUE)) {
    message(sprintf("Installing package: %s", pkg))
    install.packages(pkg, repos = "https://cloud.r-project.org")
  } else {
    message(sprintf("Already installed: %s", pkg))
  }
}

message("Package installation check complete.")
