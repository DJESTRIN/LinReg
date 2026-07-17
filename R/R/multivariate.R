build_manova_formula <- function(spec) {
  dvs <- default_if_null(spec$dependent_vars, character())
  if (length(dvs) < 2L) {
    stop("Multivariate path requires at least two dependent variables.")
  }
  if (is.null(spec$formula) || !nzchar(spec$formula)) {
    stop("analysis_spec.json is missing formula.")
  }
  rhs <- rhs_from_formula(strip_random_effects(spec$formula))
  stats::as.formula(sprintf("cbind(%s) ~ %s", paste(dvs, collapse = ", "), rhs))
}

extract_manova_anova <- function(model) {
  summary_obj <- summary(model, test = "Pillai")
  stats_table <- as.data.frame(summary_obj$stats, stringsAsFactors = FALSE)
  stats_table$term <- rownames(stats_table)
  rownames(stats_table) <- NULL
  names(stats_table) <- c("pillai", "approx_F", "num_df", "den_df", "p", "term")
  table_to_records(stats_table[, c("term", "pillai", "approx_F", "num_df", "den_df", "p")])
}

extract_manova_coefficients <- function(spec, data) {
  dvs <- default_if_null(spec$dependent_vars, character())
  base_formula <- strip_random_effects(spec$formula)
  out <- list()
  for (dv in dvs) {
    fit <- stats::lm(stats::as.formula(replace_formula_lhs(base_formula, dv)), data = data)
    coeffs <- extract_coefficients(fit)
    out <- c(out, lapply(coeffs, function(item) {
      item$response <- dv
      item
    }))
  }
  out
}

extract_multivariate_r2 <- function(spec, data) {
  dvs <- default_if_null(spec$dependent_vars, character())
  base_formula <- strip_random_effects(spec$formula)
  stats_by_dv <- lapply(dvs, function(dv) {
    fit <- stats::lm(stats::as.formula(replace_formula_lhs(base_formula, dv)), data = data)
    summary_fit <- summary(fit)
    list(
      response = dv,
      r_squared = unname(summary_fit$r.squared),
      adjusted = unname(summary_fit$adj.r.squared)
    )
  })
  stats_by_dv
}

fit_multivariate_mixed_approximation <- function(spec, data) {
  dvs <- default_if_null(spec$dependent_vars, character())
  model_family <- if (default_if_null(spec$distribution, "gaussian") == "gaussian") "lmm" else "glmm"

  per_dv <- lapply(dvs, function(dv) {
    dv_spec <- spec
    dv_spec$dependent_vars <- dv
    dv_spec$formula <- replace_formula_lhs(spec$formula, dv)
    dv_spec$model_family <- model_family
    tryCatch(fit_single_family(dv_spec, data, model_family), error = function(err) list(error = conditionMessage(err), response = dv))
  })

  valid <- Filter(function(x) is.null(x$error), per_dv)
  if (length(valid) == 0L) {
    stop(paste(vapply(per_dv, function(x) default_if_null(x$error, ""), character(1L)), collapse = "; "))
  }

  all_coeffs <- unlist(lapply(valid, function(item) {
    dv <- all.vars(stats::formula(item$model))[1L]
    lapply(item$results$coefficients, function(entry) {
      entry$response <- dv
      entry
    })
  }), recursive = FALSE)

  all_anova <- unlist(lapply(valid, function(item) {
    dv <- all.vars(stats::formula(item$model))[1L]
    lapply(item$results$anova_table, function(entry) {
      entry$response <- dv
      entry
    })
  }), recursive = FALSE)

  list(
    results = list(
      model_family_used = "manova",
      backbone_package = "lme4::lmer/glmer per-response approximation",
      formula = spec$formula,
      convergence_ok = all(vapply(valid, function(x) isTRUE(x$results$convergence_ok), logical(1L))),
      anova_table = all_anova,
      coefficients = all_coeffs,
      r_squared = lapply(valid, function(item) {
        list(response = all.vars(stats::formula(item$model))[1L], metrics = item$results$r_squared)
      }),
      aic = mean(vapply(valid, function(x) default_if_null(x$results$aic, NA_real_), numeric(1L)), na.rm = TRUE),
      bic = mean(vapply(valid, function(x) default_if_null(x$results$bic, NA_real_), numeric(1L)), na.rm = TRUE),
      logLik = sum(vapply(valid, function(x) default_if_null(x$results$logLik, 0), numeric(1L)), na.rm = TRUE),
      warnings = list("Random-effects multivariate analysis is reported as per-response mixed-model approximation.")
    ),
    diagnostic_model = valid[[1L]]$model,
    posthoc_model = valid[[1L]]$model,
    data_used = data
  )
}

fit_multivariate_model <- function(spec, data) {
  if (length(default_if_null(spec$random_effects, character())) > 0L) {
    return(fit_multivariate_mixed_approximation(spec, data))
  }

  model <- stats::manova(build_manova_formula(spec), data = data)
  list(
    results = list(
      model_family_used = "manova",
      backbone_package = "stats::manova",
      formula = format(build_manova_formula(spec)),
      convergence_ok = TRUE,
      anova_table = extract_manova_anova(model),
      coefficients = extract_manova_coefficients(spec, data),
      r_squared = extract_multivariate_r2(spec, data),
      aic = NULL,
      bic = NULL,
      logLik = NULL
    ),
    diagnostic_model = stats::lm(stats::as.formula(replace_formula_lhs(strip_random_effects(spec$formula), spec$dependent_vars[1L])), data = data),
    posthoc_model = NULL,
    data_used = data
  )
}
