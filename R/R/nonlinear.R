nonlinear_components <- function(spec, data) {
  dv <- default_if_null(spec$dependent_vars, character())
  iv <- default_if_null(spec$independent_vars, character())
  if (length(dv) != 1L || length(iv) < 1L) {
    stop("Nonlinear models require exactly one dependent_var and at least one independent_var.")
  }

  form <- default_if_null(spec$nonlinear$form, "custom")
  predictor <- iv[1L]
  response <- dv[1L]
  start_vals <- spec$nonlinear$start

  formula_obj <- switch(
    form,
    logistic = stats::as.formula(sprintf("%s ~ SSlogis(%s, Asym, xmid, scal)", response, predictor)),
    exponential = stats::as.formula(sprintf("%s ~ SSasymp(%s, Asym, R0, lrc)", response, predictor)),
    michaelis_menten = stats::as.formula(sprintf("%s ~ SSmicmen(%s, Vm, K)", response, predictor)),
    custom = {
      if (is.null(spec$nonlinear$formula) || !nzchar(spec$nonlinear$formula)) {
        stop("Custom nonlinear models require nonlinear.formula.")
      }
      stats::as.formula(spec$nonlinear$formula)
    },
    stop(sprintf("Unsupported nonlinear form '%s'.", form))
  )

  param_names <- setdiff(all.vars(formula_obj[[3L]]), c(response, predictor, names(data)))
  if (length(param_names) == 0L) {
    param_names <- setdiff(all.vars(formula_obj), c(response, predictor))
  }

  if (is.null(start_vals) && identical(form, "custom")) {
    start_vals <- as.list(stats::setNames(rep(1, length(param_names)), param_names))
  }

  list(
    form = form,
    formula = formula_obj,
    predictor = predictor,
    response = response,
    param_names = param_names,
    start = start_vals
  )
}

coerce_start_values <- function(start_vals) {
  if (is.null(start_vals)) {
    return(NULL)
  }
  if (is.list(start_vals)) {
    return(unlist(start_vals))
  }
  start_vals
}

fit_nonlinear_model <- function(spec, data) {
  parts <- nonlinear_components(spec, data)
  random_effects <- default_if_null(spec$random_effects, character())

  model <- if (length(random_effects) > 0L) {
    initial_nls <- stats::nls(parts$formula, data = data, start = coerce_start_values(parts$start))
    nlme::nlme(
      model = parts$formula,
      data = data,
      fixed = stats::as.formula(paste(paste(parts$param_names, collapse = " + "), "~ 1")),
      random = stats::as.formula(paste("~ 1 |", random_effects[1L])),
      start = stats::coef(initial_nls)
    )
  } else {
    if (is.null(parts$start)) {
      stats::nls(parts$formula, data = data)
    } else {
      stats::nls(parts$formula, data = data, start = coerce_start_values(parts$start))
    }
  }

  stats_values <- extract_model_statistics(model)
  coefficients <- if (inherits(model, "nlme")) {
    tidy_from_matrix(summary(model)$tTable)
  } else {
    tidy_from_matrix(summary(model)$coefficients)
  }

  list(
    results = list(
      model_family_used = "nonlinear",
      backbone_package = if (inherits(model, "nlme")) "nlme::nlme" else "stats::nls",
      formula = format(parts$formula),
      convergence_ok = TRUE,
      anova_table = list(),
      coefficients = coefficients,
      r_squared = extract_r_squared(model, "nonlinear"),
      aic = stats_values$aic,
      bic = stats_values$bic,
      logLik = stats_values$logLik
    ),
    diagnostic_model = model,
    posthoc_model = NULL,
    data_used = data
  )
}
