read_analysis_data <- function(spec) {
  if (is.null(spec$input_csv) || !nzchar(spec$input_csv)) {
    stop("analysis_spec.json is missing input_csv.")
  }
  data <- utils::read.csv(spec$input_csv, stringsAsFactors = TRUE, check.names = FALSE)
  data[] <- lapply(data, function(col) {
    if (is.character(col)) {
      as.factor(col)
    } else {
      col
    }
  })
  apply_transformation(data, spec)
}

apply_transformation <- function(data, spec) {
  dv <- default_if_null(spec$dependent_vars, character())
  transformation <- default_if_null(spec$transformation, "none")
  model_family <- default_if_null(spec$model_family, "lm")

  # Transforming the response only makes sense for models that assume
  # (approximately) Gaussian errors on the transformed scale. Non-Gaussian
  # families (glm/glmm/tweedie/nonparametric/nonlinear/manova) already model
  # the appropriate mean-variance relationship via their family/link, so
  # applying a normality-driven transform on top would distort or break the
  # fit (e.g. Tweedie/count data include zeros that box-cox-style transforms
  # cannot handle).
  transformable_families <- c("lm", "lmm")
  if (length(dv) != 1L || identical(transformation, "none") ||
      !model_family %in% transformable_families) {
    return(data)
  }
  if (!dv[1L] %in% names(data)) {
    stop(sprintf("Dependent variable '%s' not found in input_csv.", dv[1L]))
  }
  value <- data[[dv[1L]]]
  data[[dv[1L]]] <- switch(
    transformation,
    log = log(value),
    sqrt = sqrt(value),
    boxcox = {
      if (!requireNamespace("MASS", quietly = TRUE)) {
        stop("MASS is required for boxcox transformation.")
      }
      lambda <- MASS::boxcox(stats::lm(value ~ 1), plotit = FALSE)$x[which.max(MASS::boxcox(stats::lm(value ~ 1), plotit = FALSE)$y)]
      if (abs(lambda) < .Machine$double.eps) log(value) else (value^lambda - 1) / lambda
    },
    yeojohnson = {
      if (!requireNamespace("car", quietly = TRUE)) {
        stop("car is required for yeojohnson transformation.")
      }
      lambda <- car::powerTransform(value ~ 1, family = "yjPower")$lambda
      car::yjPower(value, lambda = lambda)
    },
    value
  )
  data
}

strip_random_effects <- function(formula_string) {
  if (is.null(formula_string) || !nzchar(formula_string)) {
    return(formula_string)
  }
  stripped <- gsub("\\s*\\+\\s*\\([^\\)]*\\|[^\\)]*\\)", "", formula_string, perl = TRUE)
  stripped <- gsub("\\([^\\)]*\\|[^\\)]*\\)\\s*\\+\\s*", "", stripped, perl = TRUE)
  stripped <- gsub("\\([^\\)]*\\|[^\\)]*\\)", "", stripped, perl = TRUE)
  stripped <- gsub("~\\s*\\+", "~", stripped, perl = TRUE)
  stripped <- gsub("\\+\\s*$", "", stripped, perl = TRUE)
  stripped <- gsub("\\s+", " ", stripped, perl = TRUE)
  trimws(stripped)
}

rhs_from_formula <- function(formula_string) {
  pieces <- strsplit(formula_string, "~", fixed = TRUE)[[1L]]
  if (length(pieces) < 2L) {
    stop("Formula must contain '~'.")
  }
  trimws(pieces[2L])
}

replace_formula_lhs <- function(formula_string, lhs) {
  paste(lhs, "~", rhs_from_formula(formula_string))
}

safe_as_numeric <- function(value) {
  if (is.null(value) || length(value) == 0L) {
    return(NULL)
  }
  as.numeric(value)
}

get_glm_family <- function(distribution) {
  switch(
    default_if_null(distribution, "gaussian"),
    gaussian = stats::gaussian(),
    poisson = stats::poisson(),
    binomial = stats::binomial(),
    gamma = stats::Gamma(),
    negbinom = NULL,
    tweedie = NULL,
    stop(sprintf("Unsupported distribution '%s'.", distribution))
  )
}

get_formula_for_family <- function(spec, family_name) {
  if (is.null(spec$formula) || !nzchar(spec$formula)) {
    stop("analysis_spec.json is missing formula.")
  }
  if (family_name %in% c("lm", "glm")) {
    return(stats::as.formula(strip_random_effects(spec$formula)))
  }
  stats::as.formula(spec$formula)
}

extract_convergence_ok <- function(model) {
  if (inherits(model, "glmmTMB")) {
    return(isTRUE(model$sdr$pdHess))
  }
  if (inherits(model, "merMod")) {
    return(is.null(model@optinfo$conv$lme4$messages))
  }
  TRUE
}

extract_model_statistics <- function(model) {
  loglik_value <- tryCatch(as.numeric(stats::logLik(model)), error = function(...) NULL)
  list(
    aic = tryCatch(stats::AIC(model), error = function(...) NULL),
    bic = tryCatch(stats::BIC(model), error = function(...) NULL),
    logLik = loglik_value
  )
}

extract_distribution_parameters <- function(model) {
  if (!inherits(model, "glmmTMB")) {
    return(NULL)
  }
  family_name <- tryCatch(model$modelInfo$family$family, error = function(...) NULL)
  if (!identical(family_name, "tweedie")) {
    return(NULL)
  }
  params <- tryCatch(glmmTMB::family_params(model), error = function(...) NULL)
  output <- list()
  if (!is.null(params) && length(params) > 0L) {
    param_names <- names(params)
    power_idx <- grep("power|psi", default_if_null(param_names, character()), ignore.case = TRUE)
    if (length(power_idx) > 0L) {
      output$tweedie_power <- safe_as_numeric(params[[power_idx[1L]]])
    }
  }
  dispersion <- tryCatch(stats::sigma(model), error = function(...) NULL)
  if (!is.null(dispersion)) {
    output$dispersion <- safe_as_numeric(dispersion)
  }
  if (length(output) == 0L) {
    return(NULL)
  }
  output
}

extract_r_squared <- function(model, family_name = NULL) {
  if (inherits(model, "lm")) {
    model_summary <- summary(model)
    return(list(r_squared = unname(model_summary$r.squared), adjusted = unname(model_summary$adj.r.squared)))
  }
  if (inherits(model, "glm")) {
    # GLM objects do not expose summary()$r.squared; use deviance-based pseudo-R^2 instead.
    return(list(pseudo = 1 - (model$deviance / model$null.deviance)))
  }
  if (inherits(model, "merMod")) {
    if (requireNamespace("MuMIn", quietly = TRUE)) {
      r2 <- suppressWarnings(MuMIn::r.squaredGLMM(model))
      return(list(marginal = unname(r2[1L]), conditional = unname(r2[2L])))
    }
    return(NULL)
  }
  if (inherits(model, "glmmTMB")) {
    if (requireNamespace("performance", quietly = TRUE)) {
      r2 <- tryCatch(
        suppressWarnings(performance::r2_nakagawa(model)),
        warning = function(...) NULL,
        error = function(...) NULL
      )
      if (is.list(r2) && !is.null(r2$R2_marginal)) {
        return(list(marginal = unname(r2$R2_marginal), conditional = unname(r2$R2_conditional)))
      }
    }
    return(NULL)
  }
  if (inherits(model, "nls") || inherits(model, "nlme")) {
    fitted_values <- stats::fitted(model)
    residuals_value <- stats::residuals(model)
    observed <- fitted_values + residuals_value
    sst <- sum((observed - mean(observed, na.rm = TRUE))^2, na.rm = TRUE)
    sse <- sum(residuals_value^2, na.rm = TRUE)
    return(list(pseudo = if (sst == 0) NA_real_ else 1 - (sse / sst)))
  }
  NULL
}

tidy_from_matrix <- function(mat) {
  if (is.null(mat)) {
    return(list())
  }
  df <- as.data.frame(mat, stringsAsFactors = FALSE)
  df$term <- rownames(df)
  rownames(df) <- NULL
  estimate_col <- grep("Estimate|Value", names(df), value = TRUE)[1L]
  se_col <- grep("Std\\.|SE", names(df), value = TRUE)[1L]
  stat_col <- grep("t value|z value|t-value|z-value|statistic", names(df), value = TRUE)[1L]
  p_col <- grep("^Pr\\(|p.value|p-value", names(df), value = TRUE)[1L]
  output <- lapply(seq_len(nrow(df)), function(i) {
    list(
      term = df$term[i],
      estimate = safe_as_numeric(df[[estimate_col]][i]),
      se = safe_as_numeric(df[[se_col]][i]),
      statistic = if (!is.na(stat_col)) safe_as_numeric(df[[stat_col]][i]) else NULL,
      p = if (!is.na(p_col)) safe_as_numeric(df[[p_col]][i]) else NULL
    )
  })
  output
}

extract_coefficients <- function(model) {
  if (requireNamespace("broom", quietly = TRUE)) {
    tidied <- tryCatch(broom::tidy(model), error = function(...) NULL)
    if (!is.null(tidied) && all(c("term", "estimate") %in% names(tidied))) {
      return(lapply(seq_len(nrow(tidied)), function(i) {
        list(
          term = tidied$term[i],
          estimate = safe_as_numeric(tidied$estimate[i]),
          se = safe_as_numeric(tidied$std.error[i]),
          statistic = safe_as_numeric(tidied$statistic[i]),
          p = safe_as_numeric(tidied$p.value[i])
        )
      }))
    }
  }

  coef_mat <- tryCatch(summary(model)$coefficients, error = function(...) NULL)
  if (is.null(coef_mat) && inherits(model, "nlme")) {
    coef_mat <- tryCatch(summary(model)$tTable, error = function(...) NULL)
  }
  if (is.list(coef_mat) && !is.data.frame(coef_mat)) {
    coef_mat <- default_if_null(coef_mat$cond, coef_mat[[1L]])
  }
  tidy_from_matrix(coef_mat)
}

table_to_records <- function(df) {
  if (is.null(df) || nrow(df) == 0L) {
    return(list())
  }
  lapply(seq_len(nrow(df)), function(i) {
    row <- as.list(df[i, , drop = FALSE])
    lapply(row, function(value) {
      if (is.factor(value)) {
        as.character(value)
      } else if (is.numeric(value)) {
        safe_as_numeric(value)
      } else {
        value
      }
    })
  })
}

extract_anova_table <- function(model) {
  anova_obj <- tryCatch(car::Anova(model, type = 3), error = function(...) NULL)
  if (is.null(anova_obj)) {
    anova_obj <- tryCatch(stats::anova(model), error = function(...) NULL)
  }
  if (is.null(anova_obj)) {
    return(list())
  }
  df <- as.data.frame(anova_obj, stringsAsFactors = FALSE)
  df$term <- rownames(df)
  rownames(df) <- NULL
  names(df) <- gsub("\\s+", "_", names(df))
  if ("Pr(>F)" %in% names(df)) {
    names(df)[names(df) == "Pr(>F)"] <- "p"
  }
  if ("Pr(>Chisq)" %in% names(df)) {
    names(df)[names(df) == "Pr(>Chisq)"] <- "p"
  }
  if ("Pr(>Chi)" %in% names(df)) {
    names(df)[names(df) == "Pr(>Chi)"] <- "p"
  }
  table_to_records(df[, c("term", setdiff(names(df), "term")), drop = FALSE])
}

fit_lm_model <- function(spec, data) {
  model <- stats::lm(get_formula_for_family(spec, "lm"), data = data)
  list(model = model, family = "lm", backbone = "stats::lm")
}

fit_glm_model <- function(spec, data) {
  formula_obj <- get_formula_for_family(spec, "glm")
  distribution <- default_if_null(spec$distribution, "gaussian")
  if (identical(distribution, "tweedie")) {
    return(fit_tweedie_model(spec, data))
  }
  if (identical(distribution, "negbinom")) {
    model <- MASS::glm.nb(formula_obj, data = data)
    return(list(model = model, family = "glm", backbone = "MASS::glm.nb"))
  }
  model <- stats::glm(formula_obj, data = data, family = get_glm_family(distribution))
  list(model = model, family = "glm", backbone = "stats::glm")
}

fit_lmm_model <- function(spec, data) {
  model <- lme4::lmer(get_formula_for_family(spec, "lmm"), data = data)
  list(model = model, family = "lmm", backbone = "lme4::lmer")
}

fit_glmm_model <- function(spec, data) {
  formula_obj <- get_formula_for_family(spec, "glmm")
  distribution <- default_if_null(spec$distribution, "gaussian")
  if (identical(distribution, "tweedie")) {
    return(fit_tweedie_model(spec, data))
  }

  primary_fit <- tryCatch(
    {
      if (identical(distribution, "negbinom")) {
        stop("Use glmmTMB for negative binomial mixed models.")
      }
      lme4::glmer(formula_obj, data = data, family = get_glm_family(distribution))
    },
    error = function(e) e
  )

  if (!inherits(primary_fit, "error") && extract_convergence_ok(primary_fit)) {
    return(list(model = primary_fit, family = "glmm", backbone = "lme4::glmer"))
  }

  if (!requireNamespace("glmmTMB", quietly = TRUE)) {
    if (inherits(primary_fit, "error")) {
      stop(conditionMessage(primary_fit))
    }
    return(list(model = primary_fit, family = "glmm", backbone = "lme4::glmer"))
  }

  family_obj <- switch(
    distribution,
    negbinom = glmmTMB::nbinom2(),
    poisson = stats::poisson(),
    binomial = stats::binomial(),
    gamma = stats::Gamma(),
    gaussian = stats::gaussian(),
    stats::gaussian()
  )
  model <- glmmTMB::glmmTMB(formula_obj, data = data, family = family_obj)
  list(model = model, family = "glmm", backbone = "glmmTMB::glmmTMB")
}

fit_tweedie_model <- function(spec, data) {
  if (!requireNamespace("glmmTMB", quietly = TRUE)) {
    stop("glmmTMB is required for Tweedie models.")
  }
  model <- glmmTMB::glmmTMB(stats::as.formula(spec$formula), data = data, family = glmmTMB::tweedie())
  list(model = model, family = "tweedie", backbone = "glmmTMB::glmmTMB")
}

fit_single_family <- function(spec, data, family_name = NULL) {
  family_name <- default_if_null(family_name, spec$model_family)
  if (identical(family_name, "nonparametric")) {
    return(fit_nonparametric_analysis(spec, data))
  }
  fit <- switch(
    family_name,
    lm = fit_lm_model(spec, data),
    glm = fit_glm_model(spec, data),
    lmm = fit_lmm_model(spec, data),
    glmm = fit_glmm_model(spec, data),
    tweedie = fit_tweedie_model(spec, data),
    stop(sprintf("Unsupported model_family '%s'.", family_name))
  )

  stats_values <- extract_model_statistics(fit$model)
  results <- list(
    model_family_used = fit$family,
    backbone_package = fit$backbone,
    formula = format(stats::formula(fit$model)),
    convergence_ok = extract_convergence_ok(fit$model),
    distribution_parameters = extract_distribution_parameters(fit$model),
    anova_table = extract_anova_table(fit$model),
    coefficients = extract_coefficients(fit$model),
    r_squared = extract_r_squared(fit$model, family_name),
    aic = stats_values$aic,
    bic = stats_values$bic,
    logLik = stats_values$logLik
  )

  list(
    model = fit$model,
    results = results,
    aic = stats_values$aic,
    data_used = stats::model.frame(fit$model)
  )
}

add_aic_weights <- function(comparison) {
  # Adds delta_aic (relative to the best/lowest AIC) and Akaike weights to a
  # list of {..., aic} records. Records with a missing/NULL aic are left
  # untouched (delta_aic/aic_weight stay NULL) so failed candidates don't
  # distort the weighting of the ones that actually fit.
  aic_values <- vapply(comparison, function(item) default_if_null(item$aic, NA_real_), numeric(1L))
  finite_idx <- which(is.finite(aic_values))
  if (length(finite_idx) == 0L) {
    return(comparison)
  }
  best_aic <- min(aic_values[finite_idx])
  deltas <- aic_values - best_aic
  rel_likelihood <- exp(-0.5 * deltas)
  weight_denom <- sum(rel_likelihood[finite_idx])
  for (i in finite_idx) {
    comparison[[i]]$delta_aic <- safe_as_numeric(deltas[i])
    comparison[[i]]$aic_weight <- if (weight_denom > 0) safe_as_numeric(rel_likelihood[i] / weight_denom) else NULL
  }
  comparison
}

# Fixed-effect term names on the RHS of a formula, excluding any lme4-style
# random-effect groups such as (1 | subject) or (time | subject).
fixed_effect_terms <- function(formula_string) {
  stripped <- strip_random_effects(formula_string)
  if (is.null(stripped) || !nzchar(stripped)) {
    return(character())
  }
  term_labels <- tryCatch(attr(stats::terms(stats::as.formula(stripped)), "term.labels"), error = function(...) character())
  default_if_null(term_labels, character())
}

# Efficient single-term-deletion AIC comparison (analogous to stats::drop1
# but implemented via update() so it works uniformly across lm/glm/lmer/
# glmer/glmmTMB). This scales linearly with the number of fixed-effect terms
# rather than combinatorially, so it stays practical for large models with
# many predictors -- unlike an exhaustive "dredge" over all term subsets.
term_drop_comparison <- function(model, spec, data = NULL, max_terms = 15L) {
  terms_in_model <- fixed_effect_terms(spec$formula)
  if (length(terms_in_model) == 0L) {
    return(list(records = list(), warning = NULL))
  }
  if (length(terms_in_model) > max_terms) {
    return(list(
      records = list(),
      warning = sprintf(
        "Skipped automatic single-term-deletion AIC comparison: model has %d fixed-effect terms (limit %d). Use --formula to test specific reduced models instead.",
        length(terms_in_model), max_terms
      )
    ))
  }

  full_aic <- tryCatch(stats::AIC(model), error = function(...) NA_real_)
  # NB: stats::update() re-evaluates the model's original call in the
  # *caller's* frame (parent.frame()), not the environment the model was
  # originally fitted in -- so any symbols referenced by that call (the
  # `data` argument, and for GLMs a `family` expression built from a local
  # `distribution` variable) must be supplied explicitly here, or the
  # refit will fail with "object not found" errors.
  update_args <- list()
  if (!is.null(data)) update_args$data <- data
  # Only forward `family` if the model's original call actually used one
  # (e.g. glm/glmer/glmmTMB) -- passing family= to update() on a plain
  # lm/lmer call errors with "unused argument".
  original_call <- tryCatch(stats::getCall(model), error = function(...) NULL)
  if (!is.null(original_call) && "family" %in% names(original_call)) {
    model_family <- tryCatch(stats::family(model), error = function(...) NULL)
    if (!is.null(model_family)) update_args$family <- model_family
  }
  records <- lapply(terms_in_model, function(term_label) {
    reduced <- tryCatch(
      do.call(stats::update, c(list(model, stats::as.formula(paste("~ . -", term_label))), update_args)),
      error = function(...) tryCatch(
        do.call(stats::update, c(list(model, stats::as.formula(paste("~ . -", sprintf("`%s`", term_label)))), update_args)),
        error = function(...) NULL
      )
    )
    if (is.null(reduced)) {
      return(list(term = term_label, aic = NULL, error = "Could not refit model without this term (likely required by marginality with an interaction)."))
    }
    reduced_aic <- tryCatch(stats::AIC(reduced), error = function(...) NA_real_)
    lrt_p <- tryCatch({
      comparison <- stats::anova(reduced, model)
      p_col <- grep("Pr\\(>Chisq\\)|Pr\\(>Chi\\)|Pr\\(>F\\)", names(comparison), value = TRUE)[1L]
      if (is.na(p_col)) NULL else safe_as_numeric(comparison[[p_col]][nrow(comparison)])
    }, error = function(...) NULL)
    list(
      term = term_label,
      aic_full_model = safe_as_numeric(full_aic),
      aic = safe_as_numeric(reduced_aic),
      likelihood_ratio_p = lrt_p
    )
  })

  valid <- Filter(function(x) is.null(x$error), records)
  invalid <- Filter(function(x) !is.null(x$error), records)
  list(records = c(add_aic_weights(valid), invalid), warning = NULL)
}

fit_analysis_model <- function(spec, data) {
  candidates <- unique(default_if_null(spec$candidate_families, default_if_null(spec$model_family, "lm")))
  candidates <- Filter(Negate(is.null), candidates)
  if (length(candidates) == 0L) {
    stop("No candidate model families were provided.")
  }

  fits <- lapply(candidates, function(candidate) {
    tryCatch(
      fit_single_family(spec, data, candidate),
      error = function(err) list(error = conditionMessage(err), family = candidate)
    )
  })

  comparison <- lapply(fits, function(fit) {
    if (!is.null(fit$error)) {
      list(family = fit$family, aic = NULL, error = fit$error)
    } else {
      list(family = fit$results$model_family_used, aic = fit$aic)
    }
  })

  valid_fits <- Filter(function(item) is.null(item$error), fits)
  if (length(valid_fits) == 0L) {
    stop(paste(vapply(fits, function(x) default_if_null(x$error, ""), character(1L)), collapse = "; "))
  }

  best_index <- which.min(vapply(valid_fits, function(item) default_if_null(item$aic, Inf), numeric(1L)))
  best_fit <- valid_fits[[best_index]]
  best_fit$results$model_comparison <- add_aic_weights(comparison)
  best_fit$results$warnings <- lapply(
    Filter(function(x) !is.null(x$error), fits),
    function(x) paste(sprintf("Candidate family '%s' failed:", x$family), x$error)
  )

  if (isTRUE(default_if_null(spec$compare_terms, TRUE)) && !is.null(best_fit$model)) {
    fit_data <- default_if_null(best_fit$data_used, data)
    term_comparison <- tryCatch(term_drop_comparison(best_fit$model, spec, data = fit_data), error = function(err) list(records = list(), warning = conditionMessage(err)))
    best_fit$results$term_comparison <- term_comparison$records
    if (!is.null(term_comparison$warning)) {
      best_fit$results$warnings <- c(best_fit$results$warnings, list(term_comparison$warning))
    }
  }

  list(
    results = best_fit$results,
    diagnostic_model = best_fit$model,
    posthoc_model = best_fit$model,
    data_used = best_fit$data_used
  )
}
