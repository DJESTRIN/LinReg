sigma_for_effect_size <- function(model) {
  tryCatch(stats::sigma(model), error = function(...) NA_real_)
}

manual_cohens_d <- function(estimate, sigma_value, se_value) {
  if (!is.na(sigma_value) && is.finite(sigma_value) && sigma_value > 0) {
    return(estimate / sigma_value)
  }
  if (!is.na(se_value) && is.finite(se_value) && se_value > 0) {
    return(estimate / se_value)
  }
  NA_real_
}

eta_squared_records <- function(model) {
  eta <- tryCatch(effectsize::eta_squared(model), error = function(...) NULL)
  if (is.null(eta)) {
    eta <- tryCatch(lsr::etaSquared(model), error = function(...) NULL)
  }
  if (is.null(eta)) {
    return(list())
  }
  eta_df <- as.data.frame(eta, stringsAsFactors = FALSE)
  term_col <- grep("Parameter|term|Term", names(eta_df), value = TRUE)[1L]
  value_col <- grep("Eta2|Eta2_partial|Eta2_partial|eta2", names(eta_df), value = TRUE)[1L]
  if (is.na(term_col) || is.na(value_col)) {
    return(list())
  }
  lapply(seq_len(nrow(eta_df)), function(i) {
    list(term = as.character(eta_df[[term_col]][i]), metric = "eta_squared", value = safe_as_numeric(eta_df[[value_col]][i]))
  })
}

# Determine which ANOVA terms (main effects AND interactions) are significant
# at the given alpha threshold, so post-hoc comparisons can be triggered
# automatically instead of requiring the caller to guess which factors need
# them. The intercept row is always excluded.
detect_significant_terms <- function(anova_table, alpha = 0.05) {
  if (length(anova_table) == 0L) {
    return(list())
  }
  significant <- Filter(function(row) {
    term <- default_if_null(row$term, "")
    p_value <- default_if_null(row$p, NA_real_)
    !identical(term, "(Intercept)") && !is.na(p_value) && is.numeric(p_value) && p_value <= alpha
  }, anova_table)
  lapply(significant, function(row) list(term = row$term, p = row$p, alpha = alpha))
}

# Split an ANOVA term label like "group:time" into its component factor
# names, keeping only those that are actual columns of `data` and are
# categorical (post-hoc pairwise contrasts on a continuous predictor aren't
# meaningful the same way -- its significance is already visible in the
# coefficients table).
posthoc_factors_for_term <- function(term, data) {
  parts <- strsplit(term, ":", fixed = TRUE)[[1L]]
  parts <- parts[parts %in% names(data)]
  parts <- parts[vapply(parts, function(col) is.factor(data[[col]]) || is.character(data[[col]]), logical(1L))]
  parts
}

run_one_posthoc_term <- function(model, term, factors, correction, sigma_value, trigger) {
  emm_spec <- stats::as.formula(paste("~", paste(factors, collapse = " * ")))
  emm <- emmeans::emmeans(model, specs = emm_spec)
  pairwise <- summary(emmeans::contrast(emm, method = "pairwise", adjust = correction))
  pairwise_df <- as.data.frame(pairwise, stringsAsFactors = FALSE)

  if (nrow(pairwise_df) == 0L) {
    return(list(posthoc = list(), effects = list()))
  }

  posthoc_rows <- lapply(seq_len(nrow(pairwise_df)), function(i) {
    list(
      factor = term,
      contrast = as.character(pairwise_df$contrast[i]),
      estimate = safe_as_numeric(pairwise_df$estimate[i]),
      se = safe_as_numeric(pairwise_df$SE[i]),
      p.adj = safe_as_numeric(pairwise_df$p.value[i]),
      trigger = trigger
    )
  })
  d_rows <- lapply(seq_len(nrow(pairwise_df)), function(i) {
    list(
      term = as.character(pairwise_df$contrast[i]),
      metric = "cohen_d",
      value = manual_cohens_d(
        estimate = safe_as_numeric(pairwise_df$estimate[i]),
        sigma_value = sigma_value,
        se_value = safe_as_numeric(pairwise_df$SE[i])
      )
    )
  })
  list(posthoc = posthoc_rows, effects = d_rows)
}

run_posthoc <- function(spec, model, data, results) {
  posthoc_results <- list()
  effect_sizes <- default_if_null(results$effect_sizes, list())
  correction <- default_if_null(spec$posthoc$correction, "bonferroni")
  alpha <- default_if_null(spec$alpha, 0.05)
  auto_enabled <- isTRUE(default_if_null(spec$posthoc$auto, TRUE))

  significant_terms <- if (auto_enabled) detect_significant_terms(results$anova_table, alpha) else list()
  results$significant_terms <- significant_terms

  # Union of user-requested factors (always tested, "explicit") and
  # ANOVA terms found significant at alpha (only tested when auto is on).
  explicit_factors <- default_if_null(spec$posthoc$factors, character())
  significant_labels <- vapply(significant_terms, function(x) x$term, character(1L))

  terms_to_test <- list()
  for (factor_name in explicit_factors) {
    terms_to_test[[factor_name]] <- list(term = factor_name, trigger = "explicit")
  }
  for (term_label in significant_labels) {
    if (is.null(terms_to_test[[term_label]])) {
      terms_to_test[[term_label]] <- list(term = term_label, trigger = "significant")
    } else {
      terms_to_test[[term_label]]$trigger <- "explicit_and_significant"
    }
  }

  sigma_value <- sigma_for_effect_size(model)

  for (term_label in names(terms_to_test)) {
    trigger <- terms_to_test[[term_label]]$trigger
    factors <- posthoc_factors_for_term(term_label, data)
    if (length(factors) == 0L) {
      next
    }

    factor_result <- tryCatch(
      run_one_posthoc_term(model, term_label, factors, correction, sigma_value, trigger),
      error = function(err) list(error = sprintf("Post-hoc failed for term '%s': %s", term_label, conditionMessage(err)))
    )

    if (!is.null(factor_result$error)) {
      results <- append_result_message(results, "warnings", factor_result$error)
    } else {
      posthoc_results <- c(posthoc_results, factor_result$posthoc)
      effect_sizes <- c(effect_sizes, factor_result$effects)
    }
  }

  effect_sizes <- c(effect_sizes, eta_squared_records(model))
  results$posthoc <- posthoc_results
  results$effect_sizes <- effect_sizes
  results
}
