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

run_posthoc <- function(spec, model, data, results) {
  posthoc_results <- list()
  effect_sizes <- default_if_null(results$effect_sizes, list())
  correction <- default_if_null(spec$posthoc$correction, "bonferroni")

  for (factor_name in default_if_null(spec$posthoc$factors, character())) {
    factor_result <- tryCatch(
      {
        emm <- emmeans::emmeans(model, specs = stats::as.formula(paste("~", factor_name)))
        pairwise <- summary(emmeans::contrast(emm, method = "pairwise", adjust = correction))
        pairwise_df <- as.data.frame(pairwise, stringsAsFactors = FALSE)
        sigma_value <- sigma_for_effect_size(model)

        if (nrow(pairwise_df) > 0L) {
          posthoc_rows <- lapply(seq_len(nrow(pairwise_df)), function(i) {
            list(
              factor = factor_name,
              contrast = as.character(pairwise_df$contrast[i]),
              estimate = safe_as_numeric(pairwise_df$estimate[i]),
              se = safe_as_numeric(pairwise_df$SE[i]),
              p.adj = safe_as_numeric(pairwise_df$p.value[i])
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
        } else {
          posthoc_rows <- list()
          d_rows <- list()
        }

        list(posthoc = posthoc_rows, effects = d_rows)
      },
      error = function(err) {
        list(error = sprintf("Post-hoc failed for factor '%s': %s", factor_name, conditionMessage(err)))
      }
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
