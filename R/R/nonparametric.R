normalize_nonparametric_data <- function(data, dv, factor_name, block_col = NULL) {
  columns <- c(dv, factor_name, block_col)
  working <- stats::na.omit(data[, columns[!is.na(columns) & nzchar(columns)], drop = FALSE])
  working[[factor_name]] <- as.factor(working[[factor_name]])
  if (!is.null(block_col) && nzchar(block_col)) {
    working[[block_col]] <- as.factor(working[[block_col]])
  }
  working
}

complete_block_data <- function(data, dv, factor_name, block_col) {
  aggregated <- stats::aggregate(
    data[[dv]],
    by = list(block = data[[block_col]], group = data[[factor_name]]),
    FUN = mean
  )
  names(aggregated) <- c(block_col, factor_name, dv)
  level_count <- nlevels(as.factor(aggregated[[factor_name]]))
  block_counts <- table(aggregated[[block_col]])
  complete_blocks <- names(block_counts[block_counts == level_count])
  aggregated[aggregated[[block_col]] %in% complete_blocks, , drop = FALSE]
}

rank_biserial_effect_size <- function(values, groups) {
  groups <- as.factor(groups)
  if (nlevels(groups) != 2L) {
    return(NA_real_)
  }
  idx_first <- groups == levels(groups)[1L]
  first_values <- values[idx_first]
  second_values <- values[!idx_first]
  if (length(first_values) == 0L || length(second_values) == 0L) {
    return(NA_real_)
  }
  wt <- suppressWarnings(stats::wilcox.test(first_values, second_values, exact = FALSE))
  n1 <- length(first_values)
  n2 <- length(second_values)
  u_stat <- unname(wt$statistic) - (n1 * (n1 + 1) / 2)
  (2 * u_stat / (n1 * n2)) - 1
}

matched_rank_biserial_effect_size <- function(data, dv, factor_name, block_col) {
  groups <- levels(as.factor(data[[factor_name]]))
  if (length(groups) != 2L) {
    return(NA_real_)
  }
  pair_data <- complete_block_data(data, dv, factor_name, block_col)
  wide <- reshape(
    pair_data,
    idvar = block_col,
    timevar = factor_name,
    direction = "wide"
  )
  first_col <- paste0(dv, ".", groups[1L])
  second_col <- paste0(dv, ".", groups[2L])
  if (!all(c(first_col, second_col) %in% names(wide))) {
    return(NA_real_)
  }
  diffs <- wide[[first_col]] - wide[[second_col]]
  diffs <- diffs[!is.na(diffs) & diffs != 0]
  if (length(diffs) == 0L) {
    return(NA_real_)
  }
  ranks <- rank(abs(diffs))
  pos_sum <- sum(ranks[diffs > 0], na.rm = TRUE)
  neg_sum <- sum(ranks[diffs < 0], na.rm = TRUE)
  (pos_sum - neg_sum) / (pos_sum + neg_sum)
}

kruskal_epsilon_squared <- function(statistic, n_obs, k_levels) {
  if (is.null(statistic) || n_obs <= k_levels) {
    return(NA_real_)
  }
  max(0, (as.numeric(statistic) - k_levels + 1) / (n_obs - k_levels))
}

kendalls_w_from_friedman <- function(statistic, n_blocks, k_levels) {
  if (is.null(statistic) || n_blocks <= 0L || k_levels <= 1L) {
    return(NA_real_)
  }
  as.numeric(statistic) / (n_blocks * (k_levels - 1))
}

nonparametric_posthoc_records <- function(data, dv, factor_name, correction = "bonferroni", block_col = NULL) {
  groups <- levels(as.factor(data[[factor_name]]))
  if (length(groups) <= 2L) {
    return(list())
  }
  correction_method <- if (correction %in% c("bonferroni", "holm", "none")) correction else "holm"

  if (!is.null(block_col) && nzchar(block_col)) {
    pair_records <- list()
    pair_index <- 1L
    for (i in seq_len(length(groups) - 1L)) {
      for (j in seq.int(i + 1L, length(groups))) {
        subset_data <- data[data[[factor_name]] %in% groups[c(i, j)], , drop = FALSE]
        pair_data <- complete_block_data(subset_data, dv, factor_name, block_col)
        wide <- reshape(pair_data, idvar = block_col, timevar = factor_name, direction = "wide")
        first_col <- paste0(dv, ".", groups[i])
        second_col <- paste0(dv, ".", groups[j])
        if (!all(c(first_col, second_col) %in% names(wide))) {
          next
        }
        test_result <- suppressWarnings(stats::wilcox.test(wide[[first_col]], wide[[second_col]], paired = TRUE, exact = FALSE))
        pair_records[[pair_index]] <- list(
          factor = factor_name,
          contrast = paste(groups[i], "-", groups[j]),
          estimate = median(wide[[first_col]] - wide[[second_col]], na.rm = TRUE),
          statistic = safe_as_numeric(unname(test_result$statistic)),
          p = safe_as_numeric(test_result$p.value)
        )
        pair_index <- pair_index + 1L
      }
    }
    if (length(pair_records) == 0L) {
      return(list())
    }
    p_adjusted <- stats::p.adjust(vapply(pair_records, function(row) row$p, numeric(1L)), method = correction_method)
    for (i in seq_along(pair_records)) {
      pair_records[[i]]$p.adj <- safe_as_numeric(p_adjusted[i])
    }
    return(pair_records)
  }

  pw <- suppressWarnings(stats::pairwise.wilcox.test(data[[dv]], data[[factor_name]], p.adjust.method = correction_method, exact = FALSE))
  if (is.null(pw$p.value)) {
    return(list())
  }
  records <- list()
  idx <- 1L
  for (row_name in rownames(pw$p.value)) {
    for (col_name in colnames(pw$p.value)) {
      p_val <- pw$p.value[row_name, col_name]
      if (is.na(p_val)) {
        next
      }
      records[[idx]] <- list(
        factor = factor_name,
        contrast = paste(row_name, "-", col_name),
        p.adj = safe_as_numeric(p_val)
      )
      idx <- idx + 1L
    }
  }
  records
}

extract_coin_statistic <- function(test_object) {
  for (stat_type in c("test", "standardized", "linear")) {
    value <- tryCatch(coin::statistic(test_object, type = stat_type), error = function(...) NULL)
    if (!is.null(value)) {
      numeric_value <- suppressWarnings(as.numeric(value))
      if (length(numeric_value) > 0L && is.finite(numeric_value[1L])) {
        return(numeric_value[1L])
      }
    }
  }
  NULL
}

permutation_test_fallback <- function(data, dv, factor_name, block_col = NULL) {
  if (!requireNamespace("coin", quietly = TRUE)) {
    stop("coin is required for nonparametric permutation fallback.")
  }
  formula_text <- if (!is.null(block_col) && nzchar(block_col)) {
    paste(dv, "~", factor_name, "|", block_col)
  } else {
    paste(dv, "~", factor_name)
  }
  formula_obj <- stats::as.formula(formula_text)
  level_count <- nlevels(as.factor(data[[factor_name]]))
  test_object <- if (!is.null(block_col) && nzchar(block_col)) {
    if (level_count == 2L) {
      coin::wilcoxsign_test(formula_obj, data = data, distribution = coin::approximate(nresample = 5000))
    } else {
      coin::friedman_test(formula_obj, data = data, distribution = coin::approximate(nresample = 5000))
    }
  } else {
    coin::oneway_test(formula_obj, data = data, distribution = coin::approximate(nresample = 5000))
  }

  method <- if (!is.null(block_col) && nzchar(block_col)) {
    if (level_count == 2L) "coin::wilcoxsign_test" else "coin::friedman_test"
  } else {
    "coin::oneway_test"
  }

  list(
    method = method,
    statistic = safe_as_numeric(extract_coin_statistic(test_object)),
    p = safe_as_numeric(coin::pvalue(test_object))
  )
}

fit_nonparametric_analysis <- function(spec, data) {
  dv <- default_if_null(spec$dependent_vars, character())[1L]
  factor_name <- default_if_null(spec$independent_vars, character())[1L]
  if (is.na(dv) || !nzchar(dv) || is.na(factor_name) || !nzchar(factor_name)) {
    stop("Nonparametric analysis requires one dependent variable and one grouping factor.")
  }

  block_candidates <- c(default_if_null(spec$id_col, NULL), default_if_null(spec$random_effects, character()))
  block_candidates <- block_candidates[!is.na(block_candidates) & nzchar(block_candidates)]
  block_col <- NULL
  for (candidate in block_candidates) {
    if (candidate %in% names(data)) {
      block_col <- candidate
      break
    }
  }

  working <- normalize_nonparametric_data(data, dv, factor_name, block_col)
  if (!is.null(block_col)) {
    working <- complete_block_data(working, dv, factor_name, block_col)
  }
  level_count <- nlevels(as.factor(working[[factor_name]]))
  if (level_count < 2L) {
    stop("Nonparametric analysis requires at least two levels in the first independent variable.")
  }

  method <- NULL
  backbone <- NULL
  statistic <- NULL
  p_value <- NULL
  df_value <- if (level_count > 2L) level_count - 1L else NULL
  effect_metric <- NULL
  effect_value <- NULL

  test_result <- tryCatch(
    {
      if (!is.null(block_col) && nzchar(block_col)) {
        method <- "stats::friedman.test"
        backbone <- "stats::friedman.test"
        stats::friedman.test(stats::as.formula(paste(dv, "~", factor_name, "|", block_col)), data = working)
      } else if (level_count == 2L) {
        method <- "stats::wilcox.test"
        backbone <- "stats::wilcox.test"
        suppressWarnings(stats::wilcox.test(stats::as.formula(paste(dv, "~", factor_name)), data = working, exact = FALSE))
      } else {
        method <- "stats::kruskal.test"
        backbone <- "stats::kruskal.test"
        stats::kruskal.test(stats::as.formula(paste(dv, "~", factor_name)), data = working)
      }
    },
    error = function(err) err
  )

  if (inherits(test_result, "error")) {
    fallback <- permutation_test_fallback(working, dv, factor_name, block_col)
    method <- fallback$method
    backbone <- fallback$method
    statistic <- fallback$statistic
    p_value <- fallback$p
  } else {
    statistic <- safe_as_numeric(unname(test_result$statistic))
    p_value <- safe_as_numeric(test_result$p.value)
    if ("parameter" %in% names(test_result)) {
      df_value <- safe_as_numeric(unname(test_result$parameter))
    }
  }

  if (!is.null(block_col) && nzchar(block_col)) {
    effect_metric <- "kendalls_w"
    effect_value <- kendalls_w_from_friedman(
      statistic,
      n_blocks = length(unique(working[[block_col]])),
      k_levels = level_count
    )
  } else if (level_count == 2L) {
    effect_metric <- "rank_biserial"
    effect_value <- rank_biserial_effect_size(working[[dv]], working[[factor_name]])
  } else {
    effect_metric <- "epsilon_squared"
    effect_value <- kruskal_epsilon_squared(statistic, nrow(working), level_count)
  }

  posthoc_records <- nonparametric_posthoc_records(
    working,
    dv = dv,
    factor_name = factor_name,
    correction = default_if_null(spec$posthoc$correction, "bonferroni"),
    block_col = block_col
  )

  results <- list(
    model_family_used = "nonparametric",
    backbone_package = backbone,
    formula = default_if_null(spec$formula, paste(dv, "~", factor_name)),
    convergence_ok = TRUE,
    distribution_parameters = NULL,
    anova_table = list(),
    coefficients = list(),
    r_squared = NULL,
    aic = NULL,
    bic = NULL,
    logLik = NULL,
    nonparametric_test = list(
      method = method,
      statistic = statistic,
      df = df_value,
      p = p_value,
      effect_size_metric = effect_metric,
      effect_size_value = safe_as_numeric(effect_value)
    ),
    posthoc = posthoc_records,
    effect_sizes = if (!is.null(effect_metric)) list(list(term = factor_name, metric = effect_metric, value = safe_as_numeric(effect_value))) else list()
  )

  list(
    model = NULL,
    results = results,
    aic = NULL,
    data_used = working
  )
}
