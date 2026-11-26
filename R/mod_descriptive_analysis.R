compute_descriptive_summary <- function(data, group_var = NULL) {
  numeric_vars <- names(data)[sapply(data, is.numeric)]
  group_data <- if (!is.null(group_var)) dplyr::group_by(data, .data[[group_var]], .drop = TRUE) else data

  summarise_numeric <- function(.data, vars, suffix, fn) {
    if (length(vars) == 0) {
      return(NULL)
    }
    .data %>%
      dplyr::summarise(
        dplyr::across(
          dplyr::all_of(vars),
          fn,
          .names = paste0(suffix, "_{.col}")
        ),
        .groups = "drop"
      )
  }

  # Define a custom skim function with full factor labels
  top_counts_full <- function(x) skimr::top_counts(x, max_char = 200, max_levels = 10)
  skim_full <- skimr::skim_with(
    factor = skimr::sfl(
      n_unique   = skimr::n_unique,
      n_singletons = function(x) {
        tab <- table(x, useNA = "ifany")
        sum(tab == 1)
      },
      top_counts = top_counts_full
    ),
    append = FALSE
  )
  
  list(
    skim = if (!is.null(group_var)) skim_full(group_data) else skim_full(data),
    cv = summarise_numeric(
      group_data,
      numeric_vars,
      "cv",
      ~ 100 * stats::sd(.x, na.rm = TRUE) / mean(.x, na.rm = TRUE)
    ),
    outliers = summarise_numeric(
      group_data,
      numeric_vars,
      "outliers",
      ~ {
        q <- stats::quantile(.x, probs = c(0.25, 0.75), na.rm = TRUE)
        iqr <- q[2] - q[1]
        sum(.x < q[1] - 1.5 * iqr | .x > q[2] + 1.5 * iqr, na.rm = TRUE)
      }
    ),
    missing = summarise_numeric(
      group_data,
      numeric_vars,
      "missing",
      ~ 100 * mean(is.na(.x))
    ),
    distribution = summarise_numeric(
      group_data,
      numeric_vars,
      "distribution",
      ~ most_likely_distribution(.x)
    ),
    skewness = summarise_numeric(
      group_data,
      numeric_vars,
      "skewness",
      ~ {
        x <- stats::na.omit(.x)
        if (length(x) < 3 || stats::sd(x) == 0) return(NA_real_)
        mean(((x - mean(x)) / stats::sd(x))^3)
      }
    ),
    kurtosis = summarise_numeric(
      group_data,
      numeric_vars,
      "kurtosis",
      ~ {
        x <- stats::na.omit(.x)
        if (length(x) < 4 || stats::sd(x) == 0) return(NA_real_)
        mean(((x - mean(x)) / stats::sd(x))^4) - 3
      }
    )
  )
}

most_likely_distribution <- function(values) {
  values <- values[is.finite(values)]
  if (length(values) < 5 || length(unique(values)) < 2) {
    return(NA_character_)
  }
  
  candidates <- c(
    norm   = TRUE,
    lnorm  = all(values > 0),
    gamma  = all(values > 0),
    weibull = all(values > 0),
    exp    = all(values > 0)
  )
  
  candidate_names <- names(candidates)[candidates]
  if (length(candidate_names) == 0) {
    return(NA_character_)
  }
  
  fits <- purrr::map(candidate_names, function(dist_name) {
    fit <- safe_fitdist(values, dist_name)
    if (is.null(fit)) return(NULL)
    tibble::tibble(distribution = dist_name, aic = stats::AIC(fit))
  })
  
  fits <- purrr::compact(fits)
  if (length(fits) == 0) return(NA_character_)
  
  best <- dplyr::bind_rows(fits) |>
    dplyr::arrange(.data$aic) |>
    dplyr::slice(1)
  
  label_map <- c(
    norm = "Normal",
    lnorm = "Log-normal",
    gamma = "Gamma",
    weibull = "Weibull",
    exp = "Exponential"
  )
  
  label_map[[best$distribution]] %||% best$distribution
}

safe_fitdist <- function(values, dist_name) {
  suppressWarnings(
    tryCatch(
      fitdistrplus::fitdist(values, dist_name),
      error = function(e) NULL
    )
  )
}

# ---- Shared printing ----
print_summary_sections <- function(results, digits = 3) {
  old_opts <- options(digits = digits)
  on.exit(options(old_opts), add = TRUE)
  # 1) Print skim (shortened headers for variable types)
  skim_lines <- capture.output(print(results$skim))
  skim_lines <- sub(
    "^── Variable type: ([[:alpha:]]+) ─+",
    "── Variable type: \\1 ──",
    skim_lines
  )
  cat(paste(skim_lines, collapse = "\n"), "\n\n", sep = "")
  
  # 2) Helper to detect if a grouping column exists and what it's called
  metric_prefix <- "^(cv_|outliers_|missing_|distribution_|skewness_|kurtosis_)"
  first_col <- if (!is.null(results$cv) && ncol(results$cv) > 0) names(results$cv)[1] else NULL
  group_col <- if (!is.null(first_col) && !grepl(metric_prefix, first_col)) first_col else NULL
  
  # 3) Robust long conversion that preserves the real group column name (if any)
  to_long <- function(df, value_name) {
    if (is.null(df) || ncol(df) == 0) {
      empty_cols <- list(variable = character())
      if (!is.null(group_col)) {
        empty_cols[[group_col]] <- character()
      }
      empty_cols[[value_name]] <- numeric()
      return(tibble::as_tibble(empty_cols))
    }
    id_cols <- intersect(names(df), group_col)
    df |>
      tidyr::pivot_longer(
        cols = -tidyselect::any_of(id_cols),
        names_to = "variable",
        values_to = value_name
      ) |>
      dplyr::mutate(variable = sub(metric_prefix, "", .data$variable))
  }

  # 4) Build pieces (no "missing" here)
  cv_long   <- to_long(results$cv, "cv")
  out_long  <- to_long(results$outliers, "outliers")
  dist_long <- to_long(results$distribution, "distribution")
  skew_long <- to_long(results$skewness, "skewness")
  kurt_long <- to_long(results$kurtosis, "kurtosis")

  # 5) Join by the right keys
  join_keys <- c(if (!is.null(group_col)) group_col, "variable")
  merged <- dplyr::full_join(cv_long, out_long, by = join_keys) |>
    dplyr::full_join(dist_long, by = join_keys) |>
    dplyr::full_join(skew_long, by = join_keys) |>
    dplyr::full_join(kurt_long, by = join_keys) |>
    dplyr::mutate(
      cv = round(cv, 2),
      skewness = round(skewness, 3),
      kurtosis = round(kurtosis, 3)
    )

  numeric_order <- NULL
  if (is.data.frame(results$skim) && all(c("skim_type", "skim_variable") %in% names(results$skim))) {
    numeric_order <- results$skim |>
      dplyr::filter(.data$skim_type == "numeric") |>
      dplyr::pull(.data$skim_variable) |>
      unique()
  }

  arrange_cols <- c(if (!is.null(group_col)) group_col, "variable")
  if (!is.null(numeric_order) && length(numeric_order) > 0) {
    merged$variable <- factor(merged$variable, levels = numeric_order)
    merged <- dplyr::arrange(merged, !!!rlang::syms(arrange_cols))
    merged$variable <- as.character(merged$variable)
  } else {
    merged <- dplyr::arrange(merged, !!!rlang::syms(arrange_cols))
  }

  # 7) Print with/without group column
  cat("── Numeric variables summary ──\n")
  final_cols <- c(
    "variable",
    arrange_cols[-length(arrange_cols)],
    "cv",
    "outliers",
    "distribution",
    "skewness",
    "kurtosis"
  )
  final_df <- merged[, final_cols, drop = FALSE]
  print(as.data.frame(final_df), row.names = FALSE)

  invisible(NULL)
}
