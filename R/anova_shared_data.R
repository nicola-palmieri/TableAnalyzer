#### Section: Summary Calculations ####

anova_summarise_stats <- function(df_subset, resp_name, factor1, factor2) {
  if (is.null(factor1) || !factor1 %in% names(df_subset)) {
    return(tibble::tibble())
  }

  if (is.null(factor2) || !factor2 %in% names(df_subset)) {
    df_subset |>
      dplyr::group_by(.data[[factor1]]) |>
      dplyr::summarise(
        mean = mean(.data[[resp_name]], na.rm = TRUE),
        se = sd(.data[[resp_name]], na.rm = TRUE) / sqrt(sum(!is.na(.data[[resp_name]]))),
        .groups = "drop"
      )
  } else {
    df_subset |>
      dplyr::group_by(.data[[factor1]], .data[[factor2]]) |>
      dplyr::summarise(
        mean = mean(.data[[resp_name]], na.rm = TRUE),
        se = sd(.data[[resp_name]], na.rm = TRUE) / sqrt(sum(!is.na(.data[[resp_name]]))),
        .groups = "drop"
      )
  }
}

apply_anova_factor_levels <- function(stats_df, factor1, factor2, order1, order2) {
  if (!is.null(factor1) && factor1 %in% names(stats_df)) {
    if (!is.null(order1)) {
      stats_df[[factor1]] <- factor(as.character(stats_df[[factor1]]), levels = order1)
      stats_df <- stats_df[!is.na(stats_df[[factor1]]), , drop = FALSE]
    } else {
      stats_df[[factor1]] <- factor(as.character(stats_df[[factor1]]))
    }
  }

  if (!is.null(factor2) && factor2 %in% names(stats_df)) {
    levels2 <- if (!is.null(order2)) {
      order2
    } else {
      unique(as.character(stats_df[[factor2]]))
    }
    stats_df[[factor2]] <- factor(as.character(stats_df[[factor2]]), levels = levels2)
    stats_df <- stats_df[!is.na(stats_df[[factor2]]), , drop = FALSE]
  }

  stats_df
}
