#### Section: Barplot Construction ####

plot_anova_barplot_meanse <- function(data,
                                      info,
                                      layout_values = list(),
                                      line_colors = NULL,
                                      base_size = 14,
                                      posthoc_all = NULL,
                                      share_y_axis = FALSE,
                                      common_legend = FALSE,
                                      legend_position = NULL) {
  context <- initialize_anova_plot_context(data, info, layout_values)
  data <- context$data
  factor1 <- context$factor1
  factor2 <- context$factor2

  allowed_positions <- c("bottom", "top", "left", "right")
  legend_position_value <- if (!is.null(legend_position) && legend_position %in% allowed_positions) {
    legend_position
  } else {
    "bottom"
  }

  if (is.null(factor1) || length(context$responses) == 0) {
    return(NULL)
  }

  shared_y_limits <- if (isTRUE(share_y_axis)) {
    compute_barplot_shared_limits(
      context,
      data,
      factor1,
      factor2,
      posthoc_all
    )
  } else {
    NULL
  }

  base_fill <- if (!is.null(line_colors) && length(line_colors) > 0) {
    unname(line_colors)[1]
  } else {
    "#3E8FC4"
  }
  
  response_plots <- list()
  strata_panel_count <- context$initial_strata_panels
  
  for (resp in context$responses) {
    posthoc_entry <- NULL
    if (!is.null(posthoc_all) && !is.null(posthoc_all[[resp]])) {
      posthoc_entry <- posthoc_all[[resp]]
    }
    
    if (context$has_strata && !is.null(context$strat_var) && context$strat_var %in% names(data)) {
      stratum_plots <- list()
      
      for (stratum in context$strata_levels) {
        subset_rows <- !is.na(data[[context$strat_var]]) & data[[context$strat_var]] == stratum
        subset_data <- data[subset_rows, , drop = FALSE]
        if (nrow(subset_data) == 0) next
        
        stats_df <- anova_summarise_stats(subset_data, resp, factor1, factor2)
        if (nrow(stats_df) == 0) next
        
        stats_df <- apply_anova_factor_levels(stats_df, factor1, factor2, context$order1, context$order2)
        
        stratum_posthoc <- NULL
        if (!is.null(posthoc_entry) && !is.null(posthoc_entry[[stratum]])) {
          stratum_posthoc <- posthoc_entry[[stratum]]
        }
        
        stratum_plots[[stratum]] <- build_bar_plot_panel(
          stats_df = stats_df,
          title_text = stratum,
          factor1 = factor1,
          factor2 = factor2,
          line_colors = line_colors,
          base_fill = base_fill,
          base_size = base_size,
          posthoc_entry = stratum_posthoc,
          nested_posthoc = stratum_posthoc,
          y_limits = shared_y_limits
        )
      }
      
      if (length(stratum_plots) > 0) {
        strata_panel_count <- max(strata_panel_count, length(stratum_plots))
        current_layout <- adjust_grid_layout(length(stratum_plots), context$strata_layout)
        combined <- patchwork::wrap_plots(
          plotlist = stratum_plots,
          nrow = current_layout$nrow,
          ncol = current_layout$ncol
        )

        title_plot <- ggplot() +
          ta_plot_theme_void() +
          ggtitle(resp) +
          theme(plot.title = element_text(size = base_size, face = "bold", hjust = 0.5))

        response_plots[[resp]] <- title_plot / combined + patchwork::plot_layout(heights = c(0.08, 1))
      }
    } else {
      stats_df <- anova_summarise_stats(data, resp, factor1, factor2)
      if (nrow(stats_df) == 0) next
      
      stats_df <- apply_anova_factor_levels(stats_df, factor1, factor2, context$order1, context$order2)
      
      response_plots[[resp]] <- build_bar_plot_panel(
        stats_df = stats_df,
        title_text = resp,
        factor1 = factor1,
        factor2 = factor2,
        line_colors = line_colors,
        base_fill = base_fill,
        base_size = base_size,
        posthoc_entry = posthoc_entry,
        nested_posthoc = posthoc_entry,
        y_limits = shared_y_limits
      )
    }
  }
  
  finalize_anova_plot_result(
    response_plots = response_plots,
    context = context,
    strata_panel_count = strata_panel_count,
    collect_guides = isTRUE(common_legend),
    legend_position = if (isTRUE(common_legend)) legend_position_value else NULL
  )
}

compute_barplot_shared_limits <- function(context,
                                          data,
                                          factor1,
                                          factor2,
                                          posthoc_all = NULL) {
  combined <- NULL

  for (resp in context$responses) {
    posthoc_entry <- NULL
    if (!is.null(posthoc_all) && !is.null(posthoc_all[[resp]])) {
      posthoc_entry <- posthoc_all[[resp]]
    }

    if (context$has_strata && !is.null(context$strat_var) && context$strat_var %in% names(data)) {
      for (stratum in context$strata_levels) {
        subset_rows <- !is.na(data[[context$strat_var]]) & data[[context$strat_var]] == stratum
        subset_data <- data[subset_rows, , drop = FALSE]
        if (nrow(subset_data) == 0) next

        stats_df <- anova_summarise_stats(subset_data, resp, factor1, factor2)
        if (nrow(stats_df) == 0) next
        stats_df <- apply_anova_factor_levels(stats_df, factor1, factor2, context$order1, context$order2)

        stratum_posthoc <- NULL
        if (!is.null(posthoc_entry) && !is.null(posthoc_entry[[stratum]])) {
          stratum_posthoc <- posthoc_entry[[stratum]]
        }

        rng <- compute_barplot_panel_range(
          stats_df, factor1, factor2,
          posthoc_entry = stratum_posthoc,
          nested_posthoc = stratum_posthoc
        )
        combined <- update_numeric_range(combined, rng)
      }
    } else {
      stats_df <- anova_summarise_stats(data, resp, factor1, factor2)
      if (nrow(stats_df) == 0) next
      stats_df <- apply_anova_factor_levels(stats_df, factor1, factor2, context$order1, context$order2)

      rng <- compute_barplot_panel_range(
        stats_df, factor1, factor2,
        posthoc_entry = posthoc_entry,
        nested_posthoc = posthoc_entry
      )
      combined <- update_numeric_range(combined, rng)
    }
  }

  if (is.null(combined)) return(NULL)

  limits <- expand_axis_limits(combined, lower_mult = 0.05, upper_mult = 0.12)
  ensure_barplot_zero_baseline(limits)
}

compute_barplot_panel_range <- function(stats_df,
                                        factor1,
                                        factor2,
                                        posthoc_entry = NULL,
                                        nested_posthoc = NULL) {
  if (is.null(stats_df) || nrow(stats_df) == 0) return(NULL)
  values <- c(stats_df$mean - stats_df$se, stats_df$mean + stats_df$se)
  values <- values[is.finite(values)]
  if (length(values) == 0) return(NULL)
  rng <- range(values)
  max_val <- rng[2]

  if (is.null(factor2) || !factor2 %in% names(stats_df)) {
    prep <- prepare_significance_annotations_data(stats_df, factor1, posthoc_entry)
  } else {
    prep <- prepare_nested_significance_annotations_data(stats_df, factor1, factor2, nested_posthoc)
  }

  if (!is.null(prep) && !is.null(prep$max_y) && is.finite(prep$max_y)) {
    max_val <- max(max_val, prep$max_y)
  }

  c(rng[1], max_val)
}


expand_axis_limits <- function(range_vals, lower_mult = 0.05, upper_mult = 0.12) {
  if (is.null(range_vals) || length(range_vals) != 2 || any(!is.finite(range_vals))) return(range_vals)
  span <- diff(range_vals)
  if (!is.finite(span) || span == 0) {
    span <- max(1, abs(range_vals[2]))
  }
  c(range_vals[1] - span * lower_mult, range_vals[2] + span * upper_mult)
}


ensure_barplot_zero_baseline <- function(range_vals) {
  if (is.null(range_vals) || length(range_vals) != 2 || any(!is.finite(range_vals))) {
    return(range_vals)
  }
  
  lower <- range_vals[1]
  if (is.na(lower)) return(range_vals)
  
  # Keep barplots anchored at zero to avoid negative baselines when no annotations
  range_vals[1] <- 0
  range_vals
}


build_bar_plot_panel <- function(stats_df,
                                 title_text,
                                 factor1,
                                 factor2,
                                 line_colors,
                                 base_fill,
                                 base_size = 14,
                                 posthoc_entry = NULL,
                                 nested_posthoc = NULL,
                                 y_limits = NULL) {

  # Compute per-panel limits when not sharing axes so we can always anchor at zero
  if (is.null(y_limits)) {
    panel_range <- compute_barplot_panel_range(
      stats_df,
      factor1,
      factor2,
      posthoc_entry = posthoc_entry,
      nested_posthoc = nested_posthoc
    )

    if (!is.null(panel_range)) {
      y_limits <- expand_axis_limits(panel_range, lower_mult = 0, upper_mult = 0.12)
      y_limits <- ensure_barplot_zero_baseline(y_limits)
    }
  }

  if (is.null(factor2) || !factor2 %in% names(stats_df)) {
    return(
      build_single_factor_barplot(
        stats_df,
        title_text,
        factor1,
        base_fill,
        base_size,
        posthoc_entry,
        y_limits = y_limits
      )
    )
  }

  build_two_factor_barplot(
    stats_df,
    title_text,
    factor1,
    factor2,
    line_colors,
    base_fill,
    base_size,
    nested_posthoc,
    y_limits = y_limits
  )
}


build_single_factor_barplot <- function(stats_df,
                                        title_text,
                                        factor1,
                                        base_fill,
                                        base_size,
                                        posthoc_entry,
                                        y_limits = NULL) {
  format_numeric_labels <- scales::label_number(accuracy = 0.01, trim = TRUE)

  plot_obj <- ggplot(stats_df, aes(x = !!sym(factor1), y = mean)) +
    geom_col(fill = base_fill, width = 0.6, alpha = 0.8) +
    geom_errorbar(
      aes(ymin = mean - se, ymax = mean + se),
      width = 0.15,
      color = "gray40",
      linewidth = 0.5
    ) +
    ta_plot_theme(base_size = base_size) +
    labs(x = factor1, y = "Mean ± SE", title = title_text) +
    theme(
      plot.title = element_text(size = base_size, face = "bold", hjust = 0.5),
      axis.title.x = element_text(margin = margin(t = 6)),
      axis.title.y = element_text(margin = margin(r = 6)),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.line = element_line(color = "#9ca3af"),
      axis.ticks = element_line(color = "#9ca3af")
    )

  expand_scale <- is.null(y_limits)

  if (!is.null(posthoc_entry)) {
    plot_obj <- add_significance_annotations(
      plot_obj, stats_df, factor1, posthoc_entry,
      allow_scale_expansion = expand_scale
    )
  }

  if (!is.null(y_limits) && all(is.finite(y_limits))) {
    plot_obj <- plot_obj + scale_y_continuous(limits = y_limits, expand = expansion(mult = c(0, 0)))
  }

  plot_obj
}

build_two_factor_barplot <- function(stats_df,
                                     title_text,
                                     factor1,
                                     factor2,
                                     line_colors,
                                     base_fill,
                                     base_size,
                                     nested_posthoc = NULL,
                                     y_limits = NULL) {

  format_numeric_labels <- scales::label_number(accuracy = 0.01, trim = TRUE)
  
  group_levels <- if (is.factor(stats_df[[factor2]])) {
    levels(stats_df[[factor2]])
  } else {
    unique(as.character(stats_df[[factor2]]))
  }
  group_levels <- group_levels[!is.na(group_levels)]
  palette <- resolve_palette_for_levels(group_levels, custom = line_colors)
  dodge <- position_dodge(width = 0.7)
  
  plot_obj <- ggplot(stats_df, aes(x = !!sym(factor1), y = mean, fill = !!sym(factor2))) +
    geom_col(position = dodge, width = 0.6, alpha = 0.85) +
    geom_errorbar(
      aes(ymin = mean - se, ymax = mean + se),
      position = dodge,
      width = 0.2,
      color = "gray40",
      linewidth = 0.5
    ) +
    ta_plot_theme(base_size = base_size) +
    labs(x = factor1, y = "Mean ± SE", fill = factor2, title = title_text) +
    theme(
      plot.title = element_text(size = base_size, face = "bold", hjust = 0.5),
      axis.title.x = element_text(margin = margin(t = 6)),
      axis.title.y = element_text(margin = margin(r = 6)),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.text.x = element_text(angle = 45, hjust = 1),
      axis.line = element_line(color = "#9ca3af"),
      axis.ticks = element_line(color = "#9ca3af")
    ) +
    scale_fill_manual(values = palette)

  expand_scale <- is.null(y_limits)

  if (!is.null(nested_posthoc)) {
    plot_obj <- add_nested_significance_annotations(
      plot_obj, stats_df, factor1, factor2, nested_posthoc,
      allow_scale_expansion = expand_scale
    )
  }

  if (!is.null(y_limits) && all(is.finite(y_limits))) {
    plot_obj <- plot_obj + scale_y_continuous(limits = y_limits, expand = expansion(mult = c(0, 0)))
  }

  plot_obj
}


#### Section: Significance Annotation System ####

prepare_significance_annotations_data <- function(stats_df, factor1, posthoc_entry) {
  if (is.null(posthoc_entry) || !is.data.frame(posthoc_entry)) return(NULL)
  if (is.null(factor1) || !factor1 %in% names(stats_df)) return(NULL)

  # Identify reference level (first level on the plot)
  level_values <- levels(stats_df[[factor1]])
  if (is.null(level_values)) level_values <- unique(as.character(stats_df[[factor1]]))
  level_values <- level_values[!is.na(level_values)]
  if (length(level_values) < 2) return(NULL)
  reference_level <- level_values[1]

  # Clean p-values
  signif_df <- posthoc_entry
  signif_df$p.value <- as.character(signif_df$p.value)
  signif_df$p.value <- gsub("[[:space:]]", "", signif_df$p.value)
  signif_df$p.value <- gsub("^<\\.?0*", "0.", signif_df$p.value)
  signif_df$p.value <- suppressWarnings(as.numeric(signif_df$p.value))
  signif_df <- signif_df |> dplyr::filter(!is.na(p.value))
  if (nrow(signif_df) == 0) return(NULL)

  bar_heights <- stats_df$mean + stats_df$se
  bar_heights <- bar_heights[is.finite(bar_heights)]
  height_span <- diff(range(bar_heights))
  if (!is.finite(height_span) || height_span == 0) {
    height_span <- if (length(bar_heights) > 0) max(bar_heights) else 0
  }
  offset <- if (is.finite(height_span) && height_span > 0) height_span * 0.08 else 0.1

  level_lookup <- stats_df |> dplyr::mutate(.height = mean + se)
  level_lookup <- setNames(level_lookup$.height, as.character(level_lookup[[factor1]]))

  markers <- list()
  for (lvl in level_values[level_values != reference_level]) {
    contrasts <- c(paste0(lvl, " - ", reference_level), paste0(reference_level, " - ", lvl))
    match_row <- signif_df[signif_df$contrast %in% contrasts, , drop = FALSE]
    if (nrow(match_row) == 0) next

    pval <- match_row$p.value[1]
    if (is.na(pval) || pval >= 0.05) next

    annotation <- dplyr::case_when(
      pval < 0.001 ~ "***",
      pval < 0.01  ~ "**",
      pval < 0.05  ~ "*",
      TRUE ~ ""
    )

    y_base <- unname(level_lookup[[as.character(lvl)]])
    if (!is.finite(y_base)) next

    markers[[length(markers) + 1]] <- tibble::tibble(
      !!factor1 := factor(lvl, levels = level_values),
      y_position = y_base + offset,
      annotations = annotation
    )
  }

  if (length(markers) == 0) return(NULL)

  marker_df <- dplyr::bind_rows(markers)

  list(
    data = marker_df,
    max_y = max(marker_df$y_position, na.rm = TRUE) * 1.05
  )
}

add_significance_annotations <- function(plot_obj,
                                         stats_df,
                                         factor1,
                                         posthoc_entry,
                                         allow_scale_expansion = TRUE) {
  prep <- prepare_significance_annotations_data(stats_df, factor1, posthoc_entry)
  if (is.null(prep)) return(plot_obj)

  plot_obj <- plot_obj + geom_text(
    data = prep$data,
    aes(x = !!sym(factor1), y = y_position, label = annotations),
    inherit.aes = FALSE,
    color = "gray30",
    size = 4,
    fontface = "bold"
  )

  if (isTRUE(allow_scale_expansion)) {
    plot_obj <- plot_obj + scale_y_continuous(
      expand = expansion(mult = c(0, 0.08)),
      limits = c(NA, prep$max_y)
    )
  }

  plot_obj
}

prepare_nested_significance_annotations_data <- function(stats_df,
                                                         factor1,
                                                         factor2,
                                                         nested_posthoc,
                                                         dodge_width = 0.7) {
  nested_name <- paste0(factor2, "_within_", factor1)

  # Accept both a flat data.frame or a list entry
  df <- NULL
  if (is.data.frame(nested_posthoc)) {
    if (!"Factor" %in% names(nested_posthoc)) return(NULL)
    df <- dplyr::filter(nested_posthoc, .data$Factor == nested_name)
  } else if (is.list(nested_posthoc) && nested_name %in% names(nested_posthoc)) {
    df <- nested_posthoc[[nested_name]]
  } else {
    return(NULL)
  }
  if (is.null(df) || nrow(df) == 0) return(NULL)
  if (!all(c("contrast", "p.value", factor1) %in% names(df))) return(NULL)

  # Clean p-values, keep all non-missing rows for filtering against the reference
  df$p.value <- as.character(df$p.value)
  df$p.value <- gsub("[[:space:]]", "", df$p.value)
  df$p.value <- gsub("^<\\.?0*", "0.", df$p.value)
  df$p.value <- suppressWarnings(as.numeric(df$p.value))
  df <- dplyr::filter(df, !is.na(.data$p.value))
  if (nrow(df) == 0) return(NULL)

  lev1 <- levels(stats_df[[factor1]])
  lev2 <- levels(stats_df[[factor2]])
  if (is.null(lev1)) lev1 <- unique(as.character(stats_df[[factor1]]))
  if (is.null(lev2)) lev2 <- unique(as.character(stats_df[[factor2]]))
  lev1 <- lev1[!is.na(lev1)]
  lev2 <- lev2[!is.na(lev2)]
  if (length(lev2) < 2) return(NULL)
  reference_level <- lev2[1]

  bar_heights <- stats_df |> dplyr::mutate(bar_height = mean + se)
  height_lookup <- function(xlvl, glvl) {
    row <- dplyr::filter(bar_heights, .data[[factor1]] == xlvl & .data[[factor2]] == glvl)
    if (nrow(row) == 0) return(NA_real_)
    row$bar_height[1]
  }

  values <- bar_heights$bar_height[is.finite(bar_heights$bar_height)]
  span <- diff(range(values))
  if (!is.finite(span) || span == 0) {
    span <- if (length(values) > 0) max(values) else 0
  }
  offset <- if (is.finite(span) && span > 0) span * 0.08 else 0.1

  markers <- list()
  for (g1 in lev1) {
    for (lvl in lev2[lev2 != reference_level]) {
      contrasts <- c(paste0(lvl, " - ", reference_level), paste0(reference_level, " - ", lvl))
      match_row <- df[df[[factor1]] == g1 & df$contrast %in% contrasts, , drop = FALSE]
      if (nrow(match_row) == 0) next

      pval <- match_row$p.value[1]
      if (is.na(pval) || pval >= 0.05) next

      annotation <- dplyr::case_when(
        pval < 0.001 ~ "***",
        pval < 0.01  ~ "**",
        pval < 0.05  ~ "*",
        TRUE ~ ""
      )

      y_base <- height_lookup(g1, lvl)
      if (!is.finite(y_base)) next

      markers[[length(markers) + 1]] <- tibble::tibble(
        !!factor1 := factor(g1, levels = lev1),
        !!factor2 := factor(lvl, levels = lev2),
        y_position = y_base + offset,
        annotations = annotation
      )
    }
  }

  if (length(markers) == 0) return(NULL)
  marker_df <- dplyr::bind_rows(markers)

  list(
    data = marker_df,
    max_y = max(marker_df$y_position, na.rm = TRUE) * 1.05
  )
}

add_nested_significance_annotations <- function(plot_obj,
                                                stats_df,
                                                factor1,
                                                factor2,
                                                nested_posthoc,
                                                dodge_width = 0.7,
                                                allow_scale_expansion = TRUE) {
  prep <- prepare_nested_significance_annotations_data(
    stats_df, factor1, factor2, nested_posthoc, dodge_width
  )
  if (is.null(prep)) return(plot_obj)

  plot_obj <- plot_obj + geom_text(
    data = prep$data,
    aes(x = !!sym(factor1), y = y_position, label = annotations, group = !!sym(factor2)),
    inherit.aes = FALSE,
    color = "gray30",
    size = 4,
    fontface = "bold",
    position = position_dodge(width = dodge_width)
  )

  if (isTRUE(allow_scale_expansion)) {
    plot_obj <- plot_obj + scale_y_continuous(
      expand = expansion(mult = c(0, 0.08)),
      limits = c(NA, prep$max_y)
    )
  }

  plot_obj
}
