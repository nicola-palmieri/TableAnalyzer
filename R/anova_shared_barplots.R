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
  legend_position_value <- if (!is.null(legend_position) && legend_position %in% allowed_positions) legend_position else "bottom"

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
  
  base_fill <- if (!is.null(line_colors) && length(line_colors) > 0) unname(line_colors)[1] else "#3E8FC4"

  response_results <- lapply(
    context$responses,
    function(resp) {
      build_response_barplot(
        resp = resp,
        context = context,
        data = data,
        factor1 = factor1,
        factor2 = factor2,
        line_colors = line_colors,
        base_fill = base_fill,
        base_size = base_size,
        shared_y_limits = shared_y_limits,
        posthoc_all = posthoc_all
      )
    }
  )

  strata_panel_count <- max(
    context$initial_strata_panels,
    vapply(response_results, function(x) x$strata_panels, integer(1))
  )

  response_plots <- lapply(response_results, `[[`, "plot")
  names(response_plots) <- context$responses
  response_plots <- Filter(Negate(is.null), response_plots)

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
    posthoc_entry <- get_posthoc_entry_for_response(posthoc_all, resp)
    stats_entries <- generate_anova_stats_entries(
      context = context,
      data = data,
      resp = resp,
      factor1 = factor1,
      factor2 = factor2,
      posthoc_entry = posthoc_entry
    )

    for (entry in stats_entries) {
      rng <- compute_barplot_panel_range(
        entry$stats_df,
        factor1,
        factor2,
        posthoc_entry = entry$posthoc,
        nested_posthoc = entry$posthoc
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
  range(values)
}

expand_axis_limits <- function(range_vals, lower_mult = 0.05, upper_mult = 0.12) {
  if (is.null(range_vals) || length(range_vals) != 2 || any(!is.finite(range_vals))) return(range_vals)
  span <- diff(range_vals)
  if (!is.finite(span) || span == 0) span <- max(1, abs(range_vals[2]))
  c(range_vals[1] - span * lower_mult, range_vals[2] + span * upper_mult)
}

ensure_barplot_zero_baseline <- function(range_vals) {
  if (is.null(range_vals) || length(range_vals) != 2 || any(!is.finite(range_vals))) {
    return(range_vals)
  }

  if (!is.na(range_vals[1])) range_vals[1] <- 0
  range_vals
}

get_posthoc_entry_for_response <- function(posthoc_all, resp) {
  if (is.null(posthoc_all) || is.null(posthoc_all[[resp]])) return(NULL)
  posthoc_all[[resp]]
}

build_response_barplot <- function(resp,
                                   context,
                                   data,
                                   factor1,
                                   factor2,
                                   line_colors,
                                   base_fill,
                                   base_size,
                                   shared_y_limits,
                                   posthoc_all) {
  posthoc_entry <- get_posthoc_entry_for_response(posthoc_all, resp)
  stats_entries <- generate_anova_stats_entries(
    context = context,
    data = data,
    resp = resp,
    factor1 = factor1,
    factor2 = factor2,
    posthoc_entry = posthoc_entry
  )

  has_strata <- context$has_strata && !is.null(context$strat_var) && context$strat_var %in% names(data)
  if (has_strata) {
    stratum_plots <- lapply(stats_entries, function(entry) {
      build_bar_plot_panel(
        stats_df = entry$stats_df,
        title_text = entry$label,
        factor1 = factor1,
        factor2 = factor2,
        line_colors = line_colors,
        base_fill = base_fill,
        base_size = base_size,
        posthoc_entry = entry$posthoc,
        nested_posthoc = entry$posthoc,
        y_limits = shared_y_limits,
        response_var = resp
      )
    })
    names(stratum_plots) <- vapply(stats_entries, function(x) x$label, character(1))
    stratum_plots <- Filter(Negate(is.null), stratum_plots)

    if (!length(stratum_plots)) {
      return(list(plot = NULL, strata_panels = 0L))
    }

    return(list(
      plot = patchwork::wrap_plots(
        plotlist = stratum_plots,
        nrow = context$strata_layout$nrow,
        ncol = context$strata_layout$ncol
      ),
      strata_panels = length(stratum_plots)
    ))
  }

  if (length(stats_entries) > 0) {
    entry <- stats_entries[[1]]
    return(list(
      plot = build_bar_plot_panel(
        stats_df = entry$stats_df,
        title_text = "",
        factor1 = factor1,
        factor2 = factor2,
        line_colors = line_colors,
        base_fill = base_fill,
        base_size = base_size,
        posthoc_entry = entry$posthoc,
        nested_posthoc = entry$posthoc,
        y_limits = shared_y_limits,
        response_var = resp
      ),
      strata_panels = 0L
    ))
  }

  list(plot = NULL, strata_panels = 0L)
}

generate_anova_stats_entries <- function(context,
                                         data,
                                         resp,
                                         factor1,
                                         factor2,
                                         posthoc_entry) {
  entries <- list()
  has_strata <- context$has_strata && !is.null(context$strat_var) && context$strat_var %in% names(data)

  if (has_strata) {
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

      entries[[length(entries) + 1]] <- list(
        label = stratum,
        stats_df = stats_df,
        posthoc = stratum_posthoc
      )
    }
  } else {
    stats_df <- anova_summarise_stats(data, resp, factor1, factor2)
    if (nrow(stats_df) == 0) return(entries)

    stats_df <- apply_anova_factor_levels(stats_df, factor1, factor2, context$order1, context$order2)

    entries[[1]] <- list(
      label = resp,
      stats_df = stats_df,
      posthoc = posthoc_entry
    )
  }

  entries
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
                                 y_limits = NULL,
                                 response_var = NULL) {
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
        stats_df = stats_df,
        title_text = title_text,
        factor1 = factor1,
        base_fill = base_fill,
        base_size = base_size,
        posthoc_entry = posthoc_entry,
        y_limits = y_limits,
        response_var = response_var
      )
    )
  }
  
  build_two_factor_barplot(
    stats_df = stats_df,
    title_text = title_text,
    factor1 = factor1,
    factor2 = factor2,
    line_colors = line_colors,
    base_fill = base_fill,
    base_size = base_size,
    nested_posthoc = nested_posthoc,
    y_limits = y_limits,
    response_var = response_var
  )
}

build_single_factor_barplot <- function(stats_df,
                                        title_text,
                                        factor1,
                                        base_fill,
                                        base_size,
                                        posthoc_entry,
                                        y_limits = NULL,
                                        response_var = NULL) {
  plot_obj <- ggplot(stats_df, aes(x = !!sym(factor1), y = mean)) +
    geom_col(fill = base_fill, width = 0.6, alpha = 0.8) +
    geom_errorbar(
      aes(ymin = mean - se, ymax = mean + se),
      width = 0.15,
      color = "gray40",
      linewidth = 0.5
    ) +
    ta_plot_theme(base_size = base_size) +
    labs(
      x = factor1,
      y = if (!is.null(response_var)) response_var else "Mean ± SE",
      title = title_text
    ) +
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
  
  plot_obj <- add_significance_after_build(
    p = plot_obj,
    stats_df = stats_df,
    factor1 = factor1,
    factor2 = NULL,
    posthoc_entry = posthoc_entry
  )
  
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
                                     y_limits = NULL,
                                     response_var = NULL) {
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
    labs(
      x = factor1,
      y = if (!is.null(response_var)) response_var else "Mean ± SE",
      fill = factor2,
      title = title_text
    ) +
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
  
  plot_obj <- add_significance_after_build(
    p = plot_obj,
    stats_df = stats_df,
    factor1 = factor1,
    factor2 = factor2,
    posthoc_entry = nested_posthoc
  )
  
  plot_obj
}

#### Section: Significance Annotation System (ggplot_build-based) ####

extract_bar_positions <- function(p, factor1, factor2 = NULL) {
  built <- ggplot_build(p)
  layer <- built$data[[1]]
  original <- built$plot$data
  
  y_top <- if ("ymax" %in% names(layer)) layer$ymax else layer$y
  
  df <- data.frame(
    x = layer$x,
    y = y_top
  )
  
  df[[factor1]] <- as.character(original[[factor1]])
  
  if (!is.null(factor2)) {
    df[[factor2]] <- as.character(original[[factor2]])
  }
  
  df
}

clean_p_values_barplot <- function(x) {
  x <- as.character(x)
  x <- gsub("[[:space:]]", "", x)
  x <- gsub("^<\\.?0*", "0.", x)
  suppressWarnings(as.numeric(x))
}


# One-way annotation builder
build_annotations_single_factor <- function(barpos,
                                            posthoc_entry,
                                            factor1,
                                            stats_df,
                                            offset_mult = 0.5) {
  
  if (is.null(posthoc_entry) || nrow(posthoc_entry) == 0) return(NULL)
  
  # --- CLEAN bar levels (remove parentheses) ---
  levels_f1 <- gsub("[()]", "", unique(as.character(barpos[[factor1]])))
  if (length(levels_f1) < 2) return(NULL)
  reference <- levels_f1[1]
  
  # --- CLEAN posthoc contrast labels ---
  posthoc_entry <- posthoc_entry |>
    dplyr::mutate(
      p.value = clean_p_values_barplot(.data$p.value),
      contrast_clean = gsub("[()]", "", .data$contrast)
    ) |>
    dplyr::filter(!is.na(.data$p.value))
  
  if (nrow(posthoc_entry) == 0) return(NULL)
  
  res <- list()
  idx <- 0L
  
  for (lvl in levels_f1[-1]) {
    
    contrasts <- c(
      paste0(lvl, " - ", reference),
      paste0(reference, " - ", lvl)
    )
    
    # --- match using cleaned labels ---
    row <- posthoc_entry[posthoc_entry$contrast_clean %in% contrasts, , drop = FALSE]
    if (nrow(row) == 0) next
    
    p <- row$p.value[1]
    if (is.na(p) || p >= 0.05) next
    
    label <- if (p < 0.001) "***" else if (p < 0.01) "**" else "*"
    
    # --- match bar position (also cleaned) ---
    bar_row <- barpos[gsub("[()]", "", as.character(barpos[[factor1]])) == lvl, , drop = FALSE]
    if (nrow(bar_row) == 0) next
    
    mean_val <- stats_df$mean[gsub("[()]", "", as.character(stats_df[[factor1]])) == lvl]
    se_val   <- stats_df$se[gsub("[()]", "", as.character(stats_df[[factor1]])) == lvl]
    
    # --- label above SE ---
    y_ann <- (mean_val + se_val) + se_val * offset_mult
    
    idx <- idx + 1L
    res[[idx]] <- data.frame(
      x = bar_row$x[1],
      y = y_ann,
      label = label
    )
  }
  
  if (length(res) == 0) return(NULL)
  do.call(rbind, res)
}

# Two-way annotation builder
build_annotations_two_factor <- function(barpos,
                                         nested_posthoc,
                                         factor1,
                                         factor2,
                                         stats_df,
                                         offset_mult = 0.5) {
  
  if (is.null(nested_posthoc) || !is.data.frame(nested_posthoc)) return(NULL)
  needed <- c("contrast", "p.value", factor1)
  if (!all(needed %in% names(nested_posthoc))) return(NULL)
  
  df <- nested_posthoc
  df$contrast_clean <- gsub("[()]", "", df$contrast)
  df$p.value <- clean_p_values_barplot(df$p.value)
  df <- df[is.finite(df$p.value), , drop = FALSE]
  if (nrow(df) == 0) return(NULL)
  
  lev1 <- unique(as.character(barpos[[factor1]]))
  lev2 <- unique(as.character(barpos[[factor2]]))
  if (length(lev2) < 2) return(NULL)
  
  reference <- lev2[1]
  
  res <- list()
  idx <- 0L
  
  for (g1 in lev1) {
    for (lvl in lev2[lev2 != reference]) {
      
      lvl_clean <- gsub("[()]", "", lvl)
      ref_clean <- gsub("[()]", "", reference)
      
      ct1 <- paste0(lvl_clean, " - ", ref_clean)
      ct2 <- paste0(ref_clean, " - ", lvl_clean)
      
      sub <- df[
        as.character(df[[factor1]]) == g1 &
          df$contrast_clean %in% c(ct1, ct2),
        ,
        drop = FALSE
      ]
      
      if (nrow(sub) == 0) next
      
      p <- sub$p.value[1]
      if (is.na(p) || p >= 0.05) next
      
      label <- if (p < 0.001) "***" else if (p < 0.01) "**" else "*"
      
      bar_row <- barpos[
        as.character(barpos[[factor1]]) == g1 &
          as.character(barpos[[factor2]]) == lvl,
        ,
        drop = FALSE
      ]
      if (nrow(bar_row) == 0) next
      
      mean_val <- stats_df$mean[
        stats_df[[factor1]] == g1 & stats_df[[factor2]] == lvl
      ]
      se_val <- stats_df$se[
        stats_df[[factor1]] == g1 & stats_df[[factor2]] == lvl
      ]
      
      y_ann <- (mean_val + se_val) + se_val * offset_mult
      
      idx <- idx + 1L
      res[[idx]] <- data.frame(
        x = bar_row$x[1],
        y = y_ann,
        label = label
      )
    }
  }
  
  if (length(res) == 0) return(NULL)
  do.call(rbind, res)
}


# Attach significance after building the plot
add_significance_after_build <- function(p,
                                         stats_df,
                                         factor1,
                                         factor2 = NULL,
                                         posthoc_entry = NULL,
                                         text_size = 4) {
  
  if (is.null(posthoc_entry)) return(p)
  
  barpos <- extract_bar_positions(p, factor1, factor2)
  barpos <- barpos[is.finite(barpos$y), , drop = FALSE]
  if (nrow(barpos) == 0) return(p)
  
  if (is.null(factor2)) {
    ann <- build_annotations_single_factor(
      barpos = barpos,
      posthoc_entry = posthoc_entry,
      factor1 = factor1,
      stats_df = stats_df
    )
  } else {
    ann <- build_annotations_two_factor(
      barpos = barpos,
      nested_posthoc = posthoc_entry,
      factor1 = factor1,
      factor2 = factor2,
      stats_df = stats_df
    )
  }
  
  if (is.null(ann) || nrow(ann) == 0) return(p)
  
  max_y <- max(ann$y, na.rm = TRUE)
  
  p_build <- ggplot_build(p)
  current_limits <- p_build$layout$panel_params[[1]]$y.range
  
  new_upper <- max(current_limits[2], max_y * 1.05)
  
  p +
    scale_y_continuous(
      limits = c(0, new_upper),
      expand = expansion(mult = c(0, 0))
    ) +
    geom_text(
      data = ann,
      aes(x = x, y = y, label = label),
      inherit.aes = FALSE,
      color = "gray30",
      size = text_size,
      fontface = "bold"
    )
}
