#### Section: Lineplot Construction ####

plot_anova_lineplot_meanse <- function(data,
                                       info,
                                       layout_values,
                                       line_colors = NULL,
                                       base_size = 14,
                                       show_lines = FALSE,
                                       show_jitter = FALSE,
                                       use_dodge = FALSE,
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
  
  shared_y_limits <- if (isTRUE(share_y_axis)) {
    compute_lineplot_shared_limits(context, data, factor1, factor2)
  } else {
    NULL
  }
  
  response_plots <- list()
  strata_panel_count <- context$initial_strata_panels
  
  for (resp in context$responses) {
    if (context$has_strata && !is.null(context$strat_var) && context$strat_var %in% names(data)) {
      stratum_stats <- list()
      y_values <- c()
      
      for (stratum in context$strata_levels) {
        subset_rows <- !is.na(data[[context$strat_var]]) & data[[context$strat_var]] == stratum
        subset_data <- data[subset_rows, , drop = FALSE]
        if (nrow(subset_data) == 0) {
          next
        }
        
        stats_df <- anova_summarise_stats(subset_data, resp, factor1, factor2)
        if (nrow(stats_df) == 0) {
          next
        }
        
        stats_df <- apply_anova_factor_levels(stats_df, factor1, factor2, context$order1, context$order2)
        y_values <- c(y_values, stats_df$mean - stats_df$se, stats_df$mean + stats_df$se)
        stratum_stats[[stratum]] <- list(
          stats = stats_df,
          raw = prepare_lineplot_raw_data(subset_data, resp, factor1, factor2)
        )
      }
      
      if (length(stratum_stats) == 0) {
        next
      }
      
      y_limits <- range(y_values, na.rm = TRUE)
      if (!all(is.finite(y_limits))) {
        y_limits <- NULL
      }
      y_limits_to_use <- if (!is.null(shared_y_limits)) shared_y_limits else y_limits
      
      strata_panel_count <- max(strata_panel_count, length(stratum_stats))
      
      strata_plot_list <- lapply(names(stratum_stats), function(stratum_name) {
        entry <- stratum_stats[[stratum_name]]
        build_line_plot_panel(
          stats_df = entry$stats,
          title_text = stratum_name,
          y_limits = y_limits_to_use,
          factor1 = factor1,
          factor2 = factor2,
          line_colors = line_colors,
          base_size = base_size,
          raw_data = entry$raw,
          response_var = resp,
          show_lines = show_lines,
          show_jitter = show_jitter,
          use_dodge = use_dodge
        )
      })

      combined <- patchwork::wrap_plots(
        plotlist = strata_plot_list,
        nrow = context$strata_layout$nrow,
        ncol = context$strata_layout$ncol
      )
      
      if (isTRUE(common_legend)) {
        combined <- collect_guides_safe(combined)
      }
      
      response_plots[[resp]] <- combined
    } else {
      stats_df <- anova_summarise_stats(data, resp, factor1, factor2)
      if (nrow(stats_df) == 0) {
        next
      }
      
      stats_df <- apply_anova_factor_levels(stats_df, factor1, factor2, context$order1, context$order2)
      y_values <- c(stats_df$mean - stats_df$se, stats_df$mean + stats_df$se)
      y_limits <- range(y_values, na.rm = TRUE)
      if (!all(is.finite(y_limits))) {
        y_limits <- NULL
      }
      
      y_limits_to_use <- if (!is.null(shared_y_limits)) shared_y_limits else y_limits
      
      response_plots[[resp]] <- build_line_plot_panel(
        stats_df = stats_df,
        title_text = "",
        y_limits = y_limits_to_use,
        factor1 = factor1,
        factor2 = factor2,
        line_colors = line_colors,
        base_size = base_size,
        raw_data = prepare_lineplot_raw_data(data, resp, factor1, factor2),
        response_var = resp,
        show_lines = show_lines,
        show_jitter = show_jitter,
        use_dodge = use_dodge
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

compute_lineplot_shared_limits <- function(context, data, factor1, factor2) {
  combined <- NULL

  for (resp in context$responses) {
    if (context$has_strata && !is.null(context$strat_var) && context$strat_var %in% names(data)) {
      for (stratum in context$strata_levels) {
        subset_rows <- !is.na(data[[context$strat_var]]) & data[[context$strat_var]] == stratum
        subset_data <- data[subset_rows, , drop = FALSE]
        if (nrow(subset_data) == 0) next

        stats_df <- anova_summarise_stats(subset_data, resp, factor1, factor2)
        if (nrow(stats_df) == 0) next

        stats_df <- apply_anova_factor_levels(stats_df, factor1, factor2, context$order1, context$order2)
        y_values <- c(stats_df$mean - stats_df$se, stats_df$mean + stats_df$se)
        combined <- update_numeric_range(combined, y_values)
      }
    } else {
      stats_df <- anova_summarise_stats(data, resp, factor1, factor2)
      if (nrow(stats_df) == 0) next
      stats_df <- apply_anova_factor_levels(stats_df, factor1, factor2, context$order1, context$order2)
      y_values <- c(stats_df$mean - stats_df$se, stats_df$mean + stats_df$se)
      combined <- update_numeric_range(combined, y_values)
    }
  }

  if (is.null(combined) || any(!is.finite(combined))) return(NULL)
  combined
}


prepare_lineplot_raw_data <- function(df, response_var, factor1, factor2 = NULL) {
  if (is.null(df) || is.null(response_var) || is.null(factor1)) return(NULL)
  if (!response_var %in% names(df) || !factor1 %in% names(df)) return(NULL)
  
  cols <- c(factor1, factor2, response_var)
  cols <- cols[!vapply(cols, is.null, FUN.VALUE = logical(1), USE.NAMES = FALSE)]
  cols <- unique(cols)
  cols <- cols[cols %in% names(df)]
  if (!response_var %in% cols || !factor1 %in% cols) return(NULL)
  
  raw_subset <- df[, cols, drop = FALSE]
  raw_subset <- raw_subset[!is.na(raw_subset[[response_var]]), , drop = FALSE]
  if (nrow(raw_subset) == 0) return(NULL)
  raw_subset
}


build_line_plot_panel <- function(stats_df,
                                  title_text,
                                  y_limits,
                                  factor1,
                                  factor2,
                                  line_colors,
                                  base_size = 13,
                                  raw_data = NULL,
                                  response_var = NULL,
                                  show_lines = FALSE,
                                  show_jitter = FALSE,
                                  use_dodge = FALSE) {
  if (is.null(factor2) || !factor2 %in% names(stats_df)) {
    color_value <- if (!is.null(line_colors) && length(line_colors) > 0) {
      unname(line_colors)[1]
    } else {
      resolve_single_color()
    }
    p <- ggplot(stats_df, aes(x = !!sym(factor1), y = mean))

    if (isTRUE(show_jitter) && !is.null(raw_data) &&
        !is.null(response_var) && response_var %in% names(raw_data) &&
        factor1 %in% names(raw_data)) {
      jitter_df <- raw_data[!is.na(raw_data[[response_var]]), , drop = FALSE]
      if (nrow(jitter_df) > 0) {
        p <- p + geom_jitter(
          data = jitter_df,
          aes(x = !!sym(factor1), y = !!sym(response_var)),
          width = 0.12,
          alpha = 0.35,
          size = 1.7,
          color = color_value,
          inherit.aes = FALSE,
          show.legend = FALSE
        )
      }
    }

    if (isTRUE(show_lines)) {
      p <- p + geom_line(aes(group = 1), color = color_value, linewidth = 1)
    }

    p <- p +
      geom_point(size = 3, color = color_value) +
      geom_errorbar(
        aes(ymin = mean - se, ymax = mean + se),
        width = 0.15,
        color = color_value
      ) +
      ta_plot_theme(base_size = base_size) +
      labs(
        x = factor1,
        y = if (!is.null(response_var)) response_var else "Mean ± SE"
      ) +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.line = element_line(color = "#9ca3af"),
        axis.ticks = element_line(color = "#9ca3af")
      )
  } else {
    group_levels <- if (is.factor(stats_df[[factor2]])) {
      levels(stats_df[[factor2]])
    } else {
      unique(as.character(stats_df[[factor2]]))
    }
    group_levels <- group_levels[!is.na(group_levels)]
    palette <- resolve_palette_for_levels(group_levels, custom = line_colors)
    stats_df[[factor2]] <- factor(as.character(stats_df[[factor2]]), levels = group_levels)
    dodge_width <- if (isTRUE(use_dodge)) 0.4 else NULL
    dodge <- if (!is.null(dodge_width)) position_dodge(width = dodge_width) else NULL
    jitter_dodge_width <- if (is.null(dodge_width)) 0 else dodge_width
    p <- ggplot(stats_df, aes(
      x = !!sym(factor1),
      y = mean,
      color = !!sym(factor2),
      group = !!sym(factor2)
    ))

    if (isTRUE(show_jitter) && !is.null(raw_data) && !is.null(response_var) &&
        all(c(factor1, factor2) %in% names(raw_data)) &&
        response_var %in% names(raw_data)) {
      jitter_df <- raw_data[!is.na(raw_data[[response_var]]), , drop = FALSE]
      if (nrow(jitter_df) > 0) {
        jitter_df[[factor2]] <- factor(as.character(jitter_df[[factor2]]), levels = group_levels)
        p <- p + geom_jitter(
          data = jitter_df,
          aes(x = !!sym(factor1), y = !!sym(response_var), color = !!sym(factor2)),
          position = position_jitterdodge(jitter.width = 0.15, dodge.width = jitter_dodge_width),
          size = 1.6,
          alpha = 0.4,
          inherit.aes = FALSE,
          show.legend = FALSE
        )
      }
    }

    if (isTRUE(show_lines)) {
      if (is.null(dodge)) {
        p <- p + geom_line(linewidth = 1)
      } else {
        p <- p + geom_line(linewidth = 1, position = dodge)
      }
    }

    point_layer <- if (is.null(dodge)) {
      geom_point(size = 3)
    } else {
      geom_point(size = 3, position = dodge)
    }

    errorbar_layer <- if (is.null(dodge)) {
      geom_errorbar(
        aes(ymin = mean - se, ymax = mean + se),
        width = 0.15
      )
    } else {
      geom_errorbar(
        aes(ymin = mean - se, ymax = mean + se),
        width = 0.15,
        position = dodge
      )
    }

    p <- p +
      point_layer +
      errorbar_layer +
      ta_plot_theme(base_size = base_size) +
      labs(
        x = factor1,
        y = if (!is.null(response_var)) response_var else "Mean ± SE",
        color = factor2
      ) +
      theme(
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.line = element_line(color = "#9ca3af"),
        axis.ticks = element_line(color = "#9ca3af")
      ) +
      scale_color_manual(values = palette)
  }

  if (!is.null(y_limits) && all(is.finite(y_limits))) {
    p <- p + scale_y_continuous(limits = y_limits)
  }

  if (!is.null(title_text) && nzchar(title_text)) {
    p + ggtitle(title_text) +
      theme(
        plot.title = element_text(
          size = base_size,
          face = "bold",
          hjust = 0.5
        )
      )
  } else {
    p
  }
}





