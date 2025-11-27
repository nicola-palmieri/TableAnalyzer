#### Section: Boxplot Construction ####

plot_anova_boxplot <- function(data,
                               info,
                               layout_values = list(),
                               line_colors = NULL,
                               base_size = 14,
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

  shared_y_limits <- NULL
  if (isTRUE(share_y_axis)) {
    vals <- c()
    for (resp in context$responses) {
      vals <- c(vals, data[[resp]])
    }
    rng <- range(vals, na.rm = TRUE)
    if (all(is.finite(rng))) shared_y_limits <- rng
  }

  base_fill <- if (!is.null(line_colors) && length(line_colors) > 0) unname(line_colors)[1] else "#3E8FC4"
  strata_panel_count <- context$initial_strata_panels

  build_boxplot_panel <- function(resp, subset_data, title_text = "") {
    if (is.null(factor2)) {
      p <- ggplot(subset_data, aes(x = .data[[factor1]], y = .data[[resp]])) +
        geom_boxplot(
          outlier.shape = NA,
          alpha = 0.8,
          width = 0.6,
          fill = base_fill
        )
    } else {
      p <- ggplot(subset_data, aes(x = .data[[factor1]], y = .data[[resp]], fill = .data[[factor2]])) +
        geom_boxplot(
          outlier.shape = NA,
          alpha = 0.8,
          width = 0.7,
          position = position_dodge(width = 0.8)
        )
    }

    p <- p +
      ta_plot_theme(base_size = base_size) +
      labs(
        title = title_text,
        x = factor1,
        y = resp,
        fill = if (!is.null(factor2)) factor2 else NULL
      ) +
      theme(
        plot.title = element_text(size = base_size, face = "bold", hjust = 0.5),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        axis.line = element_line(color = "#9ca3af"),
        axis.ticks = element_line(color = "#9ca3af")
      )

    if (!is.null(shared_y_limits)) {
      p <- p + coord_cartesian(ylim = shared_y_limits)
    }

    if (!is.null(factor2)) {
      fill_levels <- if (is.factor(subset_data[[factor2]])) levels(subset_data[[factor2]]) else unique(as.character(subset_data[[factor2]]))
      fill_levels <- fill_levels[!is.na(fill_levels)]
      palette <- resolve_palette_for_levels(fill_levels, custom = line_colors)
      p <- p + scale_fill_manual(values = palette)
    }

    p
  }

  response_plots <- list()

  for (resp in context$responses) {
    if (context$has_strata && !is.null(context$strat_var) && context$strat_var %in% names(data)) {
      strata_plots <- list()
      for (stratum in context$strata_levels) {
        subset_rows <- !is.na(data[[context$strat_var]]) & data[[context$strat_var]] == stratum
        subset_data <- data[subset_rows, , drop = FALSE]
        if (nrow(subset_data) == 0) next

        p <- build_boxplot_panel(resp, subset_data, title_text = stratum)
        strata_plots[[length(strata_plots) + 1]] <- p
      }
      if (length(strata_plots) > 0) {
        strata_panel_count <- max(strata_panel_count, length(strata_plots))
        response_plots[[resp]] <- patchwork::wrap_plots(
          plotlist = strata_plots,
          nrow = context$strata_layout$nrow,
          ncol = context$strata_layout$ncol
        )
      }
    } else {
      p <- build_boxplot_panel(resp, data, title_text = "")
      response_plots[[resp]] <- p
    }
  }

  response_plots <- Filter(Negate(is.null), response_plots)

  finalize_anova_plot_result(
    response_plots = response_plots,
    context = context,
    strata_panel_count = strata_panel_count,
    collect_guides = isTRUE(common_legend),
    legend_position = if (isTRUE(common_legend)) legend_position_value else NULL
  )
}
