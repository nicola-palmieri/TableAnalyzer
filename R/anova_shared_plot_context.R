#### Section: Plot Context Initialization ####

initialize_anova_plot_context <- function(data, info, layout_values) {
  factor1 <- info$factors$factor1
  factor2 <- info$factors$factor2
  order1 <- info$orders$order1
  order2 <- info$orders$order2

  if (!is.null(factor1) && !is.null(order1) && factor1 %in% names(data)) {
    data[[factor1]] <- factor(data[[factor1]], levels = order1)
  }
  if (!is.null(factor2) && !is.null(order2) && factor2 %in% names(data)) {
    data[[factor2]] <- factor(data[[factor2]], levels = order2)
  }

  has_strata <- !is.null(info$strata) && !is.null(info$strata$var)
  strat_var <- if (has_strata) info$strata$var else NULL
  strata_levels <- if (has_strata) info$strata$levels else character(0)

  if (has_strata && (is.null(strata_levels) || length(strata_levels) == 0) &&
      !is.null(strat_var) && strat_var %in% names(data)) {
    strata_levels <- unique(as.character(stats::na.omit(data[[strat_var]])))
  }

  layout_input <- parse_anova_layout_inputs(layout_values)

  n_expected_strata <- if (has_strata) max(1L, length(strata_levels)) else 1L
  strata_defaults <- if (has_strata) {
    compute_default_grid(n_expected_strata)
  } else {
    list(rows = 1L, cols = 1L)
  }
  strata_layout <- basic_grid_layout(
    rows = layout_input$strata_rows,
    cols = layout_input$strata_cols,
    default_rows = strata_defaults$rows,
    default_cols = strata_defaults$cols
  )

  list(
    data = data,
    responses = info$responses,
    factor1 = factor1,
    factor2 = factor2,
    order1 = order1,
    order2 = order2,
    has_strata = has_strata,
    strat_var = strat_var,
    strata_levels = strata_levels,
    n_expected_strata = n_expected_strata,
    strata_defaults = strata_defaults,
    strata_layout = strata_layout,
    layout_input = layout_input,
    initial_strata_panels = if (has_strata) 0L else 1L
  )
}

parse_anova_layout_inputs <- function(layout_values) {
  list(
    strata_rows = suppressWarnings(as.numeric(layout_values$strata_rows)),
    strata_cols = suppressWarnings(as.numeric(layout_values$strata_cols)),
    resp_rows   = suppressWarnings(as.numeric(layout_values$resp_rows)),
    resp_cols   = suppressWarnings(as.numeric(layout_values$resp_cols))
  )
}

update_numeric_range <- function(current_range, values) {
  values <- values[is.finite(values)]
  if (length(values) == 0) return(current_range)
  new_range <- range(values)
  if (any(!is.finite(new_range))) return(current_range)
  if (is.null(current_range)) {
    new_range
  } else {
    c(min(current_range[1], new_range[1]), max(current_range[2], new_range[2]))
  }
}

apply_common_legend_layout <- function(plot_obj,
                                       legend_position = NULL,
                                       collect_guides = FALSE) {
  if (is.null(plot_obj)) return(plot_obj)

  updated <- plot_obj
  if (!is.null(legend_position)) {
    updated <- add_theme_to_plot(updated, theme(legend.position = legend_position))
  }

  if (collect_guides && requireNamespace("patchwork", quietly = TRUE)) {
    if (!inherits(updated, "patchwork")) {
      updated <- patchwork::wrap_plots(updated)
    }

    exports <- tryCatch(getNamespaceExports("patchwork"), error = function(...) character())
    panel_grid <- if ("collect_guides" %in% exports) {
      patchwork::collect_guides(updated)
    } else {
      updated + patchwork::plot_layout(guides = "collect")
    }

    legend_area <- patchwork::guide_area()
    rel_panel <- 1
    rel_legend <- 0.1

    legend_position_checked <- if (is.null(legend_position)) "right" else legend_position

    layout_guided <- switch(
      legend_position_checked,
      bottom = panel_grid / legend_area + patchwork::plot_layout(heights = c(rel_panel, rel_legend)),
      right  = legend_area | panel_grid + patchwork::plot_layout(widths = c(rel_legend, rel_panel)),
      top    = legend_area / panel_grid + patchwork::plot_layout(heights = c(rel_legend, rel_panel)),
      left   = panel_grid | legend_area + patchwork::plot_layout(widths = c(rel_panel, rel_legend)),
      panel_grid
    )

    return(layout_guided)
  }

  if (collect_guides) {
    updated <- collect_guides_safe(updated)
  }

  updated
}

collect_guides_safe <- function(plot_obj) {
  if (is.null(plot_obj) || !requireNamespace("patchwork", quietly = TRUE)) {
    return(plot_obj)
  }

  is_patchwork <- inherits(plot_obj, "patchwork")
  if (!is_patchwork) {
    return(plot_obj)
  }

  exports <- tryCatch(getNamespaceExports("patchwork"), error = function(...) character())
  collected <- if ("collect_guides" %in% exports) {
    patchwork::collect_guides(plot_obj)
  } else {
    plot_obj + patchwork::plot_layout(guides = "collect")
  }

  collected + patchwork::plot_layout(guides = "collect")
}

add_theme_to_plot <- function(plot_obj, theme_obj) {
  if (inherits(plot_obj, "patchwork")) {
    plot_obj & theme_obj
  } else {
    plot_obj + theme_obj
  }
}


finalize_anova_plot_result <- function(response_plots,
                                       context,
                                       strata_panel_count,
                                       collect_guides = FALSE,
                                       legend_position = NULL) {
  if (length(response_plots) == 0) {
    return(NULL)
  }
  
  has_strata <- context$has_strata
  strata_layout <- context$strata_layout
  
  if (has_strata && strata_panel_count == 0L) {
    strata_panel_count <- context$n_expected_strata
  }

  response_defaults <- compute_default_grid(length(response_plots))
  response_layout <- basic_grid_layout(
    rows = context$layout_input$resp_rows,
    cols = context$layout_input$resp_cols,
    default_rows = response_defaults$rows,
    default_cols = response_defaults$cols
  )

  strata_validation <- if (has_strata) {
    validate_grid(max(1L, strata_panel_count), strata_layout$nrow, strata_layout$ncol)
  } else {
    list(valid = TRUE, message = NULL)
  }
  
  response_validation <- validate_grid(
    length(response_plots),
    response_layout$nrow,
    response_layout$ncol
  )
  
  warnings <- c()
  if (has_strata && !strata_validation$valid && !is.null(strata_validation$message)) {
    warnings <- c(warnings, strata_validation$message)
  }
  if (!response_validation$valid && !is.null(response_validation$message)) {
    warnings <- c(warnings, response_validation$message)
  }
  warning_text <- if (length(warnings) > 0) paste(warnings, collapse = "<br/>") else NULL
  
  panel_counts <- list(
    strata = if (has_strata) max(1L, strata_panel_count) else 1L,
    responses = length(response_plots)
  )
  
  final_plot <- NULL
  if (is.null(warning_text)) {
    if (length(response_plots) == 1) {
      final_plot <- response_plots[[1]]
      if (collect_guides && !is.null(legend_position)) {
        final_plot <- final_plot & theme(legend.position = legend_position)
      }
    } else {
      final_plot <- patchwork::wrap_plots(
        plotlist = response_plots,
        nrow = response_layout$nrow,
        ncol = response_layout$ncol
      )
    }
    if (collect_guides || !is.null(legend_position)) {
      final_plot <- apply_common_legend_layout(
        final_plot,
        legend_position = legend_position,
        collect_guides = collect_guides
      )
    }
  }
  
  list(
    plot = final_plot,
    layout = list(
      strata = list(
        rows = if (has_strata) strata_layout$nrow else 1L,
        cols = if (has_strata) strata_layout$ncol else 1L
      ),
      responses = list(
        rows = response_layout$nrow,
        cols = response_layout$ncol
      )
    ),
    warning = warning_text,
    panel_counts = panel_counts,
    defaults = list(
      strata = context$strata_defaults,
      responses = response_defaults
    )
  )
}
