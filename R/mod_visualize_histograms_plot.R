build_descriptive_numeric_histogram <- function(df,
                                                selected_vars = NULL,
                                                group_var = NULL,
                                                strata_levels = NULL,
                                                use_density = FALSE,
                                                nrow_input = NULL,
                                                ncol_input = NULL,
                                                custom_colors = NULL,
                                                base_size = 13,
                                                common_legend = FALSE,
                                                legend_position = NULL,
                                               panel_width = 400, panel_height = 300) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)

  num_vars <- names(Filter(is.numeric, df))
  if (!is.null(selected_vars) && length(selected_vars) > 0) {
    num_vars <- intersect(num_vars, selected_vars)
  }
  if (length(num_vars) == 0) return(NULL)

  if (is.null(group_var) || !group_var %in% names(df)) {
    group_var <- NULL
  } else {
    df[[group_var]] <- as.character(df[[group_var]])
    df[[group_var]][is.na(df[[group_var]]) | trimws(df[[group_var]]) == ""] <- "Missing"

    if (!is.null(strata_levels) && length(strata_levels) > 0) {
      keep_levels <- unique(strata_levels)
      df <- df[df[[group_var]] %in% keep_levels, , drop = FALSE]
      if (nrow(df) == 0) return(NULL)
      df[[group_var]] <- factor(df[[group_var]], levels = keep_levels)
    } else {
      df[[group_var]] <- factor(df[[group_var]], levels = unique(df[[group_var]]))
    }
  }

  plots <- lapply(num_vars, function(var) {
    cols <- intersect(c(var, group_var), names(df))
    plot_data <- df[, cols, drop = FALSE]

    keep <- is.finite(plot_data[[var]])
    keep[is.na(keep)] <- FALSE
    plot_data <- plot_data[keep, , drop = FALSE]
    if (nrow(plot_data) == 0) return(NULL)

    if (!is.null(group_var)) {
      plot_data[[group_var]] <- droplevels(plot_data[[group_var]])
    }

    density_mode <- isTRUE(use_density) && length(unique(plot_data[[var]])) > 1
    base <- ggplot(plot_data, aes(x = .data[[var]]))
    y_label <- if (density_mode) "Density" else "Count"

    if (!is.null(group_var)) {
      group_levels <- levels(plot_data[[group_var]])
      palette <- resolve_palette_for_levels(group_levels, custom = custom_colors)
      if (density_mode) {
        base <- base +
          geom_density(aes(color = .data[[group_var]], fill = .data[[group_var]]), alpha = 0.3) +
          scale_color_manual(values = palette) +
          scale_fill_manual(values = palette) +
          labs(color = group_var, fill = group_var)
      } else {
        base <- base +
          geom_histogram(
            aes(fill = .data[[group_var]]),
            position = "identity",
            alpha = 0.5,
            bins = 30,
            color = "black",
            linewidth = 0.2
          ) +
          scale_fill_manual(values = palette) +
          labs(fill = group_var)
      }
    } else {
      single_color <- resolve_single_color(custom_colors)
      if (density_mode) {
        base <- base + geom_density(fill = single_color, color = single_color, alpha = 0.35)
      } else {
        base <- base +
          geom_histogram(
            fill = single_color,
            color = "black",
            bins = 30,
            linewidth = 0.2
          )
      }
    }

    base +
      ta_plot_theme(base_size = base_size, kind = "bar") +
      labs(x = var, y = y_label) +
      scale_y_continuous(limits = c(0, NA), expand = expansion(mult = c(0, 0.05)))
  })

  plots <- Filter(Negate(is.null), plots)
  if (length(plots) == 0) return(NULL)

  n_panels <- length(plots)
  defaults <- compute_default_grid(n_panels, panel_width, panel_height)
  layout <- basic_grid_layout(
    rows = suppressWarnings(as.numeric(nrow_input)),
    cols = suppressWarnings(as.numeric(ncol_input)),
    default_rows = defaults$rows,
    default_cols = defaults$cols
  )

  validation <- validate_grid(n_panels, layout$nrow, layout$ncol)
  combined <- if (isTRUE(validation$valid)) {
    patchwork::wrap_plots(plots, nrow = layout$nrow, ncol = layout$ncol)
  } else {
    NULL
  }

  if (!is.null(combined)) {
    combined <- apply_common_legend_layout(
      combined,
      legend_position = legend_position,
      collect_guides = isTRUE(common_legend)
    )
  }

  list(
    plot = combined,
    layout = list(nrow = layout$nrow, ncol = layout$ncol),
    panels = n_panels,
    warning = validation$message,
    defaults = defaults
  )
}
