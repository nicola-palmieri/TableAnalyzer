build_descriptive_numeric_boxplot <- function(df,
                                              selected_vars = NULL,
                                              group_var = NULL,
                                              show_points = FALSE,
                                              show_outliers = FALSE,
                                              outlier_label_var = NULL,
                                              nrow_input = NULL,
                                              ncol_input = NULL,
                                              custom_colors = NULL,
                                              base_size = 13,
                                              common_legend = FALSE,
                                              legend_position = NULL,
                                               panel_width = 400, panel_height = 300) {
  if (is.null(df) || !is.data.frame(df) || nrow(df) == 0) return(NULL)

  num_vars <- names(df)[vapply(df, is.numeric, logical(1))]
  if (!is.null(selected_vars) && length(selected_vars) > 0) {
    num_vars <- intersect(num_vars, selected_vars)
  }
  if (length(num_vars) == 0) return(NULL)
  
  # ensure discrete x if grouped
  if (!is.null(group_var) && group_var %in% names(df)) {
    df[[group_var]] <- as.factor(df[[group_var]])
  } else {
    group_var <- NULL
  }
  
  plots <- lapply(num_vars, function(var) {
    vec <- df[[var]]
    if (all(is.na(vec))) return(NULL)

    if (!is.null(group_var)) {
      group_levels <- levels(df[[group_var]])
      palette <- resolve_palette_for_levels(group_levels, custom = custom_colors)
      p <- ggplot(df, aes(x = .data[[group_var]], y = .data[[var]], fill = .data[[group_var]])) +
        geom_boxplot(outlier.shape = NA, width = 0.6) +
        scale_fill_manual(values = palette) +
        ta_plot_theme(base_size = base_size, kind = "box") +
        labs(x = NULL, y = var) +
        theme(
          axis.text.x = element_text(angle = 45, hjust = 1)
        )

      needs_color_scale <- FALSE
      if (isTRUE(show_points)) {
        p <- p + geom_jitter(
          position = position_jitter(width = 0.2, height = 0, seed = 1L),
          shape = 21, color = "#1b2637",
          alpha = 0.6, size = 1.6, stroke = 0.25,
          show.legend = FALSE
        )
      }

      if (isTRUE(show_outliers)) {
        outliers <- prepare_boxplot_outliers(
          data = df,
          value_col = var,
          group_col = group_var,
          label_col = outlier_label_var
        )
        if (has_rows(outliers)) {
          p <- p + geom_point(
            data = outliers,
            aes(x = x, y = y, color = group),
            inherit.aes = FALSE,
            size = 2.5,
            show.legend = FALSE
          )
          needs_color_scale <- TRUE

          label_data <- filter_labeled_outliers(outliers)
          if (has_rows(label_data)) {
            p <- p + ggrepel::geom_text_repel(
              data = label_data,
              aes(x = x, y = y, label = label, color = group),
              inherit.aes = FALSE,
              size = 3,
              max.overlaps = Inf,
              min.segment.length = 0,
              box.padding = 0.3,
              point.padding = 0.2,
              show.legend = FALSE
            )
          }
        }
      }

      if (needs_color_scale) {
        p <- p + scale_color_manual(values = palette, guide = "none")
      }
    } else {
      single_color <- resolve_single_color(custom_colors)
      p <- ggplot(df, aes(x = factor(1), y = .data[[var]])) +
        geom_boxplot(fill = single_color, width = 0.3) +
        ta_plot_theme(base_size = base_size, kind = "box") +
        labs(x = NULL, y = var) +
        theme(
          axis.text.x = element_blank(),
          axis.ticks.x = element_blank()
        )

      if (isTRUE(show_points)) {
        p <- p + geom_jitter(
          position = position_jitter(width = 0.05, height = 0, seed = 1L),
          shape = 21, fill = single_color, color = "#1b2637",
          alpha = 0.6, size = 1.6, stroke = 0.25,
          show.legend = FALSE
        )
      }

      if (isTRUE(show_outliers)) {
        outliers <- prepare_boxplot_outliers(
          data = df,
          value_col = var,
          label_col = outlier_label_var
        )
        if (has_rows(outliers)) {
          p <- p + geom_point(
            data = outliers,
            aes(x = x, y = y),
            inherit.aes = FALSE,
            color = single_color,
            size = 2.5,
            show.legend = FALSE
          )

          label_data <- filter_labeled_outliers(outliers)
          if (has_rows(label_data)) {
            p <- p + ggrepel::geom_text_repel(
              data = label_data,
              aes(x = x, y = y, label = label),
              inherit.aes = FALSE,
              size = 3,
              color = single_color,
              max.overlaps = Inf,
              min.segment.length = 0,
              box.padding = 0.3,
              point.padding = 0.2,
              show.legend = FALSE
            )
          }
        }
      }
    }

    if (inherits(p, "gg")) p else NULL
  })
  
  # keep only valid ggplots
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

  combined <- NULL
  if (isTRUE(validation$valid)) {
    combined <- patchwork::wrap_plots(plots, nrow = layout$nrow, ncol = layout$ncol)

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


has_rows <- function(x) {
  is.data.frame(x) && nrow(x) > 0
}


compute_outlier_bounds <- function(values) {
  if (is.null(values)) return(NULL)
  values <- values[!is.na(values)]
  if (!length(values)) return(NULL)

  stats <- stats::quantile(values, probs = c(0.25, 0.75), na.rm = TRUE, names = FALSE)
  if (anyNA(stats)) return(NULL)

  iqr <- stats[2] - stats[1]
  list(
    lower = stats[1] - 1.5 * iqr,
    upper = stats[2] + 1.5 * iqr
  )
}


clean_outlier_labels <- function(values) {
  if (is.null(values)) return(character())
  out <- as.character(values)
  out[is.na(out) | trimws(out) == ""] <- NA_character_
  out
}


filter_labeled_outliers <- function(outliers) {
  if (!has_rows(outliers) || !"label" %in% names(outliers)) {
    return(NULL)
  }
  labeled <- outliers[!is.na(outliers$label) & nzchar(outliers$label), , drop = FALSE]
  if (has_rows(labeled)) labeled else NULL
}


validate_outlier_label <- function(label_input) {
  if (is.null(label_input) || !nzchar(label_input)) {
    return(NULL)
  }
  label_input
}


prepare_boxplot_outliers <- function(data,
                                     value_col,
                                     group_col = NULL,
                                     label_col = NULL) {
  if (is.null(data) || !is.data.frame(data) || !value_col %in% names(data)) {
    return(NULL)
  }

  extract_labels <- function(df, idx) {
    if (is.null(label_col) || !label_col %in% names(df)) {
      return(rep(NA_character_, length(idx)))
    }
    clean_outlier_labels(df[[label_col]][idx])
  }

  if (!is.null(group_col) && group_col %in% names(data)) {
    grouped <- data
    grouped[[group_col]] <- droplevels(as.factor(grouped[[group_col]]))
    group_levels <- levels(grouped[[group_col]])
    split_data <- split(grouped, grouped[[group_col]], drop = TRUE)

    out_list <- lapply(group_levels, function(lvl) {
      subset <- split_data[[lvl]]
      if (is.null(subset)) return(NULL)

      bounds <- compute_outlier_bounds(subset[[value_col]])
      if (is.null(bounds)) return(NULL)

      idx <- which(subset[[value_col]] < bounds$lower | subset[[value_col]] > bounds$upper)
      if (!length(idx)) return(NULL)

      data.frame(
        x = factor(rep(lvl, length(idx)), levels = group_levels),
        y = subset[[value_col]][idx],
        group = factor(rep(lvl, length(idx)), levels = group_levels),
        label = extract_labels(subset, idx),
        stringsAsFactors = FALSE
      )
    })

    out_list <- Filter(has_rows, out_list)
    if (!length(out_list)) return(NULL)

    outliers <- do.call(rbind, out_list)
    rownames(outliers) <- NULL
    return(outliers)
  }

  bounds <- compute_outlier_bounds(data[[value_col]])
  if (is.null(bounds)) return(NULL)

  idx <- which(data[[value_col]] < bounds$lower | data[[value_col]] > bounds$upper)
  if (!length(idx)) return(NULL)

  data.frame(
    x = factor(rep(1, length(idx))),
    y = data[[value_col]][idx],
    group = NA,
    label = extract_labels(data, idx),
    stringsAsFactors = FALSE
  )
}

