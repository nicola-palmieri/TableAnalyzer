build_pca_biplot <- function(
  pca_obj, data,
  color_var = NULL, shape_var = NULL,
  label_var = NULL, label_size = 2,
  show_loadings = FALSE, show_ellipses = FALSE,
  loading_scale = 1.2,
  custom_colors = NULL,
  subset_rows = NULL,
  x_limits = NULL, y_limits = NULL,
  base_size = 13
) {

  stopifnot(!is.null(pca_obj$x))

  # ---- PCA scores (PC1, PC2) ----
  scores <- as.data.frame(pca_obj$x[, 1:2])
  names(scores) <- c("PC1", "PC2")

  # Resolve shapes before faceting so a group keeps the same shape in every panel.
  shape_levels <- NULL
  if (!is.null(shape_var) && !is.null(data) && shape_var %in% names(data)) {
    values <- data[[shape_var]]
    shape_levels <- if (is.factor(values)) {
      levels(droplevels(values))
    } else {
      unique(as.character(values[!is.na(values)]))
    }
  }

  # ---- Subset if needed ----
  if (!is.null(subset_rows)) {
    subset_rows <- unique(subset_rows)
    subset_rows <- subset_rows[subset_rows >= 1 & subset_rows <= nrow(scores)]
    scores <- scores[subset_rows, , drop = FALSE]
    if (!is.null(data)) data <- data[subset_rows, , drop = FALSE]
  }

  # ---- Combine data + scores ----
  if (!is.null(data) && nrow(data) == nrow(scores)) {
    plot_data <- cbind(scores, data)
  } else {
    plot_data <- scores
  }

  # ---- Explained variance for axis labels ----
  var_exp <- 100 * (pca_obj$sdev^2 / sum(pca_obj$sdev^2))
  x_lab <- sprintf("PC1 (%.1f%%)", var_exp[1])
  y_lab <- sprintf("PC2 (%.1f%%)", var_exp[2])

  # ---- Labeling (optional) ----
  if (!is.null(label_var) && label_var %in% names(plot_data)) {
    labs <- as.character(plot_data[[label_var]])
    labs[is.na(labs) | trimws(labs) == ""] <- NA
    if (any(!is.na(labs))) {
      plot_data$label_value <- labs
    } else {
      label_var <- NULL
    }
  } else {
    label_var <- NULL
  }

  # ---- Colour factor handling ----
  if (!is.null(color_var) && color_var %in% names(plot_data)) {
    values <- plot_data[[color_var]]

    if (is.factor(values)) {
      lvls <- levels(droplevels(values))
    } else {
      lvls <- unique(as.character(values[!is.na(values)]))
    }

    lvls <- lvls[!is.na(lvls) & nzchar(lvls)]
    plot_data[[color_var]] <- factor(as.character(values), levels = lvls)
  }

  # ---- Aesthetic mapping ----
  aes_args <- list(x = quote(PC1), y = quote(PC2))
  if (!is.null(color_var)) aes_args$colour <- rlang::expr(.data[[color_var]])
  if (!is.null(shape_var)) aes_args$shape  <- rlang::expr(.data[[shape_var]])

  aes_mapping <- do.call(ggplot2::aes, aes_args)

  # ---- Single-color fallback ----
  single_color <- resolve_single_color(custom_colors)

  # ---- Base plot (WITHOUT geom_point yet) ----
  g <- ggplot(plot_data, aes_mapping)

  point_options <- list(size = 3)
  if (is.null(color_var)) point_options$color <- single_color
  if (is.null(shape_var)) point_options$shape <- 16
  g <- g + do.call(ggplot2::geom_point, point_options)

  if (!is.null(shape_var)) {
    shape_palette <- c(16, 17, 15, 3, 7, 8, 0, 1, 2, 5)
    shapes <- stats::setNames(rep(shape_palette, length.out = length(shape_levels)), shape_levels)
    g <- g + scale_shape_manual(values = shapes, drop = FALSE, na.value = 16)
  }

  # ---- Ellipses ----
  if (!is.null(color_var) && isTRUE(show_ellipses)) {
    g <- g + stat_ellipse(level = 0.68, linewidth = 0.6, alpha = 0.5)
  }

  # ---- Manual colours (only if a variable is mapped) ----
  if (!is.null(color_var)) {
    lvl <- levels(plot_data[[color_var]])

    palette <- resolve_palette_for_levels(lvl, custom = custom_colors)

    # Make sure names(palette) == levels(color_var)
    if (is.null(names(palette)) || !all(names(palette) == lvl)) {
      names(palette) <- lvl
    }

    g <- g + scale_color_manual(values = palette)
  }

  # ---- Axis limits ----
  if (!is.null(x_limits) || !is.null(y_limits)) {
    g <- g + coord_cartesian(xlim = x_limits, ylim = y_limits)
  }

  # ---- Labels ----
  if (!is.null(label_var)) {
    g <- g + ggrepel::geom_text_repel(
      aes(label = label_value),
      size = label_size,
      color = if (is.null(color_var)) single_color else NULL,
      max.overlaps = Inf,
      box.padding = 0.3,
      point.padding = 0.2,
      segment.size = 0.2
    )
  }

  # ---- Theme ----
  g <- g +
    ta_plot_theme(base_size = base_size, kind = "pcoa") +
    labs(
      x = x_lab,
      y = y_lab,
      color = if (!is.null(color_var)) color_var else NULL,
      shape = if (!is.null(shape_var)) shape_var else NULL
    ) +
    theme(
      legend.position = "right"
    )

  # ---- Loadings arrows ----
  if (isTRUE(show_loadings) && !is.null(pca_obj$rotation)) {
    R <- as.data.frame(pca_obj$rotation[, 1:2])
    R$variable <- rownames(R)

    rx <- diff(range(pca_obj$x[, 1], na.rm = TRUE))
    ry <- diff(range(pca_obj$x[, 2], na.rm = TRUE))
    sx <- ifelse(is.finite(rx) && rx > 0, rx, 1)
    sy <- ifelse(is.finite(ry) && ry > 0, ry, 1)

    arrows_df <- transform(
      R,
      x = 0, y = 0,
      xend = PC1 * sx * loading_scale,
      yend = PC2 * sy * loading_scale
    )

    g <- g +
      geom_segment(
        data = arrows_df,
        aes(x = x, y = y, xend = xend, yend = yend),
        inherit.aes = FALSE,
        color = "grey30",
        linewidth = 0.4,
        arrow = grid::arrow(length = grid::unit(0.02, "npc"))
      ) +
      ggrepel::geom_text_repel(
        data = arrows_df,
        aes(x = xend, y = yend, label = variable),
        inherit.aes = FALSE,
        size = 3,
        color = "grey20",
        box.padding = 0.2,
        point.padding = 0.2,
        segment.size = 0.2
      )
  }

  g
}
