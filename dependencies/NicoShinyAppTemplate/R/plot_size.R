# Intrinsic screen sizes: grow with groups/panels, never with browser width.
# Width/height are base subplot sizes in logical screen pixels.
content_plot_size <- function(plot, kind = c("bar", "box", "pcoa"), width_px = 400, height_px = 300) {
  kind <- match.arg(kind)
  b <- ggplot2::ggplot_build(plot)
  layout <- b$layout$layout
  nr <- max(layout$ROW); nc <- max(layout$COL)
  legend <- plot$theme$legend.position
  if (is.null(legend)) legend <- "right"
  guides <- if (kind == "pcoa") c("colour", "shape") else "fill"
  n_legend <- max(c(0L, vapply(guides, function(a) {
    scale <- b$plot$scales$get_scales(a)
    if (is.null(scale) || !scale$is_discrete()) 0L else length(scale$get_limits())
  }, integer(1))))
  if (identical(legend, "none")) n_legend <- 0L
  if (kind == "pcoa") {
    widths <- rep(320, nc)
  } else {
    positions <- unique(b$data[[1]][c("PANEL", "x")])
    counts <- table(positions$PANEL)
    widths <- vapply(seq_len(nc), function(col) {
      panels <- as.character(layout$PANEL[layout$COL == col])
      n <- max(c(1, unname(counts[panels])), na.rm = TRUE)
      max(220, 140 + 55 * n)
    }, numeric(1))
  }
  side_legend <- n_legend > 0 && legend %in% c("right", "left")
  width <- sum(widths) + 90 + 20 * (nc - 1) + if (side_legend) 150 else 0
  width <- ceiling(width * width_px / 400 / 10) * 10
  legend_rows <- if (n_legend && !side_legend) ceiling(n_legend / max(1, floor((width - 80) / 140))) else 0
  caption <- plot$labels$caption
  caption_lines <- if (is.null(caption)) 0 else length(strwrap(as.character(caption), width = max(30, floor(width / 7))))
  height <- (nr * if (kind == "pcoa") 300 else 260) + 60 + legend_rows * 20 + caption_lines * 15
  size <- list(width = as.integer(width), height = as.integer(ceiling(height * height_px / 300)),
       legend_rows = as.integer(legend_rows))
  if (kind == "pcoa") {
    # Guide titles/labels have physical sizes, independent of the subplot size.
    # Measure the actual combined guides rather than estimating their width from
    # the number of levels in the largest scale. Null units are panel space;
    # convertWidth/Height leave those at zero, retaining axes, margins and guides.
    measured <- content_plot_extents(content_sized_plot(plot, size), size)
    size$width <- as.integer(ceiling(max(size$width,
      measured$fixed_width + max(sum(widths) * width_px / 400, measured$horizontal_guide)) / 10) * 10)
    size$height <- as.integer(ceiling(max(size$height,
      measured$fixed_height + max(nr * 300 * height_px / 300, measured$side_guide))))
  }
  size
}

content_plot_extents <- function(plot, size) {
  # agg_capture is an in-memory device: measurement never creates Rplots.pdf or
  # changes the caller's active device. Its font metrics match the PNG exporter.
  ragg::agg_capture(width = size$width, height = size$height, units = "px", res = 96)
  device <- grDevices::dev.cur()
  on.exit(grDevices::dev.off(device), add = TRUE)
  gt <- ggplot2::ggplotGrob(plot)
  px_width <- function(x) grid::convertWidth(x, "inches", valueOnly = TRUE) * 96
  px_height <- function(x) grid::convertHeight(x, "inches", valueOnly = TRUE) * 96
  guide_extent <- function(position, dimension) {
    i <- which(gt$layout$name %in% paste0("guide-box-", position))
    max(c(0, vapply(i, function(j) {
      guide <- gt$grobs[[j]]
      if (!inherits(guide, "gtable")) return(0)
      if (dimension == "width") px_width(sum(guide$widths)) else px_height(sum(guide$heights))
    }, numeric(1))))
  }
  list(fixed_width = px_width(sum(gt$widths)), fixed_height = px_height(sum(gt$heights)),
    horizontal_guide = guide_extent(c("top", "bottom"), "width"),
    side_guide = guide_extent(c("left", "right"), "height"))
}

content_sized_plot <- function(plot, size) {
  caption <- plot$labels$caption
  if (!is.null(caption)) plot <- plot + ggplot2::labs(caption = paste(strwrap(as.character(caption),
    width = max(30, floor(size$width / 7))), collapse = "\n"))
  if (!is.null(size$legend_rows) && size$legend_rows > 0) {
    plot <- plot + ggplot2::guides(
      fill = ggplot2::guide_legend(nrow = size$legend_rows, byrow = TRUE),
      colour = ggplot2::guide_legend(nrow = size$legend_rows, byrow = TRUE),
      shape = ggplot2::guide_legend(nrow = size$legend_rows, byrow = TRUE))
  }
  plot
}

content_plot_ui <- function(id, size) {
  shiny::div(class = "plot-scroll", tabindex = "0", role = "region", `aria-label` = "Plot; scroll horizontally for larger figures",
    shiny::plotOutput(id, width = paste0(size$width, "px"), height = paste0(size$height, "px")))
}

png_pixel_dimensions <- function(size, dpi = 300) {
  sprintf("PNG: %d x %d px (%d dpi)", round(size$width * dpi / 96), round(size$height * dpi / 96), dpi)
}
