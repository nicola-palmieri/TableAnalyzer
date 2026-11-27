# One place to edit figure styling; no global ggplot defaults or external fonts.
PLOT_BASE_SIZE <- 13
PLOT_FONT_FAMILY <- "sans"

# Heatmap geometry/text policy. Keep all figure styling in this file.
PLOT_HEATMAP <- list(row_wrap = 34L, column_wrap = 44L, column_angle = 80, band_wrap = 10L, heading_wrap = 36L,
  legend_wrap = 26L, row_height = 26, column_width = 42, outer_gap = 12, panel_gap = 24,
  text_lineheight = 1.1, band_size = 11, dendrogram_width = .6, strip_width = .15)

plot_theme <- function(base_size = PLOT_BASE_SIZE,
                       kind = c("standard", "bar", "box", "pcoa", "heatmap", "canvas", "void"),
                       legend_position = "right") {
  kind <- match.arg(kind)
  ink <- "#252525"
  if (kind %in% c("canvas", "void")) {
    return(ggplot2::theme_void(base_size = base_size, base_family = PLOT_FONT_FAMILY) +
      ggplot2::theme(plot.background = ggplot2::element_rect(fill = "white", colour = NA),
                     plot.title = ggplot2::element_blank(), plot.subtitle = ggplot2::element_blank(),
                     plot.margin = ggplot2::margin(0, 0, 0, 0)))
  }
  theme <- ggplot2::theme_minimal(base_size = base_size, base_family = PLOT_FONT_FAMILY) +
    ggplot2::theme(
      text = ggplot2::element_text(colour = ink),
      plot.background = ggplot2::element_rect(fill = "white", colour = NA),
      panel.background = ggplot2::element_rect(fill = "white", colour = NA),
      plot.title = ggplot2::element_blank(),
      plot.subtitle = ggplot2::element_blank(),
      plot.caption = ggplot2::element_text(colour = "#555555", size = ggplot2::rel(0.8), hjust = 0, margin = ggplot2::margin(t = 8)),
      plot.title.position = "plot", plot.caption.position = "plot",
      plot.margin = ggplot2::margin(8, 10, 8, 8),
      axis.title = ggplot2::element_text(face = "plain", size = ggplot2::rel(1)),
      axis.title.x = ggplot2::element_text(margin = ggplot2::margin(t = 7)),
      axis.title.y = ggplot2::element_text(margin = ggplot2::margin(r = 7)),
      axis.text = ggplot2::element_text(colour = ink, size = ggplot2::rel(0.9)),
      axis.line = ggplot2::element_line(colour = ink, linewidth = 0.35),
      axis.ticks = ggplot2::element_line(colour = ink, linewidth = 0.3),
      axis.ticks.length = grid::unit(2.5, "pt"),
      panel.border = ggplot2::element_blank(),
      panel.grid.major = ggplot2::element_line(colour = "#E8E8E8", linewidth = 0.25),
      panel.grid.major.x = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      panel.spacing = grid::unit(12, "pt"),
      strip.background = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold", colour = ink, size = ggplot2::rel(0.9), margin = ggplot2::margin(5, 5, 5, 5)),
      legend.position = legend_position,
      legend.title = ggplot2::element_text(face = "bold", size = ggplot2::rel(0.9)),
      legend.text = ggplot2::element_text(size = ggplot2::rel(0.85)),
      legend.background = ggplot2::element_rect(fill = "white", colour = NA),
      legend.key = ggplot2::element_rect(fill = "white", colour = NA),
      legend.key.size = grid::unit(11, "pt"),
      legend.margin = ggplot2::margin(3, 3, 3, 3),
      legend.box.spacing = grid::unit(6, "pt")
    )
  if (kind == "bar") theme <- theme + ggplot2::theme(strip.placement = "outside")
  if (kind %in% c("pcoa", "heatmap")) theme <- theme + ggplot2::theme(panel.grid.major = ggplot2::element_blank())
  if (kind == "heatmap") theme <- theme + ggplot2::theme(
    axis.line = ggplot2::element_blank(), axis.ticks = ggplot2::element_blank(),
    axis.text = ggplot2::element_text(lineheight = PLOT_HEATMAP$text_lineheight),
    axis.text.y.right = ggplot2::element_text(hjust = 0, margin = ggplot2::margin(l = 8)),
    legend.text = ggplot2::element_text(lineheight = PLOT_HEATMAP$text_lineheight),
    legend.box.spacing = grid::unit(14, "pt"), legend.box = "vertical")
  theme
}

# Content-aware adjustments stay here too; the caller decides when they apply.
plot_panel_label <- function(plot, label, base_size = PLOT_BASE_SIZE) {
  if (is.null(label) || !length(label) || !nzchar(label)) return(plot)
  data <- plot$data
  if (!is.data.frame(data) || nrow(data) == 0L) data <- data.frame(row.names = 1L)
  key <- tail(make.unique(c(names(data), ".app_panel_label")), 1)
  data[[key]] <- rep(plot_wrap_text(label, 36L), nrow(data))
  plot$data <- data
  plot + ggplot2::facet_wrap(stats::reformulate(key)) +
    ggplot2::theme(strip.text = ggplot2::element_text(family = PLOT_FONT_FAMILY,
      face = "bold", colour = "#252525", size = base_size * .9),
      strip.background = ggplot2::element_blank())
}

plot_x_labels <- function(angle = 0, ticks = TRUE) {
  theme <- ggplot2::theme(axis.text.x = ggplot2::element_text(
    angle = angle, hjust = if (angle == 0) 0.5 else 1, vjust = if (angle == 0) 0.5 else 1))
  if (!ticks) theme <- theme + ggplot2::theme(axis.ticks.x = ggplot2::element_blank())
  theme
}

plot_group_spacing <- function(changes) {
  ggplot2::theme(panel.spacing.x = grid::unit(ifelse(changes, 1.2, 0.15), "lines"))
}

plot_heatmap_labels <- function(labels, angled) {
  plot_wrap_text(gsub(", ", "\n", labels, fixed = TRUE), PLOT_HEATMAP$column_wrap)
}

# Hard-wrap long tokens too; preserve every non-whitespace character.
plot_wrap_text <- function(labels, width) {
  vapply(labels, function(label) {
    lines <- strsplit(as.character(label), "\n", fixed = TRUE)[[1]]
    paste(vapply(lines, function(line) {
      tokens <- strsplit(line, "[[:space:]]+")[[1]]
      tokens <- unlist(lapply(tokens, function(token) {
        if (!nzchar(token)) return("")
        starts <- seq.int(1L, nchar(token), by = width)
        substring(token, starts, pmin(nchar(token), starts + width - 1L))
      }), use.names = FALSE)
      paste(strwrap(paste(tokens, collapse = " "), width = width), collapse = "\n")
    }, ""), collapse = "\n")
  }, "", USE.NAMES = FALSE)
}

plot_heatmap_legend_spacing <- function(labels) {
  wrapped <- plot_wrap_text(labels, PLOT_HEATMAP$legend_wrap)
  lines <- max(lengths(strsplit(wrapped, "\n", fixed = TRUE)))
  ggplot2::theme(legend.key.height = grid::unit(
    lines * PLOT_BASE_SIZE * .85 * PLOT_HEATMAP$text_lineheight + 5, "pt"))
}

# Compatibility for existing component checks and callers.
theme_app <- function(base_size = PLOT_BASE_SIZE) plot_theme(base_size)
