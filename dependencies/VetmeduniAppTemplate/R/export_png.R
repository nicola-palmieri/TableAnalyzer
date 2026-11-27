# fct_export -- pixel-based PNG export at 300 dpi. Styling: R/plot_theme.R.
# Plain functions without interface; tested in tests/testthat/test-components.R.

# Screen pixels follow TableAnalyzer's 96-dpi layout. Export scales them to
# 300 dpi, retaining the same relative text/figure sizes. No inches in the API.
# Device: ragg::agg_png with integer pixel dimensions, avoiding conversion through
# inches in ggsave (which can lose a pixel to floating-point rounding).
# Without ragg, grDevices::png with res = dpi is used;
# called directly, grDevices::png writes pHYs with type "windows" and "cairo"
# (checked 2026-11-24 on R 4.6.2).
save_plot_png <- function(plot, file, width_px, height_px, dpi = 300) {
  is_grid <- inherits(plot, "grob")
  if (!is_grid && !inherits(plot, c("ggplot", "ggmatrix")))
    stop("`plot` must be a ggplot, ggmatrix or grid grob.")
  for (v in list(width_px, height_px, dpi)) {
    if (!is.numeric(v) || length(v) != 1 || !is.finite(v) || v <= 0) {
      stop("Width, height and dpi must be positive numbers.")
    }
  }
  width <- round(width_px * dpi / 96)
  height <- round(height_px * dpi / 96)
  if (requireNamespace("ragg", quietly = TRUE)) {
    ragg::agg_png(file, width = width, height = height, units = "px", res = dpi, background = "white")
  } else {
    grDevices::png(file, width = width, height = height, units = "px", res = dpi, bg = "white")
  }
  device <- grDevices::dev.cur()
  on.exit(grDevices::dev.off(device), add = TRUE)
  if (is_grid) {
    grid::grid.newpage()
    grid::grid.draw(plot)
  } else print(plot)
  invisible(file)
}

# Reads width and height (IHDR) and resolution (pHYs) from a PNG header in base R.
# Returns list(width, height, dpi_x, dpi_y); dpi is NA without a pHYs chunk in metres.
png_info <- function(file) {
  bytes <- readBin(file, what = "raw", n = file.size(file))
  signature <- as.raw(c(0x89, 0x50, 0x4e, 0x47, 0x0d, 0x0a, 0x1a, 0x0a))
  if (length(bytes) < 33 || !identical(bytes[1:8], signature)) stop("Not a PNG file: ", file)

  uint32 <- function(pos) sum(as.numeric(as.integer(bytes[pos:(pos + 3)])) * 256^(3:0))

  info <- list(width = NA_real_, height = NA_real_, dpi_x = NA_real_, dpi_y = NA_real_)
  pos <- 9
  while (pos + 7 <= length(bytes)) {
    len <- uint32(pos)
    type <- rawToChar(bytes[(pos + 4):(pos + 7)])
    data <- pos + 8
    if (type == "IHDR") {
      info$width <- uint32(data)
      info$height <- uint32(data + 4)
    } else if (type == "pHYs") {
      # pixels per unit x, y (4 bytes each), unit (1 byte; 1 = metre)
      if (as.integer(bytes[data + 8]) == 1L) {
        info$dpi_x <- round(uint32(data) * 0.0254)
        info$dpi_y <- round(uint32(data + 4) * 0.0254)
      }
    } else if (type %in% c("IDAT", "IEND")) {
      break # pHYs must precede the image data
    }
    pos <- data + len + 4 # skip data and CRC
  }
  info
}
