# Legacy Table plotting entrypoints share the twin app theme.
# Names stay stable for scientific plot code and downstream callers.
ta_plot_theme <- function(base_size = VetmeduniAppTemplate::PLOT_BASE_SIZE, kind = "standard") {
  VetmeduniAppTemplate::plot_theme(base_size = base_size, kind = kind)
}

ta_plot_theme_void <- function(base_size = VetmeduniAppTemplate::PLOT_BASE_SIZE) {
  VetmeduniAppTemplate::plot_theme(base_size = base_size, kind = "void")
}
