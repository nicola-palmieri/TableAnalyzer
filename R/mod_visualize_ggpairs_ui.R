# ===============================================================
# 🧪 Pairwise Correlation — GGPairs Visualization Module
# ===============================================================

pairwise_correlation_visualize_ggpairs_ui <- function(id, base_size_value = NULL) {
  ns <- NS(id)
  base_size_value <- suppressWarnings(as.numeric(base_size_value))
  if (length(base_size_value) == 0 || is.na(base_size_value)) base_size_value <- 11
  tagList(
    VetmeduniAppTemplate::app_expander("Plot options", id = ns("options_section"),
      uiOutput(ns("grid_controls"))),
    uiOutput(ns("size_controls")),
    ta_plot_style_ui(ns, default = base_size_value,
        help_text = "Adjust the base font size used for the correlation plot.")
  )
}
