# ===============================================================
# 🟦 Descriptive Visualization — Numeric Histograms
# ===============================================================

visualize_numeric_histograms_ui <- function(id) {
  ns <- NS(id)
  tagList(
    NicoShinyAppTemplate::app_expander("Plot options", id = ns("options_section"),
    with_help_tooltip(
      checkboxInput(ns("use_density"), "Show density instead of count", FALSE),
      "Switch between showing counts or densities for each histogram."
    )
    ,
      plot_grid_ui(
      id = ns("plot_grid"),
      rows_help = "Choose how many rows of histograms to display.",
      cols_help = "Choose how many columns of histograms to display."
    )
      ),
    subplot_size_ui(
      ns,
      width_help  = "Set the width of each histogram panel in pixels.",
      height_help = "Set the height of each histogram panel in pixels."
    ),
    
    
    ta_plot_style_ui(ns, default = NicoShinyAppTemplate::PLOT_BASE_SIZE,
        help_text = "Adjust the base font size used for histogram labels.",
        legend = uiOutput(ns("common_legend_controls"))),
    
    NicoShinyAppTemplate::app_plot_actions_ui(ns)
  )
}


visualize_numeric_histograms_plot_ui <- function(id) {
  ns <- NS(id)
  tagList(NicoShinyAppTemplate::app_result_header_ui("Plots",
    NicoShinyAppTemplate::app_download_ui(ns)), uiOutput(ns("plot_container")))
}
