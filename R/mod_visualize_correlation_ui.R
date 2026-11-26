# ===============================================================
# 🧪 Visualization Module — Pairwise Correlation (Dispatcher)
# ===============================================================

visualize_ggpairs_ui <- function(id) {
  ns <- NS(id)
  NicoShinyAppTemplate::app_layout(
    sidebar = NicoShinyAppTemplate::app_sidebar(
      title = "Pairwise correlation",
      description = "Visualize pairwise relationships and correlation coefficients among numeric variables.",
      id = ns("sidebar"),
      NicoShinyAppTemplate::app_expander("Analysis setup", open = TRUE, id = ns("grouping_section"),
      with_help_tooltip(
        selectInput(
          ns("plot_type"),
          label = "Select visualization type",
          choices = c("Pairwise scatterplot matrix" = "GGPairs"),
          selected = "GGPairs"
        ),
        "Choose how to visualise the pairwise relationships between variables."
      )
      ),
      conditionalPanel(
        condition = sprintf("output['%s']", ns("show_controls")),
        pairwise_correlation_visualize_ggpairs_ui(ns("ggpairs"))
      ),
      conditionalPanel(
        condition = sprintf("output['%s']", ns("show_placeholder")),
        helpText("Run the pairwise correlation analysis to configure plots.")
      ),
      br(),
      NicoShinyAppTemplate::app_plot_actions_ui(ns)
    ),
    div(
      class = "ta-main-panel",
      NicoShinyAppTemplate::app_result_header_ui("Plots", NicoShinyAppTemplate::app_download_ui(ns)),
      uiOutput(ns("plot_container"))
    )
  )
}
