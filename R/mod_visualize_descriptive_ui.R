# ===============================================================
# Visualization Module — Descriptive Statistics (Dispatcher)
# ===============================================================

visualize_descriptive_ui <- function(id) {
  ns <- NS(id)
  NicoShinyAppTemplate::app_layout(
    sidebar = NicoShinyAppTemplate::app_sidebar(
      title = "Descriptive statistics",
      description = "Explore distributions, variability, and normality across variables.",
      id = ns("sidebar"),
      NicoShinyAppTemplate::app_expander("Analysis setup", open = TRUE, id = ns("grouping_section"),
      with_help_tooltip(
        selectInput(
          ns("plot_type"),
          label = "Select visualization type",
          choices = c(
            "Categorical barplots" = "categorical",
            "Numeric boxplots"     = "boxplots",
            "Numeric histograms"   = "histograms"
          ),
          selected = "categorical"
        ),
        "Choose the descriptive chart that best answers your question."
      )
      ),
      uiOutput(ns("sub_controls"))
    ),
    div(
      class = "ta-main-panel",
      uiOutput(ns("plot_ui"))
    )
  )
}
