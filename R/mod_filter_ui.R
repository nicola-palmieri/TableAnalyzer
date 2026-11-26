# ===============================================================
# 🧪 Table Analyzer — Filter Module (Refactored Reactive Version)
# ===============================================================

mod_filter_ui <- function(id) {
  ns <- NS(id)
  NicoShinyAppTemplate::app_layout(
    sidebar = NicoShinyAppTemplate::app_sidebar(
      title = "Filter",
      description = "Select the columns to focus on and adjust the filters to refine the dataset for analysis.",
      id = ns("sidebar"),
      uiOutput(ns("column_selector")),
      uiOutput(ns("na_controls")),
      uiOutput(ns("filter_widgets"))
    ),
    div(
      class = "ta-main-panel",
      h4("Filtered data preview"),
      DTOutput(ns("filtered_preview"))
    )
  )
}

# Compatibility alias for existing callers.
filter_ui <- mod_filter_ui
