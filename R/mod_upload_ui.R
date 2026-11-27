# ===============================================================
# 🧪 Table Analyzer — Upload Module (Stable wide-format version)
# ===============================================================

mod_upload_ui <- function(id) {
  ns <- NS(id)
  VetmeduniAppTemplate::app_layout(
    sidebar = VetmeduniAppTemplate::app_sidebar(
      title = "Upload",
      description = "Choose whether to load the example dataset or upload your own Excel file.",
      id = ns("sidebar"),
      with_help_tooltip(
        radioButtons(
          ns("data_source"),
          label = "Data source",
          choices = c(
            "Upload (long format)" = "long",
            "Upload (wide format)" = "wide",
            "Example dataset" = "example"
          ),
          selected = "long"
        ),
        "Decide whether to explore the built-in example data or load your own table."
      ),
      uiOutput(ns("file_input")),
      uiOutput(ns("sheet_selector")),
      uiOutput(ns("layout_example")),
      uiOutput(ns("replicate_col_input")),
      actionButton(ns("open_type_editor"), "Edit column types", class = "w-100"),
      uiOutput(ns("type_editor_modal"))
      
    ),
    div(
      class = "ta-main-panel",
      h4("Data preview"),
      verbatimTextOutput(ns("validation_msg")),
      DTOutput(ns("preview"))
    )
  )
}

# Compatibility alias for existing callers.
upload_ui <- mod_upload_ui
