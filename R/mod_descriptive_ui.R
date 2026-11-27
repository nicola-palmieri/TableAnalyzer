# ===============================================================
# 🧾 Table Analyzer — Descriptive Statistics Modules
# ===============================================================

descriptive_ui <- function(id) {
  ns <- NS(id)
  list(
    config = tagList(
      VetmeduniAppTemplate::app_expander("Analysis setup", open = TRUE, id = ns("grouping_section"),
      uiOutput(ns("inputs"))
      ),
      VetmeduniAppTemplate::app_expander("Advanced options",
        stratification_ui("strat", ns)
      ),
      actionButton(ns("run"), "Run analysis", width = "100%"),
      hr()
    ),
    download = VetmeduniAppTemplate::app_download_ui(ns, "download_summary", label = "Download results"),
    results = tagList(
      verbatimTextOutput(ns("summary_text"))
    )
  )
}
