# ===============================================================
# 🧪 Table  Analyzer — Pairwise Correlation Module
# ===============================================================

ggpairs_ui <- function(id) {
  ns <- NS(id)
  list(
    config = tagList(
      VetmeduniAppTemplate::app_expander("Analysis setup", open = TRUE, id = ns("grouping_section"),
      uiOutput(ns("vars_ui"))
      ),
      VetmeduniAppTemplate::app_expander("Advanced options",
        stratification_ui("strat", ns)
      ),
      actionButton(ns("run"), "Run analysis", width = "100%")
    ),
    download = VetmeduniAppTemplate::app_download_ui(ns, "download_model", label = "Download results"),
    results = tagList(
      h5("Correlation matrix"),
      verbatimTextOutput(ns("summary"))
    )
  )
}
