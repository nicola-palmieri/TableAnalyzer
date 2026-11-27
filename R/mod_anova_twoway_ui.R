# ===============================================================
# 🧪 Table Analyzer — Two-way ANOVA Module (Validated Version)
# ===============================================================

two_way_anova_ui <- function(id) {
  ns <- NS(id)
  list(
    config = tagList(
      NicoShinyAppTemplate::app_expander("Analysis setup", open = TRUE, id = ns("grouping_section"),
      uiOutput(ns("inputs")),
      uiOutput(ns("level_order_1")),
      uiOutput(ns("level_order_2"))
      ),
      NicoShinyAppTemplate::app_expander("Advanced options",
        stratification_ui("strat", ns)
      ),
      actionButton(ns("run"), "Run analysis", width = "100%")
    ),
    download = NicoShinyAppTemplate::app_download_ui(ns, "download_all", label = "Download results"),
    results = uiOutput(ns("summary_ui"))
  )
}
