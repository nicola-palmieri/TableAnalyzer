# ===============================================================
# ?? Regression module entrypoints
# ===============================================================
# Helper implementations live in regression_shared_*.R files.

regression_ui <- function(id, engine = c("lm", "lmm"), allow_multi_response = FALSE) {
  ns <- NS(id)
  engine <- match.arg(engine)
  allow_multi_response <- isTRUE(allow_multi_response)

  list(
    config = tagList(
      VetmeduniAppTemplate::app_expander("Analysis setup", open = TRUE, id = ns("grouping_section"),
      if (allow_multi_response) multi_response_ui(ns("response")) else uiOutput(ns("response_ui")),
      uiOutput(ns("fixed_selector")),
      uiOutput(ns("level_order")),
      uiOutput(ns("covar_selector")),
      if (engine == "lmm") uiOutput(ns("random_selector")),
      uiOutput(ns("interaction_select")),
      uiOutput(ns("formula_preview"))
      ),
      VetmeduniAppTemplate::app_expander("Advanced options",
        stratification_ui("strat", ns)
      ),
      actionButton(ns("run"), "Run analysis", width = "100%")
    ),
    download = VetmeduniAppTemplate::app_download_ui(ns, "download_model", label = "Download results"),
    results = tagList(
      uiOutput(ns("results_ui"))
    )
  )
}
