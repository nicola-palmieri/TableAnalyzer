# ===============================================================
# 🧪 Table Analyzer — PCA Module
# ===============================================================

pca_ui <- function(id) {
  ns <- NS(id)
  list(
    config = tagList(
      NicoShinyAppTemplate::app_expander("Analysis setup", open = TRUE, id = ns("grouping_section"),
      uiOutput(ns("vars_ui"))
      ),
      NicoShinyAppTemplate::app_expander("Advanced options",
        helpText(paste(
          "Stratify by is not available for PCA because the principal components are computed on the full numeric matrix.",
          "Splitting the data by groups would produce different coordinate systems, making the loadings and scores incomparable across groups."
        ))
      ),
      actionButton(ns("run_pca"), "Run analysis", width = "100%")
    ),
    download = NicoShinyAppTemplate::app_download_ui(ns, "download_all", label = "Download results"),
    results = tagList(
      h5("PCA summary and loadings"),
      uiOutput(ns("results_ui"))
    )
  )
}
