# ===============================================================
# 🧪 Table Analyzer — Analysis Coordinator
# ===============================================================

mod_analyze_ui <- function(id, modules = analysis_module_registry()) {
  ns <- NS(id)
  configurations <- lapply(names(modules), function(method) {
    mod <- modules[[method]]
    conditionalPanel(
      condition = sprintf("input['%s'] === %s", ns("analysis_type"),
        as.character(jsonlite::toJSON(method, auto_unbox = TRUE))),
      div(class = "analysis-method-config", `data-analysis` = method,
        mod$ui(ns(mod$id))$config)
    )
  })
  VetmeduniAppTemplate::app_layout(
    sidebar = VetmeduniAppTemplate::app_sidebar(
      title = "Statistical analysis",
      description = "Select an analysis type to explore your data, then inspect the summaries on the right.",
      id = ns("sidebar"),
      
      # --- Analysis type selector ---
      div(class = "app-complete-choices", with_help_tooltip(
        selectInput(
          ns("analysis_type"),
          "Select analysis type",
          choices = list(
            "None" = "none",
            "Descriptive" = c("Descriptive Statistics" = "Descriptive Statistics"),
            "Univariate" = c(
              "One-way ANOVA" = "One-way ANOVA",
              "Two-way ANOVA" = "Two-way ANOVA",
              "Linear Model (LM)" = "Linear Model (LM)",
              "Linear Mixed Model (LMM)" = "Linear Mixed Model (LMM)"
            ),
            "Multivariate" = c(
              "Pairwise Correlation" = "Pairwise Correlation",
              "Principal Component Analysis (PCA)" = "PCA"
            )
          ),
          selected = "none"
        ),
        "Choose the statistical method you want to run on the filtered data."
      )),
      div(id = ns("config_panel"), class = "analysis-config-panels", configurations)
    ),
    
    div(
      class = "ta-main-panel",
      div(
        class = "ta-results-header app-result-header",
        h4("Analysis results"),
        uiOutput(ns("summary_help_icon")),
        div(class = "app-result-actions", lapply(names(modules), function(method) {
          mod <- modules[[method]]
          conditionalPanel(
            condition = sprintf("input['%s'] === %s", ns("analysis_type"),
              as.character(jsonlite::toJSON(method, auto_unbox = TRUE))),
            mod$ui(ns(mod$id))$download)
        }))
      ),
      div(
        class = "ta-analysis-results",
        uiOutput(ns("results_panel"))
      )
    )
  )
}

# Compatibility alias for existing callers.
analysis_ui <- mod_analyze_ui
