# Home content is app specific; rendering and branding are shared with the twin.
mod_home_ui <- function(id) {
  NicoShinyAppTemplate::app_home_ui(
    id, main = APP_BRAND_MAIN, accent = APP_BRAND_ACCENT, version = APP_VERSION,
    artwork = APP_CONFIG$artwork,
    description = paste(
      "Explore tabular data, compare groups and fit statistical models.",
      "Create publication-ready tables and figures in a guided workflow."
    ),
    start_label = "Start an analysis", start_id = "go_upload",
    analyses = list(
      list(key = "descriptive", title = "Descriptive statistics",
        description = "Understand distributions, variability and missing values with summaries and tailored plots.",
        symbol = "chart-simple"),
      list(key = "models", title = "Group comparisons and models",
        description = "Run one-way or two-way ANOVA, linear models and mixed models with optional stratification.",
        symbol = "boxplot"),
      list(key = "pca", title = "Correlations and PCA",
        description = "Inspect pairwise relationships and multivariate patterns with correlation matrices and PCA biplots.",
        symbol = "circle-nodes")
    ),
    workflow = list(
      list(title = "Upload", description = "Load an Excel workbook in long or wide format, or try the bundled example."),
      list(title = "Filter", description = "Refine records and select the variables relevant to your study.", optional = TRUE),
      list(title = "Analyze", description = "Choose a method, configure the statistical model and inspect the results."),
      list(title = "Visualize", description = "Style the figures and download plots or formatted model reports.")
    ),
    extra = shiny::p(class = "home-developer", "Developed by Nicola Palmieri")
  )
}

# Compatibility alias for existing callers.
home_ui <- mod_home_ui
