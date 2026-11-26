# ===============================================================
# 🧩 Visualization Coordinator (Lazy + Reactive)
# ===============================================================

mod_visualize_ui <- function(id) {
  ns <- NS(id)
  panels <- list(
    oneway_anova = visualize_oneway_ui(ns("oneway")),
    twoway_anova = visualize_twoway_ui(ns("twoway")),
    pairs = visualize_ggpairs_ui(ns("ggpairs")),
    pca = visualize_pca_ui(ns("pca")),
    descriptive = visualize_descriptive_ui(ns("descriptive"))
  )
  tagList(
    div(id = ns("dynamic_ui"), class = "visualization-panels",
      lapply(names(panels), function(type) conditionalPanel(
        condition = sprintf("output['%s'] === '%s'", ns("active_type"), type),
        div(class = "visualization-method-panel", `data-visualization` = type, panels[[type]])
      ))),
    conditionalPanel(
      condition = sprintf("output['%s'] === ''", ns("active_type")),
      uiOutput(ns("empty_ui"))
    )
  )
}

# Compatibility alias for existing callers.
visualize_ui <- mod_visualize_ui
