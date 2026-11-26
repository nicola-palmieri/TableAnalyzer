mod_home_ui <- function(id) {
  NicoShinyAppTemplate::app_home_ui(id, main = APP_CONFIG$main, accent = APP_CONFIG$accent,
    version = APP_CONFIG$version, description = "Describe this application's purpose here.",
    start_id = "start", analyses = list(list(key = "explore", title = "Explore",
      description = "Describe this application's analysis.",
      symbol = "chart-simple")),
    workflow = list(list(title = "Prepare", description = "Load and validate your data."),
      list(title = "Explore", description = "Run and inspect your analysis."),
      list(title = "Export", description = "Save the resulting figures and tables.")))
}
