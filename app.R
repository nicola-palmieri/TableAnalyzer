# ===============================================================
# 🧪 Table Analyzer — Standalone App
# ===============================================================

library(bslib)
library(dplyr)
library(DT)
library(flextable)
library(ggplot2)
library(officer)
library(shiny)

options(shiny.autoreload = interactive())
options(shiny.maxRequestSize = 200 * 1024^2)

for (f in list.files("R", full.names = TRUE, pattern = "\\.R$")) source(f)

# ---------------------------------------------------------------
# UI
# ---------------------------------------------------------------
ui <- VetmeduniAppTemplate::app_navbar(
  main = APP_BRAND_MAIN, accent = APP_BRAND_ACCENT, logo = APP_CONFIG$logo, id = "main_nav",
  selected = "home_tab", fillable = FALSE, window_title = APP_NAME,
  header = tags$head(
    tags$link(rel = "icon", type = "image/jpeg", href = APP_CONFIG$logo),
    VetmeduniAppTemplate::app_busy_options()
  ),
  bslib::nav_panel("Home", value = "home_tab", icon = icon("house"), mod_home_ui("home")),
  bslib::nav_panel("Upload", value = "upload_tab", icon = icon("upload"),
    VetmeduniAppTemplate::app_page_ui(mod_upload_ui("upload"))),
  bslib::nav_panel("Filter", value = "filter_tab", icon = icon("filter"),
    VetmeduniAppTemplate::app_page_ui(mod_filter_ui("filter"))),
  bslib::nav_panel("Analyze", value = "analysis_tab", icon = icon("square-poll-horizontal"),
    VetmeduniAppTemplate::app_page_ui(mod_analyze_ui("analysis"))),
  bslib::nav_panel("Visualize", value = "visualize_tab", icon = icon("chart-area"),
    VetmeduniAppTemplate::app_page_ui(mod_visualize_ui("visualize"))),
  bslib::nav_panel("User guide", value = "guide_tab", icon = icon("book-open"),
    VetmeduniAppTemplate::app_page_ui(VetmeduniAppTemplate::mod_user_guide_ui("guide")))
)

# ---------------------------------------------------------------
# SERVER
# ---------------------------------------------------------------
server <- function(input, output, session) {
  uploaded  <- mod_upload_server("upload")
  mod_home_server("home", has_data = reactive(!is.null(uploaded())))
  observeEvent(input$go_home, {
    bslib::nav_select("main_nav", selected = "home_tab", session = session)
  })
  filtered  <- mod_filter_server("filter", uploaded)
  analyzed  <- mod_analyze_server("analysis", filtered)
  mod_visualize_server("visualize", filtered, analyzed$results, analyzed$selection)

  observe({
    has_data <- !is.null(uploaded())
    tabs <- c("filter_tab", "analysis_tab", "visualize_tab")

    lapply(tabs, function(tab) {
      session$sendCustomMessage(
        "toggleTabState",
        list(tab = tab, disable = !has_data)
      )
    })

    if (!has_data && input$main_nav %in% tabs) {
      bslib::nav_select("main_nav", selected = "upload_tab", session = session)
    }
  })
}

# ---------------------------------------------------------------
# LAUNCH
# ---------------------------------------------------------------
shinyApp(ui, server)
