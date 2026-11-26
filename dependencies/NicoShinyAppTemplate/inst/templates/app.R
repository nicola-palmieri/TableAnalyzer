# Run this directory with shiny::runApp().
for (file in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(file, local = TRUE)
ui <- NicoShinyAppTemplate::app_navbar(
  bslib::nav_panel("Home", value = "home", mod_home_ui("home")),
  bslib::nav_panel("Explore", value = "explore",
    NicoShinyAppTemplate::app_page_ui(NicoShinyAppTemplate::app_layout(
      NicoShinyAppTemplate::app_sidebar("Explore", "Add application-specific controls here.",
        NicoShinyAppTemplate::mod_export_controls_ui("figure")),
      NicoShinyAppTemplate::app_result_header_ui("Results",
        NicoShinyAppTemplate::mod_export_actions_ui("figure")),
      shiny::uiOutput("figure_warning"), shiny::uiOutput("figure_preview")))),
  main = APP_CONFIG$main, accent = APP_CONFIG$accent, version = APP_CONFIG$version,
  window_title = APP_CONFIG$name, id = "page")
server <- function(input, output, session) {
  navigate <- function(page) bslib::nav_select("page", page, session = session)
  shiny::observeEvent(input$go_home, navigate("home"))
  mod_home_server("home", navigate)
  # Replace this example with app-specific calculations; retain one snapshot for
  # preview/export so incomplete edits cannot mix a new size with an old figure.
  settings <- NicoShinyAppTemplate::mod_export_server("figure",
    plot = shiny::reactive({ shiny::req(snapshot()$plot); snapshot()$plot }),
    filename = "example_boxplot",
    size = shiny::reactive({ shiny::req(snapshot()$size); snapshot()$size }))
  snapshot <- NicoShinyAppTemplate::app_plot_snapshot(function() {
    controls <- settings()
    plot <- ggplot2::ggplot(datasets::mtcars, ggplot2::aes(factor(cyl), mpg)) +
      ggplot2::geom_boxplot(fill = NicoShinyAppTemplate::APP_PALETTE[1]) +
      NicoShinyAppTemplate::plot_theme(kind = "box") +
      ggplot2::labs(x = "Cylinders", y = "Miles per gallon")
    list(plot = plot, size = NicoShinyAppTemplate::content_plot_size(
      plot, "box", controls$width, controls$height))
  })
  output$figure_warning <- shiny::renderUI({
    message <- snapshot()$warning
    if (!is.null(message)) shiny::div(class = "text-warning", role = "alert", message)
  })
  output$figure_preview <- shiny::renderUI({
    shiny::req(snapshot()$size)
    NicoShinyAppTemplate::content_plot_ui("figure_plot", snapshot()$size)
  })
  output$figure_plot <- shiny::renderPlot({
    shiny::req(snapshot()$plot)
    NicoShinyAppTemplate::content_sized_plot(snapshot()$plot, snapshot()$size)
  }, width = function() { shiny::req(snapshot()$size); snapshot()$size$width },
     height = function() { shiny::req(snapshot()$size); snapshot()$size$height }, res = 96)
}
shiny::shinyApp(ui, server)
