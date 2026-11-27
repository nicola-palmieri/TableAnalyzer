library(shiny)
library(bslib)
library(VetmeduniAppTemplate)

ui <- app_navbar(
  nav_panel("Home", value = "home", app_home_ui("home", main = "Component", accent = "Gallery",
    description = "The same presentation components support different analysis applications.",
    analyses = list(list(key = "controls", title = "Controls and figures",
      description = "Inspect shared expanders, contextual help, colours and figure dimensions.",
      symbol = "boxplot")),
    workflow = list(list(title = "Configure", description = "Supply your app's branding and copy."),
      list(title = "Analyze", description = "Keep scientific calculations in your app.")))),
  nav_panel("Controls", value = "controls", app_page_ui(app_layout(
    app_sidebar("Figure controls", "Shared appearance, application-owned analysis",
      app_expander("Example control", open = TRUE,
        app_help_ui(selectInput("variable", "Variable", c(Cylinders = "cyl", Gears = "gear")),
          "This explanation comes from the consuming app.")),
      mod_export_controls_ui("figure"),
      app_expander("Colors", app_color_dropdown(NS("figure")))),
    app_result_header_ui("A shared figure theme", mod_export_actions_ui("figure")),
    uiOutput("figure_warning"), uiOutput("figure_preview")))),
  main = "Component", accent = "Gallery", id = "page")

server <- function(input, output, session) {
  observeEvent(input$go_home, nav_select("page", "home"))
  observeEvent(input$`home-upload`, nav_select("page", "controls"))
  # Preview, dimensions and export all consume the same complete valid snapshot.
  settings <- mod_export_server("figure",
    plot = reactive({ req(snapshot()$plot); snapshot()$plot }),
    filename = "gallery_boxplot", size = reactive({ req(snapshot()$size); snapshot()$size }))
  snapshot <- app_plot_snapshot(function() {
    controls <- settings()
    req(input$variable)
    plot_data <- data.frame(group = factor(mtcars[[input$variable]]), mpg = mtcars$mpg)
    color <- input[["figure-color_choice"]]
    if (is.null(color)) color <- "#4682B4"
    plot <- ggplot2::ggplot(plot_data, ggplot2::aes(group, mpg)) +
      ggplot2::geom_boxplot(fill = color) + plot_theme(kind = "box") +
      ggplot2::labs(x = if (input$variable == "cyl") "Cylinders" else "Gears", y = "Miles per gallon")
    list(plot = plot, size = content_plot_size(plot, "box", controls$width, controls$height))
  })
  output$figure_warning <- renderUI({
    message <- snapshot()$warning
    if (!is.null(message)) div(class = "text-warning", role = "alert", message)
  })
  output$figure_preview <- renderUI({
    req(snapshot()$size)
    content_plot_ui("figure_plot", snapshot()$size)
  })
  output$figure_plot <- renderPlot({
    req(snapshot()$plot)
    content_sized_plot(snapshot()$plot, snapshot()$size)
  }, width = function() { req(snapshot()$size); snapshot()$size$width },
     height = function() { req(snapshot()$size); snapshot()$size$height }, res = 96)
}
shinyApp(ui, server)
