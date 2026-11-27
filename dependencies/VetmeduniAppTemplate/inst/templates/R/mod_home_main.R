mod_home_server <- function(id, navigate) {
  shiny::moduleServer(id, function(input, output, session) {
    shiny::observeEvent(input$start, navigate("explore"))
  })
}
