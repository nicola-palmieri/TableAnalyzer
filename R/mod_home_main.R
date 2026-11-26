# mod_home: server coordination and reactive state.
mod_home_server <- function(id, has_data = shiny::reactive(FALSE)) {
  shiny::moduleServer(id, function(input, output, session) {
    navigate <- function(page) {
      bslib::nav_select("main_nav", selected = page, session = session$rootScope())
    }
    shiny::observeEvent(input$go_upload, navigate("upload_tab"))
  })
}

# Compatibility alias for existing callers.
home_server <- mod_home_server
