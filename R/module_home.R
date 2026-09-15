# ===============================================================
# 🏠 Table Analyzer — Home Module
# ===============================================================

home_ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    div(
      class = "home-simple-wrapper px-3",
      div(
        class = "hero home-simple mx-auto",
        div(
          class = "home-simple-header",
          tags$img(
            src = "logo.jpeg",
            class = "home-simple-logo",
            alt = "Table Analyzer logo"
          ),
          div(
            h1("Table Analyzer"),
            p(
              class = "home-simple-subtitle",
              "Statistical analysis for scientific tables, without spreadsheet chaos."
            )
          )
        ),
        p(
          class = "home-simple-intro",
          "Table Analyzer helps you upload Excel data, run analysis modules, and export publication-ready tables and plots in one guided workflow."
        ),
        div(
          class = "home-simple-cta",
          actionButton(
            ns("go_upload"),
            "Go to Upload",
            class = "btn btn-primary btn-lg"
          ),
          span(
            class = "home-simple-cta-note",
            "Start with your own workbook or the built-in example dataset."
          )
        ),
        div(
          class = "row g-3 home-simple-grid",
          div(
            class = "col-md-4",
            div(
              class = "home-simple-card",
              h5("What it is"),
              p("A browser-based app that runs statistical analyses on tabular data and creates the associated plots for reporting.")
            )
          ),
          div(
            class = "col-md-4",
            div(
              class = "home-simple-card",
              h5("Key advantages"),
              tags$ul(
                tags$li("Simple, guided interface that reduces setup time."),
                tags$li("Quick, easy plots and summaries without manual formulas or spreadsheet copy-paste."),
                tags$li("Supports multiple response variables, optional stratified analyses, and methods based on modern statistics.")
              )
            )
          ),
          div(
            class = "col-md-4",
            div(
              class = "home-simple-card",
              h5("Workflow"),
              tags$ol(
                tags$li(tags$b("Upload"), " data."),
                tags$li(tags$b("Filter"), " rows or variables."),
                tags$li(tags$b("Analyze"), " by choosing among 7 powerful analysis modules."),
                tags$li(tags$b("Visualize"), " and export.")
              )
            )
          )
        ),
        p(
          tagList(
            em("Developed by Nicola Palmieri"),
            br(),
            span("Version v1.10", style = "font-size:0.9em;")
          ),
          class = "text-muted small home-simple-footer mb-0"
        )
      )
    )
  )
}


home_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    observeEvent(input$go_upload, {
      updateNavbarPage(
        session = session$rootScope(),
        inputId = "main_nav",
        selected = "upload_tab"
      )
    })
  })
}
