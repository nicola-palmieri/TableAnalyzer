# ===============================================================
# 🏠 Table Analyzer — Home Module
# ===============================================================

home_ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    div(
      class = "home-wrapper px-3",
      div(
        class = "hero text-center mx-auto",
        div(
          class = "ta-logo",
          div(
            class = "ta-logo-mark",
            HTML(
              paste0(
                "<svg class=\"ta-logo-svg\" viewBox=\"0 0 120 120\" role=\"img\" aria-label=\"Table Analyzer logo\">",
                "<rect class=\"ta-logo-frame\" x=\"10\" y=\"10\" width=\"100\" height=\"100\" rx=\"18\"/>",
                "<path class=\"ta-logo-grid\" d=\"M30 40h60M30 60h60M30 80h60\"/>",
                "<path class=\"ta-logo-grid\" d=\"M40 30v60M60 30v60M80 30v60\"/>",
                "<path class=\"ta-logo-line\" d=\"M28 78L50 58L70 68L92 44\"/>",
                "<circle class=\"ta-logo-dot\" cx=\"92\" cy=\"44\" r=\"4\"/>",
                "</svg>"
              )
            )
          ),
          div(
            class = "ta-logo-wordmark",
            h1(class = "ta-logo-name", "Table Analyzer"),
            span(class = "ta-logo-tagline", "Modern statistics for scientific tables")
          )
        ),
        p(
          class = "lead text-muted",
          "Turn your tabular data into publication-ready tables and plots."
        ),
        br(),
        div(
          class = "home-steps",
          fluidRow(
            class = "g-4 justify-content-center",
            column(
              width = 3,
              div(
                icon("upload", class = "fa-2x text-primary mb-2"),
                h5("1. Upload"),
                p("Bring in spreadsheets with ease.")
              )
            ),
            column(
              width = 3,
              div(
                icon("filter", class = "fa-2x text-primary mb-2"),
                h5("2. Filter"),
                p("Refine rows and columns to spotlight what's important.")
              )
            ),
            column(
              width = 3,
              div(
                icon("square-poll-horizontal", class = "fa-2x text-primary mb-2"),
                h5("3. Analyze"),
                p("Run summaries and models tailored to your dataset.")
              )
            ),
            column(
              width = 3,
              div(
                icon("chart-area", class = "fa-2x text-primary mb-2"),
                h5("4. Visualize"),
                p("Create polished plots to communicate key findings.")
              )
            )
          )
        ),
        br(),
        tags$hr(class = "my-4"),
        p(
          tagList(
            em("Developed by Nicola Palmieri"),
            br(),
            span("Version v1.01", style = "color:#6c757d; font-size:0.9em;")
          ),
          class = "text-muted small"
        )
      )
    )
  )
}


home_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    # Placeholder for future home page interactivity
  })
}
