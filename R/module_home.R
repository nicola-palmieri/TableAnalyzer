# ===============================================================
# 🏠 Table Analyzer — Home Module
# ===============================================================

home_ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    div(
      class = "home-wrapper px-3",
      div(
        class = "hero home-hero mx-auto",
        div(
          class = "row gy-4 align-items-center",
          div(
            class = "col-lg-6",
            div(
              class = "ta-logo",
              div(
                class = "ta-logo-mark",
                tags$img(
                  src = "logo.jpeg",
                  class = "ta-logo-img",
                  alt = "Table Analyzer logo"
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
            div(
              class = "home-chip-row",
              span(class = "home-chip", "Auto means & SD"),
              span(class = "home-chip", "Multi-response ready"),
              span(class = "home-chip", "Stratified outputs"),
              span(class = "home-chip", "Consistent formatting")
            ),
            tags$ul(
              class = "home-checklist",
              tags$li("Automatic group means, standard deviations, and summaries — no manual spreadsheet formulas."),
              tags$li("One workflow from filtered data to model output, without copy-paste."),
              tags$li("Exports that keep tables and plots consistent across analyses.")
            )
          ),
          div(
            class = "col-lg-6",
            div(
              class = "home-panel home-panel-steps",
              h5("Workflow at a glance"),
              div(
                class = "row g-3",
                div(
                  class = "col-6",
                  div(
                    class = "home-step-card",
                    icon("upload", class = "fa-lg text-primary mb-2"),
                    h6("Upload"),
                    p("Bring in datasets and auto-detect variable types.")
                  )
                ),
                div(
                  class = "col-6",
                  div(
                    class = "home-step-card",
                    icon("filter", class = "fa-lg text-primary mb-2"),
                    h6("Filter"),
                    p("Narrow down the rows you analyze with flexible filtering.")
                  )
                ),
                div(
                  class = "col-6",
                  div(
                    class = "home-step-card",
                    icon("square-poll-horizontal", class = "fa-lg text-primary mb-2"),
                    h6("Analyze"),
                    p("Run your models with guided validation and diagnostics.")
                  )
                ),
                div(
                  class = "col-6",
                  div(
                    class = "home-step-card",
                    icon("chart-area", class = "fa-lg text-primary mb-2"),
                    h6("Visualize"),
                    p("Generate polished plots ready for reporting.")
                  )
                )
              )
            )
          )
        ),
        tags$hr(class = "my-4"),
        div(
          class = "row g-3 home-feature-row",
          div(
            class = "col-md-4",
            div(
              class = "home-feature-card",
              h6("Built-in quality checks"),
              p("Catch invalid inputs early so outputs are dependable.")
            )
          ),
          div(
            class = "col-md-4",
            div(
              class = "home-feature-card",
              h6("Simultaneous multivariate reporting"),
              p("Handle multiple responses and stratified reporting in one run.")
            )
          ),
          div(
            class = "col-md-4",
            div(
              class = "home-feature-card",
              h6("Ready-to-share outputs"),
              p("Publication-ready plots and tables, exportable with one click.")
            )
          )
        ),
        tags$hr(class = "my-4"),
        p(
          tagList(
            em("Developed by Nicola Palmieri"),
            br(),
            span("Version v1.10", style = "color:#6c757d; font-size:0.9em;")
          ),
          class = "text-muted small mb-0"
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
