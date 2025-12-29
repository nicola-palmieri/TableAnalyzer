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
              "Turn tabular datasets into publication-ready summaries, models, and visuals in minutes."
            ),
            div(
              class = "home-chip-row",
              span(class = "home-chip", "Type III ANOVA"),
              span(class = "home-chip", "LM/LMM"),
              span(class = "home-chip", "Export-ready DOCX"),
              span(class = "home-chip", "Diagnostics")
            ),
            tags$ul(
              class = "home-checklist",
              tags$li("Curate filters and strata without leaving the interface."),
              tags$li("Keep formulas and outputs aligned with validation guardrails."),
              tags$li("Export tables and plots with consistent formatting.")
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
                    p("Bring in spreadsheets and auto-detect types.")
                  )
                ),
                div(
                  class = "col-6",
                  div(
                    class = "home-step-card",
                    icon("filter", class = "fa-lg text-primary mb-2"),
                    h6("Filter"),
                    p("Focus on the rows and columns that matter.")
                  )
                ),
                div(
                  class = "col-6",
                  div(
                    class = "home-step-card",
                    icon("square-poll-horizontal", class = "fa-lg text-primary mb-2"),
                    h6("Analyze"),
                    p("Run ANOVA, LM, or LMM in a guided flow.")
                  )
                ),
                div(
                  class = "col-6",
                  div(
                    class = "home-step-card",
                    icon("chart-area", class = "fa-lg text-primary mb-2"),
                    h6("Visualize"),
                    p("Generate polished plots ready for reports.")
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
              h6("Validation-first workflow"),
              p("Surface issues early with consistent checks across analyses.")
            )
          ),
          div(
            class = "col-md-4",
            div(
              class = "home-feature-card",
              h6("Consistent formatting"),
              p("Aligned decimals and p-value styling across verbatim and DOCX outputs.")
            )
          ),
          div(
            class = "col-md-4",
            div(
              class = "home-feature-card",
              h6("Built for scientific tables"),
              p("Designed for structured experimental data and stratified reporting.")
            )
          )
        ),
        tags$hr(class = "my-4"),
        p(
          tagList(
            em("Developed by Nicola Palmieri"),
            br(),
            span("Version v1.01", style = "color:#6c757d; font-size:0.9em;")
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
