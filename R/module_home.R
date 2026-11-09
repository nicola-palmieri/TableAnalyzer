# ===============================================================
# 🏠 Table Analyzer — Home Module
# ===============================================================

home_ui <- function(id) {
  ns <- NS(id)
  preview <- function(src, label, description) {
    tags$figure(
      class = "home-feature-preview",
      tags$a(
        href = src, target = "_blank", rel = "noopener",
        `aria-label` = paste("Open", label, "at full size"),
        tags$img(src = src, alt = description, loading = "lazy")
      ),
      tags$figcaption(paste("Example", label, sep = " · "))
    )
  }

  fluidPage(
    div(
      class = "home-simple-wrapper px-3",
      div(
        class = "hero home-simple mx-auto",
        div(
          class = "home-banner",
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
          )
        ),
        div(
          class = "row g-3 home-simple-grid",
          div(
            class = "col-md-4",
            div(
              class = "home-simple-card",
              preview(
                "preview-summary.png", "Descriptive summary excerpt",
                "Descriptive summary of cortisol, glucose, and heart rate from the bundled example data."
              ),
              h5("Understand your data"),
              p("Clear summaries of distributions, variability, and missing values. Start with the shape of your data.")
            )
          ),
          div(
            class = "col-md-4",
            div(
              class = "home-simple-card",
              preview(
                "preview-anova.png", "ANOVA: mean ± SE",
                "The app's ANOVA barplot of cortisol by treatment, with standard errors and post-hoc significance annotations."
              ),
              h5("Find the relationships"),
              p("Compare groups and fit models with ANOVA, regression, mixed models, correlations, and PCA.")
            )
          ),
          div(
            class = "col-md-4",
            div(
              class = "home-simple-card",
              preview(
                "preview-pca.png", "PCA by treatment",
                "The app's PCA plot of the bundled numeric measurements, with points colored by treatment."
              ),
              h5("Make the results clear"),
              p("Explore patterns with tailored plots. Export publication-ready figures and formatted model reports.")
            )
          )
        ),
        div(
          class = "home-workflow",
          span(class = "home-workflow-label", "A clear path through your analysis"),
          lapply(seq_len(4), function(i) {
            tagList(
              span(
                class = "home-workflow-step",
                span(class = "home-workflow-number", sprintf("%02d", i)),
                c("Upload", "Filter", "Analyze", "Visualize")[i]
              ),
              if (i < 4) span(class = "home-workflow-arrow", `aria-hidden` = "true", HTML("&rarr;"))
            )
          })
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
