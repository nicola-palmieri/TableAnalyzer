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
              div(
                class = "home-title-row",
                h1("Table Analyzer"),
                span(class = "home-version", "v1.10")
              ),
              p(
                class = "home-simple-subtitle",
                "Turn your tabular data into publication-ready summary tables and plots"
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
        )
      ),
      p(class = "home-developer", "Developed by Nicola Palmieri")
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
