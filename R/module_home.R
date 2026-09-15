# ===============================================================
# 🏠 Table Analyzer — Home Module
# ===============================================================

home_ui <- function(id) {
  ns <- NS(id)

  fluidPage(
    div(
      class = "home-v2-wrapper px-3",
      div(
        class = "hero home-v2 mx-auto",
        div(
          class = "row home-v2-hero",
          div(
            class = "col-lg-6 home-v2-copy",
            span(class = "ta-eyebrow", "From Excel to evidence"),
            h1("One table. Every outcome. Publication-ready."),
            p(
              class = "home-v2-lead",
              "Upload one scientific table, repeat validated analyses across outcomes or groups, and export consistent results without spreadsheet copy-paste."
            ),
            div(
              class = "home-v2-actions",
              actionButton(
                ns("try_demo"),
                "Try the 30-second demo",
                icon = icon("play"),
                class = "btn btn-primary btn-lg"
              ),
              actionButton(
                ns("go_upload"),
                "Use my own data",
                class = "btn btn-default btn-lg"
              )
            ),
            div(
              class = "home-v2-trust",
              span(icon("circle-check"), " No account required"),
              span(icon("flask"), " Built-in example")
            )
          ),
          div(
            class = "col-lg-6 home-v2-preview",
            tags$figure(
              tags$img(
                src = "demo-preview.png",
                alt = "Three-panel example showing treatment means and standard errors for cortisol, glucose, and heart rate"
              ),
              tags$figcaption(
                tags$b("One analysis, three outcomes."),
                " The demo produces this composite treatment comparison from the bundled data."
              )
            )
          )
        ),
        div(
          class = "row home-v2-capabilities",
          div(
            class = "col-md-4",
            div(
              class = "home-v2-capability",
              span("01"),
              h4("Multiple outcomes"),
              p("Run the same model across several response variables with explicit formulas and factor ordering.")
            )
          ),
          div(
            class = "col-md-4",
            div(
              class = "home-v2-capability",
              span("02"),
              h4("Stratified analysis"),
              p("Repeat the complete analysis independently across selected experimental or biological groups.")
            )
          ),
          div(
            class = "col-md-4",
            div(
              class = "home-v2-capability",
              span("03"),
              h4("Consistent exports"),
              p("Download statistical tables, diagnostics, and configurable multi-panel figures for reporting.")
            )
          )
        ),
        div(
          class = "home-v2-privacy",
          div(class = "home-v2-privacy-icon", icon("shield-halved")),
          div(
            h4("Your scientific data stays out of analytics"),
            p(
              "Workbooks are processed only for the current app session and Table Analyzer does not write them to a permanent database. Anonymous usage logs contain only session started, example loaded, analysis run, and plot downloaded events. They never contain filenames, column names, table values, formulas, or results."
            )
          )
        ),
        div(
          class = "home-v2-footer",
          span("Developed by Nicola Palmieri · Version v1.10"),
          span(
            tags$a(
              "Live app",
              href = "https://nicola-palmieri.shinyapps.io/tableanalyzer/",
              target = "_blank",
              rel = "noopener noreferrer"
            ),
            " · ",
            tags$a(
              "DOI 10.5281/zenodo.19233119",
              href = "https://doi.org/10.5281/zenodo.19233119",
              target = "_blank",
              rel = "noopener noreferrer"
            )
          )
        )
      )
    )
  )
}


home_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    demo_requested <- reactiveVal(0L)

    observeEvent(input$go_upload, {
      updateNavbarPage(
        session = session$rootScope(),
        inputId = "main_nav",
        selected = "data_tab"
      )
      updateTabsetPanel(
        session = session$rootScope(),
        inputId = "data_views",
        selected = "upload_view"
      )
    })

    observeEvent(input$try_demo, {
      demo_requested(demo_requested() + 1L)
    })

    reactive(demo_requested())
  })
}
