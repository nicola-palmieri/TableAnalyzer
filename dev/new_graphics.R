library(shiny)
library(bslib)
library(bsicons)
library(DT)

theme <- bs_theme(
  version = 5,
  bg = "#070B14",
  fg = "#E5E7EB",
  primary = "#3B82F6",
  base_font = font_google("Inter"),
  heading_font = font_google("Inter"),
  border_radius = "0.9rem"
)

stepper_ui <- function(active = 1) {
  steps <- c("Upload", "Filter", "Analyze", "Visualize")
  tags$div(
    class = "ta-stepper",
    lapply(seq_along(steps), function(i) {
      cls <- if (i == active) "ta-step ta-active" else "ta-step"
      tags$div(class = cls,
               tags$div(class = "ta-dot", i),
               tags$div(class = "ta-label", steps[i])
      )
    })
  )
}

ui <- tagList(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "theme2.css")
  ),
  
  page_navbar(
    title = tagList(bs_icon("table"), " Table Analyzer"),
    theme = theme,
    bg = "#0B1220",
    inverse = TRUE,
    
    nav_panel(
      "Home",
      div(class = "ta-hero",
          stepper_ui(active = 1),
          tags$h1("Table Analyzer"),
          tags$p("Upload, filter, analyze and export plots in minutes."),
          actionButton("go_upload", "Start (Upload)", class = "btn-primary ta-cta"),
          div(class = "ta-cards",
              div(class = "ta-card",
                  bs_icon("upload"), tags$h3("Upload"), tags$p("Excel/CSV + preview")),
              div(class = "ta-card",
                  bs_icon("funnel"), tags$h3("Filter"), tags$p("Focus only on what matters")),
              div(class = "ta-card",
                  bs_icon("bar-chart"), tags$h3("Analyze"), tags$p("Summaries + tests")),
              div(class = "ta-card",
                  bs_icon("pie-chart"), tags$h3("Visualize"), tags$p("Clean plots + download"))
          )
      )
    ),
    
    nav_panel(
      "Upload",
      div(class="ta-page",
          stepper_ui(active = 1),
          layout_sidebar(
            sidebar = sidebar(
              width = 320,
              div(class="ta-glass",
                  tags$h4("Step 1 — Upload"),
                  fileInput("file", "Upload Excel/CSV"),
                  radioButtons("format", "Format", c("Long (tidy)"="long", "Wide"="wide")),
                  actionButton("load_demo", "Use example dataset", class="btn-outline-primary w-100")
              )
            ),
            div(class="ta-glass",
                div(class="ta-header-row",
                    tags$h4("Data preview"),
                    div(class="ta-badges",
                        div(class="ta-badge", "Rows: ", textOutput("n_rows", inline=TRUE)),
                        div(class="ta-badge", "Cols: ", textOutput("n_cols", inline=TRUE)),
                        div(class="ta-badge", "Missing: ", textOutput("n_na", inline=TRUE))
                    )
                ),
                DTOutput("preview")
            )
          )
      )
    ),
    
    nav_panel(
      "Filter",
      div(class="ta-page",
          stepper_ui(active = 2),
          layout_sidebar(
            sidebar = sidebar(
              width = 320,
              div(class="ta-glass",
                  tags$h4("Step 2 — Filter"),
                  uiOutput("filter_controls"),
                  actionButton("apply_filter", "Apply filters", class="btn-primary w-100")
              )
            ),
            div(class="ta-glass",
                tags$h4("Filtered preview"),
                DTOutput("filtered_preview")
            )
          )
      )
    ),
    
    nav_panel(
      "Analyze",
      div(class="ta-page",
          stepper_ui(active = 3),
          layout_sidebar(
            sidebar = sidebar(
              width = 320,
              div(class="ta-glass",
                  tags$h4("Step 3 — Analyze"),
                  selectInput("analysis_type", "Analysis type",
                              c("One-way ANOVA"="anova1", "Summary"="summary")),
                  actionButton("run_analysis", "Run analysis", class="btn-primary w-100")
              )
            ),
            div(class="ta-glass",
                tags$h4("Results"),
                uiOutput("analysis_ui")  # qui gestiamo empty state + risultati
            )
          )
      )
    ),
    
    nav_panel(
      "Visualize",
      div(class="ta-page",
          stepper_ui(active = 4),
          layout_sidebar(
            sidebar = sidebar(
              width = 320,
              div(class="ta-glass",
                  tags$h4("Step 4 — Visualize"),
                  selectInput("plot_type", "Plot type", c("Barplot (mean ± SE)"="bar")),
                  numericInput("w", "Width (px)", 420),
                  numericInput("h", "Height (px)", 320),
                  actionButton("make_plot", "Generate plots", class="btn-primary w-100"),
                  downloadButton("dl_plot", "Download", class="btn-outline-primary w-100")
              )
            ),
            div(class="ta-glass",
                tags$h4("Plots"),
                uiOutput("plots_ui")
            )
          )
      )
    )
  )
)

server <- function(input, output, session) {
  
  observeEvent(input$go_upload, {
    updateNavbarPage(session, "Upload")
  })
  
  # demo finto: collega qui il tuo dataframe vero
  data <- reactiveVal(mtcars)
  
  output$n_rows <- renderText(nrow(data()))
  output$n_cols <- renderText(ncol(data()))
  output$n_na   <- renderText(sum(is.na(data())))
  
  output$preview <- renderDT({
    datatable(data(), options = list(pageLength = 10), class = "display compact")
  })
  
  output$filter_controls <- renderUI({
    div(class="ta-empty",
        "Qui metti i controlli filtro veri (select, slider, ecc).")
  })
  
  output$filtered_preview <- renderDT({
    datatable(data(), options = list(pageLength = 10), class="display compact")
  })
  
  output$analysis_ui <- renderUI({
    req(input$analysis_type)
    div(class="ta-empty",
        "Risultati: qui stampa tabelle/testi. Niente pagina bianca.")
  })
  
  output$plots_ui <- renderUI({
    div(class="ta-empty",
        "Qui metti la griglia dei plot (plotOutput o imageOutput).")
  })
}

shinyApp(ui, server)