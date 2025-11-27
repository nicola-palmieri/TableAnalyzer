# ===============================================================
# Visualization Module — Descriptive Statistics (Dispatcher)
# ===============================================================

visualize_descriptive_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("Step 5 — Visualize descriptive statistics"),
      p("Explore distributions, variability, and normality across variables."),
      hr(),
      with_help_tooltip(
        selectInput(
          ns("plot_type"),
          label = "Select visualization type",
          choices = c(
            "Categorical barplots" = "categorical",
            "Numeric boxplots"     = "boxplots",
            "Numeric histograms"   = "histograms"
          ),
          selected = "categorical"
        ),
        "Choose the descriptive chart that best answers your question."
      ),
      uiOutput(ns("sub_controls"))
    ),
    mainPanel(
      width = 8,
      h4("Plots"),
      uiOutput(ns("plot_ui"))
    )
  )
}



visualize_descriptive_server <- function(id, filtered_data, descriptive_summary) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # Active plot type name (UI only)
    active_type <- reactive({
      req(input$plot_type)
      input$plot_type
    })
    
    # Mount submodules ONCE
    # They no longer depend on "is_active"
    visualize_categorical_barplots_server("categorical", filtered_data, descriptive_summary)
    visualize_numeric_boxplots_server("boxplots", filtered_data, descriptive_summary)
    visualize_numeric_histograms_server("histograms", filtered_data, descriptive_summary)
    
    # ---- SUB-CONTROLS UI ----
    output$sub_controls <- renderUI({
      tagList(
        conditionalPanel(
          condition = sprintf("input['%s'] == 'categorical'", ns("plot_type")),
          visualize_categorical_barplots_ui(ns("categorical"))
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'boxplots'", ns("plot_type")),
          visualize_numeric_boxplots_ui(ns("boxplots"))
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'histograms'", ns("plot_type")),
          visualize_numeric_histograms_ui(ns("histograms"))
        )
      )
    })
    
    # ---- PLOT AREA UI ----
    output$plot_ui <- renderUI({
      tagList(
        conditionalPanel(
          condition = sprintf("input['%s'] == 'categorical'", ns("plot_type")),
          visualize_categorical_barplots_plot_ui(ns("categorical"))
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'boxplots'", ns("plot_type")),
          visualize_numeric_boxplots_plot_ui(ns("boxplots"))
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] == 'histograms'", ns("plot_type")),
          visualize_numeric_histograms_plot_ui(ns("histograms"))
        )
      )
    })
  })
}
