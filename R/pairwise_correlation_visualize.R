# ===============================================================
# 🧪 Visualization Module — Pairwise Correlation (Dispatcher)
# ===============================================================

visualize_ggpairs_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("Step 4 — Visualize pairwise correlation"),
      p("Visualize pairwise relationships and correlation coefficients among numeric variables."),
      hr(),
      with_help_tooltip(
        selectInput(
          ns("plot_type"),
          label = "Select visualization type",
          choices = c("Pairwise scatterplot matrix" = "GGPairs"),
          selected = "GGPairs"
        ),
        "Choose how to visualise the pairwise relationships between variables."
      ),
      uiOutput(ns("sub_controls")),
      br(),
      fluidRow(
        column(6, actionButton(ns("apply_plot"), "Apply changes", width = "100%")),
        column(6, downloadButton(ns("download_plot"), "Download Plot", style = "width: 100%;"))
      )
    ),
    mainPanel(
      width = 8,
      h4("Plots"),
      uiOutput(ns("plot_warning")),
      plotOutput(ns("plot"), height = "auto")
    )
  )
}


visualize_ggpairs_server <- function(id, filtered_data, model_fit) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    correlation_info <- reactive({
      info <- model_fit()
      if (is.null(info) || is.null(info$type) || info$type != "pairs") {
        return(NULL)
      }
      info
    })
    
    apply_id <- reactiveVal(0L)
    apply_counter <- 0L
    bump_apply <- function() {
      apply_counter <<- apply_counter + 1L
      apply_id(apply_counter)
    }
    
    observeEvent(input$apply_plot, {
      bump_apply()
    })
    
    observeEvent(correlation_info(), {
      session$onFlushed(function() {
        bump_apply()
      }, once = TRUE)
    }, ignoreInit = FALSE)

    output$sub_controls <- renderUI({
      info <- correlation_info()
      if (is.null(info)) {
        helpText("Run the pairwise correlation analysis to configure plots.")
      } else if (identical(input$plot_type, "GGPairs")) {
        pairwise_correlation_visualize_ggpairs_ui(ns("ggpairs"))
      }
    })

    ggpairs_state <- pairwise_correlation_visualize_ggpairs_server(
      "ggpairs",
      filtered_data,
      correlation_info,
      apply_trigger = reactive(apply_id())
    )

    output$plot_warning <- renderUI({
      warning_text <- ggpairs_state$warning()
      if (!is.null(warning_text)) {
        div(class = "alert alert-warning", HTML(warning_text))
      }
    })

    output$plot <- renderPlot({
      plot_obj <- ggpairs_state$plot()
      validate(need(!is.null(plot_obj), "No plot available."))
      print(plot_obj)
    },
    width = function() ggpairs_state$width(),
    height = function() ggpairs_state$height(),
    res = 96)

    output$download_plot <- downloadHandler(
      filename = function() paste0("pairwise_correlation_ggpairs_", Sys.Date(), ".png"),
      content = function(file) {
        p <- ggpairs_state$plot()
        dims <- ggpairs_state$dimensions()
        validate(need(!is.null(p), "No plot"))
        validate(need(!is.null(dims), "No layout"))
        ggplot2::ggsave(
          file, p, dpi = 300,
          width = dims$width / 96,
          height = dims$height / 96,
          units = "in",
          limitsize = FALSE
        )
      }
    )
  })
}
