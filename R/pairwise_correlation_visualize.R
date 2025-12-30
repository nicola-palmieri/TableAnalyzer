# ===============================================================
# 🧪 Visualization Module — Pairwise Correlation (Dispatcher)
# ===============================================================

visualize_ggpairs_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      class = "ta-sidebar",
      width = 4,
      h4(class = "ta-sidebar-title", "Step 4 - Visualize pairwise correlation"),
      p(class = "ta-sidebar-subtitle", "Visualize pairwise relationships and correlation coefficients among numeric variables."),
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
      conditionalPanel(
        condition = sprintf("output['%s']", ns("show_controls")),
        pairwise_correlation_visualize_ggpairs_ui(ns("ggpairs"))
      ),
      conditionalPanel(
        condition = sprintf("output['%s']", ns("show_placeholder")),
        helpText("Run the pairwise correlation analysis to configure plots.")
      ),
      br(),
      fluidRow(
        column(6, actionButton(ns("apply_plot"), "Apply changes", width = "100%")),
        column(6, downloadButton(ns("download_plot"), "Download results", style = "width: 100%;"))
      )
    ),
    mainPanel(
      width = 8,
      h4("Plots"),
      uiOutput(ns("plot_container"))
    )
  )
}


visualize_ggpairs_server <- function(id, filtered_data, model_fit, reset_trigger = reactive(NULL)) {
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
    apply_reason <- reactiveVal("manual")
    apply_counter <- 0L
    bump_apply <- function() {
      apply_counter <<- apply_counter + 1L
      apply_id(apply_counter)
    }

    plot_width_id <- "ggpairs-plot_width"
    plot_height_id <- "ggpairs-plot_height"
    base_size_id <- "ggpairs-plot_base_size"

    last_reset_token <- reactiveVal(NULL)
    pending_reset_token <- reactiveVal(NULL)

    apply_reset <- function(token) {
      updateNumericInput(session, plot_width_id, value = 800)
      updateNumericInput(session, plot_height_id, value = 600)
      updateNumericInput(session, base_size_id, value = 11)
      last_reset_token(token)
      pending_reset_token(NULL)
    }

    schedule_apply <- function() {
      apply_reason("analysis")
      bump_apply()
    }

    observeEvent(reset_trigger(), {
      token <- reset_trigger()
      if (is.null(token) || identical(token, last_reset_token())) return()
      if (is.null(correlation_info())) {
        pending_reset_token(token)
        return()
      }
      session$onFlushed(function() {
        apply_reset(token)
        schedule_apply()
      }, once = TRUE)
    }, ignoreInit = TRUE)
    
    observeEvent(input$apply_plot, {
      apply_reason("manual")
      bump_apply()
    })
    
    observeEvent(correlation_info(), {
      info <- correlation_info()
      if (is.null(info)) return()
      token <- pending_reset_token()
      last_token <- last_reset_token()
      session$onFlushed(function() {
        if (!is.null(token) && !identical(token, last_token)) {
          apply_reset(token)
        }
        schedule_apply()
      }, once = TRUE)
    }, ignoreInit = FALSE)

    output$show_controls <- reactive({
      !is.null(correlation_info()) && identical(input$plot_type, "GGPairs")
    })

    output$show_placeholder <- reactive({
      is.null(correlation_info())
    })

    outputOptions(output, "show_controls", suspendWhenHidden = FALSE)
    outputOptions(output, "show_placeholder", suspendWhenHidden = FALSE)

    ggpairs_state <- pairwise_correlation_visualize_ggpairs_server(
      "ggpairs",
      filtered_data,
      correlation_info,
      apply_trigger = reactive(apply_id()),
      apply_reason = reactive(apply_reason()),
      reset_trigger = reset_trigger
    )

    output$plot_warning <- renderUI({
      warning_text <- ggpairs_state$warning()
      if (!is.null(warning_text)) {
        div(class = "alert alert-warning", HTML(warning_text))
      }
    })

    output$plot_container <- renderUI({
      plot_obj <- ggpairs_state$plot()
      warning_text <- ggpairs_state$warning()

      if (is.null(plot_obj)) {
        if (!is.null(warning_text)) {
          return(div(uiOutput(ns("plot_warning"))))
        }
        return(NULL)
      }

      tagList(
        uiOutput(ns("plot_warning")),
        plotOutput(ns("plot"), height = "auto")
      )
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
