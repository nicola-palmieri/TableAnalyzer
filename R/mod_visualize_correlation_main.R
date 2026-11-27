# mod_visualize_correlation: server coordination and reactive state.
visualize_ggpairs_server <- function(id, filtered_data, model_fit, reset_trigger = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    correlation_info <- reactive({
      info <- optional_plot_metadata(model_fit())
      if (is.null(info) || is.null(info$type) || info$type != "pairs") {
        return(NULL)
      }
      info
    })
    
    local_reset_token <- reactiveVal(NULL)
    color_reset_token <- reactive({
      local <- local_reset_token()
      external <- reset_trigger()
      if (is.null(local)) external else paste0(external %||% "initial", "_local_", local)
    })

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

    observeEvent(reset_trigger(), {
      token <- reset_trigger()
      if (is.null(token) || identical(token, last_reset_token())) return()
      if (is.null(correlation_info())) {
        pending_reset_token(token)
        return()
      }
      session$onFlushed(function() {
        apply_reset(token)

      }, once = TRUE)
    }, ignoreInit = TRUE)
    
    observeEvent(correlation_info(), {
      info <- correlation_info()
      if (is.null(info)) return()
      token <- pending_reset_token()
      last_token <- last_reset_token()
      session$onFlushed(function() {
        if (!is.null(token) && !identical(token, last_token)) {
          apply_reset(token)
        }

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
      reset_trigger = color_reset_token
    )

    observeEvent(input$reset, {
      updateSelectInput(session, "plot_type", selected = "GGPairs")
      token <- (isolate(local_reset_token()) %||% 0L) + 1L
      apply_reset(paste0("local_", token))
      local_reset_token(token)
      ggpairs_state$reset_grid()
    }, ignoreInit = TRUE)

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
        VetmeduniAppTemplate::content_plot_ui(ns("plot"), list(width = ggpairs_state$width(), height = ggpairs_state$height()))
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
        VetmeduniAppTemplate::save_plot_png(p, file,
          width_px = dims$width, height_px = dims$height, dpi = 300)
      }
    )
  })
}
