# mod_visualize_histograms: server coordination and reactive state.
visualize_numeric_histograms_server <- function(id, filtered_data, summary_info, is_active = NULL) {
  moduleServer(id, function(input, output, session) {
    
    ns <- session$ns
    
    # ================================================================
    # Stored state (same logic as the ANOVA / barplots / boxplots)
    # ================================================================
    stored <- reactiveValues(
      plot = NULL,
      warning = NULL,
      layout = NULL,
      empty_message = NULL,
      plot_width  = 400,
      plot_height = 300
    )
    
    df <- reactive(filtered_data())
    
    grid <- single_grid_settings(input, session, "plot_grid")
    base_size <- base_size_server(input, default = NicoShinyAppTemplate::PLOT_BASE_SIZE)
    
    # ================================================================
    # Color grouping
    # ================================================================
    color_var <- reactive({
      info <- optional_plot_metadata(summary_info())
      g <- optional_plot_metadata(resolve_reactive(info$group_var))
      dat <- optional_plot_metadata(df())
      if (is.null(g) || is.null(dat) || !g %in% names(dat)) return(NULL)
      g
    })
    
    local_reset_token <- reactiveVal(NULL)

    custom_colors <- add_color_customization_server(
      ns, input, output, df, color_var, multi_group = TRUE
    ,
      reset_token = local_reset_token
    )
    
    # ================================================================
    # Common legend
    # ================================================================
    legend_state <- reactiveValues(enabled = FALSE, position = "bottom")
    
    observeEvent(input$use_common_legend, {
      legend_state$enabled <- isTRUE(input$use_common_legend)
    })
    
    observeEvent(input$common_legend_position, {
      legend_state$position <- input$common_legend_position
    })
    
    output$common_legend_controls <- renderUI({
      if (is.null(color_var())) return(NULL)
      
      checkbox <- with_help_tooltip(
        checkboxInput(
          ns("use_common_legend"),
          "Use common legend",
          value = legend_state$enabled
        ),
        "Merge color legends across histogram panels."
      )
      
      pos <- NULL
      if (legend_state$enabled) {
        pos <- with_help_tooltip(
          selectInput(
            ns("common_legend_position"),
            "Legend position",
            c("Bottom"="bottom","Right"="right","Top"="top","Left"="left"),
            selected = legend_state$position
          ),
          "Choose placement for the combined legend."
        )
      }
      
      tagList(checkbox, pos)
    })
    keep_optional_ui_current(output, "common_legend_controls")
    
    # ================================================================
    # Automatic appearance snapshots
    # ================================================================
    compute_plot <- function(allow_reset = FALSE) {
      plot_width <- NicoShinyAppTemplate::app_plot_numeric(input$plot_width, 400,
        min = 200, max = 2000, label = "Plot width")
      plot_height <- NicoShinyAppTemplate::app_plot_numeric(input$plot_height, 300,
        min = 200, max = 2000, label = "Plot height")
      NicoShinyAppTemplate::app_plot_numeric(input$plot_base_size, NicoShinyAppTemplate::PLOT_BASE_SIZE, min = 6, max = 30, label = "Base size")
      
      data <- df()
      info <- summary_info()
      
      if (is.null(data) || is.null(info) || nrow(data) == 0) {
        return(list(plot = NULL, warning = "No data available."))
      }
      
      s_vars <- resolve_reactive(info$selected_vars)
      g_var  <- resolve_reactive(info$group_var)
      strata_levels <- resolve_reactive(info$strata_levels)
      
      processed <- resolve_reactive(info$processed_data)
      dat <- if (!is.null(processed)) processed else data

      num_cols <- names(dat)[vapply(dat, is.numeric, logical(1))]
      selected_num_cols <- intersect(num_cols, s_vars)
      if (length(selected_num_cols) == 0) {
        return(list(plot = NULL, warning = NULL, empty_message = "Select at least one numeric variable in the Descriptive tab to display numeric histograms."))
      }
      
      build_plot <- function(rows, cols) {
        build_descriptive_numeric_histogram(
          df = dat,
          selected_vars = s_vars,
          group_var = g_var,
          strata_levels = strata_levels,
          use_density = input$use_density,
          nrow_input = rows,
          ncol_input = cols,
          custom_colors = custom_colors(),
          base_size = base_size(),
          common_legend = legend_state$enabled,
          legend_position = if (legend_state$enabled) legend_state$position else NULL,
          panel_width = plot_width, panel_height = plot_height
        )
      }

      is_grid_warning <- function(msg) {
        !is.null(msg) && grepl("Grid", msg, fixed = TRUE)
      }

      panel_data <- dat
      if (!is.null(g_var) && g_var %in% names(panel_data) && length(strata_levels)) {
        group_values <- as.character(panel_data[[g_var]])
        group_values[is.na(group_values) | trimws(group_values) == ""] <- "Missing"
        panel_data <- panel_data[group_values %in% strata_levels, , drop = FALSE]
      }
      n_panels <- sum(vapply(panel_data[selected_num_cols], function(x) any(is.finite(x)), logical(1)))
      layout <- grid$resolve(n_panels, plot_width, plot_height)
      res <- build_plot(layout$rows, layout$cols)
      if (isTRUE(allow_reset) && !grid$is_manual() && !is.null(res) && is_grid_warning(res$warning)) {
        layout <- grid$resolve(res$panels, plot_width, plot_height)
        res <- build_plot(layout$rows, layout$cols)
      }

      if (is_grid_warning(res$warning)) res$plot <- NULL
      res$plot_width <- plot_width
      res$plot_height <- plot_height
      res$empty_message <- NULL
      res
    }

    reset_plot_inputs <- function() {
      updateNumericInput(session, "plot_width", value = 400)
      updateNumericInput(session, "plot_height", value = 300)
      updateNumericInput(session, "plot_base_size", value = NicoShinyAppTemplate::PLOT_BASE_SIZE)
      grid$reset()
      updateCheckboxInput(session, "use_density", value = FALSE)
      legend_state$enabled <- FALSE
      legend_state$position <- "bottom"
      updateCheckboxInput(session, "use_common_legend", value = FALSE)
      updateSelectInput(session, "common_legend_position", selected = "bottom")
      local_reset_token((isolate(local_reset_token()) %||% 0L) + 1L)
    }
    observeEvent(input$reset, reset_plot_inputs(), ignoreInit = TRUE)

    plot_snapshot <- NicoShinyAppTemplate::app_plot_snapshot(
      build = function() compute_plot(allow_reset = TRUE),
      ready = function() {
        info <- summary_info()
        !is.null(info) && identical(info$type, "descriptive") &&
          (is.null(is_active) || isTRUE(is_active()))
      })
    observe({
      result <- plot_snapshot()
      stored$warning <- result$warning
      stored$plot <- result$plot
      stored$layout <- result$layout
      stored$empty_message <- result$empty_message
      stored$plot_width <- result$plot_width %||% 400
      stored$plot_height <- result$plot_height %||% 300
    })

    output$grid_warning <- renderUI({
      if (!is.null(stored$warning))
        div(class = "alert alert-warning ta-grid-warning", stored$warning)
    })

    empty_state <- function(title, message, icon = "&#128221;") {
      div(
        class = "empty-state analysis-empty-state text-center my-4",
        div(
          class = "py-4 px-3",
          div(class = "empty-state-icon text-primary mb-2", HTML(icon)),
          h4(class = "mb-2", title),
          p(class = "text-muted mb-0", message)
        )
      )
    }

    output$plot_container <- renderUI({
      if (!is.null(stored$empty_message)) {
        return(empty_state("Numeric histograms unavailable", stored$empty_message))
      }

      if (is.null(stored$plot)) {
        if (!is.null(stored$warning)) {
          return(div(uiOutput(ns("grid_warning"))))
        }
        return(NULL)
      }

      tagList(
        uiOutput(ns("grid_warning")),
        NicoShinyAppTemplate::content_plot_ui(ns("plot"), list(
          width = stored$plot_width * (stored$layout$ncol %||% 1),
          height = stored$plot_height * (stored$layout$nrow %||% 1)))
      )
    })
    
    # ================================================================
    # RENDER HISTOGRAM
    # ================================================================
    output$plot <- renderPlot({
      p <- stored$plot
      if (is.null(p)) return(NULL)
      print(p)
    },
    width = function() {
      lay <- stored$layout
      if (is.null(lay)) return(600)
      stored$plot_width * (lay$ncol %||% 1)
    },
    height = function() {
      lay <- stored$layout
      if (is.null(lay)) return(600)
      stored$plot_height * (lay$nrow %||% 1)
    },
    res = 96)
    
    # ================================================================
    # DOWNLOAD — matches what user sees
    # ================================================================
    output$download_plot <- downloadHandler(
      filename = function() paste0("numeric_histograms_", Sys.Date(), ".png"),
      content = function(file) {
        p <- stored$plot
        req(!is.null(p))
        
        lay <- stored$layout
        nc <- (lay$ncol %||% 1)
        nr <- (lay$nrow %||% 1)
        
        width_px <- (stored$plot_width  * nc)
        height_px <- (stored$plot_height * nr)
        
        NicoShinyAppTemplate::save_plot_png(p, file,
          width_px = width_px, height_px = height_px, dpi = 300)
      }
    )
  })
}
