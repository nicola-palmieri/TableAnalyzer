# mod_visualize_anova_twoway: server coordination and reactive state.
visualize_twoway_server <- function(id, filtered_data, model_info) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    # ------------------------------------------------------------------
    # Stored state
    # ------------------------------------------------------------------
    stored <- reactiveValues(
      plot = NULL,
      warning = NULL,
      layout = NULL,
      plot_width = NULL,
      plot_height = NULL
    )
    
    df <- reactive(filtered_data())
    
    # ------------------------------------------------------------------
    # Color handling
    # ------------------------------------------------------------------
    color_var <- reactive({
      info <- optional_plot_metadata(model_info())
      if (is.null(info) || is.null(info$factors)) return(NULL)
      info$factors$factor2
    })
    
    factor2_levels <- reactive({
      info <- optional_plot_metadata(model_info())
      if (is.null(info) || is.null(info$orders)) return(NULL)
      info$orders$order2
    })
    
    local_reset_token <- reactiveVal(NULL)

    custom_colors <- add_color_customization_server(
      ns = ns,
      input = input,
      output = output,
      data = df,
      color_var_reactive = color_var,
      multi_group = TRUE,
      level_order_reactive = factor2_levels
    ,
      reset_token = local_reset_token
    )
    
    base_size <- base_size_server(input = input, default = NicoShinyAppTemplate::PLOT_BASE_SIZE)
    subplot_defaults <- subplot_size_defaults()
    plot_width_default <- subplot_defaults$width$value
    plot_height_default <- subplot_defaults$height$value
    
    # ------------------------------------------------------------------
    # Grid modules
    # ------------------------------------------------------------------
    grid_settings <- anova_grid_settings_server(input, session, model_info)
    grid_cache <- grid_settings$cache

    output$layout_controls <- renderUI({
      info <- optional_plot_metadata(model_info(), "twoway_anova")
      if (is.null(info)) return(NULL)
      isolate(build_anova_layout_controls(ns, input, info, grid_cache = grid_cache))
    })
    
    output$axis_and_jitter <- renderUI({
      jitter_widget <- NULL
      
      if (isTRUE(input$plot_type %in% c("lineplot_mean_se", "boxplot"))) {
        jitter_id <- if (identical(input$plot_type, "boxplot")) "boxplot_show_jitter" else "lineplot_show_jitter"
        jitter_widget <- with_help_tooltip(
          checkboxInput(
            ns(jitter_id),
            "Overlay jittered data",
            value = isTRUE(input[[jitter_id]])
          ),
          "Overlay raw observations with jitter."
        )
      }
      
      fluidRow(
        column(
          6,
          with_help_tooltip(
            checkboxInput(
              ns("share_y_axis"),
              "Use common y-axis across plots",
              value = isTRUE(input$share_y_axis)
            ),
            "Use the same y-scale for all panels."
          )
        ),
        column(6, jitter_widget)
      )
    })
    
    # ------------------------------------------------------------------
    # Common legend UI
    # ------------------------------------------------------------------
    legend_state <- reactiveValues(
      enabled = FALSE,
      position = "right"
    )
    
    common_legend_available <- reactive({
      info <- optional_plot_metadata(model_info())
      if (is.null(info) || !identical(info$type, "twoway_anova"))
        return(FALSE)
      
      has_mult_resp <- length(info$responses %||% character()) > 1
      has_strata <- !is.null(info$strata) && !is.null(info$strata$var)
      
      has_mult_resp || has_strata
    })
    
    observeEvent(input$use_common_legend, {
      legend_state$enabled <- isTRUE(input$use_common_legend)
    })
    
    observeEvent(input$common_legend_position, {
      legend_state$position <- input$common_legend_position
    })
    
    output$common_legend_controls <- renderUI({
      if (!common_legend_available()) return(NULL)
      
      legend_enabled <- isTRUE(legend_state$enabled)
      
      legend_checkbox <- with_help_tooltip(
        checkboxInput(
          ns("use_common_legend"),
          "Use common legend",
          value = legend_enabled
        ),
        "Merge legends across subplots."
      )
      
      legend_position <- NULL
      if (legend_enabled) {
        legend_position <- with_help_tooltip(
          selectInput(
            ns("common_legend_position"),
            "Legend position",
            c("Bottom" = "bottom", "Right" = "right", "Top" = "top", "Left" = "left"),
            selected = legend_state$position
          ),
          "Choose where to place the combined legend."
        )
      }
      
      tagList(legend_checkbox, legend_position)
    })
    keep_optional_ui_current(output, "layout_controls", "axis_and_jitter", "common_legend_controls")
    
    # ------------------------------------------------------------------
    # Automatic appearance snapshots
    # ------------------------------------------------------------------
    compute_plot <- function(allow_reset = FALSE) {
      data <- df()
      info <- model_info()
      
      plot_width <- NicoShinyAppTemplate::app_plot_numeric(input$plot_width,
        plot_width_default, min = 200, max = 2000, label = "Plot width")
      plot_height <- NicoShinyAppTemplate::app_plot_numeric(input$plot_height,
        plot_height_default, min = 200, max = 2000, label = "Plot height")
      NicoShinyAppTemplate::app_plot_numeric(input$plot_base_size, NicoShinyAppTemplate::PLOT_BASE_SIZE, min = 6, max = 30, label = "Base size")

      if (is.null(info) || is.null(data) || nrow(data) == 0) {
        return(list(plot = NULL, warning = "No data or ANOVA results available."))
      }
      
      # Final grid layouts
      layout_inputs <- grid_settings$values()
      layout_inputs$panel_width <- plot_width
      layout_inputs$panel_height <- plot_height
      
      # Legend handling
      use_common_legend <- common_legend_available() && legend_state$enabled
      legend_position <- if (use_common_legend) legend_state$position else NULL
      
      # Compute plots
      chosen <- input$plot_type
      if (is.null(chosen) || !chosen %in% c("lineplot_mean_se", "barplot_mean_se", "boxplot")) {
        chosen <- "lineplot_mean_se"
      }

      build_result <- function(layout_inputs) {
        switch(chosen,
          lineplot_mean_se = plot_anova_lineplot_meanse(
            data, info, layout_inputs,
            line_colors      = custom_colors(),
            base_size        = base_size(),
            show_lines       = isTRUE(input$lineplot_show_lines %||% TRUE),
            show_jitter      = isTRUE(input$lineplot_show_jitter),
            use_dodge        = isTRUE(input$lineplot_use_dodge),
            share_y_axis     = isTRUE(input$share_y_axis),
            common_legend    = use_common_legend,
            legend_position  = legend_position
          ),
          
          barplot_mean_se = plot_anova_barplot_meanse(
            data, info, layout_inputs,
            line_colors      = custom_colors(),
            base_size        = base_size(),
            posthoc_all      = info$posthoc,
            share_y_axis     = isTRUE(input$share_y_axis),
            common_legend    = use_common_legend,
            legend_position  = legend_position
          ),
          boxplot = plot_anova_boxplot(
            data, info, layout_inputs,
            show_jitter      = isTRUE(input$boxplot_show_jitter),
            line_colors      = custom_colors(),
            base_size        = base_size(),
            share_y_axis     = isTRUE(input$share_y_axis),
            common_legend    = use_common_legend,
            legend_position  = legend_position
          )
        )
      }

      chosen_result <- build_result(layout_inputs)

      is_grid_warning <- function(msg) {
        !is.null(msg) && grepl("Grid", msg, fixed = TRUE)
      }

      if (isTRUE(allow_reset) && !grid_settings$is_manual() && !is.null(chosen_result) && is_grid_warning(chosen_result$warning)) {
        default_inputs <- list(
          strata_rows = chosen_result$defaults$strata$rows,
          strata_cols = chosen_result$defaults$strata$cols,
          resp_rows   = chosen_result$defaults$responses$rows,
          resp_cols   = chosen_result$defaults$responses$cols
        )
        grid_settings$set_defaults(chosen_result$defaults)
        default_inputs$panel_width <- plot_width
        default_inputs$panel_height <- plot_height
        chosen_result <- build_result(default_inputs)
      }

      if (is_grid_warning(chosen_result$warning)) chosen_result$plot <- NULL
      chosen_result$plot_width <- plot_width
      chosen_result$plot_height <- plot_height
      chosen_result
    }

    reset_plot_inputs <- function() {
      updateNumericInput(session, "plot_width", value = plot_width_default)
      updateNumericInput(session, "plot_height", value = plot_height_default)
      updateNumericInput(session, "plot_base_size", value = NicoShinyAppTemplate::PLOT_BASE_SIZE)
      updateSelectInput(session, "plot_type", selected = "lineplot_mean_se")
      updateCheckboxInput(session, "lineplot_show_lines", value = TRUE)
      updateCheckboxInput(session, "lineplot_show_jitter", value = FALSE)
      updateCheckboxInput(session, "boxplot_show_jitter", value = FALSE)
      updateCheckboxInput(session, "share_y_axis", value = FALSE)
      grid_settings$reset()
      updateCheckboxInput(session, "lineplot_use_dodge", value = FALSE)
      legend_state$enabled <- FALSE
      legend_state$position <- "right"
      updateCheckboxInput(session, "use_common_legend", value = FALSE)
      updateSelectInput(session, "common_legend_position", selected = "right")
      local_reset_token((isolate(local_reset_token()) %||% 0L) + 1L)
    }
    observeEvent(input$reset, reset_plot_inputs(), ignoreInit = TRUE)

    plot_snapshot <- NicoShinyAppTemplate::app_plot_snapshot(
      build = function() compute_plot(allow_reset = TRUE),
      ready = function() {
        info <- model_info()
        !is.null(info) && identical(info$type, "twoway_anova")
      })
    observe({
      result <- plot_snapshot()
      stored$warning <- result$warning
      stored$plot <- result$plot
      stored$layout <- result$layout
      stored$plot_width <- result$plot_width %||% plot_width_default
      stored$plot_height <- result$plot_height %||% plot_height_default
    })

    output$plot_warning <- renderUI({
      if (!is.null(stored$warning))
        div(class = "alert alert-warning ta-grid-warning", stored$warning)
    })

    output$plot_container <- renderUI({
      if (is.null(stored$plot)) {
        if (!is.null(stored$warning)) {
          return(div(uiOutput(ns("plot_warning"))))
        }
        return(NULL)
      }

      tagList(
        uiOutput(ns("plot_warning")),
        NicoShinyAppTemplate::content_plot_ui(ns("plot"), list(
          width = (stored$plot_width %||% plot_width_default) * (stored$layout$strata$cols %||% 1) * (stored$layout$responses$cols %||% 1),
          height = (stored$plot_height %||% plot_height_default) * (stored$layout$strata$rows %||% 1) * (stored$layout$responses$rows %||% 1)))
      )
    })
    
    output$plot <- renderPlot(
      {
        p <- stored$plot
        if (is.null(p)) return(NULL)
        print(p)
      },
      width = function() {
        lay <- stored$layout
        if (is.null(lay)) return(600)
        cols <- (lay$strata$cols %||% 1) * (lay$responses$cols %||% 1)
        (stored$plot_width %||% plot_width_default) * cols
      },
      height = function() {
        lay <- stored$layout
        if (is.null(lay)) return(600)
        rows <- (lay$strata$rows %||% 1) * (lay$responses$rows %||% 1)
        (stored$plot_height %||% plot_height_default) * rows
      },
      res = 96
    )
    
    output$download_plot <- downloadHandler(
      filename = function() paste0("anova_twoway_plot_", Sys.Date(), ".png"),
      content = function(file) {
        p <- stored$plot
        req(!is.null(p))
        
        lay <- stored$layout
        req(!is.null(lay))
        
        cols <- (lay$strata$cols %||% 1) * (lay$responses$cols %||% 1)
        rows <- (lay$strata$rows %||% 1) * (lay$responses$rows %||% 1)
        
        width_px <- ((stored$plot_width %||% plot_width_default) * cols)
        height_px <- ((stored$plot_height %||% plot_height_default) * rows)
        
        NicoShinyAppTemplate::save_plot_png(p, file,
          width_px = width_px, height_px = height_px, dpi = 300)
      }
    )
  })
}
