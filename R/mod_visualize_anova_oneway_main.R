# mod_visualize_anova_oneway: server coordination and reactive state.
visualize_oneway_server <- function(id, filtered_data, model_info) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    stored <- reactiveValues(
      plot = NULL,
      warning = NULL,
      layout = NULL,
      plot_width = NULL,
      plot_height = NULL
    )
    
    df <- reactive(filtered_data())
    
    local_reset_token <- reactiveVal(NULL)

    custom_colors <- add_color_customization_server(
      ns = ns,
      input = input,
      output = output,
      data = df,
      color_var_reactive = reactive(NULL),
      multi_group = FALSE
    ,
      reset_token = local_reset_token
    )
    
    base_size <- base_size_server(input = input, default = VetmeduniAppTemplate::PLOT_BASE_SIZE)
    subplot_defaults <- subplot_size_defaults()
    plot_width_default <- subplot_defaults$width$value
    plot_height_default <- subplot_defaults$height$value
    
    grid_settings <- anova_grid_settings_server(input, session, model_info)
    grid_cache <- grid_settings$cache

    output$layout_controls <- renderUI({
      info <- optional_plot_metadata(model_info(), "oneway_anova")
      if (is.null(info)) return(NULL)
      isolate(build_anova_layout_controls(ns, input, info, grid_cache = grid_cache))
    })
    keep_optional_ui_current(output, "layout_controls")
    
    compute_plot <- function(allow_reset = FALSE) {
      data <- df()
      info <- model_info()
      
      plot_width <- VetmeduniAppTemplate::app_plot_numeric(input$plot_width,
        plot_width_default, min = 200, max = 2000, label = "Plot width")
      plot_height <- VetmeduniAppTemplate::app_plot_numeric(input$plot_height,
        plot_height_default, min = 200, max = 2000, label = "Plot height")
      VetmeduniAppTemplate::app_plot_numeric(input$plot_base_size, VetmeduniAppTemplate::PLOT_BASE_SIZE, min = 6, max = 30, label = "Base size")

      if (is.null(info) || is.null(data) || nrow(data) == 0) {
        return(list(plot = NULL, warning = "No data or ANOVA results available."))
      }
      
      chosen <- input$plot_type
      if (is.null(chosen) || !chosen %in% c("lineplot_mean_se", "barplot_mean_se", "boxplot")) {
        chosen <- "lineplot_mean_se"
      }

      build_result <- function(layout_inputs) {
        switch(chosen,
          lineplot_mean_se = plot_anova_lineplot_meanse(
            data,
            info,
            layout_values = layout_inputs,
            line_colors   = custom_colors(),
            base_size     = base_size(),
            show_lines    = isTRUE(input$lineplot_show_lines %||% TRUE),
            show_jitter   = isTRUE(input$lineplot_show_jitter),
            share_y_axis  = isTRUE(input$share_y_axis)
          ),
          barplot_mean_se = plot_anova_barplot_meanse(
            data,
            info,
            layout_values = layout_inputs,
            line_colors   = custom_colors(),
            base_size     = base_size(),
            posthoc_all   = info$posthoc,
            share_y_axis  = isTRUE(input$share_y_axis)
          ),
          boxplot = plot_anova_boxplot(
            data,
            info,
            show_jitter = isTRUE(input$boxplot_show_jitter),
            layout_values = layout_inputs,
            line_colors   = custom_colors(),
            base_size     = base_size(),
            share_y_axis  = isTRUE(input$share_y_axis)
          )
        )
      }

      layout_inputs <- grid_settings$values()
      layout_inputs$panel_width <- plot_width
      layout_inputs$panel_height <- plot_height

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
      updateNumericInput(session, "plot_base_size", value = VetmeduniAppTemplate::PLOT_BASE_SIZE)
      updateSelectInput(session, "plot_type", selected = "lineplot_mean_se")
      updateCheckboxInput(session, "lineplot_show_lines", value = TRUE)
      updateCheckboxInput(session, "lineplot_show_jitter", value = FALSE)
      updateCheckboxInput(session, "boxplot_show_jitter", value = FALSE)
      updateCheckboxInput(session, "share_y_axis", value = FALSE)
      grid_settings$reset()
      local_reset_token((isolate(local_reset_token()) %||% 0L) + 1L)
    }
    observeEvent(input$reset, reset_plot_inputs(), ignoreInit = TRUE)

    plot_snapshot <- VetmeduniAppTemplate::app_plot_snapshot(
      build = function() compute_plot(allow_reset = TRUE),
      ready = function() {
        info <- model_info()
        !is.null(info) && identical(info$type, "oneway_anova")
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
      if (!is.null(stored$warning)) {
        div(class = "alert alert-warning ta-grid-warning", HTML(stored$warning))
      }
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
        VetmeduniAppTemplate::content_plot_ui(ns("plot"), list(
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
        
        cols_strata    <- lay$strata$cols %||% 1
        cols_responses <- lay$responses$cols %||% 1
        
        (stored$plot_width %||% plot_width_default) * cols_strata * cols_responses
      },
      height = function() {
        lay <- stored$layout
        if (is.null(lay)) return(600)
        
        rows_strata    <- lay$strata$rows %||% 1
        rows_responses <- lay$responses$rows %||% 1
        
        (stored$plot_height %||% plot_height_default) * rows_strata * rows_responses
      },
      res = 96
    )
    
    output$download_plot <- downloadHandler(
      filename = function() {
        paste0("anova_plot_", Sys.Date(), ".png")
      },
      content = function(file) {
        p <- stored$plot
        req(!is.null(p))
        
        lay <- stored$layout
        req(!is.null(lay))
        
        cols_strata    <- lay$strata$cols %||% 1
        cols_responses <- lay$responses$cols %||% 1
        rows_strata    <- lay$strata$rows %||% 1
        rows_responses <- lay$responses$rows %||% 1
        
        total_cols <- cols_strata * cols_responses
        total_rows <- rows_strata * rows_responses
        
        width_px <- ((stored$plot_width  %||% plot_width_default) * total_cols)
        height_px <- ((stored$plot_height %||% plot_height_default) * total_rows)
        
        VetmeduniAppTemplate::save_plot_png(p, file,
          width_px = width_px, height_px = height_px, dpi = 300)
      }
    )
  })
}
