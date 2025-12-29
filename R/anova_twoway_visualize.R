# ===============================================================
# 🧪 Visualization Module — Two-way ANOVA (Apply-button version)
# ===============================================================

visualize_twoway_ui <- function(id) {
  ns <- NS(id)
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4(class = "ta-sidebar-title", "Step 4 - Visualize two-way ANOVA"),
      p(class = "ta-sidebar-subtitle", "Select visualization type and adjust subplot layout, axis scaling, and figure size."),
      hr(),
      
      with_help_tooltip(
        selectInput(
          ns("plot_type"),
          label = "Select visualization type",
          choices = c(
            "Lineplots (mean ± SE)" = "lineplot_mean_se",
            "Barplots (mean ± SE)"  = "barplot_mean_se",
            "Boxplots"              = "boxplot"
          ),
          selected = "lineplot_mean_se"
        ),
        "Pick the chart style you prefer for viewing group means and uncertainty."
      ),
      
      conditionalPanel(
        condition = sprintf("input['%s'] === 'lineplot_mean_se'", ns("plot_type")),
        fluidRow(
          column(
            6,
            with_help_tooltip(
              checkboxInput(
                ns("lineplot_show_lines"),
                "Connect means with lines",
                value = TRUE
              ),
              "Draw connecting lines between group means."
            )
          ),
          column(
            6,
            with_help_tooltip(
              checkboxInput(
                ns("lineplot_use_dodge"),
                "Dodge grouped means",
                value = FALSE
              ),
              "Offset the level means of the second factor along the x-axis."
            )
          )
        )
      ),
      
      uiOutput(ns("axis_and_jitter")),
      subplot_size_ui(ns),
      uiOutput(ns("layout_controls")),
      
      fluidRow(
        column(6, add_color_customization_ui(ns, multi_group = TRUE)),
        column(
          6,
          tagList(
            base_size_ui(
              ns,
              default = 13,
              help_text = "Adjust the base font size used for the ANOVA plots."
            ),
            br(),
            uiOutput(ns("common_legend_controls"))
          )
        )
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
      info <- model_info()
      if (is.null(info) || is.null(info$factors)) return(NULL)
      info$factors$factor2
    })
    
    factor2_levels <- reactive({
      info <- model_info()
      if (is.null(info) || is.null(info$orders)) return(NULL)
      info$orders$order2
    })
    
    custom_colors <- add_color_customization_server(
      ns = ns,
      input = input,
      output = output,
      data = df,
      color_var_reactive = color_var,
      multi_group = TRUE,
      level_order_reactive = factor2_levels
    )
    
    base_size <- base_size_server(input = input, default = 13)
    subplot_defaults <- subplot_size_defaults()
    plot_width_default <- subplot_defaults$width$value
    plot_height_default <- subplot_defaults$height$value
    
    # ------------------------------------------------------------------
    # Grid modules
    # ------------------------------------------------------------------
    strata_grid   <- plot_grid_server("strata_grid")
    response_grid <- plot_grid_server("response_grid")
    
    # ------------------------------------------------------------------
    # Dynamic UI
    # ------------------------------------------------------------------
    output$layout_controls <- renderUI({
      info <- model_info()
      req(info)
      build_anova_layout_controls(ns, input, info)
    })
    
    output$axis_and_jitter <- renderUI({
      jitter_widget <- NULL
      
      if (input$plot_type == "lineplot_mean_se") {
        jitter_widget <- with_help_tooltip(
          checkboxInput(
            ns("lineplot_show_jitter"),
            "Overlay jittered data",
            value = isTRUE(input$lineplot_show_jitter)
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
      position = "bottom"
    )
    
    common_legend_available <- reactive({
      info <- model_info()
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
    
    # ------------------------------------------------------------------
    # APPLY BUTTON
    # ------------------------------------------------------------------
    compute_plot <- function(allow_reset = FALSE) {
      data <- df()
      info <- model_info()
      
      plot_width <- suppressWarnings(as.numeric(input$plot_width))
      if (length(plot_width) == 0 || is.na(plot_width) || plot_width <= 0) {
        plot_width <- plot_width_default
      }
      plot_height <- suppressWarnings(as.numeric(input$plot_height))
      if (length(plot_height) == 0 || is.na(plot_height) || plot_height <= 0) {
        plot_height <- plot_height_default
      }

      stored$plot_width <- plot_width
      stored$plot_height <- plot_height
      
      if (is.null(info) || is.null(data) || nrow(data) == 0) {
        stored$warning <- "No data or ANOVA results available."
        stored$plot <- NULL
        stored$layout <- NULL
        return()
      }
      
      # Count panels
      has_strata <- !is.null(info$strata) && !is.null(info$strata$levels)
      n_strata <- if (has_strata) length(info$strata$levels) else 1L
      
      responses <- info$responses
      n_responses <- if (!is.null(responses) && length(responses) > 0L) length(responses) else 1L
      
      # Final grid layouts
      layout_inputs <- list(
        strata_rows = strata_grid$rows(),
        strata_cols = strata_grid$cols(),
        resp_rows   = response_grid$rows(),
        resp_cols   = response_grid$cols()
      )
      
      # Legend handling
      legend_supported <- TRUE
      use_common_legend <- legend_supported && common_legend_available() && legend_state$enabled
      legend_position <- if (use_common_legend) legend_state$position else NULL
      
      # Compute plots
      build_results <- function(layout_inputs) {
        list(
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
            line_colors      = custom_colors(),
            base_size        = base_size(),
            share_y_axis     = isTRUE(input$share_y_axis),
            common_legend    = use_common_legend,
            legend_position  = legend_position
          )
        )
      }

      results <- build_results(layout_inputs)
      
      chosen <- input$plot_type
      if (is.null(chosen) || !chosen %in% names(results)) {
        chosen <- "lineplot_mean_se"
      }
      chosen_result <- results[[chosen]]

      is_grid_warning <- function(msg) {
        !is.null(msg) && grepl("Grid", msg, fixed = TRUE)
      }

      if (isTRUE(allow_reset) && !is.null(chosen_result) && is_grid_warning(chosen_result$warning)) {
        default_inputs <- list(
          strata_rows = chosen_result$defaults$strata$rows,
          strata_cols = chosen_result$defaults$strata$cols,
          resp_rows   = chosen_result$defaults$responses$rows,
          resp_cols   = chosen_result$defaults$responses$cols
        )
        results <- build_results(default_inputs)
        chosen_result <- results[[chosen]]
      }

      stored$warning <- chosen_result$warning
      grid_bad <- is_grid_warning(stored$warning)
      stored$plot    <- if (grid_bad) NULL else chosen_result$plot
      stored$layout  <- if (grid_bad) NULL else chosen_result$layout

      apply_grid_defaults_if_empty(
        input,
        session,
        "strata_grid",
        defaults = chosen_result$defaults$strata,
        n_items = chosen_result$panel_counts$strata
      )

      apply_grid_defaults_if_empty(
        input,
        session,
        "response_grid",
        defaults = chosen_result$defaults$responses,
        n_items = chosen_result$panel_counts$responses
      )
    }

    observeEvent(input$apply_plot, {
      compute_plot(allow_reset = FALSE)
    })

    observeEvent(model_info(), {
      info <- model_info()
      if (is.null(info$type) || !identical(info$type, "twoway_anova")) return()
      compute_plot(allow_reset = TRUE)
    }, ignoreInit = FALSE)

    observeEvent(input$plot_type, {
      info <- model_info()
      if (is.null(info$type) || !identical(info$type, "twoway_anova")) return()
      compute_plot(allow_reset = TRUE)
    }, ignoreInit = TRUE)
    
    # ------------------------------------------------------------------
    # Outputs
    # ------------------------------------------------------------------
    output$plot_warning <- renderUI({
      if (!is.null(stored$warning))
        div(class = "alert alert-warning", stored$warning)
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
        plotOutput(ns("plot"), height = "auto")
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
        
        w_in <- ((stored$plot_width %||% plot_width_default) * cols) / 96
        h_in <- ((stored$plot_height %||% plot_height_default) * rows) / 96
        
        ggsave(file, p,
               dpi = 300,
               width = w_in,
               height = h_in,
               units = "in",
               limitsize = FALSE)
      }
    )
  })
}
