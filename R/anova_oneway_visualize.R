# ===============================================================
# 🧪 Visualization Module — One-way ANOVA (Apply-button version)
# ===============================================================

visualize_oneway_ui <- function(id) {
  ns <- NS(id)
  
  sidebarLayout(
    sidebarPanel(
      class = "ta-sidebar",
      width = 4,
      h4(class = "ta-sidebar-title", "Step 4 - Visualize one-way ANOVA"),
      p(class = "ta-sidebar-subtitle", "Select visualization type and adjust subplot layout, axis scaling, and figure size."),
      hr(),
      
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
      
      conditionalPanel(
        condition = sprintf("input['%s'] === 'lineplot_mean_se'", ns("plot_type")),
        fluidRow(
          column(6, checkboxInput(ns("lineplot_show_lines"),  "Connect means with lines", value = TRUE)),
          column(6, checkboxInput(ns("lineplot_show_jitter"), "Overlay jittered data",  value = FALSE))
        )
      ),
      
      checkboxInput(
        ns("share_y_axis"),
        "Use common y-axis across plots",
        value = FALSE
      ),
      
      subplot_size_ui(ns),
      
      fluidRow(
        column(6, add_color_customization_ui(ns)),
        column(6, base_size_ui(ns, default = 13))
      ),
      
      uiOutput(ns("layout_controls")),
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
    
    custom_colors <- add_color_customization_server(
      ns = ns,
      input = input,
      output = output,
      data = df,
      color_var_reactive = reactive(NULL),
      multi_group = FALSE
    )
    
    base_size <- base_size_server(input = input, default = 14)
    subplot_defaults <- subplot_size_defaults()
    plot_width_default <- subplot_defaults$width$value
    plot_height_default <- subplot_defaults$height$value
    
    grid_settings <- anova_grid_settings_server(input, session, model_info)
    grid_cache <- grid_settings$cache

    output$layout_controls <- renderUI({
      info <- model_info()
      req(info)
      isolate(build_anova_layout_controls(ns, input, info, grid_cache = grid_cache))
    })
    
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
      
      stored$warning <- chosen_result$warning
      grid_bad <- is_grid_warning(stored$warning)
      stored$plot   <- if (grid_bad) NULL else chosen_result$plot
      stored$layout <- if (grid_bad) NULL else chosen_result$layout
    }

    observeEvent(input$apply_plot, {
      compute_plot(allow_reset = FALSE)
    })

    observeEvent(model_info(), {
      info <- model_info()
      if (is.null(info$type) || !identical(info$type, "oneway_anova")) return()
      compute_plot(allow_reset = TRUE)
    }, ignoreInit = FALSE)

    observeEvent(input$plot_type, {
      info <- model_info()
      if (is.null(info$type) || !identical(info$type, "oneway_anova")) return()
      compute_plot(allow_reset = TRUE)
    }, ignoreInit = TRUE)
    
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
        
        w_in <- ((stored$plot_width  %||% plot_width_default) * total_cols) / 96
        h_in <- ((stored$plot_height %||% plot_height_default) * total_rows) / 96
        
        ggsave(
          filename = file,
          plot = p,
          dpi = 300,
          width = w_in,
          height = h_in,
          units = "in",
          limitsize = FALSE
        )
      }
    )
  })
}
