# ===============================================================
# 🧪 Visualization Module — One-way ANOVA (Apply-button version)
# ===============================================================

visualize_oneway_ui <- function(id) {
  ns <- NS(id)
  
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("Step 4 — Visualize one-way ANOVA"),
      p("Select visualization type and adjust subplot layout, axis scaling, and figure size."),
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
      uiOutput(ns("layout_controls")),
      
      fluidRow(
        column(6, add_color_customization_ui(ns, multi_group = FALSE)),
        column(6, base_size_ui(ns, default = 13))
      ),
      
      br(),
      
      fluidRow(
        column(6, actionButton(ns("apply_plot"), "Apply changes", width = "100%")),
        column(6, downloadButton(ns("download_plot"), "Download plot", style = "width: 100%;"))
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
    
    strata_grid <- plot_grid_server("strata_grid")
    response_grid <- plot_grid_server("response_grid")
    
    output$layout_controls <- renderUI({
      info <- model_info()
      req(info)
      build_anova_layout_controls(ns, input, info)
    })
    
    observeEvent(input$apply_plot, {
      data <- df()
      info <- model_info()
      
      stored$plot_width  <- input$plot_width  %||% 600
      stored$plot_height <- input$plot_height %||% 600
      
      if (is.null(info) || is.null(data) || nrow(data) == 0) {
        stored$warning <- "No data or ANOVA results available."
        stored$plot <- NULL
        stored$layout <- NULL
        return()
      }
      
      has_strata <- !is.null(info$strata) && !is.null(info$strata$levels)
      n_strata <- if (has_strata) length(info$strata$levels) else 1L
      
      responses <- info$responses
      n_responses <- if (!is.null(responses) && length(responses) > 0L) length(responses) else 1L
      
      layout_inputs <- list(
        strata_rows = strata_grid$rows(),
        strata_cols = strata_grid$cols(),
        resp_rows   = response_grid$rows(),
        resp_cols   = response_grid$cols()
      )
      
      results <- list(
        lineplot_mean_se = plot_anova_lineplot_meanse(
          data,
          info,
          layout_values = layout_inputs,
          line_colors   = custom_colors(),
          base_size     = base_size(),
          show_lines    = input$lineplot_show_lines,
          show_jitter   = input$lineplot_show_jitter,
          share_y_axis  = input$share_y_axis
        ),
        barplot_mean_se = plot_anova_barplot_meanse(
          data,
          info,
          layout_values = layout_inputs,
          line_colors   = custom_colors(),
          base_size     = base_size(),
          posthoc_all   = info$posthoc,
          share_y_axis  = input$share_y_axis
        ),
        boxplot = plot_anova_boxplot(
          data,
          info,
          layout_values = layout_inputs,
          line_colors   = custom_colors(),
          base_size     = base_size(),
          share_y_axis  = input$share_y_axis
        )
      )
      
      chosen <- input$plot_type
      chosen_result <- results[[chosen]]
      
      stored$warning <- chosen_result$warning
      stored$plot    <- chosen_result$plot
      stored$layout  <- chosen_result$layout
    })
    
    output$plot_warning <- renderUI({
      if (!is.null(stored$warning)) {
        div(class = "alert alert-warning", HTML(stored$warning))
      }
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
        
        (stored$plot_width %||% 600) * cols_strata * cols_responses
      },
      height = function() {
        lay <- stored$layout
        if (is.null(lay)) return(600)
        
        rows_strata    <- lay$strata$rows %||% 1
        rows_responses <- lay$responses$rows %||% 1
        
        (stored$plot_height %||% 600) * rows_strata * rows_responses
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
        
        w_in <- ((stored$plot_width  %||% 600) * total_cols) / 96
        h_in <- ((stored$plot_height %||% 600) * total_rows) / 96
        
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
