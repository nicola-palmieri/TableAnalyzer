# ===============================================================
# 📊 Visualization Module — Linear Model (LM)
# ===============================================================

visualize_lm_ui <- function(id) {
  ns <- NS(id)

  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("Step 4 — Visualize linear models"),
      p("Explore model fit diagnostics and compare observed values to model predictions."),
      hr(),

      selectInput(
        ns("plot_type"),
        label = "Select visualization type",
        choices = c(
          "Observed vs predicted" = "observed_pred",
          "Residual diagnostics" = "diagnostics"
        ),
        selected = "observed_pred"
      ),

      numericInput(
        ns("point_alpha"),
        label = "Point opacity (0-1)",
        value = 0.8,
        min = 0.1,
        max = 1,
        step = 0.1
      ),

      subplot_size_ui(ns),
      plot_grid_ui(
        ns("grid"),
        rows_label = "Grid rows",
        cols_label = "Grid columns",
        rows_help = "Rows used when arranging multiple model panels.",
        cols_help = "Columns used when arranging multiple model panels."
      ),
      base_size_ui(ns, default = 13),
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


visualize_lm_server <- function(id, filtered_data, model_info) {
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
    grid <- plot_grid_server("grid")
    base_size <- base_size_server(input = input, default = 13)

    build_residual_plot <- function(model_obj) {
      plot_df <- data.frame(
        fitted = stats::fitted(model_obj),
        residuals = stats::residuals(model_obj)
      )

      ggplot2::ggplot(plot_df, ggplot2::aes(x = fitted, y = residuals)) +
        ggplot2::geom_point(color = "#2563eb", alpha = input$point_alpha %||% 0.8) +
        ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
        ggplot2::labs(
          title = "Residuals vs fitted",
          x = "Fitted values",
          y = "Residuals"
        ) +
        ta_plot_theme(base_size = base_size())
    }

    build_qq_plot <- function(model_obj) {
      resid_vals <- stats::residuals(model_obj)
      qq_base <- stats::qqnorm(resid_vals, plot.it = FALSE)

      qq_df <- data.frame(
        theoretical = qq_base$x,
        sample = qq_base$y
      )

      resid_mean <- mean(resid_vals)
      resid_sd <- sd(resid_vals)

      ggplot2::ggplot(qq_df, ggplot2::aes(x = theoretical, y = sample)) +
        ggplot2::geom_point(color = "#2563eb", alpha = input$point_alpha %||% 0.8) +
        ggplot2::geom_abline(
          slope = resid_sd,
          intercept = resid_mean,
          linetype = "dashed"
        ) +
        ggplot2::labs(
          title = "Normal Q-Q",
          x = "Theoretical quantiles",
          y = "Sample quantiles"
        ) +
        ta_plot_theme(base_size = base_size())
    }

    build_prediction_plot <- function(entry, data) {
      response <- entry$response %||% "Response"
      stratum_label <- entry$stratum %||% "Overall"

      if (!response %in% names(data)) {
        return(list(
          plot = NULL,
          warning = sprintf("Response '%s' not found in current data.", response)
        ))
      }

      pred_vals <- tryCatch(
        stats::predict(entry$model, newdata = data),
        error = function(e) e
      )

      if (inherits(pred_vals, "error")) {
        return(list(
          plot = NULL,
          warning = sprintf("Could not generate predictions for %s (%s): %s", response, stratum_label, conditionMessage(pred_vals))
        ))
      }

      plot_df <- data.frame(
        observed = data[[response]],
        predicted = pred_vals
      )
      plot_df <- stats::na.omit(plot_df)

      if (nrow(plot_df) == 0) {
        return(list(
          plot = NULL,
          warning = sprintf("No complete cases available for %s (%s).", response, stratum_label)
        ))
      }

      p <- ggplot2::ggplot(plot_df, ggplot2::aes(x = predicted, y = observed)) +
        ggplot2::geom_point(color = "#2563eb", alpha = input$point_alpha %||% 0.8) +
        ggplot2::geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "#9ca3af") +
        ggplot2::labs(
          title = sprintf("%s — %s", response, stratum_label),
          x = "Predicted",
          y = "Observed"
        ) +
        ta_plot_theme(base_size = base_size())

      list(plot = p, warning = NULL)
    }

    subset_for_stratum <- function(data, entry, strat_details) {
      if (is.null(strat_details$var)) return(data)
      if (is.null(entry$stratum)) return(data)

      strat_var <- strat_details$var
      if (!strat_var %in% names(data)) return(data.frame())

      data[data[[strat_var]] == entry$stratum, , drop = FALSE]
    }

    observeEvent(input$apply_plot, {
      data <- df()
      info <- model_info()

      stored$plot_width <- input$plot_width %||% 400
      stored$plot_height <- input$plot_height %||% 300

      if (is.null(info) || is.null(data)) {
        stored$warning <- "No data or model results available."
        stored$plot <- NULL
        stored$layout <- NULL
        return()
      }

      if (!identical(tolower(info$type %||% ""), "lm")) {
        stored$warning <- "Visualizations are available for linear models only. Run an LM in the Analyze tab first."
        stored$plot <- NULL
        stored$layout <- NULL
        return()
      }

      entries <- info$flat_models %||% list()
      if (length(entries) == 0) {
        stored$warning <- "No fitted linear models available to visualize."
        stored$plot <- NULL
        stored$layout <- NULL
        return()
      }

      plot_type <- input$plot_type %||% "observed_pred"
      plots <- list()
      warnings <- character(0)

      for (entry in entries) {
        subset_data <- subset_for_stratum(data, entry, info$stratification %||% list())
        if (nrow(subset_data) == 0) {
          warnings <- c(warnings, sprintf("No data available for %s — %s.", entry$response %||% "response", entry$stratum %||% "overall"))
          next
        }

        if (identical(plot_type, "observed_pred")) {
          res <- build_prediction_plot(entry, subset_data)
          if (!is.null(res$warning)) warnings <- c(warnings, res$warning)
          if (!is.null(res$plot)) plots[[length(plots) + 1]] <- res$plot
        } else {
          resid_plot <- build_residual_plot(entry$model)
          qq_plot <- build_qq_plot(entry$model)

          combined <- patchwork::wrap_plots(
            resid_plot,
            qq_plot,
            nrow = 1,
            guides = "collect"
          ) + patchwork::plot_annotation(
            title = sprintf("%s — %s", entry$response %||% "Response", entry$stratum %||% "Overall")
          )

          plots[[length(plots) + 1]] <- combined
        }
      }

      if (length(plots) == 0) {
        stored$warning <- if (length(warnings) > 0) paste(unique(warnings), collapse = "<br>") else "No plots could be generated."
        stored$plot <- NULL
        stored$layout <- NULL
        return()
      }

      layout <- adjust_grid_layout(length(plots), grid$values())
      combined_plot <- patchwork::wrap_plots(plots, nrow = layout$nrow, ncol = layout$ncol)

      stored$plot <- combined_plot
      stored$layout <- layout
      stored$warning <- if (length(warnings) > 0) paste(unique(warnings), collapse = "<br>") else NULL
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
        cols <- lay$ncol %||% 1
        (stored$plot_width %||% 400) * cols
      },
      height = function() {
        lay <- stored$layout
        rows <- lay$nrow %||% 1
        (stored$plot_height %||% 300) * rows
      },
      res = 96
    )

    output$download_plot <- downloadHandler(
      filename = function() {
        paste0("lm_visualization_", Sys.Date(), ".png")
      },
      content = function(file) {
        req(stored$plot)
        lay <- stored$layout %||% list(nrow = 1, ncol = 1)

        width_in <- ((stored$plot_width %||% 400) * (lay$ncol %||% 1)) / 96
        height_in <- ((stored$plot_height %||% 300) * (lay$nrow %||% 1)) / 96

        ggplot2::ggsave(
          filename = file,
          plot = stored$plot,
          width = width_in,
          height = height_in,
          dpi = 96,
          units = "in"
        )
      }
    )
  })
}
