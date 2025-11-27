# ===============================================================
# 📊 Visualization Module — Linear Model (LM)
# ===============================================================

visualize_lm_ui <- function(id) {
  ns <- NS(id)

  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("Step 4 — Visualize linear model"),
      p("Review fitted vs observed values and common diagnostics for the selected linear model."),
      hr(),

      selectInput(ns("response"), "Response variable", choices = NULL),
      uiOutput(ns("stratum_ui")),

      selectInput(
        ns("plot_type"),
        label = "Choose plot",
        choices = c(
          "Observed vs fitted" = "obs_fit",
          "Residuals vs fitted" = "resid_fit",
          "Normal Q-Q" = "qq"
        ),
        selected = "obs_fit"
      ),

      subplot_size_ui(ns),
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

    stored <- reactiveValues(plot = NULL, warning = NULL, plot_width = NULL, plot_height = NULL)

    base_size <- base_size_server(input = input, default = 13)

    available_models <- reactive({
      info <- model_info()
      flat <- info$flat_models %||% list()
      if (length(flat) == 0) return(data.frame())

      data.frame(
        response = vapply(flat, function(x) x$response %||% "Response", character(1)),
        stratum = vapply(flat, function(x) x$stratum %||% "Overall", character(1)),
        stringsAsFactors = FALSE
      )
    })

    observe({
      models_df <- available_models()
      if (nrow(models_df) == 0) return()

      responses <- unique(models_df$response)
      updateSelectInput(session, "response", choices = responses, selected = responses[1])
    })

    strata_choices <- reactive({
      models_df <- available_models()
      if (nrow(models_df) == 0) return(character(0))

      resp <- input$response %||% models_df$response[1]
      unique(models_df$stratum[models_df$response == resp])
    })

    output$stratum_ui <- renderUI({
      choices <- strata_choices()
      if (length(choices) <= 1) return(NULL)

      selectInput(ns("stratum"), "Stratum", choices = choices, selected = choices[1])
    })

    observe({
      choices <- strata_choices()
      if (length(choices) == 0) return()

      selected <- input$stratum
      if (is.null(selected) || !selected %in% choices) selected <- choices[1]

      updateSelectInput(session, "stratum", choices = choices, selected = selected)
    })

    fetch_model <- function(info, response, stratum) {
      flat <- info$flat_models %||% list()
      if (length(flat) == 0) return(NULL)

      match_fun <- function(entry) {
        same_resp <- identical(entry$response, response)
        entry_stratum <- entry$stratum %||% "Overall"
        same_stratum <- identical(entry_stratum, stratum %||% entry_stratum)
        same_resp && same_stratum
      }

      match <- purrr::detect(flat, match_fun)
      if (is.null(match)) return(NULL)
      match$model
    }

    observeEvent(input$apply_plot, {
      info <- model_info()
      req(info)

      stored$plot_width <- input$plot_width %||% 600
      stored$plot_height <- input$plot_height %||% 400

      resp <- input$response
      stratum <- input$stratum %||% "Overall"

      model_obj <- fetch_model(info, resp, stratum)
      if (is.null(model_obj)) {
        stored$warning <- "No fitted model was found for the selected response/stratum."
        stored$plot <- NULL
        return()
      }

      title_suffix <- if (!is.null(stratum) && nzchar(stratum) && stratum != "Overall") {
        paste0(" — ", stratum)
      } else {
        ""
      }

      plot_title <- paste0(resp %||% "Model", title_suffix)

      plot_obj <- switch(
        input$plot_type,
        obs_fit = plot_lm_observed_vs_fitted(model_obj, plot_title, base_size()),
        resid_fit = plot_lm_residuals_vs_fitted(model_obj, plot_title, base_size()),
        qq = plot_lm_qq(model_obj, plot_title, base_size()),
        NULL
      )

      stored$warning <- NULL
      stored$plot <- plot_obj
    })

    output$plot_warning <- renderUI({
      if (!is.null(stored$warning)) {
        div(class = "alert alert-warning", stored$warning)
      }
    })

    output$plot <- renderPlot(
      {
        p <- stored$plot
        if (is.null(p)) return(NULL)
        print(p)
      },
      width = function() stored$plot_width %||% 600,
      height = function() stored$plot_height %||% 400,
      res = 96
    )

    output$download_plot <- downloadHandler(
      filename = function() {
        paste0("lm_plot_", Sys.Date(), ".png")
      },
      content = function(file) {
        p <- stored$plot
        req(!is.null(p))
        ggplot2::ggsave(
          filename = file,
          plot = p,
          width = (stored$plot_width %||% 600) / 96,
          height = (stored$plot_height %||% 400) / 96,
          dpi = 300,
          units = "in"
        )
      }
    )
  })
}

plot_lm_observed_vs_fitted <- function(model_obj, title, base_size) {
  mf <- stats::model.frame(model_obj)
  response_vals <- stats::model.response(mf)
  plot_df <- data.frame(
    fitted = stats::fitted(model_obj),
    observed = response_vals
  )

  ggplot2::ggplot(plot_df, ggplot2::aes(x = fitted, y = observed)) +
    ggplot2::geom_point(color = "steelblue", alpha = 0.85) +
    ggplot2::geom_abline(linetype = "dashed", color = "#6b7280") +
    ggplot2::labs(
      title = paste0(title, ": Observed vs fitted"),
      x = "Fitted values",
      y = "Observed values"
    ) +
    ta_plot_theme(base_size = base_size) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(color = "#9ca3af"),
      axis.ticks = ggplot2::element_line(color = "#9ca3af")
    )
}

plot_lm_residuals_vs_fitted <- function(model_obj, title, base_size) {
  plot_df <- data.frame(
    fitted = stats::fitted(model_obj),
    residuals = stats::residuals(model_obj)
  )

  ggplot2::ggplot(plot_df, ggplot2::aes(x = fitted, y = residuals)) +
    ggplot2::geom_point(color = "#0ea5e9", alpha = 0.85) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed", color = "#6b7280") +
    ggplot2::labs(
      title = paste0(title, ": Residuals vs fitted"),
      x = "Fitted values",
      y = "Residuals"
    ) +
    ta_plot_theme(base_size = base_size) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(color = "#9ca3af"),
      axis.ticks = ggplot2::element_line(color = "#9ca3af")
    )
}

plot_lm_qq <- function(model_obj, title, base_size) {
  resid_vals <- stats::residuals(model_obj)
  qq_base <- stats::qqnorm(resid_vals, plot.it = FALSE)

  qq_df <- data.frame(
    theoretical = qq_base$x,
    sample = qq_base$y
  )

  resid_mean <- mean(resid_vals)
  resid_sd <- sd(resid_vals)

  ggplot2::ggplot(qq_df, ggplot2::aes(x = theoretical, y = sample)) +
    ggplot2::geom_point(color = "#0ea5e9", alpha = 0.85) +
    ggplot2::geom_abline(
      slope = resid_sd,
      intercept = resid_mean,
      linetype = "dashed",
      color = "#6b7280"
    ) +
    ggplot2::labs(
      title = paste0(title, ": Normal Q-Q"),
      x = "Theoretical quantiles",
      y = "Sample quantiles"
    ) +
    ta_plot_theme(base_size = base_size) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(color = "#9ca3af"),
      axis.ticks = ggplot2::element_line(color = "#9ca3af")
    )
}
