# ===============================================================
# 🧪 Table  Analyzer — Pairwise Correlation Module
# ===============================================================

ggpairs_ui <- function(id) {
  ns <- NS(id)
  list(
    config = tagList(
      uiOutput(ns("vars_ui")),
      tags$details(
        tags$summary(strong("Advanced options")),
        stratification_ui("strat", ns)
      ),
      br(),
      fluidRow(
        column(6, with_help_tooltip(
          actionButton(ns("run"), "Show correlation matrix", width = "100%"),
          "Calculate the correlation coefficients for the selected variables."
        )),
        column(6, with_help_tooltip(
          downloadButton(ns("download_model"), "Download all results", style = "width: 100%;"),
          "Export the correlation matrices and any messages to a text file."
        ))
      )
    ),
    results = tagList(
      h5("Correlation matrix"),
      verbatimTextOutput(ns("summary"))
    )
  )
}

ggpairs_server <- function(id, data_reactive) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    df <- reactive(data_reactive())

    strat_info <- stratification_server("strat", df)

    # ---- Build variable selector (handles re-rendering) ----
    output$vars_ui <- renderUI({
      data <- req(df())
      num_vars <- names(data)[vapply(data, is.numeric, logical(1))]

      with_help_tooltip(
        selectInput(ns("vars"), "Numeric variables", choices = num_vars, selected = num_vars, multiple = TRUE),
        "Choose which numeric columns to include in the correlation matrix."
      )
    })

    build_ggpairs_object <- function(data) {
      GGally::ggpairs(
        data,
        progress = FALSE,
        upper = list(
          continuous = GGally::wrap("cor", size = 4, color = basic_color_palette[1])
        ),
        lower = list(
          continuous = GGally::wrap("points", alpha = 0.6, color = basic_color_palette[1], size = 1.5)
        ),
        diag = list(
          continuous = GGally::wrap("densityDiag", fill = basic_color_palette[1], alpha = 0.4)
        )
      ) +
        ta_plot_theme(base_size = 11) +
        ggplot2::theme(
          strip.text = ggplot2::element_text(size = 9),
          panel.grid.major.x = ggplot2::element_blank(),
          panel.grid.major.y = ggplot2::element_blank(),
          axis.line = ggplot2::element_line(color = "#9ca3af"),
          axis.ticks = ggplot2::element_line(color = "#9ca3af")
        )
    }

    correlation_store <- reactiveVal(NULL)

    # ---- Compute correlation matrix ----
    observeEvent(input$run, {
      data <- req(df())
      numeric_vars <- names(data)[vapply(data, is.numeric, logical(1))]
      selected_vars <- if (length(input$vars)) input$vars else numeric_vars
      selected_vars <- intersect(selected_vars, numeric_vars)

      if (length(selected_vars) < 2) {
        correlation_store(list(
          message = "Need at least two numeric columns.",
          matrices = list(),
          plots = list(),
          group_var = NULL,
          selected_vars = selected_vars,
          data_used = NULL,
          strata_levels = NULL
        ))
        return()
      }

      strat_details <- strat_info()
      group_var <- strat_details$var

      if (!is.null(group_var) && !group_var %in% names(data)) {
        group_var <- NULL
      }

      if (is.null(group_var)) {
        strata_levels <- "Overall"
      } else {
        levels <- strat_details$levels
        if (is.null(levels) || !length(levels)) {
          values <- data[[group_var]]
          values <- values[!is.na(values)]
          strata_levels <- unique(as.character(values))
        } else {
          strata_levels <- levels
        }
      }

      processed_data <- data[, unique(c(selected_vars, group_var)), drop = FALSE]

      if (!is.null(group_var)) {
        keep_rows <- !is.na(processed_data[[group_var]]) &
          as.character(processed_data[[group_var]]) %in% strata_levels
        processed_data <- processed_data[keep_rows, , drop = FALSE]
        processed_data[[group_var]] <- factor(
          as.character(processed_data[[group_var]]),
          levels = strata_levels
        )
      }

      if (is.null(group_var)) {
        dat <- processed_data[, selected_vars, drop = FALSE]
        matrices <- list(Overall = cor(dat, use = "pairwise.complete.obs"))
        plots <- list(Overall = build_ggpairs_object(dat))
      } else {
        split_data <- lapply(strata_levels, function(level) {
          dat <- processed_data[processed_data[[group_var]] == level, selected_vars, drop = FALSE]
          if (nrow(dat) == 0) return(NULL)
          dat
        })
        names(split_data) <- strata_levels

        matrices <- lapply(split_data, function(dat) {
          if (is.null(dat)) return(NULL)
          suppressWarnings(cor(dat, use = "pairwise.complete.obs"))
        })

        plots <- lapply(split_data, function(dat) {
          if (is.null(dat)) return(NULL)
          build_ggpairs_object(dat)
        })
      }

      correlation_store(list(
        matrices = matrices,
        plots = plots,
        group_var = group_var,
        selected_vars = selected_vars,
        data_used = processed_data,
        strata_levels = if (!is.null(group_var)) strata_levels else NULL
      ))
    })

    output$summary <- renderPrint({
      results <- correlation_store()
      if (is.null(results)) {
        return(invisible(NULL))
      }

      if (!is.null(results$message)) {
        cat(results$message)
        return(invisible(NULL))
      }

      matrices <- results$matrices
      if (is.null(matrices) || length(matrices) == 0) {
        return(invisible(NULL))
      }

      multiple <- length(matrices) > 1
      for (name in names(matrices)) {
        mat <- matrices[[name]]
        if (multiple) {
          cat(sprintf("=== Stratum: %s ===\n", name))
        }
        if (is.null(mat)) {
          cat("  No data available for this stratum.\n\n")
        } else {
          print(round(mat, 2))
          cat("\n")
        }
      }
    })
    
    # ---- Download results ----
    output$download_model <- downloadHandler(
      filename = function() paste0("Correlation_results_", Sys.Date(), ".txt"),
      content = function(file) {
        res <- correlation_store()
        if (is.null(res)) return()
        sink(file)
        on.exit(sink(), add = TRUE)
        
        if (!is.null(res$message)) {
          cat(res$message, "\n")
          return()
        }
        
        matrices <- res$matrices
        if (is.null(matrices) || length(matrices) == 0) {
          cat("No correlation matrices available.\n")
          return()
        }
        
        multiple <- length(matrices) > 1
        for (nm in names(matrices)) {
          mat <- matrices[[nm]]
          if (multiple) cat(sprintf("=== Stratum: %s ===\n", nm))
          if (is.null(mat)) {
            cat("No data available for this stratum.\n\n")
          } else {
            print(round(mat, 3))
            cat("\n")
          }
        }

      }
    )

    # ---- Return structured output for visualization ----
    df_final <- reactive({
      res <- correlation_store()
      req(res)
      res$data_used
    })

    model_fit <- reactive({
      res <- correlation_store()
      req(res)
      res$matrices
    })

    summary_table <- model_fit

    posthoc_results <- reactive(NULL)

    effect_table <- reactive(NULL)

    reactive({
      res <- correlation_store()
      if (is.null(res)) return(NULL)

      data_used <- df_final()

      list(
        analysis_type = "CORR",
        type = "pairs",
        data_used = data_used,
        model = model_fit(),
        summary = summary_table(),
        posthoc = posthoc_results(),
        effects = effect_table(),
        stats = if (!is.null(data_used)) list(n = nrow(data_used), vars = names(data_used)) else NULL,
        selected_vars = res$selected_vars,
        group_var = res$group_var,
        strata_levels = res$strata_levels,
        plots = res$plots,
        message = res$message,
        group_var_reactive = reactive({
          res <- correlation_store()
          req(res)
          res$group_var
        }),
        strata_order = reactive({
          res <- correlation_store()
          req(res)
          res$strata_levels
        }),
        results = reactive(correlation_store())
      )
    })
  })
}
