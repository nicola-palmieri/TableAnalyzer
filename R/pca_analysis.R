# ===============================================================
# 🧪 Table Analyzer — PCA Module
# ===============================================================

pca_ui <- function(id) {
  ns <- NS(id)
  list(
    config = tagList(
      uiOutput(ns("vars_ui")),
      tags$details(
        tags$summary(strong("Advanced options")),
        helpText(paste(
          "Stratify by is not available for PCA because the principal components are computed on the full numeric matrix.",
          "Splitting the data by groups would produce different coordinate systems, making the loadings and scores incomparable across groups."
        ))
      ),
      br(),
      fluidRow(
        column(6, with_help_tooltip(
          actionButton(ns("run_pca"), "Show PCA summary", width = "100%"),
          "Compute the principal components for the selected variables."
        )),
        column(6, with_help_tooltip(
          downloadButton(ns("download_all"), "Download results", style = "width: 100%;"),
          "Export the PCA summaries, loadings, and diagnostics to a text file."
        ))
      )
    ),
    results = tagList(
      h5("PCA summary and loadings"),
      verbatimTextOutput(ns("summary")),
      uiOutput(ns("excluded_rows_section"))
    )
  )
}

pca_server <- function(id, filtered_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    df <- reactive(filtered_data())

    # Dynamically populate numeric variable list (re-rendered with UI)
    output$vars_ui <- renderUI({
      data <- req(df())
      num_vars <- names(data)[vapply(data, is.numeric, logical(1))]

      with_help_tooltip(
        selectInput(ns("vars"), "Numeric variables", choices = num_vars, selected = num_vars, multiple = TRUE),
        "Pick the numeric variables whose combined patterns you want PCA to capture."
      )
    })

    selected_numeric_vars <- reactive({
      data <- req(df())
      numeric_vars <- names(data)[vapply(data, is.numeric, logical(1))]
      intersect(input$vars, numeric_vars)
    })

    has_enough_numeric_vars <- reactive(length(selected_numeric_vars()) > 1)

    `%||%` <- function(x, y) if (is.null(x)) y else x

    format_usage_line <- function(entry) {
      sprintf(
        "Rows used for PCA: %d of %d (excluded %d due to missing values in the selected variables).\n\n",
        entry$used_n %||% 0,
        entry$original_n %||% 0,
        entry$excluded_n %||% 0
      )
    }

    append_row_numbers <- function(rows, keep) {
      if (!any(!keep)) {
        return(NULL)
      }

      excluded_rows <- rows[!keep, , drop = FALSE]
      out <- cbind(
        data.frame(`Row #` = seq_len(nrow(rows))[!keep], check.names = FALSE),
        excluded_rows
      )
      rownames(out) <- NULL
      out
    }

    run_pca_on_subset <- function(subset_data, selected_vars) {
      if (is.null(subset_data) || nrow(subset_data) == 0) {
        return(list(model = NULL, data = subset_data, message = "No data available for PCA."))
      }

      numeric_subset <- subset_data[, selected_vars, drop = FALSE]
      complete_idx <- stats::complete.cases(numeric_subset)
      numeric_subset <- numeric_subset[complete_idx, , drop = FALSE]
      plot_data <- subset_data[complete_idx, , drop = FALSE]

      details <- list(
        original_n = nrow(subset_data),
        used_n = nrow(plot_data)
      )
      details$excluded_n <- details$original_n - details$used_n
      details$excluded_rows <- append_row_numbers(subset_data, complete_idx)

      if (nrow(numeric_subset) < 2) {
        return(c(
          list(
            model = NULL,
            data = plot_data,
            message = "Not enough complete observations to compute PCA."
          ),
          details
        ))
      }

      model <- purrr::safely(prcomp)(numeric_subset, center = TRUE, scale. = TRUE)

      if (!is.null(model$error)) {
        return(c(
          list(
            model = NULL,
            data = plot_data,
            message = conditionMessage(model$error),
            message_title = "PCA computation failed"
          ),
          details
        ))
      }

      c(
        list(
          model = model$result,
          data = plot_data,
          message = NULL
        ),
        details
      )
    }

    # Run PCA
    pca_result <- eventReactive(input$run_pca, {
      req(df())

      data <- df()
      validate(need(nrow(data) > 0, "No data available for PCA."))

      selected_vars <- selected_numeric_vars()
      validate(need(has_enough_numeric_vars(), "Select at least two numeric variables for PCA."))

      result <- run_pca_on_subset(data, selected_vars)

      list(
        selected_vars = selected_vars,
        result = result,
        data_used = result$data
      )
    })

    # Verbatim output: summary + loadings
    output$summary <- renderPrint({
      results <- pca_result()
      validate(need(!is.null(results), "Run the PCA analysis to view results."))

      entry <- results$result
      if (is.null(entry) || is.null(entry$model)) {
        if (!is.null(entry)) cat(format_usage_line(entry))

        if (!is.null(entry) && !is.null(entry$message) && nzchar(entry$message)) {
          if (!is.null(entry$message_title)) {
            cat(format_safe_error_message(entry$message_title, entry$message))
          } else {
            cat(entry$message)
          }
        } else {
          cat("Not enough data to compute PCA.")
        }
        return(invisible())
      }

      model <- entry$model
      cat(format_usage_line(entry))
      cat("── PCA Summary ──\n")
      print(summary(model))
      cat("\n── PCA Loadings (rotation matrix) ──\n")
      print(round(model$rotation, 3))
      cat("\n")

      invisible()
    })

    # Download combined results
    output$download_all <- downloadHandler(
      filename = function() paste0("PCA_results_", Sys.Date(), ".txt"),
      content = function(file) {
        results <- pca_result()
        req(results)

        sink(file)
        on.exit(sink(), add = TRUE)

        entry <- results$result
        if (!is.null(entry)) cat(format_usage_line(entry))

        if (is.null(entry) || is.null(entry$model)) {
          if (!is.null(entry) && !is.null(entry$message) && nzchar(entry$message)) {
            msg <- if (!is.null(entry$message_title)) {
              format_safe_error_message(entry$message_title, entry$message)
            } else {
              entry$message
            }
            cat(msg, "\n", sep = "")
          } else {
            cat("Not enough data to compute PCA.\n")
          }
          return()
        }

        model <- entry$model
        cat("── PCA Summary ──\n")
        print(summary(model))
        cat("\n── PCA Loadings (rotation matrix) ──\n")
        print(round(model$rotation, 3))
        cat("\n")
      }
    )

    analysis_result <- reactive({
      details <- pca_result()
      if (is.null(details)) {
        return(list(
          analysis_type = "PCA",
          type = "pca",
          data_used = df(),
          model = NULL,
          summary = NULL,
          posthoc = NULL,
          effects = NULL,
          stats = if (!is.null(df())) list(n = nrow(df()), vars = names(df())) else NULL,
          selected_vars = input$vars,
          group_var = NULL,
          strata_levels = NULL,
          complete_cases = NULL,
          excluded_rows = NULL,
          excluded_n = NULL,
          original_n = NULL,
          messages = NULL
        ))
      }

      entry <- details$result

      data_used <- details$data_used

      compiled <- NULL
      messages <- NULL
      if (!is.null(entry)) {
        messages <- if (!is.null(entry$message)) list(PCA = entry$message) else NULL

        if (!is.null(entry$model)) {
          model <- entry$model
          rotation_tbl <- as.data.frame(as.table(model$rotation), stringsAsFactors = FALSE)
          colnames(rotation_tbl) <- c("Variable", "Component", "Loading")

          variance_vals <- 100 * model$sdev^2 / sum(model$sdev^2)
          variance_tbl <- data.frame(
            Component = paste0("PC", seq_along(variance_vals)),
            Variance = variance_vals,
            stringsAsFactors = FALSE
          )

          compiled <- list(
            summary = list(PCA = rotation_tbl),
            effects = list(PCA = variance_tbl)
          )
        }
      }

      list(
        analysis_type = "PCA",
        type = "pca",
        data_used = data_used,
        model = entry,
        summary = if (!is.null(compiled)) compiled$summary else NULL,
        posthoc = NULL,
        effects = if (!is.null(compiled)) compiled$effects else NULL,
        stats = if (!is.null(data_used)) list(n = nrow(data_used), vars = names(data_used)) else NULL,
        selected_vars = details$selected_vars,
        group_var = NULL,
        strata_levels = NULL,
        complete_cases = if (!is.null(entry)) entry$data else NULL,
        excluded_rows = if (!is.null(entry)) entry$excluded_rows else NULL,
        excluded_n = if (!is.null(entry)) entry$excluded_n else NULL,
        original_n = if (!is.null(entry)) entry$original_n else NULL,
        messages = messages
      )
    })

    output$excluded_rows_section <- renderUI({
      req(has_enough_numeric_vars())
      results <- pca_result()
      req(results)

      entry <- results$result
      if (is.null(entry) || is.null(entry$excluded_n) || entry$excluded_n == 0) {
        return(tags$p("No rows were excluded when computing the PCA."))
      }

      tagList(
        h5(sprintf("Excluded rows (%d)", entry$excluded_n)),
        DT::DTOutput(ns("excluded_table"))
      )
    })

    output$excluded_table <- DT::renderDT({
      req(has_enough_numeric_vars())
      results <- pca_result()
      req(results)
      
      entry <- results$result
      req(entry$excluded_rows)
      
      DT::datatable(
        entry$excluded_rows,
        options = list(
          scrollX = TRUE,
          pageLength = 5,
          columnDefs = list(list(targets = "_all", className = "dt-nowrap"))
        ),
        rownames = FALSE,
        class = "compact stripe"
      )
    })
    

    analysis_result
  })
}
