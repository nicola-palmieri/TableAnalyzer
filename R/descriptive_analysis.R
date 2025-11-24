# ===============================================================
# 🧾 Table Analyzer — Descriptive Statistics Modules
# ===============================================================

descriptive_ui <- function(id) {
  ns <- NS(id)
  list(
    config = tagList(
      uiOutput(ns("inputs")),
      tags$details(
        tags$summary(strong("Advanced options")),
        stratification_ui("strat", ns)
      ),
      br(),
      fluidRow(
        column(6, with_help_tooltip(
          actionButton(ns("run"), "Show summary", width = "100%"),
          "Calculate the descriptive statistics for the selected variables."
        )),
        column(6, with_help_tooltip(
          downloadButton(ns("download_summary"), "Download summary", style = "width: 100%;"),
          "Save the displayed summary as a text file for later reference."
        ))
      ),
      hr()
    ),
    results = tagList(verbatimTextOutput(ns("summary_text")))
  )
}

descriptive_server <- function(id, filtered_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    df <- filtered_data

    # ------------------------------------------------------------
    # Dynamic inputs
    # ------------------------------------------------------------
    output$inputs <- renderUI({
      req(df())
      data <- df()
      cat_cols <- names(data)[vapply(data, function(x) is.character(x) || is.factor(x) || is.logical(x), logical(1))]
      num_cols <- names(data)[vapply(data, is.numeric, logical(1))]
      
      tagList(
        with_help_tooltip(
          selectInput(ns("cat_vars"), label = "Categorical variables", choices = cat_cols, selected = cat_cols, multiple = TRUE),
          "Choose the group variables whose counts and proportions you want to inspect."
        ),
        with_help_tooltip(
          selectInput(ns("num_vars"), label = "Numeric variables", choices = num_cols, selected = num_cols, multiple = TRUE),
          "Choose the numeric measurements you want to summarise (mean, SD, etc.)."
        )
      )
    })
    
    strat_info <- stratification_server("strat", df)
    
    # ------------------------------------------------------------
    # Summary computation
    # ------------------------------------------------------------
    summary_data <- eventReactive(input$run, {
      req(df())

      local_data <- df()
      selected_vars <- unique(c(input$cat_vars, input$num_vars))
      validate(need(length(selected_vars) > 0, "Please select at least one variable."))

      strat_details <- strat_info()
      group_var <- strat_details$var
      data_columns <- selected_vars

      if (!is.null(group_var)) {
        sel <- strat_details$levels
        if (!is.null(sel) && length(sel) > 0) {
          local_data <- dplyr::filter(local_data, .data[[group_var]] %in% sel)
          local_data[[group_var]] <- factor(as.character(local_data[[group_var]]), levels = sel)
        } else {
          local_data[[group_var]] <- factor(as.character(local_data[[group_var]]))
        }
        local_data <- droplevels(local_data)
        data_columns <- unique(c(data_columns, group_var))
      }

      data_columns <- data_columns[!is.na(data_columns) & nzchar(data_columns)]
      data_columns <- intersect(data_columns, names(local_data))
      local_data <- local_data[, data_columns, drop = FALSE]

      selected_vars <- selected_vars[!is.na(selected_vars) & nzchar(selected_vars)]
      selected_vars <- intersect(selected_vars, names(local_data))

      list(
        summary = compute_descriptive_summary(local_data, group_var),
        selected_vars = selected_vars,
        group_var = group_var,
        processed_data = local_data,
        strata_levels = if (!is.null(group_var) && group_var %in% names(local_data)) levels(local_data[[group_var]]) else NULL
      )
    })
    
    
    
    
    # ------------------------------------------------------------
    # Print summary
    # ------------------------------------------------------------
    output$summary_text <- renderPrint({
      req(summary_data())
      print_summary_sections(summary_data()$summary)
    })
    
    # ------------------------------------------------------------
    # Download
    # ------------------------------------------------------------
    output$download_summary <- downloadHandler(
      filename = function() paste0("Descriptive_Statistics_", Sys.Date(), ".txt"),
      content = function(file) {
        results <- summary_data()
        req(results)
        sink(file)
        on.exit(sink(), add = TRUE)
        print_summary_sections(results$summary)
      }
    )
    
    # ------------------------------------------------------------
    # Return full model info
    # ------------------------------------------------------------
    extract_detail <- function(field) reactive({
      details <- summary_data()
      req(details)
      details[[field]]
    })

    df_final <- extract_detail("processed_data")
    summary_table <- extract_detail("summary")
    selected_vars_reactive <- extract_detail("selected_vars")
    group_var_reactive <- extract_detail("group_var")
    strata_levels_reactive <- extract_detail("strata_levels")

    reactive({
      details <- summary_data()
      req(details)

      data_used <- df_final()

        list(
          analysis_type = "DESCRIPTIVE",
          type = "descriptive",
          data_used = data_used,
          model = NULL,
          summary = summary_table(),
          posthoc = NULL,
          effects = NULL,
          stats = if (!is.null(data_used)) list(n = nrow(data_used), vars = names(data_used)) else NULL,
          selected_vars = details$selected_vars,
          group_var = details$group_var,
          strata_levels = details$strata_levels,
          processed_data = df_final,
          selected_vars_reactive = selected_vars_reactive,
          group_var_reactive = group_var_reactive,
          strata_levels_reactive = strata_levels_reactive
      )
    })

  })
}

compute_descriptive_summary <- function(data, group_var = NULL) {
  numeric_vars <- names(data)[sapply(data, is.numeric)]
  group_data <- if (!is.null(group_var)) dplyr::group_by(data, .data[[group_var]], .drop = TRUE) else data

  summarise_numeric <- function(.data, vars, suffix, fn) {
    if (length(vars) == 0) {
      return(NULL)
    }
    .data %>%
      dplyr::summarise(
        dplyr::across(
          dplyr::all_of(vars),
          fn,
          .names = paste0(suffix, "_{.col}")
        ),
        .groups = "drop"
      )
  }

  # Define a custom skim function with full factor labels
  top_counts_full <- function(x) skimr::top_counts(x, max_char = 200, max_levels = 10)
  skim_full <- skimr::skim_with(
    factor = skimr::sfl(
      ordered    = is.ordered,
      n_unique   = skimr::n_unique,
      top_counts = top_counts_full
    ),
    append = FALSE
  )
  
  list(
    skim = if (!is.null(group_var)) skim_full(group_data) else skim_full(data),
    cv = summarise_numeric(
      group_data,
      numeric_vars,
      "cv",
      ~ 100 * stats::sd(.x, na.rm = TRUE) / mean(.x, na.rm = TRUE)
    ),
    outliers = summarise_numeric(
      group_data,
      numeric_vars,
      "outliers",
      ~ {
        q <- stats::quantile(.x, probs = c(0.25, 0.75), na.rm = TRUE)
        iqr <- q[2] - q[1]
        sum(.x < q[1] - 1.5 * iqr | .x > q[2] + 1.5 * iqr, na.rm = TRUE)
      }
    ),
    missing = summarise_numeric(
      group_data,
      numeric_vars,
      "missing",
      ~ 100 * mean(is.na(.x))
    ),
    distribution = summarise_numeric(
      group_data,
      numeric_vars,
      "distribution",
      ~ most_likely_distribution(.x)
    )
  )
}

most_likely_distribution <- function(values) {
  values <- values[is.finite(values)]
  if (length(values) < 5 || length(unique(values)) < 2) {
    return(NA_character_)
  }
  
  candidates <- c(
    norm   = TRUE,
    lnorm  = all(values > 0),
    gamma  = all(values > 0),
    weibull = all(values > 0),
    exp    = all(values > 0)
  )
  
  candidate_names <- names(candidates)[candidates]
  if (length(candidate_names) == 0) {
    return(NA_character_)
  }
  
  fits <- purrr::map(candidate_names, function(dist_name) {
    fit <- safe_fitdist(values, dist_name)
    if (is.null(fit)) return(NULL)
    tibble::tibble(distribution = dist_name, aic = stats::AIC(fit))
  })
  
  fits <- purrr::compact(fits)
  if (length(fits) == 0) return(NA_character_)
  
  best <- dplyr::bind_rows(fits) |>
    dplyr::arrange(.data$aic) |>
    dplyr::slice(1)
  
  label_map <- c(
    norm = "Normal",
    lnorm = "Log-normal",
    gamma = "Gamma",
    weibull = "Weibull",
    exp = "Exponential"
  )
  
  label_map[[best$distribution]] %||% best$distribution
}

safe_fitdist <- function(values, dist_name) {
  suppressWarnings(
    tryCatch(
      fitdistrplus::fitdist(values, dist_name),
      error = function(e) NULL
    )
  )
}

# ---- Shared printing ----
print_summary_sections <- function(results) {
  # 1) Print skim AS-IS (unchanged)
  cat(paste(capture.output(print(results$skim)), collapse = "\n"), "\n\n", sep = "")
  
  # 2) Helper to detect if a grouping column exists and what it's called
  metric_prefix <- "^(cv_|outliers_|missing_|distribution_)"
  first_col <- if (!is.null(results$cv) && ncol(results$cv) > 0) names(results$cv)[1] else NULL
  group_col <- if (!is.null(first_col) && !grepl(metric_prefix, first_col)) first_col else NULL
  
  # 3) Robust long conversion that preserves the real group column name (if any)
  to_long <- function(df, value_name) {
    if (is.null(df) || ncol(df) == 0) {
      empty_cols <- list(variable = character())
      if (!is.null(group_col)) {
        empty_cols[[group_col]] <- character()
      }
      empty_cols[[value_name]] <- numeric()
      return(tibble::as_tibble(empty_cols))
    }
    id_cols <- intersect(names(df), group_col)
    df |>
      tidyr::pivot_longer(
        cols = -tidyselect::any_of(id_cols),
        names_to = "variable",
        values_to = value_name
      ) |>
      dplyr::mutate(variable = sub(metric_prefix, "", .data$variable))
  }

  # 4) Build pieces (no "missing" here)
  cv_long   <- to_long(results$cv, "cv")
  out_long  <- to_long(results$outliers, "outliers")
  dist_long <- to_long(results$distribution, "distribution")

  # 5) Join by the right keys
  join_keys <- c(if (!is.null(group_col)) group_col, "variable")
  merged <- dplyr::full_join(cv_long, out_long, by = join_keys) |>
    dplyr::full_join(dist_long, by = join_keys) |>
    dplyr::mutate(
      cv = round(cv, 2)
    )

  numeric_order <- NULL
  if (is.data.frame(results$skim) && all(c("skim_type", "skim_variable") %in% names(results$skim))) {
    numeric_order <- results$skim |>
      dplyr::filter(.data$skim_type == "numeric") |>
      dplyr::pull(.data$skim_variable) |>
      unique()
  }

  arrange_cols <- c(if (!is.null(group_col)) group_col, "variable")
  if (!is.null(numeric_order) && length(numeric_order) > 0) {
    merged$variable <- factor(merged$variable, levels = numeric_order)
    merged <- dplyr::arrange(merged, !!!rlang::syms(arrange_cols))
    merged$variable <- as.character(merged$variable)
  } else {
    merged <- dplyr::arrange(merged, !!!rlang::syms(arrange_cols))
  }

  # 7) Print with/without group column
  cat("── Numeric variables summary ──\n")
  final_cols <- c("variable", arrange_cols[-length(arrange_cols)], "cv", "outliers", "distribution")
  final_df <- merged[, final_cols, drop = FALSE]
  print(as.data.frame(final_df), row.names = FALSE)

  cat("\nInterpretation:\n")
  cat("  • outliers = # beyond 1.5×IQR\n")
  cat("  • distribution = best fit (AIC) among Normal/Log-normal/Gamma/Weibull/Exponential\n")

  invisible(NULL)
}
