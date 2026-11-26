# mod_descriptive: server coordination and reactive state.
descriptive_server <- function(id, filtered_data, reset_trigger = reactive(NULL)) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    df <- filtered_data
    require_current <- analysis_download_guard(df, reactive(input$run), reset_trigger)

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
      
      # Raw data
      local_data <- df()
      
      # -----------------------------
      # Validate selected variables
      # -----------------------------
      selected_vars <- unique(c(input$cat_vars, input$num_vars))
      validate(
        need(length(selected_vars) > 0, "Please select at least one variable.")
      )
      
      # -----------------------------
      # Apply stratification
      # -----------------------------
      strat_details <- strat_info()
      group_var <- strat_details$var
      
      if (!is.null(group_var)) {
        sel <- strat_details$levels
        
        if (!is.null(sel) && length(sel) > 0) {
          local_data <- dplyr::filter(local_data, .data[[group_var]] %in% sel)
          local_data[[group_var]] <- factor(as.character(local_data[[group_var]]), levels = sel)
        } else {
          local_data[[group_var]] <- factor(as.character(local_data[[group_var]]))
        }
        
        local_data <- droplevels(local_data)
      }
      
      # -----------------------------
      # Validate after stratification
      # -----------------------------
      validate(
        need(nrow(local_data) > 0,
             "No data available after applying stratification.")
      )
      
      # Keep only selected variables (plus stratification column for grouping)
      selected_vars <- intersect(selected_vars, names(local_data))
      keep_cols <- unique(c(selected_vars, group_var))
      keep_cols <- intersect(keep_cols, names(local_data))
      validate(
        need(length(selected_vars) > 0,
             "None of the selected variables are present after filtering.")
      )
      
      local_data <- local_data[, keep_cols, drop = FALSE]
      
      # -----------------------------
      # Validate categorical variables levels
      # -----------------------------
      cat_vars <- intersect(input$cat_vars, names(local_data))
      if (length(cat_vars) > 0) {
        for (v in cat_vars) {
          validate(
            need(nlevels(as.factor(local_data[[v]])) > 0,
                 paste0("Variable '", v, "' contains no valid levels in the filtered data."))
          )
        }
      }
      
      # -----------------------------
      # Build the result
      # -----------------------------
      list(
        summary = compute_descriptive_summary(local_data, group_var),
        selected_vars = selected_vars,
        group_var = group_var,
        processed_data = local_data,
        strata_levels = if (!is.null(group_var) && group_var %in% names(local_data))
          levels(local_data[[group_var]])
        else NULL
      )
    })
    
    
    
    
    
    # ------------------------------------------------------------
    # Print summary
    # ------------------------------------------------------------
    output$summary_text <- renderPrint({
      req(summary_data())
      digits <- input$digits %||% 3
      print_summary_sections(summary_data()$summary, digits = digits)
    })

    observeEvent(input$summary_help, {
      showModal(
        modalDialog(
          title = "Interpreting descriptive statistics",
          easyClose = TRUE,
          size = "l",
          tagList(
            tags$p(
              class = "text-muted",
              "Use this guide to interpret the summary fields shown below."
            ),
            div(
              class = "ta-modal-grid",
              fluidRow(
                column(
                  6,
                  div(
                    class = "ta-modal-block",
                    h4("Categorical variables"),
                    tags$ul(
                      tags$li("skim_variable: column name (plus group column if you stratified)."),
                      tags$li("n_missing: number of missing entries."),
                      tags$li("complete_rate: proportion of non-missing rows."),
                      tags$li("n_unique: how many categories."),
                      tags$li("n_singletons: categories seen only once."),
                      tags$li("top_counts: most common categories with their counts.")
                    )
                  )
                ),
                column(
                  6,
                  div(
                    class = "ta-modal-block",
                    h4("Numeric variables"),
                    tags$ul(
                      tags$li("skim_variable: column name (plus group column if you stratified)."),
                      tags$li("n_missing: number of missing entries."),
                      tags$li("complete_rate: proportion of non-missing rows."),
                      tags$li("mean: arithmetic average of non-missing values."),
                      tags$li("sd: standard deviation."),
                      tags$li("p0: minimum observed value."),
                      tags$li("p25: first quartile (25th percentile)."),
                      tags$li("p50: median (50th percentile)."),
                      tags$li("p75: third quartile (75th percentile)."),
                      tags$li("p100: maximum observed value."),
                      tags$li("hist: sparkline showing the distribution shape.")
                    )
                  )
                )
              ),
              div(
                class = "ta-modal-block",
                h4("Numeric summary metrics"),
                tags$ul(
                  tags$li("variable: column name (plus group column if you stratified)."),
                  tags$li("cv: variability as % of the mean (higher = more spread)."),
                  tags$li("outliers: number of values outside the usual range (beyond 1.5xIQR)."),
                  tags$li("distribution: best-fitting curve (Normal, Log-normal, Gamma, Weibull, Exponential)."),
                  tags$li("skewness: whether values lean left (<0) or right (>0)."),
                  tags$li("kurtosis: tail heaviness (>0 = heavier tails than Normal).")
                )
              )
            )
          )
        )
      )
    })
    
    # ------------------------------------------------------------
    # Download
    # ------------------------------------------------------------
    output$download_summary <- downloadHandler(
      filename = function() paste0("Descriptive_Statistics_", Sys.Date(), ".txt"),
      content = function(file) {
        require_current()
        results <- summary_data()
        req(results)
        sink(file)
        on.exit(sink(), add = TRUE)
        digits <- input$digits %||% 3
        print_summary_sections(results$summary, digits = digits)
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
