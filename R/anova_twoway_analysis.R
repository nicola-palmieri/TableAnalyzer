# ===============================================================
# 🧪 Table Analyzer — Two-way ANOVA Module
# ===============================================================

two_way_anova_ui <- function(id) {
  ns <- NS(id)
  list(
    config = tagList(
      uiOutput(ns("inputs")),
      uiOutput(ns("level_order_1")),
      uiOutput(ns("level_order_2")),
      tags$details(
        tags$summary(strong("Advanced options")),
        stratification_ui("strat", ns)
      ),
      br(),
      fluidRow(
        column(6, with_help_tooltip(
          actionButton(ns("run"), "Show results", width = "100%"),
          "Fit the two-way ANOVA with the selected factors and responses."
        )),
        column(6, with_help_tooltip(
          downloadButton(ns("download_all"), "Download all results", style = "width: 100%;"),
          "Save all ANOVA tables, post-hoc results, and diagnostics to disk."
        ))
      )
    ),
    results = tagList(
      uiOutput(ns("summary_ui"))
    )
  )
}

two_way_anova_server <- function(id, filtered_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    responses <- multi_response_server("response", filtered_data)

    output$inputs <- renderUI({
      req(filtered_data())
      data <- filtered_data()
      cat_cols <- names(data)[sapply(data, function(x) is.character(x) || is.factor(x))]

      tagList(
        multi_response_ui(ns("response")),
        with_help_tooltip(
          selectInput(
            ns("factor1"),
            "Categorical predictor 1 (x-axis)",
            choices = cat_cols,
            selected = if (length(cat_cols) > 0) cat_cols[1] else NULL
          ),
          "Select the factor for the x-axis groups in the interaction plot."
        ),
        with_help_tooltip(
          selectInput(
            ns("factor2"),
            "Categorical predictor 2 (lines)",
            choices = cat_cols,
            selected = if (length(cat_cols) > 1) cat_cols[2] else NULL
          ),
          "Select the factor for the lines in the interaction plot."
        )
      )
    })
    
    strat_info <- stratification_server("strat", filtered_data)
    
    # -----------------------------------------------------------
    # Level order selections
    # -----------------------------------------------------------
    output$level_order_1 <- renderUI({
      req(filtered_data(), input$factor1)
      levels1 <- resolve_order_levels(filtered_data()[[input$factor1]])
      with_help_tooltip(
        selectInput(
          ns("order1"),
          paste("Order of levels (first = reference)", input$factor1, "(x-axis)"),
          choices = levels1,
          selected = levels1,
          multiple = TRUE
        ),
        sprintf("Arrange the levels of %s for the x-axis; the first level is the reference.", input$factor1)
      )
    })
    
    output$level_order_2 <- renderUI({
      req(filtered_data(), input$factor2)
      levels2 <- resolve_order_levels(filtered_data()[[input$factor2]])
      with_help_tooltip(
        selectInput(
          ns("order2"),
          paste("Order of levels (first = reference)", input$factor2, "(lines)"),
          choices = levels2,
          selected = levels2,
          multiple = TRUE
        ),
        sprintf("Arrange the levels of %s for the line colours; the first level is the reference.", input$factor2)
      )
    })
    
    # -----------------------------------------------------------
    # Model fitting (via shared helper)
    # -----------------------------------------------------------
    models <- eventReactive(input$run, {
      df <- filtered_data()
      req(df, input$factor1, input$order1, input$factor2, input$order2)
      resp_vals <- responses()
      validate(
        need(length(resp_vals) > 0, "Please select at least one response variable."),
        need(
          !identical(input$factor1, input$factor2),
          "Categorical predictor 1 and 2 must be different variables."
        ),
        need(all(input$order1 %in% unique(df[[input$factor1]])), "Invalid level order for first factor."),
        need(all(input$order2 %in% unique(df[[input$factor2]])), "Invalid level order for second factor.")
      )
      validate_numeric_columns(df, resp_vals, "response variables")
      prepare_stratified_anova(
        df = df,
        responses = resp_vals,
        model = "twoway_anova",
        factor1_var = input$factor1,
        factor1_order = input$order1,
        factor2_var = input$factor2,
        factor2_order = input$order2,
        stratification = strat_info()
      )
    })

    # -----------------------------------------------------------
    # Download all results as one combined DOCX
    # -----------------------------------------------------------
    output$download_all <- downloadHandler(
      filename = function() {
        model_info <- models()
        req(model_info)
        
        n_resp <- length(model_info$responses)
        n_strata <- if (is.null(model_info$strata)) 0 else length(model_info$strata$levels)
        strata_label <- ifelse(n_strata == 0, "nostratum", paste0(n_strata, "strata"))
        timestamp <- format(Sys.time(), "%Y%m%d-%H%M")
        sprintf("anova_results_%sresp_%s_%s.docx", n_resp, strata_label, timestamp)
      },
      content = function(file) {
        model_info <- models()
        req(model_info)
        download_all_anova_results(model_info, file)
      }
    )
    
    # -----------------------------------------------------------
    # Render results
    # -----------------------------------------------------------
    output$summary_ui <- renderUI({
      render_anova_results(ns, models(), "Two-way ANOVA")
    })
    
    # -----------------------------------------------------------
    # Render model summaries + downloads (shared helper)
    # -----------------------------------------------------------
    bind_anova_outputs(ns, output, models)

    anova_results <- reactive({
      mod <- models()
      req(mod)

      res <- compile_anova_results(mod)
      data_used <- mod$data_used

      list(
        analysis_type = "ANOVA",
        type = "twoway_anova",
        data_used = data_used,
        model = mod$models,
        summary = res$summary,
        posthoc = res$posthoc,
        effects = res$effects,
        stats = if (!is.null(data_used)) list(n = nrow(data_used), vars = names(data_used)) else NULL,
        errors = res$errors,
        responses = mod$responses,
        strata = mod$strata,
        factors = mod$factors,
        orders = mod$orders
      )
    })

    return(anova_results)
  })
}
