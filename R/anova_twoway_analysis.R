# ===============================================================
# 🧪 Table Analyzer — Two-way ANOVA Module (Validated Version)
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
          downloadButton(ns("download_all"), "Download results", style = "width: 100%;"),
          "Save all ANOVA tables, post-hoc results, and diagnostics to disk."
        ))
      )
    ),
    results = uiOutput(ns("summary_ui"))
  )
}

two_way_anova_server <- function(id, filtered_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    responses <- multi_response_server("response", filtered_data)
    strat_info <- stratification_server("strat", filtered_data)
    
    # ------------------------------------------------------------
    # Dynamic inputs
    # ------------------------------------------------------------
    output$inputs <- renderUI({
      req(filtered_data())
      data <- filtered_data()
      
      cat_cols <- names(data)[sapply(data, function(x) is.factor(x) || is.character(x))]
      
      validate(
        need(length(cat_cols) > 1,
             "At least two categorical predictors are required for a two-way ANOVA.")
      )
      
      tagList(
        multi_response_ui(ns("response")),
        
        with_help_tooltip(
          selectInput(
            ns("factor1"),
            "Categorical predictor 1 (x-axis)",
            choices = cat_cols,
            selected = cat_cols[1]
          ),
          "Select the factor for the x-axis groups in the interaction plot."
        ),
        
        with_help_tooltip(
          selectInput(
            ns("factor2"),
            "Categorical predictor 2 (lines)",
            choices = cat_cols,
            selected = cat_cols[2]
          ),
          "Select the factor for the lines in the interaction plot."
        )
      )
    })
    
    # ------------------------------------------------------------
    # Level order inputs
    # ------------------------------------------------------------
    output$level_order_1 <- renderUI({
      req(filtered_data(), input$factor1)
      lev <- resolve_order_levels(filtered_data()[[input$factor1]])
      with_help_tooltip(
        selectInput(
          ns("order1"),
          paste("Order of levels (first = reference)", input$factor1),
          choices = lev,
          selected = lev,
          multiple = TRUE
        ),
        sprintf("Arrange the levels of %s. The first is the reference.", input$factor1)
      )
    })
    
    output$level_order_2 <- renderUI({
      req(filtered_data(), input$factor2)
      lev <- resolve_order_levels(filtered_data()[[input$factor2]])
      with_help_tooltip(
        selectInput(
          ns("order2"),
          paste("Order of levels (first = reference)", input$factor2),
          choices = lev,
          selected = lev,
          multiple = TRUE
        ),
        sprintf("Arrange the levels of %s. The first is the reference.", input$factor2)
      )
    })
    
    # ------------------------------------------------------------
    # Main model fitting
    # ------------------------------------------------------------
    models <- eventReactive(input$run, {
      df <- filtered_data()
      req(df, input$factor1, input$factor2, input$order1, input$order2)
      
      resp_vals <- responses()
      validate(
        need(length(resp_vals) > 0, "Select at least one response variable.")
      )
      
      validate(
        need(!identical(input$factor1, input$factor2),
             "The two categorical predictors must be different variables.")
      )
      
      # Factor 1 must have ≥ 2 levels
      validate(
        need(dplyr::n_distinct(df[[input$factor1]]) > 1,
             paste0("'", input$factor1, "' must contain at least two levels."))
      )
      
      # Factor 2 must have ≥ 2 levels
      validate(
        need(dplyr::n_distinct(df[[input$factor2]]) > 1,
             paste0("'", input$factor2, "' must contain at least two levels."))
      )
      
      # Order must contain ≥ 2 levels
      validate(
        need(length(input$order1) > 1,
             paste0("The level order for '", input$factor1, "' must contain at least two levels."))
      )
      
      validate(
        need(length(input$order2) > 1,
             paste0("The level order for '", input$factor2, "' must contain at least two levels."))
      )
      
      # Orders must match existing data levels
      validate(
        need(all(input$order1 %in% unique(df[[input$factor1]])),
             "Invalid level order for the first factor.")
      )
      
      validate(
        need(all(input$order2 %in% unique(df[[input$factor2]])),
             "Invalid level order for the second factor.")
      )
      
      # Numeric responses
      validate_numeric_columns(df, resp_vals, "response variables")
      
      # Response variance > 0
      for (r in resp_vals) {
        validate(
          need(stats::var(df[[r]], na.rm = TRUE) > 0,
               paste("Response", r, "has zero variance and cannot be analyzed."))
        )
      }
      
      # Stratification: each stratum must contain ≥ 2 levels for each factor
      if (!is.null(strat_info()$active) && strat_info()$active) {
        s <- strat_info()
        for (lev in s$levels) {
          sub <- df[df[[s$var]] == lev, ]
          
          validate(
            need(dplyr::n_distinct(sub[[input$factor1]]) > 1,
                 paste0("Stratum '", lev, "' contains fewer than two levels of ", input$factor1, "."))
          )
          
          validate(
            need(dplyr::n_distinct(sub[[input$factor2]]) > 1,
                 paste0("Stratum '", lev, "' contains fewer than two levels of ", input$factor2, "."))
          )
        }
      }
      
      # --------
      # Fit model
      # --------
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
    
    # ------------------------------------------------------------
    # Download
    # ------------------------------------------------------------
    output$download_all <- downloadHandler(
      filename = function() {
        info <- models()
        req(info)
        
        n_resp <- length(info$responses)
        n_strata <- length(info$strata$levels %||% NULL)
        label <- ifelse(n_strata == 0, "nostratum", paste0(n_strata, "strata"))
        paste0("anova_results_", n_resp, "resp_", label, "_",
               format(Sys.time(), "%Y%m%d-%H%M"), ".docx")
      },
      content = function(file) download_all_anova_results(models(), file)
    )
    
    # ------------------------------------------------------------
    # Results UI
    # ------------------------------------------------------------
    output$summary_ui <- renderUI({
      render_anova_results(ns, models(), "Two-way ANOVA")
    })
    
    bind_anova_outputs(ns, output, models)
    
    # ------------------------------------------------------------
    # Exportable results object
    # ------------------------------------------------------------
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
