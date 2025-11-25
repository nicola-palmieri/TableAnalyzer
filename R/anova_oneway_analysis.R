# ===============================================================
# 🧪 Table Analyzer — One-way ANOVA Module (Validated Version)
# ===============================================================

one_way_anova_ui <- function(id) {
  ns <- NS(id)
  list(
    config = tagList(
      uiOutput(ns("inputs")),
      uiOutput(ns("level_order")),
      tags$details(
        tags$summary(strong("Advanced options")),
        stratification_ui("strat", ns)
      ),
      br(),
      fluidRow(
        column(
          6,
          with_help_tooltip(
            actionButton(ns("run"), "Show results", width = "100%"),
            "Run the ANOVA using the selected response and group variable."
          )
        ),
        column(
          6,
          with_help_tooltip(
            downloadButton(ns("download_all"), "Download results", style = "width: 100%;"),
            "Export the ANOVA summaries, post-hoc tests, and diagnostics."
          )
        )
      )
    ),
    results = uiOutput(ns("summary_ui"))
  )
}

one_way_anova_server <- function(id, filtered_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    responses <- multi_response_server("response", filtered_data)
    strat_info <- stratification_server("strat", filtered_data)
    
    # -----------------------------------------------------
    # UI inputs
    # -----------------------------------------------------
    output$inputs <- renderUI({
      req(filtered_data())
      data <- filtered_data()
      
      cat_cols <- names(data)[sapply(data, is.factor) | sapply(data, is.character)]
      
      validate(
        need(length(cat_cols) > 0,
             "No categorical predictors found. At least one factor or character variable is required.")
      )
      
      tagList(
        multi_response_ui(ns("response")),
        with_help_tooltip(
          selectInput(
            ns("group"),
            "Categorical predictor",
            choices = cat_cols,
            selected = cat_cols[1]
          ),
          "Choose the grouping variable that defines the comparison categories."
        )
      )
    })
    
    # -----------------------------------------------------
    # Order of levels
    # -----------------------------------------------------
    output$level_order <- renderUI({
      req(filtered_data(), input$group)
      levels <- resolve_order_levels(filtered_data()[[input$group]])
      
      with_help_tooltip(
        selectInput(
          ns("order"),
          "Order of levels (first = reference)",
          choices = levels,
          selected = levels,
          multiple = TRUE
        ),
        "Arrange the group levels; the first level is used as the reference in outputs."
      )
    })
    
    # -----------------------------------------------------
    # Run models
    # -----------------------------------------------------
    models <- eventReactive(input$run, {
      df <- filtered_data()
      req(df, input$group, input$order)
      
      resp_vals <- responses()
      
      # ------------------------
      # Validations
      # ------------------------
      
      validate(
        need(length(resp_vals) > 0,
             "Select at least one response variable.")
      )
      
      validate(
        need(all(sapply(df[resp_vals], is.numeric)),
             "All selected response variables must be numeric.")
      )
      
      # Response variance
      for (r in resp_vals) {
        validate(
          need(stats::var(df[[r]], na.rm = TRUE) > 0,
               paste("Response", r, "has zero variance and cannot be analyzed."))
        )
      }
      
      # Group must have >= 2 levels
      grp <- df[[input$group]]
      validate(
        need(dplyr::n_distinct(grp) > 1,
             "The categorical predictor must contain at least two distinct levels.")
      )
      
      # Order must have >= 2 items
      validate(
        need(length(input$order) > 1,
             "The order list must contain at least two levels.")
      )
      
      # Order must match data
      actual_levels <- unique(grp)
      missing_levels <- setdiff(input$order, actual_levels)
      validate(
        need(length(missing_levels) == 0,
             paste("Some selected levels are not present in the filtered data:",
                   paste(missing_levels, collapse = ", ")))
      )
      
      # Stratification checks
      if (!is.null(strat_info()$active) && strat_info()$active) {
        s_info <- strat_info()
        
        for (lev in s_info$levels) {
          sub_df <- df[df[[s_info$var]] == lev, ]
          
          k <- dplyr::n_distinct(sub_df[[input$group]])
          validate(
            need(k > 1,
                 paste0("Stratum '", lev,
                        "' contains less than two levels of the grouping variable."))
          )
        }
      }
      
      # -----------------------------------------------------
      # Run the main stratified ANOVA preparation
      # -----------------------------------------------------
      prepare_stratified_anova(
        df = df,
        responses = resp_vals,
        model = "oneway_anova",
        factor1_var = input$group,
        factor1_order = input$order,
        stratification = strat_info()
      )
    })
    
    # -----------------------------------------------------
    # Download handler
    # -----------------------------------------------------
    output$download_all <- downloadHandler(
      filename = function() {
        info <- models()
        n_resp <- length(info$responses)
        n_strata <- length(info$strata$levels %||% NULL)
        label <- ifelse(n_strata == 0, "nostratum", paste0(n_strata, "strata"))
        paste0("anova_results_", n_resp, "resp_", label, "_",
               format(Sys.time(), "%Y%m%d-%H%M"), ".docx")
      },
      content = function(file) download_all_anova_results(models(), file)
    )
    
    # -----------------------------------------------------
    # Summary UI
    # -----------------------------------------------------
    output$summary_ui <- renderUI({
      render_anova_results(ns, models(), "One-way ANOVA")
    })
    
    # Forwarding other outputs
    bind_anova_outputs(ns, output, models)
    
    # -----------------------------------------------------
    # Exportable ANOVA results object
    # -----------------------------------------------------
    anova_results <- reactive({
      mod <- models()
      req(mod)
      res <- compile_anova_results(mod)
      
      list(
        analysis_type = "ANOVA",
        type = "oneway_anova",
        data_used = mod$data_used,
        model = mod$models,
        summary = res$summary,
        posthoc = res$posthoc,
        effects = res$effects,
        stats = list(
          n = nrow(mod$data_used),
          vars = names(mod$data_used)
        ),
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
