# ===============================================================
# ?? Regression module entrypoints
# ===============================================================
# Helper implementations live in regression_shared_*.R files.

regression_ui <- function(id, engine = c("lm", "lmm"), allow_multi_response = FALSE) {
  ns <- NS(id)
  engine <- match.arg(engine)
  allow_multi_response <- isTRUE(allow_multi_response)

  list(
    config = tagList(
      if (allow_multi_response) multi_response_ui(ns("response")) else uiOutput(ns("response_ui")),
      uiOutput(ns("fixed_selector")),
      uiOutput(ns("level_order")),
      uiOutput(ns("covar_selector")),
      if (engine == "lmm") uiOutput(ns("random_selector")),
      uiOutput(ns("interaction_select")),
      uiOutput(ns("formula_preview")),
      br(),
      tags$details(
        tags$summary(strong("Advanced options")),
        stratification_ui("strat", ns)
      ),
      br(),
      fluidRow(
        column(6, with_help_tooltip(
          actionButton(ns("run"), "Run analysis", width = "100%"),
          "Fit the model using the chosen predictors and options."
        )),
        column(6, with_help_tooltip(
          downloadButton(ns("download_model"), "Download results", style = "width: 100%;"),
          "Export the model outputs, tables, and summaries to your computer."
        ))
      )
    ),
    results = tagList(
      uiOutput(ns("results_ui"))
    )
  )
}

regression_server <- function(id, data, engine = c("lm", "lmm"), allow_multi_response = FALSE) {
  engine <- match.arg(engine)
  allow_multi_response <- isTRUE(allow_multi_response)

  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    strat_info <- stratification_server("strat", data)

    if (allow_multi_response) {
      selected_responses <- multi_response_server("response", data)
    } else {
      output$response_ui <- renderUI({
        req(data())
        types <- reg_detect_types(data())
        with_help_tooltip(
          selectInput(ns("dep"), "Response variable (numeric)", choices = types$num),
          "Choose the outcome that the model should predict."
        )
      })

      selected_responses <- reactive({
        req(input$dep)
        input$dep
      })
    }

    output$fixed_selector <- renderUI({
      req(data())
      types <- reg_detect_types(data())
      with_help_tooltip(
        selectInput(
          ns("fixed"),
          "Categorical predictors",
          choices = types$fac,
          multiple = TRUE
        ),
        "Pick factor variables that might explain differences in the response."
      )
    })

    output$level_order <- renderUI({
      req(data())
      req(input$fixed)

      df <- data()
      fac_vars <- input$fixed

      if (length(fac_vars) == 0) return(NULL)

      tagList(
        lapply(fac_vars, function(var) {
          values <- df[[var]]
          lvls <- resolve_order_levels(values)
          with_help_tooltip(
            selectInput(
              ns(paste0("order_", var)),
              paste("Order of levels (first = reference)", var),
              choices = lvls,
              selected = lvls,
              multiple = TRUE
            ),
            sprintf("Arrange the levels of %s; the first level becomes the model reference.", var)
          )
        })
      )
    })

    output$covar_selector <- renderUI({
      req(data())
      types <- reg_detect_types(data())
      with_help_tooltip(
        selectInput(
          ns("covar"),
          "Numeric predictors",
          choices = types$num,
          multiple = TRUE
        ),
        "Pick numeric predictors that could help explain the response."
      )
    })

    if (engine == "lmm") {
      output$random_selector <- renderUI({
        req(data())
        types <- reg_detect_types(data())
        with_help_tooltip(
          tagList(
            selectInput(
              ns("random"),
              "Random effect(s) (categorical)",
              choices = types$fac,
              multiple = TRUE
            ),
            checkboxInput(ns("random_nested"), "Nest random effects in selection order", value = FALSE)
          ),
          "Choose grouping factors for random intercepts in the mixed model. Select multiple to include several random effects, or nest them to model hierarchical structure."
        )
      })
    }

    output$interaction_select <- renderUI({
      req(data())
      types <- reg_detect_types(data())
      reg_interactions_ui(ns, input$fixed, types$fac)
    })

    output$formula_preview <- renderUI({
      responses <- selected_responses()
      req(length(responses) > 0)
      rhs <- reg_compose_rhs(
        input$fixed,
        input$covar,
        input$interactions,
        if (engine == "lmm") input$random else NULL,
        random_nested = if (engine == "lmm") isTRUE(input$random_nested) else FALSE,
        engine = engine
      )
      reg_formula_preview_ui(ns, responses[1], rhs)
    })

    models <- eventReactive(input$run, {
      req(data())
      df <- data()
      responses <- selected_responses()
      req(length(responses) > 0)
      validate_numeric_columns(df, responses, "response variable(s)")

      rhs <- reg_compose_rhs(
        input$fixed,
        input$covar,
        input$interactions,
        if (engine == "lmm") input$random else NULL,
        random_nested = if (engine == "lmm") isTRUE(input$random_nested) else FALSE,
        engine = engine
      )

      strat_details <- strat_info()
      
      validate_regression_inputs(df, input, engine, strat_details)

      if (engine == "lmm") {
        df <- sanitize_random_effects(df, input$random)
      }

      # ---- Apply user-specified factor level orders (controls reference levels) ----
      if (length(input$fixed) > 0) {
        df <- apply_fixed_level_orders(df, input$fixed, input)
      }
      
      fit_all_models(df, responses, rhs, strat_details, engine, allow_multi_response)
    })

    output$results_ui <- renderUI({
      mod <- models()
      req(mod)
      build_model_ui(ns, mod)
    })

    observeEvent(models(), {
      mod <- models()
      req(mod)
      render_model_outputs(output, mod, engine)
    }, ignoreNULL = FALSE)

    output$download_model <- downloadHandler(
      filename = function() {
        mod <- models()
        if (is.null(mod) || length(mod$flat_models) == 0) {
          return(paste0(engine, "_results_", Sys.Date(), ".docx"))
        }

        if (length(mod$flat_models) == 1) {
          entry <- mod$flat_models[[1]]
          parts <- c(engine, "results", entry$response)
          if (!is.null(entry$stratum)) parts <- c(parts, entry$stratum)
          paste0(paste(parts, collapse = "_"), "_", Sys.Date(), ".docx")
        } else {
          paste0(engine, "_all_results_", Sys.Date(), ".docx")
        }
      },
      content = function(file) {
        mod <- models()
        if (is.null(mod)) stop("No models available. Please run the analysis first.")
        flat_models <- mod$flat_models
        if (length(flat_models) == 0) stop("No models available. Please run the analysis first.")

        write_lm_docx_combined(flat_models, file)
      }
    )

    df_final <- reactive({
      data()
    })

    model_fit <- reactive({
      mod <- models()
      req(mod)
      mod$models
    })

    compiled_results <- reactive({
      mod <- models()
      req(mod)
      compile_regression_results(mod, engine)
    })

    summary_table <- reactive({
      res <- compiled_results()
      req(res)
      res$summary
    })

    effect_table <- reactive({
      res <- compiled_results()
      req(res)
      res$effects
    })

    error_table <- reactive({
      res <- compiled_results()
      req(res)
      res$errors
    })

    reactive({
      mod <- models()
      req(mod)

      data_used <- df_final()

      list(
        analysis_type = if (engine == "lm") "LM" else "LMM",
        type = if (engine == "lm") "lm" else "lmm",
        data_used = data_used,
        model = model_fit(),
        summary = summary_table(),
        posthoc = NULL,
        effects = effect_table(),
        stats = if (!is.null(data_used)) list(n = nrow(data_used), vars = names(data_used)) else NULL,
        responses = mod$responses,
        success_responses = mod$success_responses,
        error_responses = mod$error_responses,
        errors = mod$errors,
        stratification = mod$stratification,
        rhs = mod$rhs,
        allow_multi = mod$allow_multi,
        compiled_errors = error_table(),
        flat_models = mod$flat_models,
        engine = engine,
        message = mod$error
      )
    })
  })
}

