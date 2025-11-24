# ===============================================================
# 🧪 Table Analyzer — One-way ANOVA Module (Clean & Minimal)
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
            downloadButton(ns("download_all"), "Download all results", style = "width: 100%;"),
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
    
    output$inputs <- renderUI({
      req(filtered_data())
      data <- filtered_data()
      cat_cols <- names(data)[sapply(data, is.factor) | sapply(data, is.character)]
      
      tagList(
        multi_response_ui(ns("response")),
        with_help_tooltip(
          selectInput(
            ns("group"),
            "Categorical predictor",
            choices = cat_cols,
            selected = if (length(cat_cols) > 0) cat_cols[1] else NULL
          ),
          "Choose the grouping variable that defines the comparison categories."
        )
      )
    })
    
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
    
    models <- eventReactive(input$run, {
      df <- filtered_data()
      req(df, input$group, input$order)
      resp_vals <- responses()
      validate(need(length(resp_vals) > 0, "Select at least one response variable."))
      validate_numeric_columns(df, resp_vals, "response variables")
      prepare_stratified_anova(
        df = df,
        responses = resp_vals,
        model = "oneway_anova",
        factor1_var = input$group,
        factor1_order = input$order,
        stratification = strat_info()
      )
    })
    
    output$download_all <- downloadHandler(
      filename = function() {
        info <- models()
        n_resp <- length(info$responses)
        n_strata <- length(info$strata$levels %||% NULL)
        label <- ifelse(n_strata == 0, "nostratum", paste0(n_strata, "strata"))
        paste0("anova_results_", n_resp, "resp_", label, "_", format(Sys.time(), "%Y%m%d-%H%M"), ".docx")
      },
      content = function(file) download_all_anova_results(models(), file)
    )
    
    output$summary_ui <- renderUI({
      render_anova_results(ns, models(), "One-way ANOVA")
    })
    
    bind_anova_outputs(ns, output, models)
    
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
