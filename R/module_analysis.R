# ===============================================================
# 🧪 Table Analyzer — Analysis Coordinator
# ===============================================================

analysis_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4(class = "ta-sidebar-title", "Step 3 - Analyze results"),
      p(class = "ta-sidebar-subtitle", "Select an analysis type to explore your data, then inspect the summaries on the right."),
      hr(),
      
      # --- CSS: expand dropdown height for better visibility ---
      tags$style(HTML(sprintf("
        #%s + .selectize-control .selectize-dropdown,
        #%s + .selectize-control .selectize-dropdown .selectize-dropdown-content {
          max-height: none !important;
        }
      ", ns("analysis_type"), ns("analysis_type")))),
      
      # --- Analysis type selector ---
      with_help_tooltip(
        selectInput(
          ns("analysis_type"),
          "Select analysis type",
          choices = list(
            " " = "",
            "Descriptive" = c("Descriptive Statistics" = "Descriptive Statistics"),
            "Univariate" = c(
              "One-way ANOVA" = "One-way ANOVA",
              "Two-way ANOVA" = "Two-way ANOVA",
              "Linear Model (LM)" = "Linear Model (LM)",
              "Linear Mixed Model (LMM)" = "Linear Mixed Model (LMM)"
            ),
            "Multivariate" = c(
              "Pairwise Correlation" = "Pairwise Correlation",
              "Principal Component Analysis (PCA)" = "PCA"
            )
          ),
          selected = ""
        ),
        "Choose the statistical method you want to run on the filtered data."
      ),
      uiOutput(ns("config_panel"))
    ),
    
    mainPanel(
      width = 8,
      div(
        class = "ta-results-header",
        h4("Analysis results"),
        uiOutput(ns("summary_help_icon"))
      ),
      uiOutput(ns("results_panel"))
    )
  )
}


analysis_server <- function(id, filtered_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    df <- reactive(filtered_data())
    has_rows_available <- reactive({
      data <- df()
      !is.null(data) && nrow(data) > 0
    })
    analysis_switch_token <- reactiveVal(0L)
    has_run <- reactiveVal(FALSE)
    run_baseline <- reactiveVal(0L)
    
    # ---- Mapping of available modules ----
    modules <- list(
      "Descriptive Statistics" = list(id = "desc",  ui = descriptive_ui, server = descriptive_server, type = "desc"),
      "One-way ANOVA"          = list(id = "anova1", ui = one_way_anova_ui, server = one_way_anova_server, type = "anova1"),
      "Two-way ANOVA"          = list(id = "anova2", ui = two_way_anova_ui, server = two_way_anova_server, type = "anova2"),
      "Linear Model (LM)"      = list(id = "lm",     ui = lm_ui, server = lm_server, type = "lm"),
      "Linear Mixed Model (LMM)" = list(id = "lmm",  ui = lmm_ui, server = lmm_server, type = "lmm"),
      "Pairwise Correlation"   = list(
        id = "pairs",
        ui = ggpairs_ui,
        server = ggpairs_server,
        type = "pairs"
      ),
      "PCA"                    = list(id = "pca",    ui = pca_ui, server = pca_server, type = "pca")
    )
    
    # ---- Cache for lazily created servers ----
    server_cache <- reactiveValues()
    
    # ---- Current module getter ----
    current_mod <- reactive({
      type <- input$analysis_type
      req(type)
      mod <- modules[[type]]
      req(mod)
    })
    
    ensure_module_server <- function(mod) {
      ensure_analysis_server(mod, df, server_cache, reset_trigger = analysis_switch_token)
    }

    run_suffix_map <- list(
      desc = "run",
      anova1 = "run",
      anova2 = "run",
      lm = "run",
      lmm = "run",
      pairs = "run",
      pca = "run_pca"
    )

    current_run_signal <- reactive({
      selection <- input$analysis_type
      if (is.null(selection) || !nzchar(selection)) return(NULL)
      mod <- modules[[selection]]
      if (is.null(mod)) return(NULL)
      suffix <- run_suffix_map[[mod$id]] %||% "run"
      input[[paste0(mod$id, "-", suffix)]]
    })

    observeEvent(input$analysis_type, {
      analysis_switch_token(analysis_switch_token() + 1L)
      has_run(FALSE)
      run_baseline(current_run_signal() %||% 0L)
    }, ignoreInit = TRUE)

    observeEvent(current_run_signal(), {
      current_value <- current_run_signal()
      baseline_value <- run_baseline() %||% 0L
      if (is.null(current_value)) return()
      if (current_value > baseline_value) {
        has_run(TRUE)
      }
    }, ignoreInit = TRUE)


    analysis_empty_state <- function(title, message, icon = "&#128221;") {
      div(
        class = "empty-state analysis-empty-state text-center my-4",
        div(
          class = "py-4 px-3",
          div(class = "empty-state-icon text-primary mb-2", HTML(icon)),
          h4(class = "mb-2", title),
          p(class = "text-muted mb-0", message)
        )
      )
    }

    # ---- Render active submodule UI ----
    output$config_panel <- renderUI({
      mod <- current_mod()
      ensure_module_server(mod)
      ui <- mod$ui(ns(mod$id))
      req(ui)
      ui$config
    })

    output$results_panel <- renderUI({
      if (!has_rows_available()) {
        return(analysis_empty_state(
          "No data to analyze",
          "Adjust the filters or upload data so the table contains rows before running an analysis."
        ))
      }

      selection <- input$analysis_type
      if (is.null(selection) || !nzchar(selection)) {
        return(analysis_empty_state(
          "No analysis selected yet",
          "Select an analysis type to view results."
        ))
      }

      if (!isTRUE(has_run())) {
        return(analysis_empty_state(
          "Run the selected analysis",
          "Run the analysis to view results."
        ))
      }

      mod <- current_mod()
      ui <- mod$ui(ns(mod$id))
      req(ui)
      ui$results
    })

    output$summary_help_icon <- renderUI({
      if (!identical(input$analysis_type, "Descriptive Statistics")) {
        return(NULL)
      }

      actionButton(
        ns("desc-summary_help"),
        label = NULL,
        icon = icon("circle-question"),
        class = "ta-help-icon",
        title = "How to read the summary"
      )
    })

    # ---- Unified model output ----
    model_out <- reactive({
      mod <- current_mod()
      srv <- ensure_module_server(mod)
      req(srv)
      srv()
    })
    
    list(
      results = model_out,
      selection = reactive(input$analysis_type)
    )
  })
}

normalize_analysis_type <- function(mod_type) {
  lookup <- c(
    desc = "DESCRIPTIVE",
    anova1 = "ANOVA",
    anova2 = "ANOVA",
    lm = "LM",
    lmm = "LMM",
    pairs = "CORR",
    pca = "PCA"
  )
  lookup[[mod_type]] %||% toupper(mod_type)
}

analysis_defaults <- function(mod_type) {
  list(
    analysis_type = normalize_analysis_type(mod_type),
    type = mod_type,
    data_used = NULL,
    model = NULL,
    summary = NULL,
    posthoc = NULL,
    effects = NULL,
    stats = NULL
  )
}

fill_analysis_defaults <- function(val, defaults) {
  for (name in names(defaults)) {
    if (is.null(val[[name]])) {
      val[[name]] <- defaults[[name]]
    }
  }
  val
}

ensure_analysis_server <- function(mod, df, server_cache, reset_trigger = NULL) {
  key <- mod$id
  cached <- server_cache[[key]]
  if (!is.null(cached)) {
    return(cached)
  }

  call_args <- list(mod$id, df)
  if (!is.null(reset_trigger)) {
    param_names <- names(formals(mod$server))
    if (!is.null(param_names) && "reset_trigger" %in% param_names) {
      call_args$reset_trigger <- reset_trigger
    }
  }

  result <- tryCatch(do.call(mod$server, call_args), error = function(e) {
    warning(sprintf("Module '%s' failed to initialize: %s", key, conditionMessage(e)))
    NULL
  })

  defaults <- analysis_defaults(mod$type)

  standardized <- reactive({
    val <- resolve_reactive(result)
    req(val)
    fill_analysis_defaults(val, defaults)
  })

  server_cache[[key]] <- standardized
  standardized
}

