# ===============================================================
# 🧪 Table Analyzer — Analysis Coordinator
# ===============================================================

analysis_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("Step 3 — Analyze results"),
      p("Select an analysis type to explore your data, then inspect the summaries on the right."),
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
      h4("Analysis results"),
      uiOutput(ns("results_panel"))
    )
  )
}


analysis_server <- function(id, filtered_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    df <- reactive(filtered_data())
    
    # ---- Mapping of available modules ----
    modules <- list(
      "Descriptive Statistics" = list(id = "desc",  ui = descriptive_ui, server = descriptive_server, type = "desc"),
      "One-way ANOVA"          = list(id = "anova1", ui = one_way_anova_ui, server = one_way_anova_server, type = "anova1"),
      "Two-way ANOVA"          = list(id = "anova2", ui = two_way_anova_ui, server = two_way_anova_server, type = "anova2"),
      "Linear Model (LM)"      = list(id = "lm",     ui = lm_ui, server = lm_server, type = "lm"),
      "Linear Mixed Model (LMM)" = list(id = "lmm",  ui = lmm_ui, server = lmm_server, type = "lmm"),
      "Pairwise Correlation"   = list(id = "pairs",  ui = ggpairs_ui, server = ggpairs_server, type = "pairs"),
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
    
    # ---- Lazy server initialization ----
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


    ensure_module_server <- function(mod) {
      key <- mod$id
      if (!is.null(server_cache[[key]])) return(server_cache[[key]])

      result <- tryCatch(mod$server(mod$id, df), error = function(e) {
        warning(sprintf("Module '%s' failed to initialize: %s", key, conditionMessage(e)))
        NULL
      })

      defaults <- list(
        analysis_type = normalize_analysis_type(mod$type),
        type = mod$type,
        data_used = NULL,
        model = NULL,
        summary = NULL,
        posthoc = NULL,
        effects = NULL,
        stats = NULL
      )

      fill_defaults <- function(val) {
        for (name in names(defaults)) {
          if (is.null(val[[name]])) val[[name]] <- defaults[[name]]
        }
        val
      }

      # --- Standardize all outputs to a reactive returning a list ---
      standardized <- reactive({
        val <- resolve_reactive(result)
        req(val)
        fill_defaults(val)
      })

      server_cache[[key]] <- standardized
      standardized
    }


    # ---- Render active submodule UI ----
    output$config_panel <- renderUI({
      mod <- current_mod()
      ui <- mod$ui(ns(mod$id))
      req(ui)
      ui$config
    })

    output$results_panel <- renderUI({
      mod <- current_mod()
      ui <- mod$ui(ns(mod$id))
      req(ui)
      ui$results
    })

    # ---- Unified model output ----
    model_out <- reactive({
      mod <- current_mod()
      srv <- ensure_module_server(mod)
      req(srv)
      srv()
    })
    
    # Return the active model output as a reactive
    model_out
  })
}
