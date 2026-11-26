# ===============================================================
# 🧭 Stratification helpers (shared across analysis modules)
# ===============================================================

MAX_STRATIFICATION_LEVELS <- 10

stratification_ui <- function(id, ns = NULL) {
  ns_fn <- if (is.null(ns)) {
    NS(id)
  } else if (is.function(ns)) {
    function(x) ns(paste(id, x, sep = "-"))
  } else {
    NS(id)
  }
  
  tagList(
    uiOutput(ns_fn("stratify_var_ui")),
    uiOutput(ns_fn("strata_order_ui"))
  )
}

stratification_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    df <- reactive({
      d <- if (is.function(data)) data() else data
      req(is.data.frame(d))
      validate(need(nrow(d) > 0, "No data available for stratification."))
      d
    })
    
    # ---- UI: variable selector ----
    output$stratify_var_ui <- renderUI({
      d <- req(df())
      cat_cols <- names(d)[vapply(d, function(x) is.factor(x) || is.character(x), logical(1))]
      with_help_tooltip(
        selectInput(
          ns("stratify_var"),
          "Stratify by",
          choices = c("None", cat_cols),
          selected = "None"
        ),
        "Pick a categorical variable if you want separate summaries for each group."
      )
    })
    
    # ---- UI: order/levels selector ----
    output$strata_order_ui <- renderUI({
      req(input$stratify_var)
      if (input$stratify_var == "None") return(NULL)
      d <- req(df())
      
      values <- d[[input$stratify_var]]
      available_levels <- resolve_order_levels(values)
      
      n_levels <- length(available_levels)
      validate(need(n_levels <= MAX_STRATIFICATION_LEVELS,
                    sprintf("'%s' has too many levels (%d > %d).",
                            input$stratify_var, n_levels, MAX_STRATIFICATION_LEVELS)))
      
      with_help_tooltip(
        selectInput(
          ns("strata_order"),
          "Order of levels",
          choices = available_levels,
          selected = available_levels,
          multiple = TRUE
        ),
        "Decide which group levels to include and in what order they should appear."
      )
    })
    
    # ---- Unified reactive output ----
    reactive({
      list(
        var = if (identical(input$stratify_var, "None")) NULL else input$stratify_var,
        levels = input$strata_order
      )
    })
  })
}
  

