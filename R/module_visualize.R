# ===============================================================
# 🧩 Visualization Coordinator (Lazy + Reactive)
# ===============================================================

visualize_ui <- function(id) {
  ns <- NS(id)
  tagList(
    conditionalPanel(
      condition = sprintf("output['%s']", ns("show_visual")),
      uiOutput(ns("dynamic_ui"))
    ),
    conditionalPanel(
      condition = sprintf("output['%s']", ns("show_empty")),
      uiOutput(ns("empty_ui"))
    )
  )
}

visualize_server <- function(id, filtered_data, model_fit, analysis_selection = NULL) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns

    empty_state <- function(icon, title, message) {
      div(
        class = "empty-state card bg-light border-0 shadow-sm text-center my-5",
        div(
          class = "card-body py-5 px-4",
          div(class = "empty-state-icon text-primary mb-3", HTML(icon)),
          h4(class = "mb-2", title),
          p(class = "text-muted mb-0", message)
        )
      )
    }

    model_info_or_null <- reactive({
      tryCatch(
        model_fit(),
        shiny.silent.stop = function(e) NULL
      )
    })

    model_info <- reactive({
      info <- model_info_or_null()
      req(info)
      info
    })

    analysis_type <- reactive({
      info <- model_info()
      type <- info$type %||% "oneway_anova"
      tolower(type)
    })

    selection_type <- reactive({
      if (is.null(analysis_selection)) return(NULL)
      selection <- analysis_selection()
      if (is.null(selection) || !nzchar(selection)) return(NULL)
      switch(
        selection,
        "Descriptive Statistics" = "descriptive",
        "One-way ANOVA" = "oneway_anova",
        "Two-way ANOVA" = "twoway_anova",
        "Pairwise Correlation" = "pairs",
        "PCA" = "pca",
        "Linear Model (LM)" = "lm",
        "Linear Mixed Model (LMM)" = "lmm",
        NULL
      )
    })

    pairs_reset_token <- reactiveVal(0L)
    last_selection <- reactiveVal(NULL)

    observeEvent(selection_type(), {
      if (is.null(analysis_selection)) return()
      current <- selection_type()
      previous <- last_selection()
      if (!identical(current, previous) && identical(current, "pairs")) {
        pairs_reset_token(pairs_reset_token() + 1L)
      }
      last_selection(current)
    }, ignoreInit = FALSE)

    vis_cache <- reactiveValues()

    ensure_vis_server <- function(key, create_fn) {
      if (is.null(vis_cache[[key]])) {
        vis_cache[[key]] <- create_fn()
      }
      vis_cache[[key]]
    }

    visualization_specs <- list(
      oneway_anova = list(
        id = "oneway",
        ui = function(ns) visualize_oneway_ui(ns("oneway")),
        server = function() visualize_oneway_server("oneway", filtered_data, model_info)
      ),
      twoway_anova = list(
        id = "twoway",
        ui = function(ns) visualize_twoway_ui(ns("twoway")),
        server = function() visualize_twoway_server("twoway", filtered_data, model_info)
      ),
      pairs = list(
        id = "ggpairs",
        ui = function(ns) visualize_ggpairs_ui(ns("ggpairs")),
        server = function() visualize_ggpairs_server(
          "ggpairs",
          filtered_data,
          model_info,
          reset_trigger = pairs_reset_token
        )
      ),
      pca = list(
        id = "pca",
        ui = function(ns) visualize_pca_ui(ns("pca"), filtered_data()),
        server = function() visualize_pca_server("pca", filtered_data, model_info)
      ),
      descriptive = list(
        id = "descriptive",
        ui = function(ns) visualize_descriptive_ui(ns("descriptive")),
        server = function() visualize_descriptive_server("descriptive", filtered_data, model_info)
      )
    )

    ui_cache <- reactiveVal(NULL)
    current_key <- reactiveVal(NULL)

    display_state <- reactive({
      info <- model_info_or_null()
      selection_label <- if (is.null(analysis_selection)) NULL else analysis_selection()
      selected <- selection_type()
      info_type <- if (is.null(info)) NULL else tolower(info$type %||% "oneway_anova")

      if (!is.null(selection_label) && nzchar(selection_label)) {
        if (is.null(selected) || is.null(info) || !identical(selected, info_type)) {
          return(list(
            show_visual = FALSE,
            empty_ui = empty_state(
              "&#128221;",
              "Run the selected analysis",
              "Run the analysis in the Analyze tab to see visualizations for those results."
            )
          ))
        }
      } else if (is.null(info)) {
        return(list(
          show_visual = FALSE,
          empty_ui = empty_state(
            "&#128221;",
            "No analysis selected yet",
            "Select an analysis in the Analyze tab to unlock tailored visualizations."
          )
        ))
      }

      if (is.null(info)) {
        return(list(
          show_visual = FALSE,
          empty_ui = empty_state(
            "&#128221;",
            "No analysis selected yet",
            "Run an analysis in the Analyze tab to unlock tailored visualizations for your results."
          )
        ))
      }

      list(show_visual = TRUE, empty_ui = NULL)
    })

    observeEvent(model_info_or_null(), {
      info <- model_info_or_null()
      if (is.null(info)) return()

      type <- tolower(info$type %||% "oneway_anova")
      key <- paste0("ui:", type)
      if (!identical(current_key(), key) || is.null(ui_cache())) {
        spec <- visualization_specs[[type]]
        if (!is.null(spec)) {
          ui_cache(spec$ui(ns))
        } else {
          ui_cache(empty_state(
            "&#128065;",
            "Visualization coming soon",
            "We're still crafting charts for this analysis type. In the meantime, explore the other visualizations available!"
          ))
        }
        current_key(key)
      }
    }, ignoreInit = FALSE)

    output$dynamic_ui <- renderUI({
      ui_cache()
    })

    output$empty_ui <- renderUI({
      display_state()$empty_ui
    })

    output$show_visual <- reactive({
      display_state()$show_visual
    })

    output$show_empty <- reactive({
      !isTRUE(display_state()$show_visual)
    })

    outputOptions(output, "show_visual", suspendWhenHidden = FALSE)
    outputOptions(output, "show_empty", suspendWhenHidden = FALSE)

    observeEvent(analysis_type(), {
      type <- analysis_type()
      spec <- visualization_specs[[type]]
      if (!is.null(spec)) {
        ensure_vis_server(spec$id, spec$server)
      }
    }, ignoreInit = FALSE)
  })
}
