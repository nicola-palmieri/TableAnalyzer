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

    empty_state <- function(icon, title, message, extra_class = NULL) {
      class_parts <- "empty-state text-center my-4"
      if (!is.null(extra_class) && nzchar(extra_class)) {
        class_parts <- c(class_parts, extra_class)
      }
      classes <- paste(class_parts, collapse = " ")
      div(
        class = classes,
        div(
          class = "py-4 px-3",
          div(class = "empty-state-icon text-primary mb-2", HTML(icon)),
          h4(class = "mb-2", title),
          p(class = "text-muted mb-0", message)
        )
      )
    }

    model_info_or_null <- reactive({
      tryCatch(
        model_fit(),
        shiny.silent.error = function(e) NULL,
        error = function(e) NULL
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
    pca_reset_token <- reactiveVal(0L)
    vis_cache <- reactiveValues()
    last_selection <- reactiveVal(NULL)

    observeEvent(selection_type(), {
      if (is.null(analysis_selection)) return()
      current <- selection_type()
      previous <- last_selection()
      if (!identical(current, previous) && identical(current, "pairs")) {
        pairs_reset_token(pairs_reset_token() + 1L)
        vis_cache[["ggpairs"]] <- NULL
      }
      if (!identical(current, previous) && identical(current, "pca")) {
        pca_reset_token(pca_reset_token() + 1L)
        vis_cache[["pca"]] <- NULL
      }
      last_selection(current)
    }, ignoreInit = FALSE)

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
        server = function() visualize_pca_server(
          "pca",
          filtered_data,
          model_info,
          reset_trigger = pca_reset_token
        )
      ),
      descriptive = list(
        id = "descriptive",
        ui = function(ns) visualize_descriptive_ui(ns("descriptive")),
        server = function() visualize_descriptive_server("descriptive", filtered_data, model_info)
      )
    )

    ui_cache <- reactiveVal(NULL)
    current_key <- reactiveVal(NULL)

    has_visual_results <- function(info) {
      if (is.null(info)) return(FALSE)
      fields <- c("summary", "model", "plots", "effects", "posthoc")
      has_value <- function(val) {
        if (is.null(val)) return(FALSE)
        if (is.list(val) && length(val) == 0) return(FALSE)
        TRUE
      }
      any(vapply(info[fields], has_value, logical(1)))
    }

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
              "Run the analysis in the Analyze tab to see visualizations for those results.",
              extra_class = "analysis-empty-state"
            )
          ))
        }
      } else if (is.null(info)) {
        return(list(
          show_visual = FALSE,
          empty_ui = empty_state(
            "&#128221;",
            "No analysis selected yet",
            "Select an analysis in the Analyze tab to unlock tailored visualizations.",
            extra_class = "analysis-empty-state"
          )
        ))
      }

      if (is.null(info)) {
        return(list(
          show_visual = FALSE,
          empty_ui = empty_state(
            "&#128221;",
            "No analysis selected yet",
            "Run an analysis in the Analyze tab to unlock tailored visualizations for your results.",
            extra_class = "analysis-empty-state"
          )
        ))
      }

      if (!has_visual_results(info)) {
        return(list(
          show_visual = FALSE,
          empty_ui = empty_state(
            "&#9888;",
            "Run the analysis first",
            "Run the selected analysis in the Analyze tab to activate the visualization."
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
            "We're still crafting charts for this analysis type. In the meantime, explore the other visualizations available!",
            extra_class = "analysis-empty-state"
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
