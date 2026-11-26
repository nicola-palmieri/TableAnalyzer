# mod_visualize: server coordination and reactive state.
mod_visualize_server <- function(id, filtered_data, model_fit, analysis_selection = NULL) {
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
    # Registration is a session lifetime fact, independent of a module's return
    # value. Existing modules consume reactive data and reset tokens themselves.
    initialized_servers <- new.env(parent = emptyenv())
    last_selection <- reactiveVal(NULL)

    filtered_has_rows <- reactive({
      data <- filtered_data()
      !is.null(data) && nrow(data) > 0
    })

    observeEvent(selection_type(), {
      if (is.null(analysis_selection)) return()
      current <- selection_type()
      previous <- last_selection()
      if (!identical(current, previous) && identical(current, "pairs")) {
        pairs_reset_token(pairs_reset_token() + 1L)
      }
      if (!identical(current, previous) && identical(current, "pca")) {
        pca_reset_token(pca_reset_token() + 1L)
      }
      last_selection(current)
    }, ignoreInit = FALSE)

    ensure_vis_server <- function(key, create_fn) {
      if (!exists(key, envir = initialized_servers, inherits = FALSE)) {
        create_fn()
        assign(key, TRUE, envir = initialized_servers)
      }
      invisible(NULL)
    }

    visualization_specs <- list(
      oneway_anova = list(
        id = "oneway",
        server = function() visualize_oneway_server("oneway", filtered_data, model_info)
      ),
      twoway_anova = list(
        id = "twoway",
        server = function() visualize_twoway_server("twoway", filtered_data, model_info)
      ),
      pairs = list(
        id = "ggpairs",
        server = function() visualize_ggpairs_server(
          "ggpairs",
          filtered_data,
          model_info,
          reset_trigger = pairs_reset_token
        )
      ),
      pca = list(
        id = "pca",
        server = function() visualize_pca_server(
          "pca",
          filtered_data,
          model_info,
          reset_trigger = pca_reset_token
        )
      ),
      descriptive = list(
        id = "descriptive",
        server = function() visualize_descriptive_server("descriptive", filtered_data, model_info)
      )
    )

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

      if (!filtered_has_rows()) {
        return(list(
          show_visual = FALSE,
          empty_ui = empty_state(
            "&#9888;",
            "No data to visualize",
            "Adjust the filters or upload data so the dataset contains rows before opening the Visualize tab.",
            extra_class = "analysis-empty-state"
          )
        ))
      }

      if (!is.null(selection_label) && nzchar(selection_label)) {
        if (is.null(selected) || is.null(info) || !identical(selected, info_type)) {
          return(list(
            show_visual = FALSE,
            empty_ui = empty_state(
              "&#128221;",
              "Run an analysis first",
              "Run an analysis in the Analyze tab to see visualizations for those results.",
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

      if (is.null(visualization_specs[[info_type]])) {
        return(list(show_visual = FALSE, empty_ui = empty_state(
          "&#128065;", "Visualization coming soon",
          "We're still crafting charts for this analysis type. In the meantime, explore the other visualizations available!",
          extra_class = "analysis-empty-state"
        )))
      }

      list(show_visual = TRUE, empty_ui = NULL)
    })

    active_type <- reactive({
      if (!isTRUE(display_state()$show_visual)) return("")
      tolower(model_info_or_null()$type %||% "oneway_anova")
    })

    # Scaffolds are already bound. Initialize only a valid selected method, and
    # keep its registration across data changes and later visits to the page.
    observeEvent(active_type(), {
      type <- active_type()
      if (!nzchar(type)) return()
      spec <- visualization_specs[[type]]
      ensure_vis_server(spec$id, spec$server)
    }, ignoreNULL = FALSE)

    output$active_type <- renderText(active_type())

    output$empty_ui <- renderUI({
      display_state()$empty_ui
    })

    outputOptions(output, "active_type", suspendWhenHidden = FALSE)

  })
}

# Compatibility alias for existing callers.
visualize_server <- mod_visualize_server
