# ===============================================================
# 🧩 Visualization Coordinator (Lazy + Reactive)
# ===============================================================

visualize_ui <- function(id) {
  ns <- NS(id)
  uiOutput(ns("dynamic_ui"))
}

visualize_server <- function(id, filtered_data, model_fit) {
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
        server = function() visualize_ggpairs_server("ggpairs", filtered_data, model_info)
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

    output$dynamic_ui <- renderUI({
      info <- model_info_or_null()
      if (is.null(info)) {
        return(empty_state(
          "&#128221;",
          "No analysis selected yet",
          "Run an analysis in the Analyze tab to unlock tailored visualizations for your results."
        ))
      }

      type <- analysis_type()
      spec <- visualization_specs[[type]]
      if (!is.null(spec)) {
        return(spec$ui(ns))
      }

      empty_state(
        "&#128065;",
        "Visualization coming soon",
        "We're still crafting charts for this analysis type. In the meantime, explore the other visualizations available!"
      )
    })

    observeEvent(analysis_type(), {
      type <- analysis_type()
      spec <- visualization_specs[[type]]
      if (!is.null(spec)) {
        ensure_vis_server(spec$id, spec$server)
      }
    }, ignoreInit = FALSE)
  })
}
