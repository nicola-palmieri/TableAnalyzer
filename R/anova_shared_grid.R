# Recommend both nested grids together, before the ANOVA render observers run.
anova_grid_settings_server <- function(input, session, model_info) {
  settings <- grid_widget_settings(input, session, c(
    strata_rows = "strata_grid-rows", strata_cols = "strata_grid-cols",
    response_rows = "response_grid-rows", response_cols = "response_grid-cols"
  ))
  grid_values <- function(defaults) c(
    strata_rows = defaults$strata$rows, strata_cols = defaults$strata$cols,
    response_rows = defaults$responses$rows, response_cols = defaults$responses$cols
  )
  recommendation <- shiny::reactive({
    info <- model_info()
    shiny::req(info)
    n_responses <- max(1L, length(info$responses))
    n_strata <- if (!is.null(info$strata$var)) max(1L, length(info$strata$levels)) else 1L
    list(
      counts = c(responses = n_responses, strata = n_strata),
      defaults = compute_joint_grid(n_responses, n_strata, input$plot_width, input$plot_height)
    )
  })
  shiny::observeEvent(recommendation(), {
    suggested <- recommendation()
    settings$recommend(grid_values(suggested$defaults), suggested$counts)
  }, priority = 100)
  list(
    cache = settings$cache,
    values = shiny::reactive(list(
      strata_rows = settings$cache$strata_rows, strata_cols = settings$cache$strata_cols,
      resp_rows = settings$cache$response_rows, resp_cols = settings$cache$response_cols
    )),
    is_manual = settings$is_manual,
    set_defaults = function(defaults) settings$set_values(grid_values(defaults))
  )
}
