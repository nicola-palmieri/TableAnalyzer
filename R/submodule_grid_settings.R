# Track recommendations separately from edits made in the existing numeric inputs.
grid_widget_settings <- function(input, session, ids) {
  cache <- shiny::reactiveValues()
  for (field in names(ids)) cache[[field]] <- NULL
  state <- shiny::reactiveValues(counts = NULL, manual = FALSE, pending = list(), previous = list())
  integer_value <- function(value) {
    value <- suppressWarnings(as.integer(value[1]))
    if (length(value) == 0L || is.na(value)) NULL else value
  }
  set_values <- function(values) {
    for (field in names(ids)) {
      value <- as.integer(values[[field]])
      if (identical(shiny::isolate(cache[[field]]), value)) next
      old_input <- shiny::isolate(integer_value(input[[ids[[field]]]]))
      state$previous[field] <- list(old_input)
      state$pending[field] <- list(if (identical(old_input, value)) NULL else value)
      cache[[field]] <- value
      shiny::updateNumericInput(session, ids[[field]], value = value)
    }
  }
  recommend <- function(values, counts) {
    if (!identical(state$counts, counts)) state$manual <- FALSE
    if (!state$manual) set_values(values)
    state$counts <- counts
    shiny::isolate(shiny::reactiveValuesToList(cache))
  }
  for (field in names(ids)) local({
    key <- field
    id <- ids[[key]]
    shiny::observeEvent(input[[id]], {
      raw <- integer_value(input[[id]])
      if (is.null(raw)) {
        if (!is.null(cache[[key]])) {
          state$pending[key] <- list(cache[[key]])
          shiny::updateNumericInput(session, id, value = cache[[key]])
        }
        return()
      }
      value <- max(1L, min(10L, raw))
      if (!identical(value, raw)) shiny::updateNumericInput(session, id, value = value)
      expected <- state$pending[[key]]
      if (!is.null(expected) && identical(value, expected)) {
        state$pending[key] <- list(NULL)
        return()
      }
      # Ignore a delayed echo of the old widget value while a reset is pending.
      if (!is.null(expected) && identical(value, state$previous[[key]])) return()
      if (!identical(value, cache[[key]])) {
        cache[[key]] <- value
        state$pending[key] <- list(NULL)
        state$manual <- TRUE
      }
    }, ignoreNULL = FALSE)
  })
  list(cache = cache, recommend = recommend, set_values = set_values,
       is_manual = shiny::reactive(state$manual))
}

single_grid_settings <- function(input, session, grid_id) {
  settings <- grid_widget_settings(input, session, c(
    rows = paste0(grid_id, "-rows"), cols = paste0(grid_id, "-cols")
  ))
  list(
    cache = settings$cache,
    resolve = function(n, panel_width, panel_height) {
      defaults <- compute_default_grid(n, panel_width, panel_height)
      settings$recommend(defaults, as.integer(n))
    },
    is_manual = settings$is_manual
  )
}
