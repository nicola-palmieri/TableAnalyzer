# Keep a complete valid preview/export snapshot while controls are being edited.
# The builder's actual reactive reads define dependencies; analysis runs stay app-owned.
app_plot_snapshot <- function(build, ready = function() TRUE) {
  last <- shiny::reactiveVal(NULL)
  warning <- shiny::reactiveVal(NULL)
  shiny::observe({
    if (!isTRUE(ready())) return()
    candidate <- tryCatch(build(), error = identity)
    if (inherits(candidate, "condition")) {
      message <- conditionMessage(candidate)
      if (nzchar(message)) warning(message)
      return()
    }
    if (is.null(candidate)) return()
    if (is.null(candidate$plot) && !is.null(candidate$warning)) {
      warning(candidate$warning)
      return()
    }
    last(candidate)
    warning(candidate$warning)
  })
  shiny::reactive({
    result <- last()
    if (is.null(result)) result <- list(plot = NULL)
    result$warning <- warning()
    result
  })
}

app_plot_numeric <- function(value, default, min = 0, max = Inf, label = "Plot setting") {
  if (is.null(value)) return(default)
  if (!is.numeric(value) || length(value) != 1L || !is.finite(value) || value < min || value > max)
    stop(label, " must be a finite number between ", min, " and ", max, ".", call. = FALSE)
  value
}
