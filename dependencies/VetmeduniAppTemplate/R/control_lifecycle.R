# Keep lightweight controls current, never enable expensive hidden plot/model outputs.
app_keep_outputs_current <- function(output, ...) {
  for (id in c(...)) shiny::outputOptions(output, id, suspendWhenHidden = FALSE)
  invisible(NULL)
}

app_optional_value <- function(value) {
  tryCatch(value, shiny.silent.error = function(error) NULL)
}

app_busy_options <- function(...) {
  defaults <- list(spinner_delay = "350ms", spinner_color = APP_SEMANTIC_COLORS$running$accent, spinner_size = "24px",
    spinner_selector = ".app-polish .shiny-plot-output",
    fade_opacity = 1, fade_selector = ".app-polish .shiny-bound-output")
  do.call(shiny::busyIndicatorOptions, utils::modifyList(defaults, list(...)))
}
