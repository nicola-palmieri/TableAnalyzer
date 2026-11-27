# Compatibility adapters preserve plot input IDs and font defaults.
base_size_ui <- function(ns, input_id = "plot_base_size", default = VetmeduniAppTemplate::PLOT_BASE_SIZE,
                         min = 6, max = 30, step = 1,
                         help_text = "Adjust the base font size used for plot text.") {
  VetmeduniAppTemplate::app_numeric_input(
    ns(input_id), "Base size", value = default,
    min = min, max = max, step = step, help = help_text
  )
}

base_size_server <- function(input, input_id = "plot_base_size", default = VetmeduniAppTemplate::PLOT_BASE_SIZE) {
  shiny::reactive({
    value <- input[[input_id]]
    if (is.null(value) || !is.numeric(value) || length(value) == 0 || is.na(value)) default else value
  })
}
