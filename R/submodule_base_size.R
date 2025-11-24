# ===============================================================
# 🔤 Base size controls (compact)
# ===============================================================

base_size_ui <- function(ns,
                         input_id = "plot_base_size",
                         default = 13,
                         min = 6,
                         max = 30,
                         step = 1,
                         help_text = "Adjust the base font size used for plot text.") {
  
  style_block <- "
    .ta-base-size-input .shiny-input-container,
    .ta-base-size-input .form-group { margin-bottom: 0; }
    .ta-base-size-input input.form-control {
      height: 32px;
      padding: 4px 10px;
    }
  "
  
  tagList(
    shiny::singleton(tags$style(HTML(style_block))),
    h5("Base size"),
    div(
      class = "ta-base-size-input",
      with_help_tooltip(
        numericInput(
          inputId = ns(input_id),
          label = NULL,
          value = default,
          min = min,
          max = max,
          step = step,
          width = "100%"
        ),
        help_text
      )
    )
  )
}

base_size_server <- function(input,
                             input_id = "plot_base_size",
                             default = 13) {
  
  reactive({
    value <- input[[input_id]]
    if (is.null(value) || !is.numeric(value) || is.na(value) || length(value) == 0) {
      default
    } else {
      value
    }
  })
}

