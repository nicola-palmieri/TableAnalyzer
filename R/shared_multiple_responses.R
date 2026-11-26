# ===============================================================
# 🔁 Multi-Response Selector Module (final simple version)
# ===============================================================

multi_response_ui <- function(id) {
  ns <- NS(id)
  tagList(
    with_help_tooltip(
      checkboxInput(
        ns("multi_resp"),
        "Allow multiple response variables",
        value = FALSE
      ),
      "Tick this to analyse several response variables one after another."
    ),
    uiOutput(ns("response_ui"))
  )
}

multi_response_server <- function(id, data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    
    df <- reactive({
      d <- if (is.function(data)) data() else data
      req(is.data.frame(d))
      d
    })
    
    # --- Re-render only when multi-select mode changes
    output$response_ui <- renderUI({
      d <- req(df())
      num_vars <- names(d)[sapply(d, is.numeric)]
      validate(need(length(num_vars) > 0, "No numeric variables available."))
      
      with_help_tooltip(
        selectInput(
          ns("response"),
          label = if (isTRUE(input$multi_resp))
            "Response variables (numeric)"
          else
            "Response variable (numeric)",
          choices = num_vars,
          selected = num_vars[1],
          multiple = isTRUE(input$multi_resp)
        ),
        "Choose the numeric outcomes the model should process."
      )
    })
    
    # --- Reactive standardized vector
    reactive({
      res <- req(input$response)
      if (!isTRUE(input$multi_resp)) res <- res[1]
      unique(res)
    })
  })
}
