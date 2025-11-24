# ===============================================================
# 🧪 Table Analyzer — Filter Module (Refactored Reactive Version)
# ===============================================================

filter_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4("Step 2 — Filter records"),
      p("Select the columns to focus on and adjust the filters to refine the dataset for analysis."),
      hr(),
      uiOutput(ns("column_selector")),
      uiOutput(ns("filter_widgets"))
    ),
    mainPanel(
      width = 8,
      h4("Filtered data preview"),
      DTOutput(ns("filtered_preview"))
    )
  )
}

filter_server <- function(id, uploaded_data) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    df <- reactive(uploaded_data())

    column_key <- function(prefix, col) paste0(prefix, col)

    safe_range <- function(x) {
      rng <- suppressWarnings(range(x, na.rm = TRUE))
      if (any(!is.finite(rng))) rep(0, 2) else rng
    }

    numeric_step <- function(rng) {
      span <- diff(rng)
      if (span == 0 || any(!is.finite(span))) 1 else span / 100
    }

    build_numeric_widget <- function(col, x) {
      rng <- safe_range(x)
      step_val <- numeric_step(rng)
      fluidRow(
        column(
          6,
          with_help_tooltip(
            numericInput(
              ns(column_key("min_", col)), paste(col, "(min)"),
              value = rng[1], min = rng[1], max = rng[2], step = step_val
            ),
            sprintf("Enter the smallest value to keep for %s.", col)
          )
        ),
        column(
          6,
          with_help_tooltip(
            numericInput(
              ns(column_key("max_", col)), paste(col, "(max)"),
              value = rng[2], min = rng[1], max = rng[2], step = step_val
            ),
            sprintf("Enter the largest value to keep for %s.", col)
          )
        )
      )
    }

    build_logical_widget <- function(col) {
      with_help_tooltip(
        checkboxGroupInput(
          ns(column_key("filter_", col)), label = col,
          choices = c(TRUE, FALSE), selected = c(TRUE, FALSE), inline = TRUE
        ),
        sprintf("Tick the logical values you want to keep for %s.", col)
      )
    }

    build_factor_widget <- function(col, x) {
      choices <- sort(unique(as.character(x)))
      with_help_tooltip(
        selectInput(
          ns(column_key("filter_", col)), label = col,
          choices = choices, multiple = TRUE, selected = choices
        ),
        sprintf("Choose which categories should remain for %s.", col)
      )
    }

    build_widget <- function(col) {
      x <- df()[[col]]
      if (is.numeric(x)) build_numeric_widget(col, x)
      else if (is.logical(x)) build_logical_widget(col)
      else build_factor_widget(col, x)
    }

    filter_column <- function(data, col) {
      x <- data[[col]]
      if (is.numeric(x)) {
        if (all(is.na(x))) return(data)
        min_val <- input[[column_key("min_", col)]] %||% -Inf
        max_val <- input[[column_key("max_", col)]] %||% Inf
        keep <- is.na(x) | (x >= min_val & x <= max_val)
      } else {
        sel <- input[[column_key("filter_", col)]] %||% character(0)
        if (!length(sel)) return(data[0, , drop = FALSE])
        keep <- is.na(x) | (as.character(x) %in% sel)
      }
      data[keep, , drop = FALSE]
    }

    # --- 1. Column selector ---
    output$column_selector <- renderUI({
      data <- req(df())
      with_help_tooltip(
        selectInput(
          ns("columns"),
          "Select columns to filter",
          choices = names(data),
          multiple = TRUE
        ),
        "Choose which variables you want to filter before running analyses."
      )
    })

    # --- 2. Dynamic filter widgets ---
    output$filter_widgets <- renderUI({
      cols <- req(input$columns)
      tagList(lapply(cols, build_widget))
    })

    # --- 3. Reactive filtering ---
    filtered_df <- reactive({
      data <- req(df())
      cols <- input$columns
      if (!length(cols)) return(droplevels(data))
      filtered <- Reduce(filter_column, cols, init = data, right = FALSE)
      droplevels(filtered)
    })

    # --- 4. Preview table ---
    output$filtered_preview <- renderDT(
      filtered_df(),
      options = list(
        scrollX = TRUE,
        pageLength = 10,
        columnDefs = list(
          list(targets = "_all", className = "dt-nowrap")
        )
      ),
      class = "display nowrap"
    )

    # --- 5. Return filtered data downstream ---
    filtered_df
  })
}
