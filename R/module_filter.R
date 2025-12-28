# ===============================================================
# 🧪 Table Analyzer — Filter Module (Refactored Reactive Version)
# ===============================================================

filter_ui <- function(id) {
  ns <- NS(id)
  sidebarLayout(
    sidebarPanel(
      width = 4,
      h4(class = "ta-sidebar-title", "Step 2 - Filter records"),
      p(class = "ta-sidebar-subtitle", "Select the columns to focus on and adjust the filters to refine the dataset for analysis."),
      hr(),
      uiOutput(ns("column_selector")),
      uiOutput(ns("na_controls")),
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
    filter_state <- reactiveValues()
    active_cols <- reactiveVal(character(0))

    column_key <- function(prefix, col) paste0(prefix, col)

    safe_range <- function(x) {
      rng <- suppressWarnings(range(x, na.rm = TRUE))
      if (any(!is.finite(rng))) rep(0, 2) else rng
    }

    numeric_step <- function(rng) {
      span <- diff(rng)
      if (span == 0 || any(!is.finite(span))) 1 else span / 100
    }

    init_filter_state <- function(col, data) {
      x <- data[[col]]
      if (is.numeric(x)) {
        rng <- safe_range(x)
        return(list(type = "numeric", min = rng[1], max = rng[2]))
      }
      if (is.logical(x)) {
        return(list(type = "logical", selected = c(TRUE, FALSE)))
      }
      choices <- sort(unique(as.character(x)))
      list(type = "factor", selected = choices)
    }

    build_numeric_widget <- function(col, x) {
      rng <- safe_range(x)
      step_val <- numeric_step(rng)
      state <- filter_state[[col]]
      min_val <- state$min %||% rng[1]
      max_val <- state$max %||% rng[2]
      min_val <- max(min_val, rng[1])
      max_val <- min(max_val, rng[2])
      if (min_val > max_val) {
        min_val <- rng[1]
        max_val <- rng[2]
      }
      fluidRow(
        column(
          6,
          with_help_tooltip(
            numericInput(
              ns(column_key("min_", col)), paste(col, "(min)"),
              value = min_val, min = rng[1], max = rng[2], step = step_val
            ),
            sprintf("Enter the smallest value to keep for %s.", col)
          )
        ),
        column(
          6,
          with_help_tooltip(
            numericInput(
              ns(column_key("max_", col)), paste(col, "(max)"),
              value = max_val, min = rng[1], max = rng[2], step = step_val
            ),
            sprintf("Enter the largest value to keep for %s.", col)
          )
        )
      )
    }

    build_logical_widget <- function(col) {
      state <- filter_state[[col]]
      selected <- state$selected %||% c(TRUE, FALSE)
      selected <- intersect(selected, c(TRUE, FALSE))
      if (!length(selected)) selected <- c(TRUE, FALSE)
      with_help_tooltip(
        checkboxGroupInput(
          ns(column_key("filter_", col)), label = col,
          choices = c(TRUE, FALSE), selected = selected, inline = TRUE
        ),
        sprintf("Tick the logical values you want to keep for %s.", col)
      )
    }

    build_factor_widget <- function(col, x) {
      choices <- sort(unique(as.character(x)))
      state <- filter_state[[col]]
      selected <- state$selected %||% choices
      selected <- intersect(selected, choices)
      if (!length(selected)) selected <- choices
      with_help_tooltip(
        selectInput(
          ns(column_key("filter_", col)), label = col,
          choices = choices, multiple = TRUE, selected = selected
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
      if (is.null(col) || !col %in% names(data)) return(data)
      x <- data[[col]]
      if (is.null(x) || length(x) == 0) return(data)
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

    observeEvent(df(), {
      current <- names(reactiveValuesToList(filter_state))
      if (length(current)) {
        for (col in current) {
          filter_state[[col]] <- NULL
        }
      }
      active_cols(character(0))
      updateSelectInput(session, "columns", choices = names(df()), selected = character(0))
    }, ignoreInit = TRUE)

    observeEvent(input$columns, {
      data <- req(df())
      prev <- active_cols()
      requested <- input$columns %||% character(0)
      current <- intersect(requested, names(data))

      kept <- intersect(prev, current)
      removed <- setdiff(prev, current)
      added <- setdiff(current, prev)

      if (length(setdiff(requested, current)) > 0) {
        updateSelectInput(session, "columns", choices = names(data), selected = current)
      }

      for (col in kept) {
        x <- data[[col]]
        if (is.numeric(x)) {
          min_val <- isolate(input[[column_key("min_", col)]])
          max_val <- isolate(input[[column_key("max_", col)]])
          if (!is.null(min_val) && !is.null(max_val)) {
            filter_state[[col]] <- list(type = "numeric", min = min_val, max = max_val)
          }
        } else {
          sel <- isolate(input[[column_key("filter_", col)]])
          if (!is.null(sel)) {
            filter_state[[col]] <- list(type = "categorical", selected = sel)
          }
        }
      }

      for (col in removed) {
        filter_state[[col]] <- NULL
      }

      for (col in added) {
        filter_state[[col]] <- init_filter_state(col, data)
      }

      active_cols(current)

      if (length(added) > 0) {
        added_cols <- added
        data_snapshot <- data
        session$onFlushed(function() {
          for (col in added_cols) {
            x <- data_snapshot[[col]]
            if (is.numeric(x)) {
              rng <- safe_range(x)
              step_val <- numeric_step(rng)
              updateNumericInput(
                session,
                column_key("min_", col),
                value = rng[1],
                min = rng[1],
                max = rng[2],
                step = step_val
              )
              updateNumericInput(
                session,
                column_key("max_", col),
                value = rng[2],
                min = rng[1],
                max = rng[2],
                step = step_val
              )
            } else if (is.logical(x)) {
              updateCheckboxGroupInput(
                session,
                column_key("filter_", col),
                choices = c(TRUE, FALSE),
                selected = c(TRUE, FALSE)
              )
            } else {
              choices <- sort(unique(as.character(x)))
              updateSelectInput(
                session,
                column_key("filter_", col),
                choices = choices,
                selected = choices
              )
            }
          }
        }, once = TRUE)
      }
    }, ignoreInit = TRUE)

    # --- 1b. NA handling controls ---
    output$na_controls <- renderUI({
      data <- req(df())
      cols <- names(data)
      tagList(
        checkboxInput(
          ns("drop_na_rows"),
          label = "Drop rows with any NA in selected columns",
          value = FALSE
        ),
        conditionalPanel(
          condition = sprintf("input['%s'] === true", ns("drop_na_rows")),
          with_help_tooltip(
            selectInput(
              ns("na_columns"),
              label = "Columns to check for NA",
              choices = cols,
              selected = cols,
              multiple = TRUE
            ),
            "Rows containing at least one NA in these columns will be removed."
          )
        ),
        tags$hr()
      )
    })

    # --- 2. Dynamic filter widgets ---
    output$filter_widgets <- renderUI({
      data <- req(df())
      cols <- intersect(req(input$columns), names(data))
      tagList(lapply(cols, build_widget))
    })

    # --- 3. Reactive filtering ---
    filtered_df <- reactive({
      data <- req(df())
      cols <- intersect(input$columns %||% character(0), names(data))
      filtered <- data
      if (length(cols)) {
        filtered <- Reduce(filter_column, cols, init = data, right = FALSE)
      }
      if (isTRUE(input$drop_na_rows)) {
        na_cols <- input$na_columns %||% character(0)
        na_cols <- intersect(na_cols, names(filtered))
        if (length(na_cols) > 0) {
          filtered <- filtered[stats::complete.cases(filtered[, na_cols, drop = FALSE]), , drop = FALSE]
        }
      }
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
