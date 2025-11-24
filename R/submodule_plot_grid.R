# ===============================================================
# 🧩 Plot grid module
# ===============================================================

compute_default_grid <- function(n) {
  n <- coerce_grid_value(n, default = 1L)
  rows <- ceiling(sqrt(n))
  list(rows = rows, cols = ceiling(n / rows))
}

coerce_grid_value <- function(value,
                              default = 1L,
                              min_value = 1L,
                              max_value = NULL) {
  raw <- suppressWarnings(as.integer(value[1]))
  if (length(value) == 0 || is.na(raw)) raw <- default
  raw <- max(as.integer(min_value), raw)
  if (!is.null(max_value)) raw <- min(as.integer(max_value), raw)
  as.integer(raw)
}

validate_grid <- function(n_items, rows, cols) {
  n_items <- coerce_grid_value(n_items, default = 1L)
  rows <- coerce_grid_value(rows, default = 1L)
  cols <- coerce_grid_value(cols, default = 1L)

  too_small <- rows * cols < n_items
  empty_row <- n_items <= (rows - 1L) * cols
  empty_col <- n_items <= rows * (cols - 1L)

  if (too_small) {
    return(list(
      valid = FALSE,
      message = sprintf("⚠️ Grid %dx%d too small for %d subplots.", rows, cols, n_items)
    ))
  }

  if (empty_row || empty_col) {
    return(list(
      valid = FALSE,
      message = sprintf("⚠️ Grid %dx%d too large for %d subplots.", rows, cols, n_items)
    ))
  }

  list(valid = TRUE, message = NULL)
}

basic_grid_layout <- function(rows = NULL,
                              cols = NULL,
                              default_rows = 1L,
                              default_cols = 1L,
                              min_rows = 1L,
                              min_cols = 1L,
                              max_rows = 10L,
                              max_cols = 10L) {
  list(
    nrow = coerce_grid_value(rows, default_rows, min_rows, max_rows),
    ncol = coerce_grid_value(cols, default_cols, min_cols, max_cols)
  )
}

apply_grid_defaults_if_empty <- function(input, session, grid_id, defaults, n_items = NULL) {
  if (is.null(defaults) || !is.list(defaults)) return()

  rows_default <- suppressWarnings(as.integer(defaults$rows[1]))
  cols_default <- suppressWarnings(as.integer(defaults$cols[1]))

  if (any(!is.finite(c(rows_default, cols_default)))) return()

  rows_id <- paste0(grid_id, "-rows")
  cols_id <- paste0(grid_id, "-cols")

  current_rows <- isolate(input[[rows_id]])
  current_cols <- isolate(input[[cols_id]])

  needs_reset <- function(value) {
    if (length(value) == 0) return(TRUE)
    v <- suppressWarnings(as.integer(value[1]))
    is.na(v)
  }

  reset_rows <- needs_reset(current_rows)
  reset_cols <- needs_reset(current_cols)

  if (reset_rows) {
    updateNumericInput(session, rows_id, value = rows_default)
  }

  if (reset_cols) {
    updateNumericInput(session, cols_id, value = cols_default)
  }
}

adjust_grid_layout <- function(n_items, layout) {
  if (is.null(layout) || length(layout) == 0) {
    layout <- list(nrow = 1L, ncol = 1L)
  }

  n_items <- coerce_grid_value(n_items, default = 1L)
  rows <- coerce_grid_value(layout$nrow, default = 1L)
  cols <- coerce_grid_value(layout$ncol, default = 1L)

  if (rows * cols < n_items) {
    defaults <- compute_default_grid(n_items)
    rows <- defaults$rows
    cols <- defaults$cols
  }

  while (rows > 1L && n_items <= (rows - 1L) * cols) {
    rows <- rows - 1L
  }

  while (cols > 1L && n_items <= rows * (cols - 1L)) {
    cols <- cols - 1L
  }

  list(nrow = rows, ncol = cols)
}

plot_grid_ui <- function(id,
                         rows_label = "Grid rows",
                         cols_label = "Grid columns",
                         rows_help = NULL,
                         cols_help = NULL,
                         rows_min = 1L,
                         rows_max = 10L,
                         cols_min = 1L,
                         cols_max = 10L,
                         rows_value = NA,
                         cols_value = NA) {
  ns <- NS(id)
  
  rows_input <- numericInput(
    ns("rows"),
    label = rows_label,
    value = rows_value,
    min = rows_min,
    max = rows_max,
    step = 1
  )
  
  cols_input <- numericInput(
    ns("cols"),
    label = cols_label,
    value = cols_value,
    min = cols_min,
    max = cols_max,
    step = 1
  )
  
  if (!is.null(rows_help)) rows_input <- with_help_tooltip(rows_input, rows_help)
  if (!is.null(cols_help)) cols_input <- with_help_tooltip(cols_input, cols_help)
  
  tagList(
    fluidRow(
      column(6, rows_input),
      column(6, cols_input)
    )
  )
}

plot_grid_server <- function(id,
                             rows_min = 1L,
                             rows_max = 10L,
                             cols_min = 1L,
                             cols_max = 10L) {
  moduleServer(id, function(input, output, session) {
    sanitize <- function(x, min_value, max_value) {
      if (length(x) == 0) return(NA_integer_)
      v <- suppressWarnings(as.integer(x[1]))
      if (is.na(v)) return(NA_integer_)
      v <- max(as.integer(min_value), v)
      v <- min(as.integer(max_value), v)
      as.integer(v)
    }
    
    rows_raw <- reactive(sanitize(input$rows, rows_min, rows_max))
    cols_raw <- reactive(sanitize(input$cols, cols_min, cols_max))
    
    rows <- reactive(rows_raw())
    cols <- reactive(cols_raw())
    
    list(
      rows = rows,
      cols = cols,
      values = reactive(list(rows = rows(), cols = cols()))
    )
  })
}
