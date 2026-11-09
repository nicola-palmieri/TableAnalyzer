# ===============================================================
# 🧩 Plot grid module
# ===============================================================

grid_candidates <- function(n, max_rows = 10L, max_cols = 10L) {
  n <- coerce_grid_value(n, default = 1L)
  rows <- seq_len(min(n, max_rows))
  cols <- ceiling(n / rows)
  keep <- cols <= max_cols & n > (rows - 1L) * cols
  data.frame(rows = rows[keep], cols = as.integer(cols[keep]))
}

compute_joint_grid <- function(n_responses, n_strata = 1L,
                               panel_width = 400, panel_height = 300) {
  n_responses <- coerce_grid_value(n_responses, default = 1L)
  n_strata <- coerce_grid_value(n_strata, default = 1L)
  positive_size <- function(value, fallback) {
    value <- suppressWarnings(as.numeric(value[1]))
    if (length(value) == 0L || !is.finite(value) || value <= 0) fallback else value
  }
  panel_width <- positive_size(panel_width, 400)
  panel_height <- positive_size(panel_height, 300)
  responses <- grid_candidates(n_responses)
  strata <- grid_candidates(n_strata)
  if (nrow(responses) == 0L || nrow(strata) == 0L) {
    # Keep widgets inside their bounds; existing validation explains the limit.
    return(list(
      responses = if (nrow(responses)) compute_default_grid(n_responses, panel_width, panel_height) else list(rows = 10L, cols = 10L),
      strata = if (nrow(strata)) compute_default_grid(n_strata, panel_width, panel_height) else list(rows = 10L, cols = 10L)
    ))
  }
  r <- rep(seq_len(nrow(responses)), each = nrow(strata))
  s <- rep(seq_len(nrow(strata)), times = nrow(responses))
  rows <- responses$rows[r] * strata$rows[s]
  cols <- responses$cols[r] * strata$cols[s]
  empty <- rows * cols - n_responses * n_strata
  aspect <- cols * panel_width / (rows * panel_height)
  target_aspect <- 1.6
  aspect_error <- abs(log(aspect / target_aspect))
  reasonable <- aspect >= target_aspect / 2.5 & aspect <= target_aspect * 2.5
  eligible <- if (any(reasonable)) which(reasonable) else seq_along(rows)
  # Among sensible shapes, empty cells matter first. Ties keep responses in rows.
  ranking <- if (any(reasonable)) {
    order(empty[eligible], aspect_error[eligible], responses$cols[r[eligible]], strata$rows[s[eligible]])
  } else {
    order(aspect_error[eligible], empty[eligible], responses$cols[r[eligible]])
  }
  best <- eligible[ranking[1]]
  list(
    responses = list(rows = responses$rows[r[best]], cols = responses$cols[r[best]]),
    strata = list(rows = strata$rows[s[best]], cols = strata$cols[s[best]])
  )
}

compute_default_grid <- function(n, panel_width = 400, panel_height = 300) {
  compute_joint_grid(n, 1L, panel_width, panel_height)$responses
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
                         cols_value = NA,
                         advanced = TRUE) {
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
  
  controls <- fluidRow(
    column(6, rows_input),
    column(6, cols_input)
  )
  if (!advanced) return(controls)

  tags$details(
    tags$summary(strong("Advanced options")),
    controls
  )
}
