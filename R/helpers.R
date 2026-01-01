#### Error helpers ####

format_safe_error_message <- function(title, details = NULL) {
  if (is.null(title) || !nzchar(title)) {
    title <- "Error"
  }

  if (inherits(details, "condition")) {
    details <- conditionMessage(details)
  }

  if (is.null(details)) {
    details <- ""
  }

  if (is.list(details)) {
    details <- unlist(details, recursive = TRUE, use.names = FALSE)
  }

  details <- vapply(details, as.character, character(1), USE.NAMES = FALSE)
  details <- trimws(details)
  details <- details[nzchar(details)]

  if (length(details) == 0) {
    return(paste0(title, ":"))
  }

  paste0(title, ":\n", paste(details, collapse = "\n"))
}

validate_numeric_columns <- function(data, columns, context_label = "response variables") {
  if (is.null(data) || !is.data.frame(data) || length(columns) == 0) {
    return(invisible(TRUE))
  }

  missing_cols <- setdiff(columns, names(data))
  if (length(missing_cols) > 0) {
    shiny::validate(shiny::need(
      FALSE,
      sprintf(
        "The following columns are no longer available: %s.",
        paste(missing_cols, collapse = ", ")
      )
    ))
  }

  non_numeric <- columns[!vapply(columns, function(col) is.numeric(data[[col]]), logical(1))]
  if (length(non_numeric) > 0) {
    shiny::validate(shiny::need(
      FALSE,
      sprintf(
        "The selected %s must be numeric. Please check their type in the Upload tab: %s.",
        context_label,
        paste(non_numeric, collapse = ", ")
      )
    ))
  }

  invisible(TRUE)
}

#### Levels helpers ####

resolve_order_levels <- function(values) {
  if (is.null(values)) return(character())
  
  if (is.factor(values)) {
    levels(values)
  } else {
    values <- values[!is.na(values)]
    unique(as.character(values))
  }
}

#### Reactive helpers ####

resolve_reactive <- function(value, default = NULL) {
  if (is.null(value)) {
    return(default)
  }
  
  resolved <- if (is.reactive(value)) value() else value
  
  if (is.null(resolved)) default else resolved
}

#### Export filename helpers ####

sanitize_export_part <- function(value) {
  safe <- gsub("[^A-Za-z0-9]+", "_", as.character(value))
  safe <- gsub("_+", "_", safe)
  safe <- gsub("^_|_$", "", safe)
  if (!nzchar(safe)) safe <- "unnamed"
  safe
}

format_export_timestamp <- function(time = Sys.time()) {
  format(time, "%Y%m%d-%H%M")
}

build_export_filename <- function(analysis, scope,
                                  response = NULL,
                                  stratum = NULL,
                                  extra = NULL,
                                  time = Sys.time()) {
  parts <- c(sanitize_export_part(analysis))
  if (!is.null(extra)) {
    parts <- c(parts, vapply(extra, sanitize_export_part, character(1)))
  }
  if (!is.null(response)) {
    parts <- c(parts, sanitize_export_part(response))
  }
  if (!is.null(stratum)) {
    parts <- c(parts, "stratum", sanitize_export_part(stratum))
  }
  paste0(paste(parts, collapse = "_"), "_", format_export_timestamp(time), ".docx")
}
