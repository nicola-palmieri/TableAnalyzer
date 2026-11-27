# Presentation only: values, column semantics and filtering belong to the app.
app_metric <- function(label, value, detail = NULL, class = NULL, value_class = NULL) {
  shiny::div(class = paste(c("app-metric upload-stat", class), collapse = " "),
    shiny::div(class = "upload-stat-label", label),
    shiny::div(class = if (is.null(value_class)) "upload-stat-value" else value_class, value),
    if (!is.null(detail)) shiny::div(class = "upload-stat-label", detail))
}

app_metric_strip <- function(..., columns = NULL, class = NULL) {
  shiny::div(class = paste(c("app-metric-strip", class), collapse = " "),
    style = if (!is.null(columns)) paste0("--app-metric-columns:", paste(columns, collapse = " "), ";"), ...)
}

app_data_section <- function(title, ..., actions = NULL, id = NULL, icon = NULL) {
  bslib::card(id = id, class = "upload-data-section", fill = FALSE,
    bslib::card_header(shiny::div(class = "upload-section-heading",
      shiny::h3(icon, title), actions)),
    bslib::card_body(..., fill = FALSE, fillable = FALSE))
}

# DT is optional: applications that display data tables already depend on it.
# Override options explicitly for app-specific search, column and paging policy.
app_table_options <- function(placeholder = "Search table", scroll_y = "360px", options = list()) {
  if (!requireNamespace("DT", quietly = TRUE)) stop("Install DT to use app_table_options().", call. = FALSE)
  defaults <- list(pageLength = 10L, lengthMenu = c(10L, 25L, 50L), scrollX = TRUE,
    scrollY = scroll_y, scrollCollapse = FALSE, autoWidth = FALSE,
    deferRender = TRUE, processing = TRUE, searchDelay = 600L, order = list(),
    language = list(search = "Search:", searchPlaceholder = placeholder),
    initComplete = DT::JS("function() { var body = this.api().table().container().querySelector('.dataTables_scrollBody'); if (body) { body.tabIndex = 0; body.setAttribute('role', 'region'); body.setAttribute('aria-label', 'Data preview; use arrow keys to scroll horizontally and vertically'); } }"),
    dom = "<'upload-table-toolbar'lf>rt<'upload-table-footer'ip>")
  utils::modifyList(defaults, options, keep.null = TRUE)
}
