app_download_ui <- function(ns, download_id = "download_plot", dimensions_id = NULL,
                             label = "Download PNG (300 dpi)") {
  shiny::div(class = "app-downloads",
    if (!is.null(dimensions_id)) shiny::tags$small(class = "text-muted", shiny::textOutput(ns(dimensions_id))),
    if (!is.null(download_id)) shiny::downloadButton(ns(download_id), label = NULL,
      icon = shiny::icon("download"), class = "app-download-icon",
      title = label, `aria-label` = label))
}

app_result_header_ui <- function(title = NULL, ...) {
  shiny::div(class = "app-result-header",
    if (is.character(title)) shiny::h4(title) else title,
    shiny::div(class = "app-result-actions", ...))
}

app_plot_actions_ui <- function(ns, reset_id = "reset", download_id = NULL,
    dimensions_id = NULL, reset_label = "Reset", download_label = "Download PNG (300 dpi)",
    reset_aria_label = NULL, ...) {
  if (!is.null(download_id) || !is.null(dimensions_id)) {
    warning("Sidebar downloads in app_plot_actions_ui() are deprecated; place app_download_ui() inside app_result_header_ui().",
      call. = FALSE)
  }
  shiny::div(class = "app-plot-actions d-grid", style = "gap:6px;",
    if (!is.null(reset_id)) shiny::actionButton(ns(reset_id), reset_label,
      class = "w-100", `aria-label` = reset_aria_label),
    app_download_ui(ns, download_id, dimensions_id, label = download_label), ...)
}
