# Presentation only. Branding and scientific copy belong to consuming apps.
app_wordmark_ui <- function(class = NULL, main = "Analysis", accent = "App") {
  shiny::span(class = paste(c("app-wordmark", class), collapse = " "),
    shiny::HTML(paste0(htmltools::htmlEscape(main),
      '<span class="app-wordmark-accent">', htmltools::htmlEscape(accent), '</span>')))
}

app_brand_ui <- function(main, accent = "", version = NULL, logo = NULL,
                         class = "navbar-wordmark", home_input = "go_home") {
  shiny::span(class = "app-brand", `data-home-input` = home_input,
    `data-brand-label` = paste0(main, accent),
    if (!is.null(logo)) shiny::tags$img(src = logo, class = "ta-navbar-logo", alt = ""),
    app_wordmark_ui(class, main, accent),
    if (!is.null(version)) shiny::span(class = "app-brand-version", version))
}

app_plot_alt <- function(title, roles = list(), samples = NULL, input = NULL) {
  labels <- c(inner = "Inner grouping", outer = "Outer grouping", rows = "Facet rows",
    facet_rows = "Facet rows", cols = "Facet columns", facet_cols = "Facet columns",
    facet = "Facet columns", colour = "Colour", shape = "Shape")
  descriptions <- vapply(intersect(names(roles), names(labels)), function(role) {
    value <- roles[[role]]
    if (is.null(value) || !length(value) || value %in% c("None", ".none")) return("")
    paste0(labels[[role]], ": ", value)
  }, character(1))
  pieces <- c(title, descriptions, if (!is.null(samples)) paste(samples, "samples"),
    if (!is.null(input)) paste(input, "input"))
  paste0(paste(pieces[nzchar(pieces)], collapse = ". "), ".")
}

app_analysis_icon <- function(name) {
  if (!identical(name, "boxplot")) return(shiny::icon(name))
  htmltools::tags$svg(
    class = "svg-inline--fa app-boxplot-icon", viewBox = "0 0 24 24",
    width = "1.25em", height = "1em", `aria-hidden` = "true", focusable = "false",
    style = "vertical-align:-0.125em", fill = "none", stroke = "currentColor",
    `stroke-width` = "2", `stroke-linecap` = "round", `stroke-linejoin` = "round",
    htmltools::tags$path(d = "M6 2v5m0 10v5M3 2h6M3 22h6M3 7h6v10H3zM3 12h6M18 2v3m0 9v6M15 2h6M15 20h6M15 5h6v9h-6zM15 9h6"))
}

app_page_ui <- function(content) {
  htmltools::attachDependencies(shiny::div(class = "app-polish", content), app_dependencies(), append = TRUE)
}

app_expander <- function(title, ..., open = FALSE, id = NULL) {
  shiny::tags$details(id = id, class = "sidebar-expander", open = if (open) "open" else NULL,
    shiny::tags$summary(title), shiny::div(class = "sidebar-expander-body", ...))
}

app_numeric_input <- function(inputId, label, value, min = NA, max = NA,
                              step = NA, help = NULL, ...) {
  control <- shiny::numericInput(inputId, label, value, min = min, max = max, step = step, ...)
  if (!is.null(help)) control <- app_help_ui(control, help)
  control
}

app_dimension_controls <- function(ns, width_id = "width", height_id = "height",
    width_value = 400, height_value = 300, width_label = "Subplot width (px)",
    height_label = "Subplot height (px)", width_min = 200, height_min = 200,
    max = 2000, step = 50, gap = "16px", width_help = NULL, height_help = NULL) {
  bslib::layout_columns(col_widths = c(6, 6), gap = gap, fill = FALSE,
    app_numeric_input(ns(width_id), width_label, width_value, min = width_min,
      max = max, step = step, help = width_help),
    app_numeric_input(ns(height_id), height_label, height_value, min = height_min,
      max = max, step = step, help = height_help))
}

app_plot_size_ui <- function(ns, width_id = "width", height_id = "height", ...) {
  app_expander("Plot size", id = ns("size_section"),
    app_dimension_controls(ns, width_id, height_id, ...))
}

app_plot_style_ui <- function(ns, base_size_id = "base_size", base_size = PLOT_BASE_SIZE,
    show_legend_id = "show_legend", legend_position_id = "legend_position", colors_id = "colors",
    colors = shiny::uiOutput(ns(colors_id)),
    font = shiny::numericInput(ns(base_size_id), "Base size", base_size, min = 6, max = 30),
    legend = shiny::tagList(
      shiny::checkboxInput(ns(show_legend_id), "Show legend", TRUE),
      shiny::conditionalPanel(sprintf("input['%s']", ns(show_legend_id)),
        shiny::selectInput(ns(legend_position_id), "Legend position",
          c(Bottom = "bottom", Right = "right", Top = "top", Left = "left"), "right")))) {
  shiny::tagList(
    app_expander("Colors", id = ns("colors_section"), colors),
    app_expander("Text and legend", id = ns("style_section"),
      font, legend))
}

app_sidebar <- function(title, description = NULL, ..., width = 330, class = NULL) {
  bslib::sidebar(shiny::h4(class = "ta-sidebar-title", title),
    if (!is.null(description)) shiny::p(class = "ta-sidebar-subtitle", description),
    shiny::hr(), ..., width = width, open = "always", resizable = FALSE,
    class = paste(c("ta-sidebar app-sidebar", class), collapse = " "))
}

app_layout <- function(sidebar, ..., fill = FALSE, fillable = FALSE, border = FALSE,
                       padding = NULL, gap = NULL, class = NULL) {
  bslib::layout_sidebar(sidebar = sidebar, ..., fill = fill, fillable = fillable,
    border = border, padding = padding, gap = gap, class = class)
}

app_theme <- function(...) {
  defaults <- list(version = 5, bg = "#e8ebf0", fg = "#1b2637", primary = "#0b1d3a",
    secondary = "#647184",
    base_font = bslib::font_collection("-apple-system", "BlinkMacSystemFont", "Segoe UI", "sans-serif"),
    heading_font = bslib::font_collection("-apple-system", "BlinkMacSystemFont", "Segoe UI", "sans-serif"))
  do.call(bslib::bs_theme, utils::modifyList(defaults, list(...)))
}

app_navbar <- function(..., title = NULL, main = "Analysis", accent = "App", version = NULL,
    logo = NULL, id = "page", theme = app_theme(), header = NULL) {
  if (is.null(title)) title <- app_brand_ui(main, accent, version, logo)
  page <- bslib::page_navbar(..., title = title, id = id, theme = theme, header = header)
  # Root dependencies precede descendants when htmltools collects assets. Put
  # Bootstrap/widget dependencies first so our common overrides load last.
  dependencies <- htmltools::resolveDependencies(htmltools::findDependencies(page))
  dependencies <- Filter(function(x) !identical(x$name, "NicoShinyAppTemplate"), dependencies)
  htmltools::attachDependencies(page, c(dependencies, list(app_dependencies())), append = FALSE)
}
