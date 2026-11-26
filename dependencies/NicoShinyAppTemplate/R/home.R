# Legacy category/button arguments remain accepted; cards are informational.
home_analysis_card <- function(ns, key, title, category = NULL, description = NULL,
                               symbol = "chart-simple", button = "Open analysis") {
  shiny::tags$article(id = ns(key), class = paste("home-analysis-card", paste0("home-card-", key)),
    shiny::div(class = "home-card-top",
      shiny::span(class = "home-card-icon", app_analysis_icon(symbol)),
      shiny::h3(title)),
    shiny::p(description))
}

app_home_ui <- function(id, main = "Analysis", accent = "App", version = NULL,
    description = NULL, analyses = list(), workflow = list(), artwork = NULL,
    start_label = "Start an analysis", start_id = "upload", analyses_title = NULL,
    workflow_title = "From your files to your figures", class = NULL, extra = NULL) {
  ns <- shiny::NS(id)
  cards <- lapply(analyses, function(item) {
    if (inherits(item, c("shiny.tag", "shiny.tag.list"))) return(item)
    do.call(home_analysis_card, c(list(ns = ns), item))
  })
  steps <- lapply(seq_along(workflow), function(i) {
    item <- workflow[[i]]
    if (inherits(item, c("shiny.tag", "shiny.tag.list"))) return(item)
    shiny::tags$li(shiny::span(class = "home-step-number", sprintf("%02d", i)),
      shiny::div(shiny::h3(item$title,
        if (isTRUE(item$optional)) shiny::span(class = "home-optional", "Optional")),
        shiny::p(item$description)))
  })
  result <- shiny::div(class = paste(c("app-home", class), collapse = " "),
    shiny::tags$section(class = paste("home-hero", if (is.null(artwork)) "home-hero-no-art"),
      `aria-labelledby` = ns("title"),
      if (!is.null(artwork)) shiny::tags$img(class = "home-hero-art", src = artwork, alt = "", `aria-hidden` = "true"),
      shiny::div(class = "home-hero-copy",
        shiny::h1(id = ns("title"), class = "home-wordmark",
          `aria-label` = paste(paste0(main, accent), version),
          app_wordmark_ui("home-brand-name", main, accent),
          if (!is.null(version)) shiny::span(class = "home-brand-version", version)),
        shiny::p(class = "home-hero-description", description),
        if (!is.null(start_label)) shiny::div(class = "home-hero-actions",
          shiny::actionButton(ns(start_id), start_label,
            icon = shiny::icon("arrow-right"), class = "home-start")))),
    if (length(steps)) shiny::tags$section(class = "home-workflow", `aria-labelledby` = ns("workflow_title"),
      shiny::h2(id = ns("workflow_title"), workflow_title), shiny::tags$ol(class = "home-workflow-grid",
        style = sprintf("--home-step-columns:%d;", max(1L, length(steps))), steps)),
    if (length(cards)) shiny::tags$section(class = "home-analyses", id = ns("analyses"),
      `aria-labelledby` = if (!is.null(analyses_title)) ns("analyses_title"),
      `aria-label` = if (is.null(analyses_title)) "Analyses",
      if (!is.null(analyses_title)) shiny::div(class = "home-section-heading", shiny::h2(id = ns("analyses_title"), analyses_title)),
      shiny::div(class = "home-analysis-grid",
        style = sprintf("--home-card-columns:%d;", max(1L, min(4L, length(cards)))), cards)),
    extra)
  htmltools::attachDependencies(result, app_dependencies(), append = TRUE)
}
