app_dependencies <- function() {
  htmltools::htmlDependency(name = "NicoShinyAppTemplate", version = as.character(getNamespaceVersion("NicoShinyAppTemplate")),
    src = c(file = system.file("www", package = "NicoShinyAppTemplate", mustWork = TRUE)),
    stylesheet = c("theme-table-core.css", "theme-table-components.css", "theme-table-datatables.css",
      "theme-layout.css", "theme-workspace.css", "color-controls.css", "widget-help.css"),
    script = c("color-controls.js", "navigation.js", "widget-help.js", "analysis.js"),
    all_files = FALSE)
}

app_help_ui <- function(content, text) {
  htmltools::attachDependencies(shiny::div(class = "app-help-control", `data-help` = text, content),
    app_dependencies(), append = TRUE)
}

app_help_icon <- function(text, label = "this control") {
  htmltools::attachDependencies(shiny::tags$button(type = "button", class = "app-help-icon",
    `aria-label` = paste0("Help: ", label), `data-help` = text, "?"), app_dependencies(), append = TRUE)
}

app_help_attributes <- function(text) list(`data-help` = text)

app_color_dropdown <- function(...) {
  htmltools::attachDependencies(barplot_color_dropdown(...), app_dependencies(), append = TRUE)
}
