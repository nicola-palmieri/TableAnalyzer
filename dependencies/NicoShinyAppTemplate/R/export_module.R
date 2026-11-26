# mod_export -- pixel subplot controls and 300-dpi export, like TableAnalyzer.

mod_export_controls_ui <- function(id) {
  app_plot_size_ui(shiny::NS(id))
}

mod_export_actions_ui <- function(id) {
  app_download_ui(shiny::NS(id), "download", "dimensions")
}

# Legacy combined placement; keep existing IDs and handlers during migration.
mod_export_ui <- function(id) {
  warning("mod_export_ui() is deprecated; use mod_export_controls_ui() in the sidebar and mod_export_actions_ui() inside app_result_header_ui().",
    call. = FALSE)
  shiny::tagList(
    mod_export_controls_ui(id),
    mod_export_actions_ui(id)
  )
}

# `plot`: reactive returning a ggplot. `filename`: file name without extension,
# a string or a function/reactive returning one. `prepare(plot, size)` must
# match what the page preview draws (default: content_sized_plot()).
mod_export_server <- function(id, plot, filename = "figure", size = NULL, prepare = content_sized_plot) {
  shiny::moduleServer(id, function(input, output, session) {
    settings <- shiny::reactive({
      w <- if (is.null(input$width)) 400 else input$width
      h <- if (is.null(input$height)) 300 else input$height
      ok <- function(x) is.numeric(x) && length(x) == 1 && is.finite(x) && x >= 200 && x <= 2000
      shiny::validate(shiny::need(ok(w) && ok(h), "Subplot dimensions must be between 200 and 2000 pixels."))
      list(width = w, height = h)
    })
    dimensions <- shiny::reactive(if (is.null(size)) settings() else size())
    output$dimensions <- shiny::renderText(png_pixel_dimensions(dimensions()))
    output$download <- shiny::downloadHandler(
      filename = function() {
        base <- if (is.function(filename)) filename() else filename
        paste0(base, ".png")
      },
      content = function(file) {
        d <- dimensions()
        p <- prepare(plot(), d)
        save_plot_png(p, file, width_px = d$width, height_px = d$height, dpi = 300)
      },
      contentType = "image/png"
    )
    settings
  })
}
