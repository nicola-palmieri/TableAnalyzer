test_that("two applications configure the same home and navbar components", {
  apps <- list(
    list(main = "Microbiome", accent = "Analyzer", key = "alpha"),
    list(main = "Table", accent = "Analyzer", key = "oneway"))
  for (app in apps) {
    home <- app_home_ui("home", main = app$main, accent = app$accent,
      version = "v1.00", description = paste(app$main, "description"),
      analyses = list(list(key = app$key, title = "Comparison", category = "Explore",
        description = "Application supplied copy", symbol = "boxplot", button = "Open")),
      workflow = list(list(title = "Upload", description = "Choose a file")))
    markup <- htmltools::renderTags(home)
    expect_match(markup$html, paste0(app$main, "<span"), fixed = TRUE)
    expect_match(markup$html, paste0('id="home-', app$key, '"'), fixed = TRUE)
    expect_match(markup$html, 'id="home-upload"', fixed = TRUE)
    expect_match(markup$html, 'aria-labelledby="home-title"', fixed = TRUE)
    expect_match(markup$html, 'alt="Vetmeduni Austria"', fixed = TRUE)
    expect_identical(sum(vapply(markup$dependencies, function(x) x$name == "VetmeduniAppTemplate", logical(1))), 1L)
    navbar <- app_navbar(bslib::nav_panel("Home", value = "home", home),
      main = app$main, accent = app$accent, window_title = paste0(app$main, app$accent))
    expect_match(as.character(navbar), 'data-home-input="go_home"', fixed = TRUE)
  }
  expect_match(as.character(app_wordmark_ui(main = "<custom>", accent = "&")), "&lt;custom&gt;", fixed = TRUE)
  expect_false(grepl('home-institution', as.character(app_home_ui("home", institution = FALSE)), fixed = TRUE))
  cards <- lapply(1:3, function(i) list(key = paste0("analysis", i), title = paste("Analysis", i)))
  expect_match(as.character(app_home_ui("home", analyses = cards)), "--home-card-columns:3;", fixed = TRUE)
  expect_match(as.character(app_home_ui("home", analyses = c(cards, cards))), "--home-card-columns:4;", fixed = TRUE)
  expect_equal(unname(bslib::bs_get_variables(app_theme(primary = "#123456"), "primary")), "#123456")
})

test_that("all dependency assets resolve in a loaded or installed package", {
  dependency <- app_dependencies()
  expect_identical(dependency$name, "VetmeduniAppTemplate")
  assets <- c(dependency$stylesheet, dependency$script)
  expect_true(all(file.exists(file.path(dependency$src$file, assets))))
  expect_true(file.exists(file.path(dependency$src$file, "vetmeduni-logo.svg")))
  expect_true(file.exists(system.file("branding", "farbschema_vetmeduni.pdf", package = "VetmeduniAppTemplate")))
  expect_equal(tolower(unname(bslib::bs_get_variables(app_theme(), "primary"))), "#af2144")
  help <- paste(readLines(file.path(dependency$src$file, "widget-help.js"), warn = FALSE), collapse = "\n")
  expect_match(help, "VetmeduniAppHelp", fixed = TRUE)
  expect_false(grepl("Microbiome|LinDA|PERMANOVA|phylum", help))
  expect_match(as.character(app_help_ui(shiny::textInput("name", "Name"), "Application explanation")),
    'data-help="Application explanation"', fixed = TRUE)
})

test_that("compact dimensions preserve namespaced IDs and app defaults", {
  controls <- app_dimension_controls(shiny::NS("page"), "plot_width", "plot_height",
    width_value = 650, height_value = 450, width_min = 100, height_min = 100)
  markup <- as.character(controls)
  expect_match(markup, 'id="page-plot_width"', fixed = TRUE)
  expect_match(markup, 'value="650"', fixed = TRUE)
  expect_match(markup, 'id="page-plot_height"', fixed = TRUE)
  expect_match(markup, 'value="450"', fixed = TRUE)
})

test_that("complete level order controls never filter incomplete selections", {
  shiny::testServer(mod_level_order_server, args = list(
    variables = shiny::reactive(list(condition = c("Control", "Treated", "Other")))), {
    session$flushReact()
    expect_identical(session$returned()$condition, c("Control", "Treated", "Other"))
    session$setInputs(order_all_condition = c("Treated", "Control", "Other"))
    expect_identical(session$returned()$condition, c("Treated", "Control", "Other"))
    session$setInputs(order_all_condition = "Other")
    expect_identical(session$returned()$condition, c("Treated", "Control", "Other"))
    session$setInputs(order_all_condition = c("Other", "Control", "Other"))
    expect_identical(session$returned()$condition, c("Treated", "Control", "Other"))
  })
})

test_that("palette allocation extends deterministically without changing initial colours", {
  expect_identical(app_colours(c("A", "B", "A")), stats::setNames(APP_PALETTE[1:2], c("A", "B")))
  expect_length(app_colours(character()), 0)
  extended <- app_colours(paste0("G",1:101))
  expect_length(extended,101)
  expect_identical(unname(head(extended,20)),APP_PALETTE)
  expect_identical(app_colours(paste0("G",1:100)),head(extended,100))
  expect_true(all(grepl("^#[0-9A-Fa-f]{6}$",extended)))
})

test_that("plot helpers leave global defaults alone and export exact screen proportions", {
  old <- ggplot2::theme_get()
  p <- ggplot2::ggplot(data.frame(x = 1:3, y = c(2, 5, 3)), ggplot2::aes(x, y)) +
    ggplot2::geom_point() + plot_theme(kind = "standard")
  expect_identical(ggplot2::theme_get(), old)
  expect_s3_class(plot_theme(kind = "void"), "theme")
  target <- tempfile(fileext = ".png")
  on.exit(unlink(target), add = TRUE)
  before <- grDevices::dev.cur()
  save_plot_png(p, target, width_px = 400, height_px = 300)
  expect_identical(grDevices::dev.cur(), before)
  expect_equal(png_info(target), list(width = 1250, height = 938, dpi_x = 300, dpi_y = 300))
  expect_error(save_plot_png(p, target, width_px = NA_real_, height_px = 300), "positive")
  expect_error(save_plot_png(p, target, width_px = Inf, height_px = 300), "positive")
})

test_that("manuscript themes omit titles without hiding semantic facet labels", {
  base <- ggplot2::ggplot(data.frame(x=1:3,y=c(2,5,3)),ggplot2::aes(x,y)) +
    ggplot2::geom_point() + ggplot2::labs(title="Figure title",subtitle="Figure subtitle",caption="Statistical note")
  for (kind in c("standard","bar","box","pcoa","heatmap","canvas","void")) {
    p <- base + plot_theme(kind=kind)
    g <- ggplot2::ggplotGrob(p)
    for (name in c("title","subtitle")) expect_s3_class(g$grobs[[match(name,g$layout$name)]],"zeroGrob")
  }
  original <- base + plot_theme()
  panel <- plot_panel_label(original,"Treatment: Control")
  g <- ggplot2::ggplotGrob(panel)
  expect_true(any(grepl("^strip",g$layout$name)))
  expect_equal(ggplot2::ggplot_build(panel)$data[[1]][c("x","y")],
    ggplot2::ggplot_build(original)$data[[1]][c("x","y")])
  expect_identical(plot_panel_label(original,NULL),original)
})

test_that("guide compilation namespaces duplicate headings and rewrites only local assets", {
  guide <- tempfile(fileext = ".md")
  on.exit(unlink(guide), add = TRUE)
  writeLines(c("# Guide", "## First section", "See [details](#details).",
    "### Details", "[![Local image](assets/plot.png)](assets/plot.png)",
    "### Details", "![Remote](https://example.invalid/remote.png)",
    "```r", "stop('must never execute')", "```"), guide)
  rendered <- user_guide_document(guide, shiny::NS("manual"), asset_prefix = "manual-assets")
  expect_identical(rendered$contents$id,
    c("manual-section-first-section", "manual-section-details", "manual-section-details-1"))
  expect_match(rendered$html, 'href="#manual-section-details"', fixed = TRUE)
  expect_match(rendered$html, 'src="manual-assets/plot.png"', fixed = TRUE)
  expect_match(rendered$html, 'href="manual-assets/plot.png" target="_blank" rel="noopener"', fixed = TRUE)
  expect_match(rendered$html, 'src="https://example.invalid/remote.png"', fixed = TRUE)
  expect_match(rendered$html, "must never execute", fixed = TRUE)
  expect_match(as.character(mod_user_guide_ui("manual", guide)), 'id="manual-article"', fixed = TRUE)
})

test_that("the shared PNG renderer supports grid and pairwise matrix figures", {
  target <- tempfile(fileext = ".png")
  on.exit(unlink(target), add = TRUE)
  before <- grDevices::dev.cur()
  save_plot_png(grid::textGrob("Shared figure"), target, 400, 300)
  expect_equal(png_info(target), list(width=1250,height=938,dpi_x=300,dpi_y=300))
  expect_identical(grDevices::dev.cur(), before)
  if (requireNamespace("GGally", quietly = TRUE)) {
    pair <- GGally::ggpairs(data.frame(a=c(1,3,2,5,4,6),b=c(4,2,6,3,1,5)),progress=FALSE)
    save_plot_png(pair, target, 400, 300)
    expect_equal(png_info(target), list(width=1250,height=938,dpi_x=300,dpi_y=300))
    expect_identical(grDevices::dev.cur(), before)
  }
})

test_that("new app scaffolds load, namespace navigation and never overwrite content", {
  path <- tempfile("new-analysis-app-")
  on.exit(unlink(path, recursive = TRUE), add = TRUE)
  create_app(path, name = "ExampleAnalyzer", main = 'Example "quoted"', accent = "Analyzer")
  expect_true(all(file.exists(file.path(path, c("app.R", "R/00_shared_dependencies.R",
    "R/shared_app_config.R", "R/mod_home_ui.R", "R/mod_home_main.R", "README.md")))))
  original <- getwd()
  on.exit(setwd(original), add = TRUE)
  setwd(path)
  appenv <- new.env(parent = globalenv())
  app <- source("app.R", local = appenv)$value
  expect_s3_class(app, "shiny.appobj")
  expect_identical(appenv$APP_CONFIG$main, 'Example "quoted"')
  expect_match(htmltools::renderTags(appenv$ui)$html, 'id="home-start"', fixed = TRUE)
  selected <- NULL
  shiny::testServer(appenv$mod_home_server, args = list(navigate = function(page) selected <<- page), {
    session$setInputs(start = 1)
    expect_identical(selected, "explore")
  })
  before <- readLines("R/shared_app_config.R")
  expect_error(create_app(path, main = "Replacement"), "never overwritten")
  expect_identical(readLines("R/shared_app_config.R"), before)
})

test_that("automatic snapshots commit valid settings and retain complete last-valid figures", {
  shiny::testServer(function(input, output, session) {
    builds <- 0L
    snapshot <- app_plot_snapshot(function() {
      builds <<- builds + 1L
      width <- app_plot_numeric(input$width, 400, min = 200, max = 2000, label = "Width")
      list(plot = paste("figure", width), width = width, warning = NULL)
    })
  }, {
    session$setInputs(width = 600)
    expect_identical(snapshot()$plot, "figure 600")
    before <- builds
    session$setInputs(unrelated = "keep the figure")
    expect_identical(builds, before)
    session$setInputs(width = NA_real_)
    expect_identical(snapshot()$plot, "figure 600")
    expect_equal(snapshot()$width, 600)
    expect_match(snapshot()$warning, "Width must be a finite number")
    session$setInputs(width = 800)
    expect_identical(snapshot()$plot, "figure 800")
    expect_null(snapshot()$warning)
  })
})
