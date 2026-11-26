library(testthat)
library(shiny)

for (file in list.files("../R", pattern = "[.]R$", full.names = TRUE)) source(file)

test_that("five persistent visual panels have independent namespaces and visibility flags", {
  context <- new.env(parent = environment(mod_visualize_ui))
  for (name in c("visualize_oneway_server", "visualize_twoway_server", "visualize_ggpairs_server",
                 "visualize_pca_server", "visualize_descriptive_server")) {
    assign(name, function(...) stop("Building visual controls must not initialize a server"), envir = context)
  }
  make_ui <- mod_visualize_ui
  environment(make_ui) <- context
  document <- xml2::read_html(htmltools::renderTags(make_ui("visualize"))$html)
  panels <- xml2::xml_find_all(document, "//*[@class='visualization-method-panel']")
  types <- c("oneway_anova", "twoway_anova", "pairs", "pca", "descriptive")
  expect_equal(xml2::xml_attr(panels, "data-visualization"), types)
  for (i in seq_along(types)) {
    condition <- xml2::xml_attr(xml2::xml_parent(panels[[i]]), "data-display-if")
    expect_match(condition, "output['visualize-active_type']", fixed = TRUE)
    expect_match(condition, paste0("=== '", types[[i]], "'"), fixed = TRUE)
  }
  ids <- xml2::xml_attr(xml2::xml_find_all(document, "//*[@id]"), "id")
  expect_equal(anyDuplicated(ids), 0L)
  for (key in c("oneway", "twoway", "ggpairs", "pca", "descriptive")) {
    expect_true(paste0("visualize-", key, "-plot_type") %in% ids)
  }
})

test_that("visual servers initialize once only for valid selected results", {
  fixture <- new.env(parent = environment(mod_visualize_server))
  fixture$mounted <- c(oneway = 0L, twoway = 0L, ggpairs = 0L, pca = 0L, descriptive = 0L)
  stub_server <- function(key) {
    force(key)
    function(...) {
      fixture$mounted[[key]] <- fixture$mounted[[key]] + 1L
      NULL
    }
  }
  for (key in names(fixture$mounted)) {
    assign(paste0("visualize_", key, "_server"), stub_server(key), envir = fixture)
  }
  coordinator <- mod_visualize_server
  environment(coordinator) <- fixture
  data <- reactiveVal(data.frame(value = 1:3))
  model <- reactiveVal(NULL)
  selected <- reactiveVal(NULL)
  labels <- c(oneway_anova = "One-way ANOVA", twoway_anova = "Two-way ANOVA",
    pairs = "Pairwise Correlation", pca = "PCA", descriptive = "Descriptive Statistics")
  keys <- c(oneway_anova = "oneway", twoway_anova = "twoway", pairs = "ggpairs",
    pca = "pca", descriptive = "descriptive")
  testServer(coordinator,
    args = list(filtered_data = data, model_fit = model, analysis_selection = selected), {
    session$flushReact()
    expect_true(all(fixture$mounted == 0L))
    expect_identical(output$active_type, "")
    for (kind in names(labels)) {
      model(NULL)
      selected(labels[[kind]])
      session$flushReact()
      expect_identical(output$active_type, "")
      model(list(type = kind, summary = list(value = 1)))
      session$flushReact()
      expect_identical(output$active_type, kind)
      expect_equal(fixture$mounted[[keys[[kind]]]], 1L)
    }
    selected("PCA")
    session$flushReact()
    expect_identical(output$active_type, "")
    model(list(type = "pca", summary = list(value = 2)))
    session$flushReact()
    expect_identical(output$active_type, "pca")
    expect_true(all(fixture$mounted == 1L))
    data(NULL)
    session$flushReact()
    expect_identical(output$active_type, "")
    expect_false(display_state()$show_visual)
    data(data.frame(value = 4:6))
    session$flushReact()
    expect_identical(output$active_type, "pca")
    expect_true(all(fixture$mounted == 1L))
    selected("Linear Model (LM)")
    model(list(type = "lm", summary = list(value = 3)))
    session$flushReact()
    expect_identical(output$active_type, "")
    expect_match(output$empty_ui$html, "Visualization coming soon", fixed = TRUE)
    expect_true(all(fixture$mounted == 1L))
  })
})
