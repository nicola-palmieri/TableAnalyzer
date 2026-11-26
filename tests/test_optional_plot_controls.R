library(testthat)
library(shiny)

for (file in list.files("../R", pattern = "[.]R$", full.names = TRUE)) source(file)

test_that("optional metadata distinguishes unavailable fits from technical errors", {
  metadata <- list(type = "oneway_anova", responses = c("value", "second"))
  expect_null(optional_plot_metadata(NULL))
  expect_null(optional_plot_metadata(req(FALSE)))
  expect_null(optional_plot_metadata(metadata, "twoway_anova"))
  expect_identical(optional_plot_metadata(metadata, "oneway_anova"), metadata)
  expect_error(optional_plot_metadata(stop("technical failure")), "technical failure", fixed = TRUE)
})

test_that("ANOVA layout widgets clear and recover when the active analysis changes", {
  values <- data.frame(value = 1:8, second = 8:1, group = factor(rep(c("A", "B"), 4)))
  for (kind in c("oneway_anova", "twoway_anova")) {
    server <- if (kind == "oneway_anova") visualize_oneway_server else visualize_twoway_server
    metadata <- reactiveVal(NULL)
    provider <- reactive({ req(metadata()); metadata() })
    frozen <- list(type = kind, responses = c("value", "second"), models = list())
    testServer(server, args = list(filtered_data = reactive(values), model_info = provider), {
      session$setInputs(plot_width = 400, plot_height = 300)
      expect_null(output$layout_controls)
      metadata(frozen)
      session$flushReact()
      expect_match(output$layout_controls$html, "response_grid", fixed = TRUE)
      metadata(list(type = "pca"))
      session$flushReact()
      expect_null(output$layout_controls)
      metadata(NULL)
      session$flushReact()
      expect_null(output$layout_controls)
      metadata(frozen)
      session$flushReact()
      expect_match(output$layout_controls$html, "response_grid", fixed = TRUE)
      expect_identical(metadata(), frozen)
    })
  }
})

test_that("descriptive legend and outlier widgets survive unavailable metadata", {
  values <- data.frame(value = 1:8, group = factor(rep(c("A", "B"), 4)))
  for (server in list(visualize_categorical_barplots_server,
      visualize_numeric_boxplots_server, visualize_numeric_histograms_server)) {
    data <- reactiveVal(values)
    metadata <- reactiveVal(NULL)
    provider <- reactive({ req(metadata()); metadata() })
    frozen <- list(type = "descriptive", selected_vars = "value", group_var = "group")
    testServer(server, args = list(filtered_data = reactive({ req(data()); data() }),
        summary_info = provider), {
      expect_null(output$common_legend_controls)
      metadata(frozen)
      session$flushReact()
      expect_match(output$common_legend_controls$html, "use_common_legend", fixed = TRUE)
      metadata(NULL)
      data(NULL)
      session$flushReact()
      expect_null(output$common_legend_controls)
      data(values)
      metadata(frozen)
      session$flushReact()
      expect_match(output$common_legend_controls$html, "use_common_legend", fixed = TRUE)
      expect_identical(metadata(), frozen)
    })
  }
})

test_that("correlation grid widgets survive an unavailable analysis", {
  values <- data.frame(value = 1:8, second = 8:1, group = factor(rep(c("A", "B"), 4)))
  metadata <- reactiveVal(NULL)
  provider <- reactive({ req(metadata()); metadata() })
  frozen <- list(type = "pairs", group_var = "group", selected_vars = c("value", "second"))
  testServer(pairwise_correlation_visualize_ggpairs_server,
      args = list(filtered_data = reactive(values), correlation_info = provider), {
    expect_null(output$grid_controls)
    metadata(frozen)
    session$flushReact()
    expect_match(output$grid_controls$html, "plot_grid", fixed = TRUE)
    metadata(NULL)
    session$flushReact()
    expect_null(output$grid_controls)
    metadata(frozen)
    session$flushReact()
    expect_identical(metadata(), frozen)
  })
})
