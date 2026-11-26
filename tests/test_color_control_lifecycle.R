library(testthat)
library(shiny)

for (file in list.files("../R", pattern = "[.]R$", full.names = TRUE)) source(file)

test_that("optional color controls clear and recover during ordinary no-data states", {
  source_data <- reactiveVal(NULL)
  metadata_available <- reactiveVal(FALSE)
  values <- data.frame(group = factor(c("A", "B", "A")))
  testServer(function(input, output, session) {
    colors <- add_color_customization_server(
      session$ns, input, output,
      data = reactive({ req(source_data()); source_data() }),
      color_var_reactive = reactive({ req(metadata_available()); "group" }),
      level_order_reactive = reactive({ req(metadata_available()); c("A", "B") })
    )
  }, {
    expect_null(output$color_custom_ui)
    source_data(values)
    session$flushReact()
    expect_match(output$color_custom_ui$html, "Plot color", fixed = TRUE)
    metadata_available(TRUE)
    session$flushReact()
    expect_match(output$color_custom_ui$html, "col_group_1", fixed = TRUE)
    expect_equal(colors(), stats::setNames(NicoShinyAppTemplate::APP_PALETTE[1:2], c("A", "B")))
    source_data(NULL)
    metadata_available(FALSE)
    session$flushReact()
    expect_null(output$color_custom_ui)
    source_data(values)
    metadata_available(TRUE)
    session$flushReact()
    expect_match(output$color_custom_ui$html, "col_group_2", fixed = TRUE)
    expect_identical(source_data(), values)
  })
})

test_that("PCA color metadata survives switching analyses and clearing uploaded data", {
  values <- iris
  names(values)[5] <- "group"
  frozen <- list(type = "pca", model = list(data = values,
    model = stats::prcomp(values[1:4], scale. = TRUE)))
  model <- reactiveVal(frozen)
  filtered <- reactiveVal(values)
  testServer(visualize_pca_server, args = list(filtered_data = filtered, model_fit = model), {
    session$setInputs(plot_type = "biplot", pca_color = "group", facet_var = "group")
    expect_identical(color_data(), values)
    expect_match(output$color_custom_ui$html, "col_group_1", fixed = TRUE)
    expect_match(output$layout_controls$html, "facet_grid", fixed = TRUE)
    model(list(type = "pairs", summary = list(ready = TRUE)))
    session$flushReact()
    expect_identical(color_data(), values)
    expect_match(output$color_custom_ui$html, "col_group_1", fixed = TRUE)
    expect_null(output$layout_controls)
    model(NULL)
    filtered(NULL)
    session$flushReact()
    expect_null(color_data())
    expect_null(output$color_custom_ui)
    filtered(values)
    model(frozen)
    session$flushReact()
    session$setInputs(facet_var = "group")
    expect_identical(color_data(), values)
    expect_match(output$color_custom_ui$html, "col_group_1", fixed = TRUE)
    expect_match(output$layout_controls$html, "facet_grid", fixed = TRUE)
    expect_identical(model(), frozen)
  })
})

test_that("PCA keeps optional metadata controls current without enabling hidden plots", {
  fixture <- new.env(parent = environment(visualize_pca_server))
  current_outputs <- character()
  fixture$keep_optional_ui_current <- function(output, ...) {
    current_outputs <<- c(current_outputs, unlist(list(...), use.names = FALSE))
    keep_optional_ui_current(output, ...)
  }
  fixture$add_color_customization_server <- add_color_customization_server
  environment(fixture$add_color_customization_server) <- fixture
  server <- visualize_pca_server
  environment(server) <- fixture
  testServer(server, args = list(filtered_data = reactive(NULL), model_fit = reactive(NULL)), {
    session$flushReact()
    expect_setequal(current_outputs,
      c("color_custom_ui", "pca_color_ui", "pca_shape_ui", "layout_controls"))
    expect_false(any(c("plot", "plot_container", "plot_warning") %in% current_outputs))
    expect_null(output$color_custom_ui)
    expect_match(output$pca_color_ui$html, "None", fixed = TRUE)
    expect_null(output$layout_controls)
  })
})
