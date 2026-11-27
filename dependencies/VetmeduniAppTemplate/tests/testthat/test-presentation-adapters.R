test_that("custom style slots retain app input IDs without duplicate default widgets", {
  markup <- as.character(app_plot_style_ui(shiny::NS("page"),
    colors = shiny::uiOutput("page-custom_colors"),
    font = shiny::numericInput("page-plot_base_size", "Font size", 17), legend = NULL))
  expect_match(markup, 'id="page-custom_colors"', fixed = TRUE)
  expect_match(markup, 'id="page-plot_base_size"', fixed = TRUE)
  expect_false(grepl('id="page-show_legend"', markup, fixed = TRUE))
  expect_match(as.character(app_plot_style_ui(shiny::NS("page"))), 'id="page-show_legend"', fixed = TRUE)
})

test_that("optional controls suppress only Shiny silent validation", {
  expect_null(app_optional_value(shiny::req(FALSE)))
  expect_error(app_optional_value(stop("unexpected failure")), "unexpected failure")
  expect_identical(app_optional_value(list(type = "model")), list(type = "model"))
})

test_that("table defaults preserve explicit app search and paging policies", {
  skip_if_not_installed("DT")
  options <- app_table_options("Taxon", options = list(pageLength = 25L,
    language = list(search = "Find:"), search = list(regex = FALSE, smart = FALSE)))
  expect_identical(options$pageLength, 25L)
  expect_identical(options$language, list(search = "Find:", searchPlaceholder = "Taxon"))
  expect_identical(options$search, list(regex = FALSE, smart = FALSE))
  expect_true(options$deferRender && options$scrollX)
  expect_match(as.character(options$initComplete), "tabIndex = 0", fixed = TRUE)
})

test_that("split export controls preserve legacy IDs and separate download placement", {
  controls <- as.character(mod_export_controls_ui("figure"))
  actions <- as.character(mod_export_actions_ui("figure"))
  expect_match(controls, 'id="figure-width"', fixed = TRUE)
  expect_false(grepl("shiny-download-link", controls))
  expect_match(actions, 'id="figure-download"', fixed = TRUE)
  expect_warning(legacy <- as.character(mod_export_ui("figure")), "deprecated")
  expect_match(legacy, 'id="figure-download"', fixed = TRUE)
})
