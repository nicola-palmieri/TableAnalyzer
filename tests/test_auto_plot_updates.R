library(testthat)
library(shiny)
library(ggplot2)

for (file in list.files("../R", pattern = "[.]R$", full.names = TRUE)) source(file)

test_that("every appearance page has no Apply action or nested layout expander", {
  pages <- list(visualize_oneway_ui("oneway"), visualize_twoway_ui("twoway"),
    visualize_pca_ui("pca"), visualize_ggpairs_ui("correlation"),
    tagList(visualize_categorical_barplots_ui("categorical"), visualize_categorical_barplots_plot_ui("categorical")),
    tagList(visualize_numeric_boxplots_ui("boxplots"), visualize_numeric_boxplots_plot_ui("boxplots")),
    tagList(visualize_numeric_histograms_ui("histograms"), visualize_numeric_histograms_plot_ui("histograms")))
  for (page in pages) {
    document <- xml2::read_html(htmltools::renderTags(page)$html)
    expect_length(xml2::xml_find_all(document, "//button[contains(@id,'apply_plot')]"), 0L)
    expect_length(xml2::xml_find_all(document, "//details//details"), 0L)
    expect_false(any(grepl("Advanced", xml2::xml_text(xml2::xml_find_all(document, "//summary")))))
    expect_identical(xml2::xml_text(xml2::xml_find_first(document, "//button[contains(@id,'-reset')]")), "Reset")
    download <- xml2::xml_find_first(document, "//a[contains(@id,'download_plot')]")
    expect_identical(xml2::xml_attr(download, "aria-label"), "Download PNG (300 dpi)")
    expect_identical(trimws(xml2::xml_text(download)), "")
    expect_length(xml2::xml_find_all(download, "ancestor::div[contains(@class,'app-result-header')]"), 1L)
  }
})

capture_plot_reset <- function(server) {
  context <- new.env(parent = environment(server))
  context$messages <- list()
  context$updateNumericInput <- function(session, inputId, value = NULL, ...) {
    context$messages[[inputId]] <- list(value = value)
    shiny::updateNumericInput(session, inputId, value = value, ...)
  }
  context$updateCheckboxInput <- function(session, inputId, value = NULL, ...) {
    context$messages[[inputId]] <- list(value = value)
    shiny::updateCheckboxInput(session, inputId, value = value, ...)
  }
  context$updateSelectInput <- function(session, inputId, selected = NULL, ...) {
    context$messages[[inputId]] <- list(value = selected)
    shiny::updateSelectInput(session, inputId, selected = selected, ...)
  }
  environment(server) <- context
  list(server = server, context = context)
}

test_that("PCA Reset restores context defaults and colours without changing its fitted model", {
  data <- iris
  names(data)[5] <- "group"
  frozen <- list(type = "pca", model = list(data = data, model = stats::prcomp(data[1:4], scale. = TRUE)))
  fit <- reactiveVal(frozen)
  filtered <- reactiveVal(data)
  fixture <- capture_plot_reset(visualize_pca_server)
  testServer(fixture$server, args = list(filtered_data = filtered, model_fit = fit), {
    session$setInputs(plot_type = "biplot", pca_color = "group", plot_width = 650,
      plot_height = 450, plot_base_size = 17, col_group_1 = "#FF0000")
    expect_equal(unname(custom_colors()[1]), "#FF0000")
    fixture$context$messages <- list()
    session$setInputs(reset = 1)
    expect_equal(fixture$context$messages$plot_width$value, 800)
    expect_equal(fixture$context$messages$plot_height$value, 600)
    expect_equal(fixture$context$messages$plot_base_size$value, NicoShinyAppTemplate::PLOT_BASE_SIZE)
    expect_equal(unname(custom_colors()[1]), NicoShinyAppTemplate::APP_PALETTE[1])
    do.call(session$setInputs, lapply(fixture$context$messages, `[[`, "value"))
    expect_equal(plot_dimensions(), list(w = 800, h = 600))
    expect_identical(fit(), frozen)
  })
})

test_that("descriptive Reset restores default appearance, automatic grid and core palette", {
  data <- data.frame(value = seq_len(18), group = factor(rep(c("A", "B", "C"), each = 6)))
  frozen <- list(type = "descriptive", selected_vars = "value", group_var = "group")
  summary <- reactiveVal(frozen)
  filtered <- reactiveVal(data)
  fixture <- capture_plot_reset(visualize_numeric_histograms_server)
  testServer(fixture$server, args = list(filtered_data = filtered, summary_info = summary), {
    session$setInputs(plot_width = 650, plot_height = 450, plot_base_size = 17,
      use_density = TRUE, col_group_1 = "#FF0000", `plot_grid-rows` = 4, `plot_grid-cols` = 4)
    fixture$context$messages <- list()
    session$setInputs(reset = 1)
    expect_equal(fixture$context$messages$plot_width$value, 400)
    expect_equal(fixture$context$messages$plot_height$value, 300)
    expect_equal(fixture$context$messages$plot_base_size$value, NicoShinyAppTemplate::PLOT_BASE_SIZE)
    expect_false(grid$is_manual())
    expect_equal(unname(custom_colors()), NicoShinyAppTemplate::APP_PALETTE[1:3])
    do.call(session$setInputs, lapply(fixture$context$messages, `[[`, "value"))
    expect_equal(c(stored$plot_width, stored$plot_height), c(400, 300))
    expect_null(stored$warning)
    expect_identical(summary(), frozen)
  })
})

test_that("ANOVA Reset is local and works before a model is available", {
  fit <- reactiveVal(NULL)
  filtered <- reactiveVal(NULL)
  fixture <- capture_plot_reset(visualize_oneway_server)
  testServer(fixture$server, args = list(filtered_data = filtered, model_info = fit), {
    session$flushReact()
    session$setInputs(plot_width = 650, plot_height = 450, plot_base_size = 17)
    fixture$context$messages <- list()
    session$setInputs(reset = 1)
    expect_equal(fixture$context$messages$plot_width$value, 400)
    expect_equal(fixture$context$messages$plot_height$value, 300)
    expect_equal(fixture$context$messages$plot_base_size$value, NicoShinyAppTemplate::PLOT_BASE_SIZE)
    expect_identical(fixture$context$messages$plot_type$value, "lineplot_mean_se")
    expect_null(fit())
    expect_null(filtered())
  })
})

test_that("two-way ANOVA Reset restores its appearance and core palette without refitting", {
  data <- expand.grid(group = factor(c("A", "B")), treatment = factor(c("x", "y")), replicate = 1:6)
  data$value <- as.integer(data$group) * 8 + as.integer(data$treatment) * 2 + data$replicate
  frozen <- list(type = "twoway_anova", responses = "value",
    factors = list(factor1 = "group", factor2 = "treatment"),
    orders = list(order1 = c("A", "B"), order2 = c("x", "y")),
    models = list(value = list(model = stats::lm(value ~ group * treatment, data))))
  fit <- reactiveVal(frozen)
  filtered <- reactiveVal(data)
  fixture <- capture_plot_reset(visualize_twoway_server)
  testServer(fixture$server, args = list(filtered_data = filtered, model_info = fit), {
    session$setInputs(plot_type = "boxplot", plot_width = 650, plot_height = 450,
      plot_base_size = 17, col_treatment_1 = "#FF0000", use_common_legend = TRUE)
    fixture$context$messages <- list()
    session$setInputs(reset = 1)
    expect_equal(fixture$context$messages$plot_width$value, 400)
    expect_equal(fixture$context$messages$plot_height$value, 300)
    expect_equal(fixture$context$messages$plot_base_size$value, NicoShinyAppTemplate::PLOT_BASE_SIZE)
    expect_false(grid_settings$is_manual())
    expect_equal(unname(custom_colors()), NicoShinyAppTemplate::APP_PALETTE[1:2])
    do.call(session$setInputs, lapply(fixture$context$messages, `[[`, "value"))
    expect_equal(c(stored$plot_width, stored$plot_height), c(400, 300))
    expect_null(stored$warning)
    expect_identical(fit(), frozen)
  })
})

test_that("PCA appearance updates use the existing model and retain invalid edits", {
  data <- iris
  names(data)[5] <- "group"
  entry <- list(data = data, model = stats::prcomp(data[1:4], scale. = TRUE))
  frozen <- list(type = "pca", model = entry)
  fit <- reactiveVal(frozen)
  filtered <- reactiveVal(data)
  testServer(visualize_pca_server, args = list(filtered_data = filtered, model_fit = fit), {
    session$setInputs(plot_type = "biplot", plot_width = 800, plot_height = 600, plot_base_size = 13)
    original <- plot_info()$plot
    session$setInputs(plot_base_size = 17, plot_width = 600, plot_height = 400)
    expect_false(identical(plot_info()$plot, original))
    expect_equal(plot_dimensions(), list(w = 600, h = 400))
    expect_identical(fit(), frozen)
    valid <- plot_info()$plot
    session$setInputs(plot_width = NA_real_)
    expect_identical(plot_info()$plot, valid)
    expect_equal(plot_dimensions(), list(w = 600, h = 400))
    expect_match(plot_info()$warning, "Plot width must be a finite number")
    session$setInputs(plot_width = 650)
    expect_null(plot_info()$warning)
    expect_equal(plot_dimensions()$w, 650)
    expect_identical(fit(), frozen)
  })
})

test_that("descriptive dimensions and appearances refresh without replacing the summary", {
  data <- data.frame(value = seq_len(18), group = factor(rep(c("A", "B", "C"), each = 6)))
  frozen <- list(type = "descriptive", selected_vars = c("value", "group"), group_var = NULL)
  summary <- reactiveVal(frozen)
  filtered <- reactiveVal(data)
  testServer(visualize_numeric_histograms_server,
    args = list(filtered_data = filtered, summary_info = summary), {
    session$setInputs(plot_width = 400, plot_height = 300, use_density = FALSE)
    original <- stored$plot
    session$setInputs(use_density = TRUE, plot_width = 600, plot_height = 400)
    expect_false(identical(stored$plot, original))
    expect_equal(c(stored$plot_width, stored$plot_height), c(600, 400))
    expect_identical(summary(), frozen)
    valid <- stored$plot
    session$setInputs(plot_base_size = NA_real_)
    expect_identical(stored$plot, valid)
    expect_equal(c(stored$plot_width, stored$plot_height), c(600, 400))
    expect_match(stored$warning, "Base size must be a finite number")
  })
})

test_that("Table downloads preserve the shared exact 300-dpi pixel contract", {
  server <- visualize_numeric_histograms_server
  context <- new.env(parent = environment(server))
  context$download_content <- NULL
  context$downloadHandler <- function(filename, content, ...) {
    context$download_content <- content
    shiny::downloadHandler(filename = filename, content = content, ...)
  }
  environment(server) <- context
  data <- data.frame(value = seq_len(18))
  filtered <- reactiveVal(data)
  summary <- reactiveVal(list(type = "descriptive", selected_vars = "value", group_var = NULL))
  testServer(server, args = list(filtered_data = filtered, summary_info = summary), {
    session$setInputs(plot_width = 400, plot_height = 300, use_density = FALSE)
    target <- tempfile(fileext = ".png")
    on.exit(unlink(target), add = TRUE)
    context$download_content(target)
    expect_equal(NicoShinyAppTemplate::png_info(target),
      list(width = 1250, height = 938, dpi_x = 300, dpi_y = 300))
    session$setInputs(plot_width = NA_real_)
    context$download_content(target)
    expect_equal(NicoShinyAppTemplate::png_info(target),
      list(width = 1250, height = 938, dpi_x = 300, dpi_y = 300))
  })
})
