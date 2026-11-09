library(testthat)
library(shiny)
library(ggplot2)

for (source_file in list.files("../R", pattern = "[.]R$", full.names = TRUE)) source(source_file)

boxplot_panels <- function(plot) {
  if (!inherits(plot, "patchwork")) return(list(plot))
  unlist(lapply(seq_len(length(plot)), function(i) boxplot_panels(plot[[i]])), recursive = FALSE)
}

boxplot_points <- function(plot) {
  layers <- vapply(plot$layers, function(layer) inherits(layer$geom, "GeomPoint"), logical(1))
  ggplot_build(plot)$data[layers]
}

boxplot_fixture <- function(two_way = FALSE, stratified = FALSE) {
  data <- expand.grid(group = factor(c("a", "b")), treatment = factor(c("x", "y")),
                      batch = factor(c("first", "second")), replicate = 1:4)
  data$value <- as.integer(data$group) * 10 + as.integer(data$treatment) * 3 + data$replicate
  info <- list(responses = "value", factors = list(factor1 = "group"), orders = list(order1 = c("a", "b")))
  if (two_way) {
    info$factors$factor2 <- "treatment"
    info$orders$order2 <- c("x", "y")
  }
  if (stratified) info$strata <- list(var = "batch", levels = c("first", "second"))
  list(data = data, info = info)
}

test_that("ANOVA boxplot jitter is opt-in and retains measured y values", {
  for (two_way in c(FALSE, TRUE)) {
    fixture <- boxplot_fixture(two_way)
    ordinary <- plot_anova_boxplot(fixture$data, fixture$info)$plot
    expect_length(boxplot_points(ordinary), 0L)
    jittered <- plot_anova_boxplot(fixture$data, fixture$info, show_jitter = TRUE)$plot
    points <- boxplot_points(jittered)
    expect_length(points, 1L)
    expect_equal(sort(points[[1]]$y), sort(fixture$data$value))
    expect_equal(nrow(points[[1]]), nrow(fixture$data))
    expect_equal(boxplot_points(jittered)[[1]]$x, points[[1]]$x)
    disabled <- plot_anova_boxplot(fixture$data, fixture$info, show_jitter = FALSE)$plot
    expect_length(boxplot_points(disabled), 0L)
  }
})

test_that("two-way boxplot jitter stays over the corresponding dodged boxes", {
  fixture <- boxplot_fixture(two_way = TRUE)
  for (sparse in c(FALSE, TRUE)) {
    data <- fixture$data
    if (sparse) data <- data[!(data$group == "b" & data$treatment == "y"), ]
    plot <- plot_anova_boxplot(data, fixture$info, show_jitter = TRUE)$plot
    built <- ggplot_build(plot)$data
    boxes <- built[[1]]
    points <- built[[2]]
    centers <- boxes$x[match(points$group, boxes$group)]
    expect_false(anyNA(centers))
    expect_true(all(abs(points$x - centers) < 0.08))
    expect_equal(points$fill, boxes$fill[match(points$group, boxes$group)])
    expect_equal(sort(points$y), sort(data$value))
  }
})

test_that("jitter is included in every response and stratum without changing shared limits", {
  fixture <- boxplot_fixture(two_way = TRUE, stratified = TRUE)
  fixture$data$other <- fixture$data$value * 2
  fixture$info$responses <- c("value", "other")
  result <- plot_anova_boxplot(fixture$data, fixture$info, show_jitter = TRUE, share_y_axis = TRUE)
  expect_null(result$warning)
  panels <- boxplot_panels(result$plot)
  expect_length(panels, 4L)
  for (panel in panels) {
    built <- ggplot_build(panel)
    points <- boxplot_points(panel)
    expect_length(points, 1L)
    expect_equal(nrow(points[[1]]), nrow(fixture$data) / 2)
    expect_equal(sort(points[[1]]$y), sort(panel$data[[panel$labels$y]]))
    expect_equal(built$layout$panel_params[[1]]$y.range,
                 ggplot_build(panels[[1]])$layout$panel_params[[1]]$y.range)
  }
})

test_that("descriptive boxplots default to no overlay and jitter only horizontally", {
  fixture <- boxplot_fixture()
  for (group in list(NULL, "group")) {
    ordinary <- build_descriptive_numeric_boxplot(fixture$data, selected_vars = "value", group_var = group)
    expect_length(boxplot_points(boxplot_panels(ordinary$plot)[[1]]), 0L)
    jittered <- build_descriptive_numeric_boxplot(fixture$data, selected_vars = "value", group_var = group, show_points = TRUE)
    panel <- boxplot_panels(jittered$plot)[[1]]
    points <- boxplot_points(panel)[[1]]
    boxes <- ggplot_build(panel)$data[[1]]
    expect_equal(points$fill, boxes$fill[match(points$group, boxes$group)])
    expect_equal(sort(points$y), sort(fixture$data$value))
    expect_equal(boxplot_points(panel)[[1]]$x, points$x)
  }
})

test_that("static boxplot controls are labeled consistently and unchecked", {
  for (entry in list(
    list(ui = visualize_oneway_ui("oneway"), id = "oneway-boxplot_show_jitter"),
    list(ui = visualize_numeric_boxplots_ui("descriptive"), id = "descriptive-show_points")
  )) {
    doc <- xml2::read_html(htmltools::renderTags(entry$ui)$html)
    checkbox <- xml2::xml_find_all(doc, paste0("//input[@id='", entry$id, "']"))
    expect_length(checkbox, 1L)
    expect_true(is.na(xml2::xml_attr(checkbox, "checked")))
    expect_match(xml2::xml_text(xml2::xml_parent(checkbox)), "Overlay jittered data", fixed = TRUE)
  }
})

for (engine in c("oneway", "twoway")) {
  test_that(paste(engine, "boxplot checkbox affects the plot only after Apply"), {
    fixture <- boxplot_fixture(two_way = engine == "twoway")
    fixture$info$type <- paste0(engine, "_anova")
    filtered <- reactiveVal(fixture$data)
    model <- reactiveVal(fixture$info)
    server <- if (engine == "oneway") visualize_oneway_server else visualize_twoway_server
    testServer(server, args = list(filtered_data = filtered, model_info = model), {
      session$flushReact()
      if (engine == "twoway") expect_match(output$axis_and_jitter$html, "share_y_axis", fixed = TRUE)
      session$setInputs(plot_type = "boxplot")
      expect_length(boxplot_points(stored$plot), 0L)
      session$setInputs(lineplot_show_jitter = TRUE)
      expect_length(boxplot_points(stored$plot), 0L)
      if (engine == "twoway") {
        doc <- xml2::read_html(output$axis_and_jitter$html)
        checkbox <- xml2::xml_find_all(doc, "//input[contains(@id,'boxplot_show_jitter')]")
        expect_length(checkbox, 1L)
        expect_true(is.na(xml2::xml_attr(checkbox, "checked")))
        expect_match(xml2::xml_text(xml2::xml_parent(checkbox)), "Overlay jittered data", fixed = TRUE)
      }
      session$setInputs(boxplot_show_jitter = TRUE)
      expect_length(boxplot_points(stored$plot), 0L)
      session$setInputs(apply_plot = 1L)
      expect_length(boxplot_points(stored$plot), 1L)
      session$setInputs(boxplot_show_jitter = FALSE, apply_plot = 2L)
      expect_length(boxplot_points(stored$plot), 0L)
    })
  })
}

test_that("descriptive boxplot checkbox applies and removes the overlay", {
  fixture <- boxplot_fixture()
  filtered <- reactiveVal(fixture$data)
  summary <- reactiveVal(list(type = "descriptive", selected_vars = "value", group_var = "group"))
  testServer(visualize_numeric_boxplots_server, args = list(filtered_data = filtered, summary_info = summary), {
    session$setInputs(plot_width = 400, plot_height = 300)
    expect_length(boxplot_points(boxplot_panels(stored$plot)[[1]]), 0L)
    session$setInputs(show_points = TRUE)
    expect_length(boxplot_points(boxplot_panels(stored$plot)[[1]]), 0L)
    session$setInputs(apply_plot = 1L)
    expect_length(boxplot_points(boxplot_panels(stored$plot)[[1]]), 1L)
    session$setInputs(show_points = FALSE, apply_plot = 2L)
    expect_length(boxplot_points(boxplot_panels(stored$plot)[[1]]), 0L)
  })
})
