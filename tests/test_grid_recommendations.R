library(testthat)
library(shiny)
library(ggplot2)

for (source_file in list.files("../R", pattern = "[.]R$", full.names = TRUE)) source(source_file)

combined_shape <- function(grid) c(
  rows = grid$responses$rows * grid$strata$rows,
  cols = grid$responses$cols * grid$strata$cols
)

test_that("nested grids minimize blanks without choosing extreme figure shapes", {
  expect_equal(combined_shape(compute_joint_grid(3, 5)), c(rows = 3, cols = 5))
  expect_equal(combined_shape(compute_joint_grid(4, 6)), c(rows = 4, cols = 6))
  seven <- compute_joint_grid(2, 7)
  expect_equal(combined_shape(seven), c(rows = 4, cols = 4))
  expect_equal(seven$responses, list(rows = 2L, cols = 1L))
  expect_equal(seven$strata, list(rows = 2L, cols = 4L))
  expect_equal(compute_default_grid(5), list(rows = 2L, cols = 3L))
  expect_equal(compute_default_grid(7), list(rows = 2L, cols = 4L))
})

test_that("recommendations fit every supported count and respect widget bounds", {
  cases <- expand.grid(responses = 1:100, strata = 1:10)
  valid <- vapply(seq_len(nrow(cases)), function(i) {
    nr <- cases$responses[i]
    ns <- cases$strata[i]
    grid <- compute_joint_grid(nr, ns)
    values <- unlist(grid)
    all(values >= 1L & values <= 10L) &&
      validate_grid(nr, grid$responses$rows, grid$responses$cols)$valid &&
      validate_grid(ns, grid$strata$rows, grid$strata$cols)$valid
  }, logical(1))
  expect_true(all(valid), info = paste("Invalid count pairs:", paste(which(!valid), collapse = ", ")))
  expect_equal(combined_shape(compute_joint_grid(100, 10)), c(rows = 20, cols = 50))
  over_limit <- compute_joint_grid(101, 10)
  expect_equal(over_limit$responses, list(rows = 10L, cols = 10L))
  expect_false(validate_grid(101, over_limit$responses$rows, over_limit$responses$cols)$valid)
})

test_that("joint recommendations achieve the fewest empty cells among reasonable shapes", {
  # Enumerate independently, including deliberately oversized candidate grids.
  grids <- expand.grid(rows = 1:10, cols = 1:10)
  for (counts in list(c(3, 5), c(4, 6), c(2, 7), c(7, 7), c(13, 9), c(31, 10))) {
    nr <- counts[1]
    ns <- counts[2]
    rg <- grids[grids$rows * grids$cols >= nr, ]
    sg <- grids[grids$rows * grids$cols >= ns, ]
    alternatives <- merge(rg, sg, by = NULL, suffixes = c("_response", "_strata"))
    area <- with(alternatives, rows_response * cols_response * rows_strata * cols_strata)
    aspect <- with(alternatives, cols_response * cols_strata * 400 / (rows_response * rows_strata * 300))
    best_area <- min(area[aspect >= 0.64 & aspect <= 4])
    selected <- combined_shape(compute_joint_grid(nr, ns))
    expect_equal(prod(selected), best_area, info = paste(nr, "responses and", ns, "strata"))
  }
})

test_that("panel dimensions affect recommendations and missing sizes are safe", {
  wide <- combined_shape(compute_joint_grid(3, 5, 1200, 200))
  tall <- combined_shape(compute_joint_grid(3, 5, 200, 1200))
  expect_gt(wide[[1]], wide[[2]])
  expect_gt(tall[[2]], tall[[1]])
  expect_equal(compute_joint_grid(3, 5, NULL, NA), compute_joint_grid(3, 5))
  expect_equal(compute_joint_grid(3, 5, Inf, -1), compute_joint_grid(3, 5))
})

test_that("ANOVA widgets preserve manual layouts until panel counts change", {
  model <- reactiveVal(list(responses = letters[1:3], strata = list(var = "batch", levels = LETTERS[1:5])))
  testServer(function(input, output, session) {
    grids <- anova_grid_settings_server(input, session, model)
    output$controls <- renderUI(isolate(build_anova_layout_controls(session$ns, input, model(), grids$cache)))
  }, {
    session$flushReact()
    expect_equal(grids$values(), list(strata_rows = 1L, strata_cols = 5L, resp_rows = 3L, resp_cols = 1L))
    html <- xml2::read_html(output$controls$html)
    expect_equal(xml2::xml_attr(xml2::xml_find_first(html, "//input[contains(@id,'strata_grid-cols')]"), "value"), "5")
    session$setInputs(`strata_grid-rows` = 1L, `strata_grid-cols` = 5L,
                      `response_grid-rows` = 3L, `response_grid-cols` = 1L)
    expect_false(grids$is_manual())

    session$setInputs(`strata_grid-rows` = 5L, `strata_grid-cols` = 1L)
    expect_true(grids$is_manual())
    manual <- grids$values()
    session$setInputs(plot_width = 1200, plot_height = 200, apply_plot = 1L, plot_type = "boxplot")
    expect_equal(grids$values(), manual)
    model(list(responses = rev(letters[1:3]), strata = list(var = "batch", levels = LETTERS[1:5])))
    session$flushReact()
    expect_equal(grids$values(), manual)

    session$setInputs(plot_width = 400, plot_height = 300)
    model(list(responses = letters[1:4], strata = list(var = "batch", levels = LETTERS[1:5])))
    session$flushReact()
    expect_false(grids$is_manual())
    expect_equal(grids$values(), list(strata_rows = 1L, strata_cols = 5L, resp_rows = 4L, resp_cols = 1L))
    # Browser acknowledgements of programmatic updates must not count as edits.
    session$setInputs(`strata_grid-rows` = 1L, `strata_grid-cols` = 5L, `response_grid-rows` = 4L)
    expect_false(grids$is_manual())
    session$setInputs(plot_width = 1200, plot_height = 200)
    expect_false(grids$is_manual())
    expected <- compute_joint_grid(4, 5, 1200, 200)
    expect_equal(grids$values(), list(strata_rows = expected$strata$rows, strata_cols = expected$strata$cols,
                                     resp_rows = expected$responses$rows, resp_cols = expected$responses$cols))
  })
})

test_that("single-grid recommendations refresh counts and retain manual values", {
  n <- reactiveVal(7L)
  testServer(function(input, output, session) {
    grid <- single_grid_settings(input, session, "panels")
    layout <- reactiveVal(NULL)
    observeEvent(list(n(), input$plot_width, input$plot_height), {
      layout(grid$resolve(n(), input$plot_width, input$plot_height))
    })
  }, {
    session$flushReact()
    expect_equal(layout(), list(rows = 2L, cols = 4L))
    session$setInputs(`panels-rows` = 2L, `panels-cols` = 4L)
    expect_false(grid$is_manual())
    session$setInputs(`panels-rows` = 4L, `panels-cols` = 2L)
    expect_true(grid$is_manual())
    session$setInputs(plot_width = 1000, plot_height = 200)
    expect_equal(layout(), list(rows = 4L, cols = 2L))
    n(6L)
    session$flushReact()
    expect_false(grid$is_manual())
    expect_equal(layout(), compute_default_grid(6, 1000, 200))
    # The old 4x2 grid also fits six panels; a count change must still refresh it.
    expect_false(identical(layout(), list(rows = 4L, cols = 2L)))
  })
})

test_that("all ANOVA builders use the recommended nested layout without dropping panels", {
  data <- expand.grid(group = factor(c("a", "b")), treatment = factor(c("x", "y")),
                      stratum = factor(LETTERS[1:5]), replicate = 1:3)
  data$first <- seq_len(nrow(data))
  data$second <- data$first * 2
  data$third <- sqrt(data$first)
  info <- list(responses = c("first", "second", "third"), factors = list(factor1 = "group"),
               orders = list(order1 = c("a", "b")), strata = list(var = "stratum", levels = LETTERS[1:5]))
  for (two_way in c(FALSE, TRUE)) {
    if (two_way) {
      info$factors$factor2 <- "treatment"
      info$orders$order2 <- c("x", "y")
    }
    for (builder in list(plot_anova_barplot_meanse, plot_anova_lineplot_meanse, plot_anova_boxplot)) {
      result <- builder(data, info, layout_values = list())
      expect_null(result$warning)
      expect_equal(result$layout, list(strata = list(rows = 1L, cols = 5L), responses = list(rows = 3L, cols = 1L)))
      expect_equal(result$panel_counts, list(strata = 5L, responses = 3L))
      expect_s3_class(result$plot, "patchwork")
    }
  }
})

for (kind in c("histograms", "boxplots", "categorical")) {
  test_that(paste(kind, "plot layout stays synchronized with its editable widgets"), {
    numeric_data <- as.data.frame(setNames(rep(list(1:12), 7), letters[1:7]))
    data <- if (kind == "categorical") as.data.frame(lapply(numeric_data, function(x) factor(x %% 3))) else numeric_data
    filtered <- reactiveVal(data)
    summary <- reactiveVal(list(type = "descriptive", selected_vars = names(data), processed_data = data))
    server <- switch(kind, histograms = visualize_numeric_histograms_server,
                     boxplots = visualize_numeric_boxplots_server, categorical = visualize_categorical_barplots_server)
    testServer(server, args = list(filtered_data = filtered, summary_info = summary), {
      session$setInputs(plot_width = 400, plot_height = 300)
      expect_equal(stored$layout, list(nrow = 2L, ncol = 4L))
      expect_equal(c(grid$cache$rows, grid$cache$cols), c(2L, 4L))
      session$setInputs(`plot_grid-rows` = 2L, `plot_grid-cols` = 4L)
      session$setInputs(`plot_grid-rows` = 4L, `plot_grid-cols` = 2L, apply_plot = 1L)
      expect_equal(stored$layout, list(nrow = 4L, ncol = 2L))
      session$setInputs(plot_width = 450, apply_plot = 2L)
      expect_equal(stored$layout, list(nrow = 4L, ncol = 2L))
      summary(list(type = "descriptive", selected_vars = names(data)[1:6], processed_data = data))
      session$flushReact()
      expect_equal(stored$layout, list(nrow = 2L, ncol = 3L))
      expect_equal(c(grid$cache$rows, grid$cache$cols), c(2L, 3L))
      session$setInputs(`plot_grid-rows` = 2L, `plot_grid-cols` = 3L)
      session$setInputs(`plot_grid-rows` = 1L, `plot_grid-cols` = 1L, apply_plot = 3L)
      expect_match(stored$warning, "too small")
      expect_equal(c(grid$cache$rows, grid$cache$cols), c(1L, 1L))
      expect_null(stored$plot)
    })
  })
}

test_that("PCA facets keep the rendered layout and grid widget values together", {
  data <- data.frame(x = sin(1:24), y = cos((1:24) / 2), batch = factor(rep(letters[1:3], each = 8)))
  model <- reactiveVal(list(type = "pca", model = list(model = prcomp(data[c("x", "y")], scale. = TRUE), data = data)))
  filtered <- reactiveVal(data)
  testServer(visualize_pca_server, args = list(filtered_data = filtered, model_fit = model), {
    session$flushReact()
    session$setInputs(plot_type = "biplot", facet_var = "batch", plot_width = 400, plot_height = 300, apply_plot = 1L)
    expect_equal(plot_info()$layout, list(nrow = 1L, ncol = 3L))
    expect_equal(c(facet_grid_inputs$cache$rows, facet_grid_inputs$cache$cols), c(1L, 3L))
    session$setInputs(`facet_grid-rows` = 1L, `facet_grid-cols` = 3L)
    session$setInputs(`facet_grid-rows` = 3L, `facet_grid-cols` = 1L, apply_plot = 2L)
    expect_equal(plot_info()$layout, list(nrow = 3L, ncol = 1L))
    session$setInputs(facet_var = "None", apply_plot = 3L)
    expect_equal(plot_info()$panels, 1L)
    session$setInputs(facet_var = "batch", apply_plot = 4L)
    expect_equal(plot_info()$layout, list(nrow = 1L, ncol = 3L))
    expect_equal(c(facet_grid_inputs$cache$rows, facet_grid_inputs$cache$cols), c(1L, 3L))
  })
})

test_that("correlation grids use their new recommendations immediately and retain edits", {
  data <- data.frame(x = sin(1:24), y = cos((1:24) / 2), batch = factor(rep(letters[1:3], each = 8)))
  info <- reactiveVal(list(group_var = "batch", strata_order = letters[1:3],
                           results = list(selected_vars = c("x", "y"), matrices = setNames(rep(list(diag(2)), 3), letters[1:3]))))
  trigger <- reactiveVal(0L)
  filtered <- reactiveVal(data)
  testServer(pairwise_correlation_visualize_ggpairs_server,
             args = list(filtered_data = filtered, correlation_info = info, apply_trigger = trigger), {
    session$setInputs(plot_width = 400, plot_height = 300)
    trigger(1L)
    session$flushReact()
    expect_equal(plot_info()$layout, list(rows = 1L, cols = 3L))
    expect_equal(c(grid$cache$rows, grid$cache$cols), c(1L, 3L))
    session$setInputs(`plot_grid-rows` = 1L, `plot_grid-cols` = 3L)
    session$setInputs(`plot_grid-rows` = 3L, `plot_grid-cols` = 1L)
    trigger(2L)
    session$flushReact()
    expect_equal(plot_info()$layout, list(rows = 3L, cols = 1L))
    info(list(group_var = NULL, results = list(selected_vars = c("x", "y"), matrices = list(Overall = diag(2)))))
    trigger(3L)
    session$flushReact()
    expect_equal(plot_info()$panels, 1L)
    info(list(group_var = "batch", strata_order = letters[1:3],
              results = list(selected_vars = c("x", "y"), matrices = setNames(rep(list(diag(2)), 3), letters[1:3]))))
    trigger(4L)
    session$flushReact()
    expect_equal(plot_info()$layout, list(rows = 1L, cols = 3L))
    expect_equal(c(grid$cache$rows, grid$cache$cols), c(1L, 3L))
  })
})

test_that("categorical grid counts include the Missing group panel on every Apply", {
  data <- data.frame(group = rep(NA_character_, 8), other = factor(rep(c("a", "b"), 4)))
  filtered <- reactiveVal(data)
  summary <- reactiveVal(list(type = "descriptive", selected_vars = names(data), group_var = "group"))
  testServer(visualize_categorical_barplots_server, args = list(filtered_data = filtered, summary_info = summary), {
    session$setInputs(plot_width = 400, plot_height = 300)
    expect_equal(stored$layout, list(nrow = 1L, ncol = 2L))
    session$setInputs(apply_plot = 1L)
    expect_null(stored$warning)
    expect_equal(stored$layout, list(nrow = 1L, ncol = 2L))
    expect_equal(c(grid$cache$rows, grid$cache$cols), c(1L, 2L))
  })
})
