library(testthat)
library(dplyr)
library(ggplot2)
library(shiny)

source("../R/shared_analysis_helpers.R")
source("../R/shared_theme.R")
source("../R/shared_colors.R")
source("../R/shared_anova_plot_context.R")
source("../R/shared_anova_utils.R")
source("../R/shared_anova_data.R")
source("../R/shared_anova_barplots.R")
source("../R/shared_anova_lineplots.R")
source("../R/shared_regression_validation.R")

test_that("stratified ANOVA panels keep group identities as facet strips without titles", {
  stats_df <- data.frame(treatment=factor(c("a","b")),.ta_mean=c(2,4),.ta_se=c(.2,.3))
  bar <- build_single_factor_barplot(stats_df,"Day 14","treatment","steelblue",13,NULL)
  line <- build_line_plot_panel(stats_df,"Day 14",NULL,"treatment",NULL,NULL)
  for (p in list(bar,line)) {
    central <- NicoShinyAppTemplate::plot_theme()
    expect_equal(p$theme$axis.line$colour, central$axis.line$colour)
    expect_equal(p$theme$axis.ticks$colour, central$axis.ticks$colour)
    expect_equal(p$theme$panel.grid.major$colour, central$panel.grid.major$colour)
    g <- ggplotGrob(p)
    expect_true(any(grepl("^strip",g$layout$name)))
    expect_s3_class(g$grobs[[match("title",g$layout$name)]],"zeroGrob")
    expect_equal(ggplot_build(p)$data[[1]]$y,c(2,4))
  }
})

test_that("barplot limits preserve zero for negative-only values", {
  limits <- ensure_barplot_zero_baseline(c(-11, -4))
  expect_equal(limits, c(-11, 0))

  stats_df <- data.frame(
    treatment = factor(c("a", "b")),
    .ta_mean = c(-10, -5),
    .ta_se = c(1, 1)
  )
  plot <- build_single_factor_barplot(
    stats_df = stats_df,
    title_text = "",
    factor1 = "treatment",
    base_fill = "#2463eb",
    base_size = 12,
    posthoc_entry = NULL,
    y_limits = limits,
    response_var = "response"
  )
  plotted_limits <- ggplot_build(plot)$layout$panel_params[[1]]$y.range

  expect_lt(plotted_limits[1], -10)
  expect_gte(plotted_limits[2], 0)
})

test_that("ANOVA summaries and plots accept columns named mean and se", {
  response_mean <- data.frame(group = factor(c("a", "a", "b", "b")), mean = c(1,3,5,7))
  stats <- anova_summarise_stats(response_mean, "mean", "group", NULL)
  expect_equal(stats$.ta_mean, c(2, 6))
  expect_equal(stats$.ta_se, c(1, 1))

  predictor_mean <- data.frame(mean = factor(c("a", "a", "b", "b")), se = c(1,3,5,7))
  stats <- anova_summarise_stats(predictor_mean, "se", "mean", NULL)
  expect_equal(as.character(stats$mean), c("a", "b"))
  expect_equal(stats$.ta_mean, c(2, 6))
  expect_equal(stats$.ta_se, c(1, 1))
  plot <- build_single_factor_barplot(stats, "", "mean", "steelblue", 12, NULL)
  built <- ggplot_build(plot)
  expect_equal(built$data[[1]]$y, c(2, 6))
  expect_equal(built$data[[2]]$ymin, c(1, 5))
  expect_equal(built$data[[2]]$ymax, c(3, 7))

  two_way <- expand.grid(mean = factor(c("a", "b")), se = factor(c("x", "y")), replicate = 1:2)
  two_way$value <- seq_len(nrow(two_way))
  stats <- anova_summarise_stats(two_way, "value", "mean", "se")
  expect_equal(stats$.ta_mean, c(3, 5, 4, 6))
  expect_equal(stats$.ta_se, rep(2, 4))
  expect_true(is.factor(stats$mean))
  expect_true(is.factor(stats$se))
})

test_that("lineplot limits include raw observations when jitter is shown", {
  data <- data.frame(
    treatment = factor(c("a", "a", "a", "b", "b", "b")),
    response = c(0, 0, 90, 10, 10, 10)
  )
  context <- list(
    responses = "response",
    has_strata = FALSE,
    strat_var = NULL,
    strata_levels = character(0),
    order1 = c("a", "b"),
    order2 = NULL
  )

  summary_limits <- compute_lineplot_shared_limits(
    context, data, "treatment", NULL, include_raw = FALSE
  )
  raw_limits <- compute_lineplot_shared_limits(
    context, data, "treatment", NULL, include_raw = TRUE
  )

  expect_lt(summary_limits[2], 90)
  expect_equal(raw_limits[2], 90)
})

test_that("missing values are not treated as observed factor levels", {
  input <- list(
    fixed = "group",
    covar = character(0),
    random = character(0)
  )

  expect_error(
    validate_regression_inputs(
      data.frame(group = c("only", NA)),
      input,
      engine = "lm",
      strat_details = list(var = NULL, levels = NULL)
    ),
    class = "shiny.silent.error"
  )
})


test_that("grouped default colors follow the shared twin palette", {
  levels <- c("control", "low", "high")
  expect_equal(resolve_palette_for_levels(levels),
    stats::setNames(NicoShinyAppTemplate::APP_PALETTE[1:3], levels))
  custom <- c(high = "#FFFFFF", control = "#000000", low = "#FF0000")
  expect_equal(resolve_palette_for_levels(levels, custom), custom[levels])
  expect_equal(unname(resolve_palette_for_levels(letters[1:25])),
    rep(NicoShinyAppTemplate::APP_PALETTE, length.out = 25))
})
