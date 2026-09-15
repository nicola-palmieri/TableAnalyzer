library(testthat)
library(dplyr)
library(ggplot2)
library(shiny)

source("../R/helpers.R")
source("../R/submodule_theme.R")
source("../R/submodule_colors.R")
source("../R/anova_shared_plot_context.R")
source("../R/anova_shared_utils.R")
source("../R/anova_shared_data.R")
source("../R/anova_shared_barplots.R")
source("../R/anova_shared_lineplots.R")
source("../R/regression_shared_validation.R")

test_that("barplot limits preserve zero for negative-only values", {
  limits <- ensure_barplot_zero_baseline(c(-11, -4))
  expect_equal(limits, c(-11, 0))

  stats_df <- data.frame(
    treatment = factor(c("a", "b")),
    mean = c(-10, -5),
    se = c(1, 1)
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
