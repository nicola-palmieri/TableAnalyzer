library(testthat)
library(shiny)
library(dplyr)
library(ggplot2)

for (path in list.files("../R", pattern = "\\.R$", full.names = TRUE)) source(path)

test_that("one-way numeric levels receive their significant post-hoc stars", {
  data <- data.frame(
    group = factor(rep(c(0, 1, 2), each = 10)),
    response = rep(c(1, 10, 20), each = 10) + rep(seq(-.4, .5, by = .1), 3)
  )
  info <- prepare_stratified_anova(data, "response", "oneway_anova", "group", c("0", "1", "2"))
  outputs <- prepare_anova_outputs(info$models$response$model, "group")
  expect_equal(outputs$posthoc_table$contrast, c("1 - 0", "2 - 0"))
  expect_true(all(outputs$posthoc_table$p.value < .001))

  stats <- anova_summarise_stats(info$data_used, "response", "group", NULL)
  plot <- build_single_factor_barplot(stats, "", "group", "steelblue", 12, outputs$posthoc_table)
  expect_equal(ggplot_build(plot)$data[[3]]$label, rep("***", 2))
})

test_that("two-way numeric levels receive stars within each first-factor level", {
  data <- expand.grid(batch = factor(c("a", "b")), dose = factor(c(0, 1, 2)), replicate = 1:10)
  data$response <- as.integer(data$dose) * 10 + as.integer(data$batch) + seq(-.4, .5, by = .1)[data$replicate]
  info <- prepare_stratified_anova(
    data, "response", "twoway_anova", "batch", c("a", "b"), "dose", c("0", "1", "2")
  )
  outputs <- prepare_anova_outputs(info$models$response$model, c("batch", "dose"))
  expect_equal(unique(as.character(outputs$posthoc_table$contrast)), c("1 - 0", "2 - 0"))
  expect_true(all(outputs$posthoc_table$p.value < .001))

  stats <- anova_summarise_stats(info$data_used, "response", "batch", "dose")
  plot <- build_two_factor_barplot(stats, "", "batch", "dose", NULL, 12, outputs$posthoc_table)
  expect_equal(ggplot_build(plot)$data[[3]]$label, rep("***", 4))
})
