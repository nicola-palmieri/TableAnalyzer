library(testthat)
library(dplyr)
library(skimr)

source("../R/descriptive_analysis.R")
source("../R/anova_shared_data.R")

test_that("descriptive summaries report grouped numeric metrics", {
  data <- tibble::tibble(
    group = factor(c("a", "a", "b", "b")),
    value = c(1, 3, NA, 10),
    category = factor(c("x", "y", "x", "x"))
  )

  result <- compute_descriptive_summary(data, group_var = "group")

  expect_equal(result$cv$group, factor(c("a", "b"), levels = c("a", "b")))
  expect_equal(result$missing$missing_value, c(0, 50))
  expect_equal(result$outliers$outliers_value, c(0, 0))
  expect_true(all(is.na(result$distribution$distribution_value)))
  expect_true(all(c("skim_variable", "skim_type") %in% names(result$skim)))
})

test_that("distribution selection rejects insufficient or constant data", {
  expect_true(is.na(most_likely_distribution(c(1, 2, 3, 4))))
  expect_true(is.na(most_likely_distribution(rep(2, 10))))
})

test_that("ANOVA summaries calculate grouped means and standard errors", {
  data <- tibble::tibble(
    treatment = c("control", "control", "treated", "treated"),
    sex = c("f", "m", "f", "m"),
    response = c(1, 3, 5, 7)
  )

  one_way <- anova_summarise_stats(data, "response", "treatment", NULL)
  two_way <- anova_summarise_stats(data, "response", "treatment", "sex")

  expect_equal(one_way$mean, c(2, 6))
  expect_equal(one_way$se, c(1, 1))
  expect_equal(nrow(two_way), 4)
  expect_true(all(is.na(two_way$se)))
})

test_that("ANOVA factor levels follow requested plotting order", {
  stats <- tibble::tibble(
    treatment = c("control", "treated", "unused"),
    sex = c("f", "m", "f"),
    mean = c(1, 2, 3)
  )

  ordered <- apply_anova_factor_levels(
    stats,
    factor1 = "treatment",
    factor2 = "sex",
    order1 = c("treated", "control"),
    order2 = c("m", "f")
  )

  expect_equal(levels(ordered$treatment), c("treated", "control"))
  expect_equal(levels(ordered$sex), c("m", "f"))
  expect_equal(nrow(ordered), 2)
})
