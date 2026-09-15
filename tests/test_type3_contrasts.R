library(testthat)
library(shiny)

source("../R/helpers.R")
source("../R/anova_shared_utils.R")
source("../R/anova_shared_model.R")

test_that("two-way ANOVA fits sum contrasts before computing Type III tests", {
  data <- data.frame(
    factor_a = c(rep("a", 5), rep("b", 6)),
    factor_b = c(rep("x", 3), rep("y", 2), rep("x", 2), rep("y", 4)),
    response = c(2.0, 2.2, 1.8, 3.0, 3.1, 4.0, 4.2, 6.0, 6.3, 5.8, 6.1)
  )

  result <- prepare_stratified_anova(
    df = data,
    responses = "response",
    model = "twoway_anova",
    factor1_var = "factor_a",
    factor1_order = c("a", "b"),
    factor2_var = "factor_b",
    factor2_order = c("x", "y")
  )
  model <- result$models$response$model

  expect_s3_class(model, "lm")
  expect_equal(colSums(model$contrasts$factor_a), 0)
  expect_equal(colSums(model$contrasts$factor_b), 0)

  expected <- stats::lm(
    response ~ factor_a * factor_b,
    data = result$data_used,
    contrasts = list(
      factor_a = stats::contr.sum(2),
      factor_b = stats::contr.sum(2)
    )
  )

  expect_equal(
    unname(as.matrix(car::Anova(model, type = 3))),
    unname(as.matrix(car::Anova(expected, type = 3))),
    tolerance = 1e-10
  )
})
