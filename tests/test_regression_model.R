library(testthat)

source("../R/regression_shared_utils.R")
source("../R/helpers.R")
source("../R/regression_shared_model.R")

test_that("LM helpers fit and tidy a model with protected column names", {
  data <- data.frame(
    check.names = FALSE,
    "response value" = c(2.0, 2.7, 3.2, 4.1, 4.8, 5.4),
    "group name" = factor(rep(c("control", "treated"), each = 3)),
    age = c(1, 2, 3, 1, 2, 3)
  )
  rhs <- c("`group name`", "`age`")

  model <- reg_fit_model("response value", rhs, data, engine = "lm")
  result <- tidy_regression_model(model, engine = "lm")

  expect_s3_class(model, "lm")
  expect_equal(colSums(model$contrasts[["group name"]]), 0)
  expect_true(all(c("term", "estimate", "std_error", "statistic", "p_value") %in% names(result$summary)))
  expect_equal(result$effects$metrics$metric, c("sigma", "r_squared", "adj_r_squared", "nobs"))
  expect_equal(result$effects$metrics$value[result$effects$metrics$metric == "nobs"], 6)
  expect_true("Effect" %in% names(result$effects$anova))
})

test_that("LMM fixed factors use sum-to-zero contrasts", {
  data <- data.frame(
    response = c(2.0, 2.2, 3.0, 3.1, 4.0, 4.3, 5.0, 5.2),
    group = factor(rep(c("control", "treated"), each = 4)),
    subject = factor(rep(paste0("s", 1:4), 2))
  )

  model <- suppressWarnings(reg_fit_model(
    "response",
    c("`group`", "(1|`subject`)"),
    data,
    engine = "lmm"
  ))

  expect_s4_class(model, "lmerModLmerTest")
  model_contrasts <- attr(lme4::getME(model, "X"), "contrasts")
  expect_equal(colSums(model_contrasts$group), 0)
})

test_that("ANOVA formatting handles missing and very small p-values", {
  table <- data.frame(
    Effect = c("group", "Residuals"),
    `F value` = c(12.34567, NA_real_),
    `Pr(>F)` = c(0.00001, NA_real_),
    check.names = FALSE
  )

  formatted <- format_anova_f_column(table)
  formatted <- format_anova_p_column(formatted)

  expect_equal(formatted$`F value`, c("12.3457", ""))
  expect_equal(formatted$`Pr(>F)`, c("<0.0001", ""))
})
