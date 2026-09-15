library(testthat)

source("../R/regression_shared_utils.R")
source("../R/regression_shared_formula.R")
source("../R/regression_shared_validation.R")

test_that("regression variables and interactions are protected for formulas", {
  expect_equal(reg_protect_vars(c("group name", "dose")), c("`group name`", "`dose`"))
  expect_equal(reg_protect_vars(c("", NA_character_)), character())
  expect_equal(reg_protect_interactions("group name:dose"), "`group name`:`dose`")
})

test_that("regression formula helpers compose LM and LMM terms", {
  lm_rhs <- reg_compose_rhs("group", "age", "group:sex", engine = "lm")
  expect_equal(lm_rhs, c("`group`", "`age`", "`group`:`sex`"))
  expect_equal(reg_formula_text("outcome", lm_rhs), "`outcome` ~ `group` + `age` + `group`:`sex`")
  expect_equal(reg_formula_text("outcome", character()), "`outcome` ~ 1")

  expect_equal(
    reg_compose_random_terms(c("site", "subject"), nested = TRUE),
    "(1|`site`/`subject`)"
  )
  expect_equal(
    reg_compose_rhs(NULL, NULL, NULL, random = "subject", engine = "lmm"),
    "(1|`subject`)"
  )
})

test_that("regression data helpers apply levels without mutating unrelated columns", {
  data <- data.frame(
    group = c("control", "treated", "control"),
    subject = c("s2", "s1", "s2"),
    value = 1:3
  )
  input <- list(order_group = c("treated", "control"))

  ordered <- apply_fixed_level_orders(data, "group", input)
  sanitized <- sanitize_random_effects(ordered, "subject")

  expect_equal(levels(sanitized$group), c("treated", "control"))
  expect_true(is.factor(sanitized$subject))
  expect_equal(levels(sanitized$subject), c("s1", "s2"))
  expect_equal(sanitized$value, 1:3)
})
