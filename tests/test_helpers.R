library(testthat)

source("../R/helpers.R")

test_that("format_safe_error_message normalizes common error inputs", {
  expect_equal(format_safe_error_message(NULL), "Error:")
  expect_equal(format_safe_error_message("Upload", simpleError("bad file")), "Upload:\nbad file")
  expect_equal(format_safe_error_message("Model", list("first", " second ")), "Model:\nfirst\nsecond")
})

test_that("resolve_order_levels preserves factor order and character appearance order", {
  expect_equal(resolve_order_levels(factor(c("b", "a"), levels = c("a", "b"))), c("a", "b"))
  expect_equal(resolve_order_levels(c("b", NA, "a", "b")), c("b", "a"))
  expect_equal(resolve_order_levels(NULL), character())
})

test_that("sum contrasts are built only for categorical predictors", {
  data <- data.frame(
    group = factor(c("control", "treated", "control")),
    batch = c("b", "a", "b"),
    value = 1:3
  )

  contrasts <- build_sum_contrasts(data, c("group", "batch", "value", "missing"))

  expect_equal(names(contrasts), c("group", "batch"))
  expect_equal(colSums(contrasts$group), 0)
  expect_equal(colSums(contrasts$batch), 0)
})

test_that("export filenames are deterministic and filesystem safe", {
  timestamp <- as.POSIXct("2026-09-15 10:30:00", tz = "UTC")

  expect_equal(sanitize_export_part("Weight (g)"), "Weight_g")
  expect_equal(sanitize_export_part("***"), "unnamed")
  expect_equal(
    build_export_filename(
      analysis = "Linear Model",
      scope = "all",
      response = "Weight (g)",
      stratum = "Diet A",
      time = timestamp
    ),
    "Linear_Model_Weight_g_stratum_Diet_A_20260915-1030.docx"
  )
})
