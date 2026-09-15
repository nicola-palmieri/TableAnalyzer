library(testthat)
library(dplyr)
library(janitor)

source("../R/module_upload_helpers.R")   # contains preprocess_uploaded_table()

test_that("preprocess_uploaded_table cleans column names and orders factors", {
  
  df <- tibble(
    "ID "      = c("b", "a"),
    "Group Name" = c("Low", "High"),
    "Value 1"    = c(10, 20)
  )
  
  out <- preprocess_uploaded_table(df)
  
  # Should remain a tibble
  expect_s3_class(out, "tbl_df")
  
  # Column names should be cleaned
  expect_equal(names(out), c("id", "group_name", "value_1"))
  
  # Factors should be created with ordered levels
  expect_true(is.factor(out$group_name))
  expect_true(is.factor(out$id))
  
  # auto_factor_order() → alphabetical order expected
  expect_equal(levels(out$id), c("a", "b"))
  expect_equal(levels(out$group_name), c("High", "Low"))
  
  # Numeric column untouched
  expect_equal(out$value_1, c(10, 20))
})

test_that("preprocess_uploaded_table handles all-character dataframes", {

  df <- tibble(
    ColA = c("x", "y", "z"),
    ColB = c("10", "2", "30")
  )
  
  out <- preprocess_uploaded_table(df)
  
  expect_s3_class(out, "tbl_df")
  expect_true(all(sapply(out, is.factor)))
  expect_equal(names(out), c("col_a", "col_b"))
})

test_that("auto_factor_order reorders factors with numeric-aware sorting", {

  df <- tibble(
    Letter = factor(c("B", "A", "B"), levels = c("B", "A")),
    Mix    = c("v10", "v2", "v1")
  )

  out <- preprocess_uploaded_table(df)

  expect_equal(levels(out$letter), c("A", "B"))
  expect_equal(levels(out$mix), c("v1", "v2", "v10"))
})

test_that("preprocess_uploaded_table handles numeric-only dataframes", {
  
  df <- tibble(
    A = c(1, 2, 3),
    B = c(10, 20, 30)
  )
  
  out <- preprocess_uploaded_table(df)
  
  expect_s3_class(out, "tbl_df")
  expect_equal(names(out), c("a", "b"))
  
  # Should not convert to factor
  expect_true(is.numeric(out$a))
  expect_true(is.numeric(out$b))
})

test_that("preprocess_uploaded_table works on empty dataframe", {
  
  df <- tibble()
  
  out <- preprocess_uploaded_table(df)
  
  expect_s3_class(out, "tbl_df")
  expect_equal(ncol(out), 0)
})
