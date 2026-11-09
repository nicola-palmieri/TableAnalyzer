library(testthat)
library(shiny)
library(DT)

source("../R/helpers.R")
source("../R/submodule_colors.R")
source("../R/module_upload_helpers.R")
source("../R/module_upload.R")

test_that("rejected replacement files clear the previous dataset", {
  valid <- normalizePath("../data/toy_animal_trial_data_long.xlsx")
  corrupt <- tempfile(fileext = ".xlsx")
  writeLines("not an Excel workbook", corrupt)
  on.exit(unlink(corrupt), add = TRUE)

  testServer(upload_server, {
    session$setInputs(data_source = "long")
    session$setInputs(file = data.frame(name = "valid.xlsx", datapath = valid))
    expect_gt(nrow(df()), 0)
    suppressWarnings(session$setInputs(file = data.frame(name = "corrupt.xlsx", datapath = corrupt)))
    expect_null(df())

    session$setInputs(file = data.frame(name = "valid.xlsx", datapath = valid))
    expect_gt(nrow(df()), 0)
    session$setInputs(file = data.frame(name = "invalid.csv", datapath = corrupt))
    expect_null(df())
  })
})

test_that("a failed sheet clears data and allows selecting a valid sheet again", {
  path <- tempfile(fileext = ".xlsx")
  on.exit(unlink(path), add = TRUE)
  openxlsx::write.xlsx(list(
    valid = rbind(c("ID", "A", "A"), c("", "R1", "R2"), c("s1", "10", "20")),
    invalid = rbind(c("ID", "A", "A"), c("", "R1", "R1"), c("s1", "10", "20"))
  ), path, colNames = FALSE)

  testServer(upload_server, {
    session$setInputs(data_source = "wide")
    session$setInputs(file = data.frame(name = "sheets.xlsx", datapath = path))
    expect_equal(nrow(df()), 2)
    session$setInputs(sheet = "invalid")
    expect_null(df())
    expect_match(output$sheet_selector$html, 'value="valid"')

    session$setInputs(sheet = "valid")
    expect_equal(nrow(df()), 2)
  })
})
