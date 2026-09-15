library(testthat)
library(shiny)

source("../R/module_home.R")

test_that("home demo requests are exposed to the app coordinator", {
  testServer(home_server, {
    expect_equal(demo_requested(), 0L)

    session$setInputs(try_demo = 1)
    session$flushReact()

    expect_equal(demo_requested(), 1L)
  })
})
