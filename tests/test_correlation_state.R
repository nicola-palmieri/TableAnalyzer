library(testthat)
library(shiny)
library(ggplot2)

for (path in list.files("../R", pattern = "\\.R$", full.names = TRUE)) source(path)

test_that("failed correlation reruns replace previous matrices with the error", {
  data <- reactiveVal(data.frame(x = 1:6, y = c(2,1,5,3,8,6), z = rep(NA_real_, 6)))
  testServer(ggpairs_server, args = list(data_reactive = data), {
    session$setInputs(vars = c("x", "y"), `strat-stratify_var` = "None")
    session$setInputs(run = 1)
    expect_equal(correlation_store()$selected_vars, c("x", "y"))
    expect_length(correlation_store()$matrices, 1)

    session$setInputs(vars = c("x", "z"), run = 2)
    expect_length(correlation_store()$matrices, 0)
    expect_length(correlation_store()$plots, 0)
    expect_match(output$summary, "Variable 'z'.*fewer than 2")
    expect_match(paste(readLines(output$download_model), collapse = "\n"), "Variable 'z'")

    session$setInputs(vars = c("x", "y"), run = 3)
    expect_null(correlation_store()$message)
    expect_equal(correlation_store()$matrices$Overall, cor(data()[c("x", "y")]))
  })
})
