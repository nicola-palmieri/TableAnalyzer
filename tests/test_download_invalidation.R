library(testthat)
library(shiny)
library(ggplot2)
library(officer)
library(flextable)

for (path in list.files("../R", pattern = "\\.R$", full.names = TRUE)) source(path)

test_that("download guards require a new run after data or analysis changes", {
  data <- reactiveVal(data.frame(value = 1:3))
  reset <- reactiveVal(0L)
  guard_server <- function(id) {
    moduleServer(id, function(input, output, session) {
      guard <- analysis_download_guard(data, reactive(input$run), reset)
    })
  }

  testServer(guard_server, {
    expect_error(guard(), "Run the analysis again", class = "shiny.silent.error")
    session$setInputs(run = 1)
    expect_true(guard())
    data(NULL)
    session$flushReact()
    expect_error(guard(), "Run the analysis again", class = "shiny.silent.error")
    data(data.frame(value = 4:6))
    session$flushReact()
    session$setInputs(run = 2)
    expect_true(guard())
    reset(1L)
    session$flushReact()
    expect_error(guard(), "Run the analysis again", class = "shiny.silent.error")
  })
})

test_that("the analysis coordinator blocks stale model downloads until rerun", {
  filtered <- reactiveVal(data.frame(x = 1:10, y = c(2,3,1,5,3,6,8,5,9,10)))
  testServer(analysis_server, args = list(filtered_data = filtered), {
    session$setInputs(analysis_type = "Linear Model (LM)")
    session$setInputs(
      `lm-response-response` = "y", `lm-response-multi_resp` = FALSE,
      `lm-covar` = "x", `lm-strat-stratify_var` = "None"
    )
    session$setInputs(`lm-run` = 1)
    expect_equal(nobs(model_out()$flat_models[[1]]$model), 10)
    expect_true(file.exists(output[["lm-download_model"]]))

    filtered(data.frame(x = 1:4, y = c(5,8,7,9)))
    session$flushReact()
    expect_null(model_out())
    expect_error(output[["lm-download_model"]], "Run the analysis again", class = "shiny.silent.error")

    session$setInputs(`lm-run` = 2)
    expect_equal(nobs(model_out()$flat_models[[1]]$model), 4)
    expect_true(file.exists(output[["lm-download_model"]]))
  })
})
