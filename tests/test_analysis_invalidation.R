library(testthat)
library(shiny)

source("../R/helpers.R")
source("../R/module_analysis.R")

test_that("analysis results are invalidated when filtered data changes", {
  filtered <- reactiveVal(data.frame(value = 1:3))

  stub_ui <- function(id) {
    ns <- NS(id)
    list(
      config = actionButton(ns("run"), "Run"),
      results = textOutput(ns("result"))
    )
  }

  stub_server <- function(id, data) {
    moduleServer(id, function(input, output, session) {
      result <- eventReactive(input$run, {
        list(type = "stub", summary = sum(data()$value))
      })
      output$result <- renderText(result()$summary)
      result
    })
  }

  modules <- list(
    "Test analysis" = list(
      id = "stub",
      ui = stub_ui,
      server = stub_server,
      type = "stub"
    )
  )

  testServer(
    analysis_server,
    args = list(filtered_data = filtered, modules = modules),
    {
      session$setInputs(analysis_type = "Test analysis")
      session$setInputs(`stub-run` = 1)
      session$flushReact()

      expect_true(has_run())
      expect_equal(model_out()$summary, 6)

      filtered(data.frame(value = 10:12))
      session$flushReact()

      expect_false(has_run())
      expect_true(data_changed_since_run())
      expect_null(model_out())
    }
  )
})
