library(testthat)
library(shiny)

for (file in list.files("../R", pattern = "[.]R$", full.names = TRUE)) source(file)

test_that("each method has one persistent configuration without initializing analyses", {
  registry <- analysis_module_registry()
  calls <- setNames(integer(length(registry)), names(registry))
  modules <- setNames(lapply(names(registry), function(method) {
    spec <- registry[[method]]
    original_ui <- spec$ui
    spec$ui <- function(id) {
      calls[[method]] <<- calls[[method]] + 1L
      original_ui(id)
    }
    spec$server <- function(...) stop("Rendering controls must not initialize an analysis")
    spec
  }), names(registry))
  document <- xml2::read_html(htmltools::renderTags(mod_analyze_ui("analysis", modules))$html)
  panels <- xml2::xml_find_all(document, "//*[@class='analysis-method-config']")
  expect_equal(calls, setNames(rep(1L, length(registry)), names(registry)))
  expect_equal(xml2::xml_attr(panels, "data-analysis"), names(registry))
  for (i in seq_along(registry)) {
    condition <- xml2::xml_attr(xml2::xml_parent(panels[[i]]), "data-display-if")
    expect_match(condition, "input['analysis-analysis_type']", fixed = TRUE)
    expect_match(condition, as.character(jsonlite::toJSON(names(registry)[[i]], auto_unbox = TRUE)), fixed = TRUE)
  }
  ids <- xml2::xml_attr(xml2::xml_find_all(document, "//*[@id]"), "id")
  expect_equal(anyDuplicated(ids), 0L)
  expect_true("analysis-pairs-vars_ui" %in% ids)
})

test_that("selected servers initialize once and returning to a method still requires Run", {
  initialized <- c(A = 0L, B = 0L)
  fitted <- c(A = 0L, B = 0L)
  make_spec <- function(key) {
    force(key)
    list(id = tolower(key), type = "stub",
      ui = function(id) list(config = actionButton(NS(id)("run"), "Run"),
        results = textOutput(NS(id)("result"))),
      server = function(id, data) {
        initialized[[key]] <<- initialized[[key]] + 1L
        moduleServer(id, function(input, output, session) {
          result <- eventReactive(input$run, {
            fitted[[key]] <<- fitted[[key]] + 1L
            list(type = "stub", summary = sum(data()$value))
          })
          output$result <- renderText(result()$summary)
          result
        })
      })
  }
  modules <- setNames(lapply(c("A", "B"), make_spec), c("A", "B"))
  data <- reactiveVal(data.frame(value = 1:3))
  testServer(mod_analyze_server, args = list(filtered_data = data, modules = modules), {
    session$flushReact()
    expect_equal(initialized, c(A = 0L, B = 0L))
    session$setInputs(analysis_type = "A")
    expect_equal(initialized, c(A = 1L, B = 0L))
    expect_equal(fitted, c(A = 0L, B = 0L))
    expect_null(model_out())
    session$setInputs(`a-run` = 1L)
    expect_equal(model_out()$summary, 6)
    session$setInputs(analysis_type = "B")
    expect_equal(initialized, c(A = 1L, B = 1L))
    expect_false(has_run())
    session$setInputs(analysis_type = "A")
    expect_equal(initialized, c(A = 1L, B = 1L))
    expect_false(has_run())
    expect_null(model_out())
    session$setInputs(`a-run` = 2L)
    expect_equal(model_out()$summary, 6)
    expect_equal(fitted, c(A = 2L, B = 0L))
    data(data.frame(value = 4:6))
    session$flushReact()
    expect_false(has_run())
    expect_null(model_out())
    expect_equal(initialized, c(A = 1L, B = 1L))
    session$setInputs(`a-run` = 3L)
    expect_equal(model_out()$summary, 15)
    expect_equal(fitted, c(A = 3L, B = 0L))
  })
})
