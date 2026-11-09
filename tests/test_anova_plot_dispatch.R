library(testthat)
library(shiny)

for (path in list.files("../R", pattern = "[.]R$", full.names = TRUE)) source(path)

dispatch_fixture <- function(server) {
  context <- new.env(parent = environment(server))
  context$calls <- character()
  context$layouts <- list()
  context$warn_next <- FALSE
  builder <- function(kind) {
    force(kind)
    function(...) {
      context$calls <- c(context$calls, kind)
      arguments <- list(...)
      layout <- if ("layout_values" %in% names(arguments)) arguments$layout_values else arguments[[3]]
      context$layouts <- c(context$layouts, list(layout))
      warning <- if (context$warn_next) "Grid is too large for the selected panels." else NULL
      context$warn_next <- FALSE
      list(
        plot = NULL, warning = warning,
        layout = list(strata = list(rows = 1L, cols = 1L), responses = list(rows = 1L, cols = 1L)),
        defaults = list(strata = list(rows = 1L, cols = 1L), responses = list(rows = 1L, cols = 1L)),
        panel_counts = list(strata = 1L, responses = 1L)
      )
    }
  }
  context$plot_anova_lineplot_meanse <- builder("lineplot_mean_se")
  context$plot_anova_barplot_meanse <- builder("barplot_mean_se")
  context$plot_anova_boxplot <- builder("boxplot")
  environment(server) <- context
  list(server = server, context = context)
}

for (engine in c("oneway", "twoway")) {
  test_that(paste(engine, "ANOVA builds only the selected plot on Apply"), {
    server <- if (engine == "oneway") visualize_oneway_server else visualize_twoway_server
    fixture <- dispatch_fixture(server)
    filtered <- reactiveVal(data.frame(group = factor(c("a", "b")), value = 1:2))
    model <- reactiveVal(list(type = paste0(engine, "_anova"), responses = "value", strata = NULL))

    testServer(fixture$server, args = list(filtered_data = filtered, model_info = model), {
      session$flushReact()
      expect_true(length(fixture$context$calls) > 0L)
      expect_true(all(fixture$context$calls == "lineplot_mean_se"))
      for (i in seq_along(c("lineplot_mean_se", "barplot_mean_se", "boxplot"))) {
        kind <- c("lineplot_mean_se", "barplot_mean_se", "boxplot")[i]
        session$setInputs(plot_type = kind)
        fixture$context$calls <- character()
        session$setInputs(apply_plot = i)
        expect_identical(fixture$context$calls, kind)
      }
      session$setInputs(plot_type = "unknown")
      fixture$context$calls <- character()
      session$setInputs(apply_plot = 4L)
      expect_identical(fixture$context$calls, "lineplot_mean_se")
    })
  })

  test_that(paste(engine, "ANOVA retries only the selected builder when resetting an invalid grid"), {
    server <- if (engine == "oneway") visualize_oneway_server else visualize_twoway_server
    fixture <- dispatch_fixture(server)
    filtered <- reactiveVal(data.frame(group = factor(c("a", "b")), value = 1:2))
    model <- reactiveVal(list(type = paste0(engine, "_anova"), responses = "value", strata = NULL))

    testServer(fixture$server, args = list(filtered_data = filtered, model_info = model), {
      session$flushReact()
      fixture$context$calls <- character()
      fixture$context$layouts <- list()
      fixture$context$warn_next <- TRUE
      session$setInputs(plot_type = "barplot_mean_se", `strata_grid-rows` = 4L, `strata_grid-cols` = 4L)
      expect_identical(fixture$context$calls, rep("barplot_mean_se", 2))
      expect_null(stored$warning)
      expect_identical(tail(fixture$context$layouts, 1)[[1]]$strata_rows, 1L)
      expect_identical(tail(fixture$context$layouts, 1)[[1]]$strata_cols, 1L)
    })
  })
}
