# Visual audit using the bundled synthetic example; no data files are changed.
# Run from the app root: Rscript scripts/review_colors.R [evidence-directory]
options(device = function(...) grDevices::pdf(file = NULL))
Sys.setenv(NOT_CRAN = "true")
source("R/00_shared_dependencies.R")

review_colors <- function(directory, methods_filter = character()) {
  dir.create(directory, recursive = TRUE, showWarnings = FALSE)
  app <- shinytest2::AppDriver$new(app_dir = ".", name = "table-color-review",
    width = 1440, height = 1000, timeout = 120000, load_timeout = 60000)
  on.exit(app$stop(), add = TRUE)
  js <- function(x) as.character(jsonlite::toJSON(x, auto_unbox = TRUE))
  app$run_js(paste(readLines(system.file("review", "colors.js", package = "VetmeduniAppTemplate")), collapse = "\n"))
  report_file <- file.path(directory, "color-review.json")
  results <- if (length(methods_filter) && file.exists(report_file)) jsonlite::read_json(report_file) else list()
  checks <- 0L
  check <- function(ok, message) {
    if (!isTRUE(ok)) stop(message, call. = FALSE)
    checks <<- checks + 1L
    cat("PASS:", message, "\n"); flush.console()
  }
  set <- function(...) app$set_inputs(..., wait_ = FALSE)
  ready <- function(id) app$wait_for_js(sprintf("document.getElementById(%s)?.classList.contains('shiny-bound-input')", js(id)))
  snapshot <- function(name, widths = c(1440L, 768L, 390L)) {
    app$wait_for_idle(timeout = 180000)
    for (width in widths) {
      app$set_window_size(width = width, height = 1000)
      app$wait_for_idle(timeout = 180000)
      app$run_js("document.activeElement?.blur();document.querySelectorAll('.navbar .dropdown-menu.show,.navbar .dropdown-toggle.show').forEach(e=>e.classList.remove('show'));window.scrollTo(0,0)")
      state <- app$get_js("window.vetmeduniColorAudit.snapshot()")
      results[[paste(name, width, sep = "-")]] <<- state
      jsonlite::write_json(results, file.path(directory, "color-review.json"), auto_unbox = TRUE, pretty = TRUE)
      check(!state$overflow && state$logoInside, paste(name, width, "page and logo fit"))
      if (length(state$failures)) cat("CONTRAST:", jsonlite::toJSON(state$failures, auto_unbox = TRUE), "\n")
      app$get_chromote_session()$screenshot(file.path(directory, paste0(name, "-", width, ".png")))
    }
  }
  figures <- function(name, prefix = NULL) {
    set(main_nav = "visualize_tab")
    if (is.null(prefix)) { app$wait_for_idle(); snapshot(name); return() }
    id <- paste0(prefix, "-plot_type")
    ready(id)
    choices <- app$get_js(sprintf("Object.keys(document.getElementById(%s).selectize.options)", js(id)))
    for (choice in choices) {
      do.call(set, stats::setNames(list(choice), id))
      app$wait_for_js("[...document.querySelectorAll('.tab-pane.active .shiny-plot-output')].some(e=>e.checkVisibility() && e.querySelector('img')?.complete && e.querySelector('img').naturalWidth>0)")
      snapshot(paste(name, choice, sep = "-"))
    }
  }
  app$wait_for_js("document.querySelector('.navbar-institution img')?.naturalWidth>0 && document.querySelector('.ta-navbar-logo')?.naturalWidth>0")
  check(app$get_js("document.querySelector('link[rel=icon]').type==='image/svg+xml' && document.querySelector('link[rel=icon]').getAttribute('href')===document.querySelector('.ta-navbar-logo').getAttribute('src')"),
    "self-contained SVG icon and favicon agree")
  feedback <- app$get_js("window.vetmeduniColorAudit.feedback()")
  check(all(vapply(feedback, function(x) x$contrast >= 4.5, logical(1))), "alerts, notifications and running states meet text contrast")
  jsonlite::write_json(feedback, file.path(directory, "semantic-colors.json"), auto_unbox = TRUE, pretty = TRUE)
  snapshot("home")
  set(main_nav = "upload_tab", `upload-data_source` = "example")
  app$wait_for_js("document.querySelectorAll('#upload-preview tbody tr').length>1")
  snapshot("upload")
  app$set_window_size(width = 1440, height = 1000)
  app$click("upload-open_type_editor")
  app$wait_for_js("document.querySelector('.modal select')?.selectize != null")
  app$run_js("const s=[...document.querySelectorAll('.modal select')].find(e=>e.selectize).selectize;s.focus();s.open()")
  check(app$get_js("(()=>{const e=document.querySelector('.modal .selectize-input.focus');return !!e && getComputedStyle(e).outlineColor==='rgb(175, 33, 68)' && parseFloat(getComputedStyle(e).outlineWidth)>=2})()"),
    "column-type modal has an institutional focus ring")
  app$get_chromote_session()$screenshot(file.path(directory, "upload-column-types-focus.png"))
  app$run_js("document.querySelector('.modal [data-dismiss=modal],.modal [data-bs-dismiss=modal]').click()")
  app$wait_for_js("document.querySelector('.modal.show')===null")
  set(main_nav = "filter_tab")
  snapshot("filter")

  methods <- list(
    list(label = "Descriptive Statistics", id = "desc-cat_vars", run = "desc-run", plot = "visualize-descriptive",
      inputs = list(`analysis-desc-cat_vars` = "treatment", `analysis-desc-num_vars` = "cortisol")),
    list(label = "One-way ANOVA", id = "anova1-group", run = "anova1-run", plot = "visualize-oneway",
      inputs = list(`analysis-anova1-group` = "treatment", `analysis-anova1-response-response` = "cortisol")),
    list(label = "Two-way ANOVA", id = "anova2-factor1", run = "anova2-run", plot = "visualize-twoway",
      inputs = list(`analysis-anova2-factor1` = "treatment", `analysis-anova2-factor2` = "day", `analysis-anova2-response-response` = "cortisol")),
    list(label = "Linear Model (LM)", id = "lm-response-response", run = "lm-run", plot = NULL,
      inputs = list(`analysis-lm-response-response` = "cortisol", `analysis-lm-fixed` = "treatment", `analysis-lm-covar` = "glucose")),
    list(label = "Linear Mixed Model (LMM)", id = "lmm-response-response", run = "lmm-run", plot = NULL,
      inputs = list(`analysis-lmm-response-response` = "cortisol", `analysis-lmm-fixed` = "treatment", `analysis-lmm-random` = "animal_id")),
    list(label = "Pairwise Correlation", id = "pairs-vars", run = "pairs-run", plot = NULL,
      inputs = list(`analysis-pairs-vars` = c("cortisol", "glucose"))),
    list(label = "PCA", id = "pca-vars", run = "pca-run_pca", plot = "visualize-pca",
      inputs = list(`analysis-pca-vars` = c("cortisol", "glucose", "weight_baseline"))))
  if (length(methods_filter)) methods <- Filter(function(x) x$label %in% methods_filter, methods)
  for (method in methods) {
    app$set_window_size(width = 1440, height = 1000)
    set(main_nav = "analysis_tab")
    set(`analysis-analysis_type` = method$label)
    ready(paste0("analysis-", method$id))
    for (id in names(method$inputs)) {
      ready(id)
      choices <- js(as.character(method$inputs[[id]]))
      app$wait_for_js(sprintf("(()=>{const e=document.getElementById(%s);return !e.selectize || [].concat(%s).every(v=>e.selectize.options[v]!=null)})()", js(id), choices))
      do.call(set, stats::setNames(list(method$inputs[[id]]), id))
    }
    app$wait_for_idle()
    app$click(paste0("analysis-", method$run))
    app$wait_for_idle(timeout = 180000)
    name <- gsub("[^a-z0-9]+", "-", tolower(method$label))
    snapshot(paste0("analysis-", name))
    figures(paste0("visualize-", name), method$plot)
  }
  set(main_nav = "guide_tab")
  app$wait_for_js("document.getElementById('guide-article')?.offsetWidth>0")
  snapshot("guide")
  failures <- sum(vapply(results, function(x) length(x$failures), integer(1)))
  check(failures == 0L, paste("normal-text contrast across", length(results), "page/viewport cases"))
  cat(checks, "checks passed. Evidence:", normalizePath(directory), "\n")
}

args <- commandArgs(trailingOnly = TRUE)
selected <- sub("^--method=", "", grep("^--method=", args, value = TRUE))
directory <- args[!grepl("^--method=", args)]
review_colors(if (length(directory)) directory[[1]] else file.path(Sys.getenv("TEMP", tempdir()), "TableAnalyzer-color-review"), selected)
