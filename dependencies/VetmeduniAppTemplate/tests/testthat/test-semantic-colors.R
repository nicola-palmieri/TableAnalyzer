color_contrast <- function(foreground, background) {
  luminance <- function(color) {
    rgb <- as.numeric(grDevices::col2rgb(color)) / 255
    linear <- ifelse(rgb <= .04045, rgb / 12.92, ((rgb + .055) / 1.055)^2.4)
    sum(linear * c(.2126, .7152, .0722))
  }
  values <- c(luminance(foreground), luminance(background))
  (max(values) + .05) / (min(values) + .05)
}

test_that("semantic feedback uses institutional accents with readable normal text", {
  expect_identical(vapply(APP_SEMANTIC_COLORS, `[[`, "", "accent"),
    c(success = "#286e53", warning = "#a86044", error = "#af2144", info = "#757575", running = "#286e53"))
  for (state in names(APP_SEMANTIC_COLORS)) {
    colors <- APP_SEMANTIC_COLORS[[state]]
    expect_true(color_contrast(colors$text, colors$background) >= 4.5, info = state)
    expect_true(color_contrast(colors$accent, "#ffffff") >= 3, info = state)
  }
  expect_gte(color_contrast("#af2144", "#ffffff"), 4.5)
  expect_gte(color_contrast("#ffffff", "#286e53"), 4.5)
})

test_that("rendered dependencies and Bootstrap consume the same semantic definitions", {
  dependency <- app_dependencies()
  for (state in names(APP_SEMANTIC_COLORS)) {
    for (part in names(APP_SEMANTIC_COLORS[[state]])) {
      expect_match(dependency$head, sprintf("--app-%s-%s:%s;", state, part,
        APP_SEMANTIC_COLORS[[state]][[part]]), fixed = TRUE)
    }
  }
  expect_true("semantic-colors.css" %in% dependency$stylesheet)
  actual <- tolower(unname(bslib::bs_get_variables(app_theme(), c("success", "warning", "danger", "info"))))
  expect_identical(actual, c("#286e53", "#a86044", "#af2144", "#757575"))
})

test_that("institutional feedback does not replace scientific categorical colors", {
  original <- unname(grDevices::palette.colors(palette = "Polychrome 36"))
  original <- original[!toupper(original) %in% c("#5A5156", "#E4E1E3")][1:20]
  expect_identical(APP_PALETTE, original)
  expect_identical(unname(app_colours(as.character(1:20))), original)
})

test_that("shared interface assets contain no retired brand accents", {
  files <- list.files(system.file("www", package = "VetmeduniAppTemplate"),
    pattern = "\\.(css|js)$", full.names = TRUE)
  text <- paste(unlist(lapply(files, readLines, warn = FALSE)), collapse = "\n")
  expect_false(grepl("#(0b1d3a|f8007a|587da3|1c345c|496a88)", text, ignore.case = TRUE))
})
