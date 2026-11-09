library(testthat)
library(shiny)

for (path in list.files("../R", pattern = "\\.R$", full.names = TRUE)) source(path)

test_that("the PCA ellipse condition resolves namespaced input IDs in JavaScript", {
  node <- Sys.which("node")
  skip_if(!nzchar(node), "Node.js is needed to execute the generated JavaScript condition")
  ui <- visualize_pca_ui("visualize-pca", data.frame(group = factor(c("a", "b"))))
  document <- xml2::read_html(htmltools::renderTags(ui)$html)
  conditions <- xml2::xml_attr(xml2::xml_find_all(document, "//*[@data-display-if]"), "data-display-if")
  condition <- conditions[grepl("pca_color", conditions, fixed = TRUE)]
  expect_length(condition, 1)

  script <- tempfile(fileext = ".js")
  on.exit(unlink(script), add = TRUE)
  writeLines(c(
    paste0("const condition = ", jsonlite::toJSON(condition, auto_unbox = TRUE), ";"),
    "const evaluate = new Function('input', 'return (' + condition + ');');",
    "const values = ['None', 'group'].map(value => evaluate({'visualize-pca-pca_color': value}));",
    "process.stdout.write(JSON.stringify(values));"
  ), script)
  result <- system2(node, shQuote(script), stdout = TRUE, stderr = TRUE)
  expect_null(attr(result, "status"))
  expect_equal(jsonlite::fromJSON(result), c(FALSE, TRUE))
})
