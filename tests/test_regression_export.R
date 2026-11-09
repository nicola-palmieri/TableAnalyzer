library(testthat)
library(officer)
library(flextable)

source("../R/helpers.R")
source("../R/regression_shared_export.R")

test_that("Word reports classify significance using unrounded p-values", {
  for (target_p in c(.04996, .05004, .00007)) {
    x <- 1:20
    residual <- residuals(lm(sin(x) ~ x))
    target_t <- qt(1 - target_p / 2, 18)
    residual <- residual * sqrt(sum((x - mean(x))^2) * 18 / (sum(residual^2) * target_t^2))
    model <- lm(y ~ x, data = data.frame(x = x, y = x + residual))
    entry <- list(model = model, response = "y")

    coefficients <- extract_coef_entry(entry)
    anova <- extract_anova_entry(entry)
    expect_equal(coefficients$`Pr(>|t|)`[coefficients$Term == "x"], target_p, tolerance = 1e-10)
    expect_equal(anova$`Pr(>F)`[anova$Effect == "x"], target_p, tolerance = 1e-10)

    path <- tempfile(fileext = ".docx")
    write_lm_docx_combined(list(entry), path)
    document <- xml2::read_xml(unz(path, "word/document.xml"))
    unlink(path)
    rows <- xml2::xml_find_all(document, ".//w:tr[w:tc//w:t[text()='x']]")
    expect_length(rows, 2)
    expected_label <- if (target_p < .0001) "<0.0001" else sprintf("%.4f", target_p)
    for (row in rows) {
      cell <- xml2::xml_find_first(row, "./w:tc[last()]")
      expect_equal(xml2::xml_text(cell), expected_label)
      bold <- xml2::xml_find_all(cell, ".//w:rPr/w:b[not(@w:val='0') and not(@w:val='false')]")
      expect_equal(length(bold) > 0, target_p < .05)
    }
  }
})
