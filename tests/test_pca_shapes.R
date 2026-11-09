library(testthat)
library(shiny)
library(ggplot2)

for (path in list.files("../R", pattern = "\\.R$", full.names = TRUE)) source(path)

test_that("PCA maps two through ten groups to distinct shapes", {
  set.seed(1)
  for (n_groups in c(2L, 10L)) {
    data <- data.frame(
      x = rnorm(n_groups * 4), y = rnorm(n_groups * 4),
      group = factor(rep(letters[seq_len(n_groups)], each = 4))
    )
    pca <- prcomp(data[c("x", "y")], scale. = TRUE)
    for (color_var in list(NULL, "group")) {
      expect_no_warning(built <- ggplot_build(build_pca_biplot(
        pca, data, shape_var = "group", color_var = color_var
      )))
      expect_length(unique(built$data[[1]]$shape), n_groups)
      expect_false(anyNA(built$data[[1]]$shape))
      if (!is.null(color_var)) expect_length(unique(built$data[[1]]$colour), n_groups)
    }
  }
})

test_that("PCA shapes remain consistent across facets and retain missing groups", {
  set.seed(2)
  data <- data.frame(x = rnorm(16), y = rnorm(16), group = factor(rep(c("a", "b", "c", NA), 4)))
  pca <- prcomp(data[c("x", "y")], scale. = TRUE)
  full <- ggplot_build(build_pca_biplot(pca, data, shape_var = "group"))$data[[1]]
  subset <- which(data$group %in% c("b", "c"))
  facet <- ggplot_build(build_pca_biplot(pca, data, shape_var = "group", subset_rows = subset))$data[[1]]
  expect_equal(facet$shape, full$shape[subset])
  expect_false(anyNA(full$shape))
  expect_equal(nrow(full), nrow(data))
})
