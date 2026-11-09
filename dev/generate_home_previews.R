# Run from the repository root: Rscript dev/generate_home_previews.R
# These examples use the same data preparation and output builders as the app.
suppressPackageStartupMessages(invisible(source("app.R")))

data <- preprocess_uploaded_table(readxl::read_excel("data/toy_animal_trial_data_long.xlsx"))
preview_width <- 4.6
preview_height <- 3.1
preview_dpi <- 200

# An excerpt of the real descriptive summary, with the UI's column labels.
summary_vars <- c("cortisol", "glucose", "heart_rate")
descriptive <- compute_descriptive_summary(data[summary_vars])
summary_table <- as.data.frame(descriptive$skim[
  c("skim_variable", "n_missing", "numeric.mean", "numeric.sd")
])
names(summary_table) <- c("skim_variable", "n_missing", "mean", "sd")
summary_text <- c(
  "Data Summary",
  sprintf("Number of rows     %d", nrow(data)),
  sprintf("Number of columns  %d", length(summary_vars)),
  "",
  "Variable type: numeric",
  "",
  capture.output(print(summary_table, digits = 3, row.names = FALSE))
)
grDevices::png(
  "www/preview-summary.png", width = preview_width, height = preview_height,
  units = "in", res = preview_dpi
)
grid::grid.rect(gp = grid::gpar(fill = "#f8fafc", col = NA))
grid::grid.text(
  paste(summary_text, collapse = "\n"), x = .055, y = .89,
  just = c("left", "top"),
  gp = grid::gpar(fontfamily = "mono", fontsize = 10, lineheight = 1.55, col = "#34435b")
)
invisible(grDevices::dev.off())
stopifnot(file.exists("www/preview-summary.png"))

# The one-way ANOVA gallery's actual mean +/- SE barplot and Dunnett annotations.
anova <- prepare_stratified_anova(
  df = data, responses = "cortisol", model = "oneway_anova",
  factor1_var = "treatment", factor1_order = levels(data$treatment)
)
anova_outputs <- compile_anova_results(anova)
anova_plot <- plot_anova_barplot_meanse(
  data = anova$data_used, info = anova,
  line_colors = resolve_single_color(), base_size = 11,
  posthoc_all = anova_outputs$posthoc
)
stopifnot(is.null(anova_plot$warning), !is.null(anova_plot$plot))
ggplot2::ggsave(
  "www/preview-anova.png", plot = anova_plot$plot,
  width = preview_width, height = preview_height, dpi = preview_dpi, bg = "white"
)

# PCA uses all numeric demo columns and the gallery's treatment color mapping.
numeric_vars <- names(data)[vapply(data, is.numeric, logical(1))]
complete <- data[stats::complete.cases(data[numeric_vars]), , drop = FALSE]
pca <- stats::prcomp(complete[numeric_vars], center = TRUE, scale. = TRUE)
pca_plot <- build_pca_biplot(
  pca_obj = pca, data = complete, color_var = "treatment",
  x_limits = range(pca$x[, 1]), y_limits = range(pca$x[, 2]), base_size = 10
)
ggplot2::ggsave(
  "www/preview-pca.png", plot = pca_plot,
  width = preview_width, height = preview_height, dpi = preview_dpi, bg = "white"
)
stopifnot(all(file.exists(file.path("www", c("preview-summary.png", "preview-anova.png", "preview-pca.png")))))
cat("Generated descriptive, ANOVA, and PCA previews from the bundled example data.\n")
