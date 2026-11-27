# Interface feedback only. Scientific figure palettes remain in plot_palette.R.
APP_SEMANTIC_COLORS <- list(
  success = list(accent = "#286e53", text = "#205842", background = "#eef5f1", border = "#b4d1c1"),
  warning = list(accent = "#a86044", text = "#7e4833", background = "#faf1ed", border = "#d5ac9d"),
  error = list(accent = "#af2144", text = "#8c1a36", background = "#fbf1f4", border = "#dbacb8"),
  info = list(accent = "#757575", text = "#505050", background = "#f4f4f4", border = "#d0d0d0"),
  running = list(accent = "#286e53", text = "#205842", background = "#eef5f1", border = "#b4d1c1")
)

# Emit CSS variables from the same definitions used by Bootstrap and the spinner.
app_semantic_css <- function() {
  declarations <- character()
  for (state in names(APP_SEMANTIC_COLORS)) {
    for (part in names(APP_SEMANTIC_COLORS[[state]])) {
      declarations <- c(declarations, sprintf("--app-%s-%s:%s;",
        state, part, APP_SEMANTIC_COLORS[[state]][[part]]))
    }
  }
  paste0(":root{", paste(declarations, collapse = ""), "}")
}
