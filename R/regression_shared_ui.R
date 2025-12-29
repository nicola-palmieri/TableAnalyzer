# ===============================================================
# ?? Regression shared UI helpers
# ===============================================================

reg_detect_types <- function(df) {
  num_vars <- names(df)[sapply(df, is.numeric)]
  fac_vars <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
  list(num = num_vars, fac = fac_vars)
}

reg_interactions_ui <- function(ns, fixed, fac_vars) {
  if (is.null(fixed) || length(fixed) < 2) return(NULL)
  cats_only <- intersect(fixed, fac_vars)
  if (length(cats_only) < 2) return(NULL)
  pairs <- combn(cats_only, 2, simplify = FALSE)
  pair_labels <- vapply(pairs, function(p) paste(p, collapse = " × "), character(1))
  pair_values <- vapply(pairs, function(p) paste(p, collapse = ":"), character(1))
  with_help_tooltip(
    checkboxGroupInput(
      ns("interactions"),
      label = "Select 2-way interactions (optional)",
      choices = stats::setNames(pair_values, pair_labels)
    ),
    "Tick pairs of factors to let the model test if their joint effect matters."
  )
}

# ---------------------------------------------------------------
# Formula construction
# ---------------------------------------------------------------

reg_formula_preview_ui <- function(ns, dep, rhs) {
  if (is.null(dep) || !nzchar(dep)) return(NULL)
  form_txt <- reg_formula_text(dep, rhs)
  tagList(
    tags$label(`for` = ns("formula_preview"), class = "control-label", "Model formula"),
    wellPanel(
      class = "ta-formula-preview",
      code(form_txt, id = ns("formula_preview"))
    )
  )
}

