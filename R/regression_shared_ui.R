# ===============================================================
# ?? Regression shared UI helpers
# ===============================================================

reg_detect_types <- function(df) {
  num_vars <- names(df)[sapply(df, is.numeric)]
  fac_vars <- names(df)[sapply(df, function(x) is.factor(x) || is.character(x))]
  list(num = num_vars, fac = fac_vars)
}

reg_variable_selectors_ui <- function(ns, types, allow_random = FALSE) {
  out <- list(
    with_help_tooltip(
      selectInput(ns("dep"), "Response variable (numeric)", choices = types$num),
      "Pick the numeric outcome you want the model to explain."
    ),
    with_help_tooltip(
      selectInput(ns("fixed"), "Categorical predictors", choices = types$fac, multiple = TRUE),
      "Select group variables that might influence the response."
    ),
    with_help_tooltip(
      selectInput(ns("covar"), "Numeric predictors", choices = types$num, multiple = TRUE),
      "Select numeric predictors that could explain changes in the response."
    )
  )
  if (allow_random) {
    out <- c(out, list(
      with_help_tooltip(
        tagList(
          selectInput(ns("random"), "Random effect(s) (categorical)", choices = types$fac, multiple = TRUE),
          checkboxInput(ns("random_nested"), "Nest random effects in selection order", value = FALSE)
        ),
        "Choose grouping factors for random intercepts when using mixed models. Select multiple to include several random effects, or nest them to model hierarchical structure."
      )
    ))
  }
  do.call(tagList, out)
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
  wellPanel(
    strong("Model formula: "),
    code(form_txt)
  )
}

