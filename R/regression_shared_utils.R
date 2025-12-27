# ===============================================================
# ?? Regression shared utilities
# ===============================================================

compact_chr <- function(x) {
  if (is.null(x) || length(x) == 0) return(character(0))
  x
}

reg_protect_vars <- function(vars) {
  if (is.null(vars) || length(vars) == 0) return(vars)

  vals <- vapply(vars, function(v) {
    if (is.null(v) || is.na(v) || !nzchar(v)) return("")
    if (grepl("^`.*`$", v)) v else paste0("`", v, "`")
  }, character(1))

  vals[nzchar(vals)]
}

reg_protect_interactions <- function(interactions) {
  if (is.null(interactions) || length(interactions) == 0) return(interactions)

  vapply(interactions, function(term) {
    parts <- strsplit(term, ":", fixed = TRUE)[[1]]
    paste(reg_protect_vars(parts), collapse = ":")
  }, character(1))
}

reg_formula_text <- function(dep, rhs) {
  dep <- reg_protect_vars(dep)
  if (length(rhs) == 0) {
    paste(dep, "~ 1")
  } else {
    paste(dep, "~", paste(rhs, collapse = " + "))
  }
}
