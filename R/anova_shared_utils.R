#### Section: Utility Helpers ####

anova_protect_vars <- function(vars) {
  if (is.null(vars) || length(vars) == 0) return(vars)

  vals <- vapply(vars, function(v) {
    if (is.null(v) || is.na(v) || !nzchar(v)) return("")
    if (grepl("^`.*`$", v)) v else paste0("`", v, "`")
  }, character(1))

  vals[nzchar(vals)]
}
