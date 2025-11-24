#### Section: Utility Helpers ####

anova_protect_vars <- function(vars) {
  if (is.null(vars) || length(vars) == 0) return(vars)

  vals <- vapply(vars, function(v) {
    if (is.null(v) || is.na(v) || !nzchar(v)) return("")
    if (grepl("^`.*`$", v)) v else paste0("`", v, "`")
  }, character(1))

  vals[nzchar(vals)]
}

get_emmeans_adjustment_choices <- function() {
  methods <- tryCatch(getFromNamespace(".all.adjustments", "emmeans"), error = function(e) NULL)
  if (is.null(methods)) {
    methods <- tryCatch(getFromNamespace(".all.adj", "emmeans"), error = function(e) NULL)
  }
  if (is.null(methods)) {
    methods <- tryCatch(stats::p.adjust.methods, error = function(e) c("none"))
  }

  methods <- unique(tolower(c("none", methods)))
  stats::setNames(methods, vapply(methods, format_adjust_label, character(1)))
}

format_adjust_label <- function(method) {
  method <- tolower(method)
  if (identical(method, "none")) {
    return("None (no adjustment)")
  }

  pretty <- gsub("_", " ", method)
  paste0(toupper(substr(pretty, 1, 1)), substr(pretty, 2, nchar(pretty)))
}
