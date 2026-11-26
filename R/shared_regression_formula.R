# ===============================================================
# ?? Regression shared formula builders
# ===============================================================

reg_compose_random_terms <- function(random, nested = FALSE) {
  random <- reg_protect_vars(compact_chr(random))
  if (length(random) == 0) return(character(0))

  if (isTRUE(nested) && length(random) > 1) {
    nested_term <- paste(random, collapse = "/")
    return(paste0("(1|", nested_term, ")"))
  }

  paste0("(1|", random, ")")
}

reg_compose_rhs <- function(fixed, covar, interactions, random = NULL, random_nested = FALSE, engine = c("lm","lmm")) {
  engine <- match.arg(engine)
  fixed <- reg_protect_vars(compact_chr(fixed))
  covar <- reg_protect_vars(compact_chr(covar))
  interactions <- reg_protect_interactions(compact_chr(interactions))

  rhs <- c(fixed, covar, interactions)
  if (engine == "lmm") {
    rand_terms <- reg_compose_random_terms(random, random_nested)
    rhs <- c(rhs, rand_terms)
  }
  rhs
}
