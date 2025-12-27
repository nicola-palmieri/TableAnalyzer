# ===============================================================
# ?? Regression validation helpers
# ===============================================================

validate_regression_inputs <- function(df, input, engine, strat_details) {
  # ---- 1) Validate fixed effect factors ----
  if (length(input$fixed) > 0) {
    for (f in input$fixed) {
      validate(
        need(dplyr::n_distinct(df[[f]]) > 1,
             paste0("Categorical predictor '", f, "' must contain at least two levels."))
      )
    }
  }

  # ---- 2) Validate factor level orders ----
  if (length(input$fixed) > 0) {
    for (f in input$fixed) {
      ord <- input[[paste0("order_", f)]]
      if (!is.null(ord)) {
        validate(
          need(length(ord) > 1,
               paste0("The level order for '", f, "' must contain at least two levels."))
        )
        validate(
          need(all(ord %in% unique(df[[f]])),
               paste0("Invalid level order for '", f, "'. Some selected levels are not present in the data."))
        )
      }
    }
  }

  # ---- 3) Validate numeric covariates (optional but recommended) ----
  if (length(input$covar) > 0) {
    for (v in input$covar) {
      validate(
        need(stats::var(df[[v]], na.rm = TRUE) > 0,
             paste0("Numeric predictor '", v, "' has zero variance and cannot be used in the model."))
      )
    }
  }

  # ---- 4) Require a random effect for LMM ----
  if (identical(engine, "lmm")) {
    validate(
      need(length(input$random) > 0,
           "Select at least one random effect for the LMM; otherwise use the LM option.")
    )
  }

  # ---- 5) Validate stratification (each stratum must allow model fitting) ----
  if (!is.null(strat_details$var)) {
    s_var <- strat_details$var
    for (lev in strat_details$levels) {
      sub <- df[df[[s_var]] == lev, , drop = FALSE]

      validate(
        need(nrow(sub) > 1,
             paste0("No sufficient observations available for stratum '", lev, "'."))    
      )

      # Each fixed factor must have =2 levels within each stratum
      if (length(input$fixed) > 0) {
        for (f in input$fixed) {
          validate(
            need(dplyr::n_distinct(sub[[f]]) > 1,
                 paste0("In stratum '", lev, "', predictor '", f,
                        "' contains fewer than two levels."))
          )
        }
      }

      if (engine == "lmm" && length(input$random) > 0) {
        for (r in input$random) {
          if (!r %in% names(sub)) next
          sub[[r]] <- droplevels(factor(sub[[r]]))
          validate(
            need(dplyr::n_distinct(sub[[r]]) > 1,
                 paste0("In stratum '", lev, "', random effect '", r,
                        "' must have at least two levels."))
          )
        }
      }
    }
  }
}

sanitize_random_effects <- function(df, random_vars) {
  if (length(random_vars) == 0) {
    return(df)
  }

  for (r in random_vars) {
    if (!r %in% names(df)) next
    df[[r]] <- droplevels(factor(df[[r]]))
  }

  df
}

apply_fixed_level_orders <- function(df, fixed_vars, input) {
  for (f in fixed_vars) {
    ord <- input[[paste0("order_", f)]]
    if (is.null(ord) || length(ord) <= 1 || !f %in% names(df)) next

    df[[f]] <- factor(df[[f]], levels = ord)
    df[[f]] <- droplevels(df[[f]])
  }

  df
}
