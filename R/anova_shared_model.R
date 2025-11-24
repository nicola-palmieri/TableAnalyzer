#### Section: Model Fitting & Preparation ####

prepare_stratified_anova <- function(
    df,
    responses,
    model,
    factor1_var = NULL,
    factor1_order = NULL,
    factor2_var = NULL,
    factor2_order = NULL,
    stratification = NULL,
    stratify_var = NULL,
    strata_order = NULL
) {
  req(df, responses, model)

  if (!is.null(stratification)) {
    if (!is.null(stratification$var)) {
      stratify_var <- stratification$var
    }
    if (!is.null(stratification$levels)) {
      strata_order <- stratification$levels
    }
  }

  

  df <- df |>
    set_factor_levels(factor1_var, factor1_order) |>
    set_factor_levels(factor2_var, factor2_order) |>
    set_factor_levels(stratify_var, strata_order, default_factor = TRUE)

  strata <- if (!is.null(stratify_var) && stratify_var %in% names(df)) {
    levels(df[[stratify_var]])
  } else {
    NULL
  }

  factor1_rhs <- anova_protect_vars(factor1_var)
  factor2_rhs <- anova_protect_vars(factor2_var)

  rhs <- switch(
    model,
    oneway_anova = factor1_rhs,
    twoway_anova = if (length(factor1_rhs) > 0 && length(factor2_rhs) > 0) {
      paste(factor1_rhs, factor2_rhs, sep = " *")
    } else {
      factor1_rhs
    },
    factor1_rhs
  )
  rhs <- if (is.null(rhs) || rhs == "") "1" else rhs

  build_formula <- function(resp) stats::as.formula(paste(anova_protect_vars(resp), "~", rhs))
  safe_fit <- purrr::safely(function(fml, data) stats::lm(fml, data = data))

  base_info <- list(
    type = model,
    responses = responses,
    factors = list(factor1 = factor1_var, factor2 = factor2_var),
    orders = list(order1 = factor1_order, order2 = factor2_order),
    data_used = df
  )

  if (is.null(strata)) {
    return(c(base_info, list(models = fit_models_for_data(df, responses, build_formula, safe_fit), strata = NULL)))
  }

  models <- lapply(strata, function(s) {
    subset_rows <- df[[stratify_var]] == s & !is.na(df[[stratify_var]])
    fit_models_for_data(df[subset_rows, , drop=FALSE], responses, build_formula, safe_fit)
  })
  names(models) <- strata

  c(base_info, list(models = models, strata = list(var = stratify_var, levels = strata)))
}


set_factor_levels <- function(data, var, levels = NULL, default_factor = FALSE) {
  if (is.null(var) || !var %in% names(data)) return(data)
  if (!is.null(levels)) {
    data[[var]] <- factor(as.character(data[[var]]), levels = levels)
  } else if (default_factor) {
    data[[var]] <- as.factor(as.character(data[[var]]))
  }
  data
}


fit_models_for_data <- function(data, responses, build_formula, safe_fit) {
  lapply(responses, function(resp) {
    fit_result <- safe_fit(build_formula(resp), data)
    list(
      model = fit_result$result,
      error = if (!is.null(fit_result$error)) conditionMessage(fit_result$error) else NULL
    )
  }) |>
    stats::setNames(responses)
}


compile_anova_results <- function(model_info) {
  if (is.null(model_info) || is.null(model_info$models)) return(NULL)

  factor_names <- unlist(model_info$factors)
  factor_names <- factor_names[!is.na(factor_names) & nzchar(factor_names)]

  if (is.null(model_info$strata)) {
    summary_list <- list()
    posthoc_list <- list()
    effects_list <- list()
    errors_list <- list()

    for (resp in names(model_info$models)) {
      entry <- model_info$models[[resp]]
      entry_errors <- character(0)
      if (!is.null(entry$model)) {
        outputs <- prepare_anova_outputs(entry$model, factor_names)
        if (!is.null(outputs$error)) {
          entry_errors <- c(entry_errors, outputs$error)
          summary_list[[resp]] <- NULL
          posthoc_list[[resp]] <- NULL
          effects_list[[resp]] <- NULL
        } else {
          summary_list[[resp]] <- outputs$anova_table
          posthoc_list[[resp]] <- outputs$posthoc_table
          effects_list[[resp]] <- build_effects(outputs)
        }
      } else {
        summary_list[[resp]] <- NULL
        posthoc_list[[resp]] <- NULL
        effects_list[[resp]] <- NULL
      }
      if (!is.null(entry$error)) {
        entry_errors <- c(entry_errors, entry$error)
      }
      if (length(entry_errors) > 0) {
        errors_list[[resp]] <- paste(unique(entry_errors), collapse = "\n")
      }
    }

    return(list(
      summary = summary_list,
      posthoc = posthoc_list,
      effects = effects_list,
      errors = errors_list
    ))
  }

  summary_list <- list()
  posthoc_list <- list()
  effects_list <- list()
  errors_list <- list()

  for (stratum_name in names(model_info$models)) {
    stratum_models <- model_info$models[[stratum_name]]
    if (is.null(stratum_models)) next

    for (resp in names(stratum_models)) {
      entry <- stratum_models[[resp]]
      outputs <- NULL
      entry_error <- NULL
      if (!is.null(entry$model)) {
        outputs <- prepare_anova_outputs(entry$model, factor_names)
        if (!is.null(outputs$error)) {
          entry_error <- outputs$error
          outputs <- NULL
        }
      }

      if (is.null(summary_list[[resp]])) summary_list[[resp]] <- list()
      if (is.null(posthoc_list[[resp]])) posthoc_list[[resp]] <- list()
      if (is.null(effects_list[[resp]])) effects_list[[resp]] <- list()
      if (is.null(errors_list[[resp]])) errors_list[[resp]] <- list()

      summary_list[[resp]][[stratum_name]] <- if (!is.null(outputs)) outputs$anova_table else NULL
      posthoc_list[[resp]][[stratum_name]] <- if (!is.null(outputs)) outputs$posthoc_table else NULL
      effects_list[[resp]][[stratum_name]] <- if (!is.null(outputs)) build_effects(outputs) else NULL

      if (!is.null(entry$error)) {
        entry_error <- c(entry_error, entry$error)
      }

      if (!is.null(entry_error)) {
        errors_list[[resp]][[stratum_name]] <- paste(unique(entry_error), collapse = "\n")
      }
    }
  }


  list(
    summary = summary_list,
    posthoc = posthoc_list,
    effects = effects_list,
    errors = errors_list
  )
}


build_effects <- function(outputs) {
  if (is.null(outputs) || is.null(outputs$anova_table)) return(NULL)
  effects <- data.frame(
    Effect = outputs$anova_table$Effect,
    significant = outputs$anova_significant,
    stringsAsFactors = FALSE
  )
  if ("p.value" %in% names(outputs$anova_table)) {
    effects$p.value <- outputs$anova_table$p.value
  }
  effects
}