#### Table Analyzer — Shared ANOVA Module  ####
#### Section: UI & Output Binding ####

build_anova_layout_controls <- function(ns, input, info) {
  has_strata <- !is.null(info$strata) && !is.null(info$strata$var)
  n_responses <- if (!is.null(info$responses)) length(info$responses) else 0

  build_grid_section <- function(title, grid_id, row_help, col_help) {
    tagList(
      h5(title),
      plot_grid_ui(
        id = ns(grid_id),
        rows_help = row_help,
        cols_help = col_help
      )
    )
  }

  strata_inputs <- if (has_strata) {
    build_grid_section(
      title = "Across strata:",
      grid_id = "strata_grid",
      row_help = "Set how many rows of plots to use when displaying different strata.",
      col_help = "Set how many columns of plots to use when displaying different strata."
    )
  } else {
    NULL
  }

  response_inputs <- if (!is.null(n_responses) && n_responses > 1) {
    build_grid_section(
      title = "Across responses:",
      grid_id = "response_grid",
      row_help = "Set the number of plot rows when multiple responses are shown together.",
      col_help = "Set the number of plot columns when multiple responses are shown together."
    )
  } else {
    NULL
  }

  tagList(strata_inputs, response_inputs)
}


#### Formula utilities ####

render_anova_results <- function(ns, model_info, module_label = "ANOVA") {
  if (is.null(model_info)) return(NULL)
  
  responses <- model_info$responses
  strata_info <- model_info$strata
  
  # No stratification
  if (is.null(strata_info)) {
    tabs <- lapply(seq_along(responses), function(i) {
      tabPanel(
        title = responses[i],
        tags$div(
          verbatimTextOutput(ns(paste0("summary_", i)))
        )
      )
    })
    return(do.call(tabsetPanel, c(list(id = ns("results_tabs")), tabs)))
  }
  
  # Stratified
  strata_levels <- strata_info$levels
  tabs <- lapply(seq_along(responses), function(i) {
    response_name <- responses[i]
    stratum_tabs <- lapply(seq_along(strata_levels), function(j) {
      stratum_name <- strata_levels[j]
      tabPanel(
        title = stratum_name,
        tags$div(
          verbatimTextOutput(ns(paste0("summary_", i, "_", j)))
        )
      )
    })
    tabPanel(
      title = response_name,
      do.call(tabsetPanel, c(list(id = ns(paste0("strata_tabs_", i))), stratum_tabs))
    )
  })
  do.call(tabsetPanel, c(list(id = ns("results_tabs")), tabs))
}

bind_anova_outputs <- function(ns, output, models_reactive) {
  observeEvent(models_reactive(), {
    model_info <- models_reactive()
    if (is.null(model_info)) return()

    responses <- model_info$responses
    model_list <- model_info$models
    strata_info <- model_info$strata
    factors <- unlist(model_info$factors, use.names = FALSE)
    p_adjust_method <- if (!is.null(model_info$p_adjust_method)) model_info$p_adjust_method else "none"
    
    # --- Non-stratified case ---
    if (is.null(strata_info)) {
      for (i in seq_along(responses)) {
        local({
          idx <- i
          response_name <- responses[i]
          model_entry <- model_list[[response_name]]
          bind_single_model_outputs(
            output,
            summary_id = paste0("summary_", idx),
            download_id = paste0("download_", idx),
            model_entry = model_entry,
            response_name = response_name,
            factors = factors,
            p_adjust_method = p_adjust_method
          )
        })
      }
      return()
    }
    
    # --- Stratified case ---
    strata_levels <- strata_info$levels
    for (i in seq_along(responses)) {
      for (j in seq_along(strata_levels)) {
        local({
          idx <- i
          stratum_idx <- j
          response_name <- responses[i]
          stratum_label <- strata_levels[j]
          model_entry <- model_list[[stratum_label]][[response_name]]
          bind_single_model_outputs(
            output,
            summary_id = paste0("summary_", idx, "_", stratum_idx),
            download_id = paste0("download_", idx, "_", stratum_idx),
            model_entry = model_entry,
            response_name = response_name,
            factors = factors,
            stratum_label = stratum_label,
            p_adjust_method = p_adjust_method
          )
        })
      }
    }
  })
}

#### Results export ####

bind_single_model_outputs <- function(output, summary_id, download_id,
                                      model_entry, response_name, factors,
                                      stratum_label = NULL,
                                      p_adjust_method = "none") {
  output[[summary_id]] <- renderPrint({
    print_anova_summary_and_posthoc(model_entry, factors, p_adjust_method)
  })

  output[[download_id]] <- downloadHandler(
    filename = function() {
      base <- paste0("anova_results_", sanitize_name(response_name))
      if (!is.null(stratum_label)) {
        base <- paste0(base, "_stratum_", sanitize_name(stratum_label))
      }
      paste0(base, "_", Sys.Date(), ".docx")
    },
    content = function(file) {
      if (is.null(model_entry) || !is.null(model_entry$error) || is.null(model_entry$model)) {
        stop("Model not available for download due to fitting error.")
      }
      results <- prepare_anova_outputs(model_entry$model, factors, p_adjust_method)
      if (!is.null(results$error)) {
        stop(paste0("ANOVA results unavailable: ", results$error))
      }
      if (is.null(results$anova_table)) {
        stop("ANOVA results are unavailable for export.")
      }
      write_anova_docx(
        file = file,
        content = results,
        response_name = response_name,
        stratum_label = stratum_label
      )
    }
  )
}

sanitize_name <- function(name) {
  safe <- gsub("[^A-Za-z0-9]+", "_", name)
  safe <- gsub("_+", "_", safe)
  safe <- gsub("^_|_$", "", safe)
  if (!nzchar(safe)) safe <- "unnamed"
  safe
}

print_anova_summary_and_posthoc <- function(model_entry, factors, p_adjust_method = "none") {
  if (is.null(model_entry) || (is.list(model_entry) && is.null(model_entry$model))) {
    cat("Model is not available.\n")
    return(invisible(NULL))
  }

  if (!is.null(model_entry$error)) {
    cat(format_safe_error_message("Model fitting failed", model_entry$error), "\n", sep = "")
    return(invisible(NULL))
  }

  model_obj <- model_entry$model
  results <- prepare_anova_outputs(model_obj, factors, p_adjust_method)
  if (!is.null(results$error)) {
    cat(format_safe_error_message("ANOVA computation failed", results$error), "\n", sep = "")
    return(invisible(NULL))
  }
  if (is.null(results$anova_object)) {
    cat("ANOVA results are unavailable.\n")
    return(invisible(NULL))
  }
  print(results$anova_object)

  if (length(results$posthoc_details) == 0) {
    cat("\nNo post-hoc Tukey comparisons were generated.\n")
  } else {
    for (factor_nm in names(results$posthoc_details)) {
      details <- results$posthoc_details[[factor_nm]]
      if (!is.null(details$error)) {
        cat(
          "\n",
          format_safe_error_message(
            paste("Post-hoc Tukey comparisons for", factor_nm, "failed"),
            details$error
          ),
          "\n",
          sep = ""
        )
      } else if (!is.null(details$table)) {
        cat("\nPost-hoc Tukey comparisons for", factor_nm, ":\n")
        print(details$table)
      }
    }
  }
  invisible(results)
}

#### Section: Model Fitting & Preparation ####

