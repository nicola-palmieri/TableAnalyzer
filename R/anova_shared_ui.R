#### Table Analyzer — Shared ANOVA Module  ####
#### Section: UI & Output Binding ####

build_anova_layout_controls <- function(ns, input, info, grid_cache = NULL) {
  has_strata <- !is.null(info$strata) && !is.null(info$strata$var)
  strata_levels <- if (has_strata) info$strata$levels %||% character(0) else character(0)
  n_responses <- if (!is.null(info$responses)) length(info$responses) else 0

  grid_state <- function(grid_id, axis) {
    cache_key <- paste0(sub("_grid$", "", grid_id), "_", axis)
    if (!is.null(grid_cache)) {
      cached <- grid_cache[[cache_key]]
      if (!is.null(cached) && !is.na(cached)) return(cached)
    }

    value <- input[[paste0(grid_id, "-", axis)]]
    if (is.null(value)) value <- input[[paste0(ns(grid_id), "-", axis)]]
    if (length(value) == 0) return(NULL)
    parsed <- suppressWarnings(as.integer(value[1]))
    if (is.na(parsed)) return(NULL)
    parsed
  }

  strata_inputs <- if (has_strata) {
    strata_defaults <- compute_default_grid(max(1L, length(strata_levels)))
    plot_grid_ui(
      id = ns("strata_grid"),
      rows_label = sprintf("Rows for strata (%s, n=%d)", info$strata$var, length(strata_levels)),
      cols_label = sprintf("Cols for strata (%s, n=%d)", info$strata$var, length(strata_levels)),
      rows_help = "Rows of plots when displaying each stratum.",
      cols_help = "Columns of plots when displaying each stratum.",
      rows_value = grid_state("strata_grid", "rows") %||% strata_defaults$rows,
      cols_value = grid_state("strata_grid", "cols") %||% strata_defaults$cols
    )
  } else {
    NULL
  }

  response_inputs <- if (!is.null(n_responses) && n_responses > 1) {
    response_defaults <- compute_default_grid(n_responses)
    plot_grid_ui(
      id = ns("response_grid"),
      rows_label = sprintf("Rows for responses (n=%d)", n_responses),
      cols_label = sprintf("Cols for responses (n=%d)", n_responses),
      rows_help = "Rows of plots when multiple responses are shown together.",
      cols_help = "Columns of plots when multiple responses are shown together.",
      rows_value = grid_state("response_grid", "rows") %||% response_defaults$rows,
      cols_value = grid_state("response_grid", "cols") %||% response_defaults$cols
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
            factors = factors
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
            stratum_label = stratum_label
          )
        })
      }
    }
  })
}

#### Results export ####

bind_single_model_outputs <- function(output, summary_id, download_id,
                                      model_entry, response_name, factors,
                                      stratum_label = NULL) {
  output[[summary_id]] <- renderPrint({
    old_opts <- options(scipen = 6)
    on.exit(options(old_opts), add = TRUE)
    print_anova_summary_and_posthoc(model_entry, factors)
  })

  output[[download_id]] <- downloadHandler(
    filename = function() {
      build_export_filename(
        analysis = "anova",
        scope = "response",
        response = response_name,
        stratum = stratum_label
      )
    },
    content = function(file) {
      if (is.null(model_entry) || !is.null(model_entry$error) || is.null(model_entry$model)) {
        stop("Model not available for download due to fitting error.")
      }
      results <- prepare_anova_outputs(model_entry$model, factors)
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
  sanitize_export_part(name)
}

print_anova_summary_and_posthoc <- function(model_entry, factors) {
  if (is.null(model_entry) || (is.list(model_entry) && is.null(model_entry$model))) {
    cat("Model is not available.\n")
    return(invisible(NULL))
  }

  if (!is.null(model_entry$error)) {
    cat(format_safe_error_message("Model fitting failed", model_entry$error), "\n", sep = "")
    return(invisible(NULL))
  }

  model_obj <- model_entry$model
  results <- prepare_anova_outputs(model_obj, factors)
  if (!is.null(results$error)) {
    cat(format_safe_error_message("ANOVA computation failed", results$error), "\n", sep = "")
    return(invisible(NULL))
  }
  if (is.null(results$anova_object)) {
    cat("ANOVA results are unavailable.\n")
    return(invisible(NULL))
  }
  resp_name <- tryCatch(all.vars(formula(model_entry$model))[1], error = function(...) "Response")
  cat("Anova Table (Type III tests)\n\n")
  cat(sprintf("Response: %s\n", resp_name))
  anova_tbl <- results$anova_table
  if (!is.null(anova_tbl)) {
    if (!"p.label" %in% names(anova_tbl)) {
      p_vals <- suppressWarnings(as.numeric(anova_tbl$p.value))
      if (length(p_vals) == 0) p_vals <- rep(NA_real_, nrow(anova_tbl))
      anova_tbl$p.label <- ifelse(
        is.na(p_vals),
        "",
        ifelse(p_vals < 0.0001, "<0.0001", sprintf("%.4f", p_vals))
      )
    }
    if (!"Fvalue_label" %in% names(anova_tbl)) {
      f_vals <- suppressWarnings(as.numeric(anova_tbl$Fvalue))
      if (length(f_vals) == 0) f_vals <- rep(NA_real_, nrow(anova_tbl))
      anova_tbl$Fvalue_label <- ifelse(
        is.na(f_vals),
        "",
        sprintf("%.4f", f_vals)
      )
    }
    display_cols <- c("Effect", setdiff(names(anova_tbl), c("p.value", "p.label")), "Pr(>F)")
    anova_tbl$`Pr(>F)` <- anova_tbl$p.label
    if ("Fvalue_label" %in% names(anova_tbl)) {
      anova_tbl$`F value` <- anova_tbl$Fvalue_label
    }
    print(anova_tbl[, intersect(display_cols, names(anova_tbl)), drop = FALSE], row.names = FALSE)
  } else {
    print(results$anova_object)
  }

  if (length(results$posthoc_details) == 0) {
    cat("\nNo post-hoc Dunnett comparisons were generated.\n")
  } else {
    for (factor_nm in names(results$posthoc_details)) {
      details <- results$posthoc_details[[factor_nm]]
      if (!is.null(details$error)) {
        cat(
          "\n",
          format_safe_error_message(
            paste("Post-hoc Dunnett comparisons for", factor_nm, "failed"),
            details$error
          ),
          "\n",
          sep = ""
        )
      } else if (!is.null(details$table)) {
        cat("\nPost-hoc Dunnett comparisons for", factor_nm, ":\n")
        print(format_posthoc_table_for_print(details$table))
      }
    }
  }
  invisible(results)
}

format_posthoc_table_for_print <- function(df) {
  if (is.null(df) || !is.data.frame(df)) return(df)

  df <- as.data.frame(df)
  class(df) <- "data.frame"

  p_cols <- intersect(c("p.value", "p.value."), names(df))
  for (col in names(df)) {
    if (!is.numeric(df[[col]])) next
    if (length(p_cols) > 0 && col == p_cols[1]) {
      p_vals <- df[[col]]
      df[[col]] <- ifelse(
        is.na(p_vals),
        "",
        ifelse(p_vals < 0.0001, "<0.0001", sprintf("%.4f", p_vals))
      )
    } else {
      df[[col]] <- sprintf("%.4f", df[[col]])
    }
  }

  df
}

