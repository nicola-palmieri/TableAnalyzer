# ===============================================================
# 🧬 Common module for LM and LMM
# ===============================================================

reg_diagnostic_explanation <- paste(
  "The Residuals vs Fitted plot shows how far the model's predictions are from the observed values.",
  "A healthy model has points that bounce randomly around the dashed zero line; clear curves or a funnel shape mean the model is missing structure or the error size changes across fitted values.",
  "The Normal Q-Q plot checks whether the residuals follow a roughly normal distribution.",
  "Points that stay close to the dashed line support the normality assumption, while steady bends or extreme outliers hint at skewed or heavy-tailed errors.",
  "If you spot strong patterns in either plot, consider transforming variables, adding predictors, or trying a different model to improve the fit."
)

fit_all_models <- function(df, responses, rhs, strat_details, engine, allow_multi_response) {
  safe_fit <- purrr::safely(reg_fit_model)

  make_entry <- function(label = NULL, display = label, model = NULL, error = NULL) {
    list(label = label, display = display, model = model, error = error)
  }

  record_success <- function(resp, model_obj, model_key, stratum_label = NULL) {
    success_resps <<- c(success_resps, resp)
    current <- success_models[[resp]]
    if (is.null(current)) current <- list()
    current[[model_key]] <- model_obj
    success_models[[resp]] <<- current

    flat_models[[length(flat_models) + 1]] <<- list(
      response = resp,
      stratum = stratum_label,
      model = model_obj
    )
    if (is.null(primary_model)) primary_model <<- model_obj
  }

  record_error <- function(resp, message) {
    error_resps <<- c(error_resps, resp)
    error_messages[[resp]] <<- message
    if (is.null(primary_error)) primary_error <<- message
  }

  fits <- list()
  success_resps <- character(0)
  error_resps <- character(0)
  success_models <- list()
  error_messages <- list()
  flat_models <- list()
  primary_model <- NULL
  primary_error <- NULL

  for (resp in responses) {
    if (is.null(strat_details$var)) {
      result <- safe_fit(resp, rhs, df, engine = engine)
      strata_entry <- make_entry(display = "Overall")

      if (is.null(result$error)) {
        strata_entry$model <- result$result
        record_success(resp, result$result, "Overall")
      } else {
        strata_entry$error <- result$error$message
        record_error(resp, strata_entry$error)
      }

      fits[[resp]] <- list(stratified = FALSE, strata = list(strata_entry))
    } else {
      strata_entries <- list()
      successful_strata <- list()

      for (level in strat_details$levels) {
        entry <- make_entry(label = level, display = level)
        subset_data <- df[df[[strat_details$var]] == level, , drop = FALSE]

        if (nrow(subset_data) == 0) {
          entry$error <- paste0("No observations available for stratum '", level, "'.")
        } else {
          result <- safe_fit(resp, rhs, subset_data, engine = engine)
          if (is.null(result$error)) {
            entry$model <- result$result
            successful_strata[[level]] <- result$result
            record_success(resp, result$result, level, stratum_label = level)
          } else {
            entry$error <- result$error$message
          }
        }

        strata_entries[[length(strata_entries) + 1]] <- entry
      }

      fits[[resp]] <- list(stratified = TRUE, strata = strata_entries)

      if (length(successful_strata) > 0) {
        success_models[[resp]] <- successful_strata
      } else {
        errors_vec <- vapply(
          strata_entries,
          function(entry) {
            if (!is.null(entry$error) && nzchar(entry$error)) {
              paste0(entry$display, ": ", entry$error)
            } else {
              NA_character_
            }
          },
          character(1)
        )
        errors_vec <- errors_vec[!is.na(errors_vec)]
        combined_error <- if (length(errors_vec) > 0) {
          paste(errors_vec, collapse = "\n")
        } else {
          "Model fitting failed."
        }
        record_error(resp, combined_error)
      }
    }
  }

  list(
    responses = responses,
    success_responses = unique(success_resps),
    error_responses = unique(error_resps),
    fits = fits,
    models = success_models,
    flat_models = flat_models,
    model = primary_model,
    errors = error_messages,
    error = primary_error,
    rhs = rhs,
    allow_multi = allow_multi_response,
    stratification = strat_details
  )
}

render_model_summary <- function(engine, model_obj) {
  if (engine == "lm") {
    reg_display_lm_summary(model_obj)
  } else {
    reg_display_lmm_summary(model_obj)
  }
}

render_residual_plot <- function(model_obj) {
  plot_df <- data.frame(
    fitted = stats::fitted(model_obj),
    residuals = stats::residuals(model_obj)
  )

  ggplot2::ggplot(plot_df, ggplot2::aes(x = fitted, y = residuals)) +
    ggplot2::geom_point(color = "steelblue", alpha = 0.8) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed") +
    ggplot2::labs(
      title = "Residuals vs Fitted",
      x = "Fitted values",
      y = "Residuals"
    ) +
    ta_plot_theme(base_size = 13) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(color = "#9ca3af"),
      axis.ticks = ggplot2::element_line(color = "#9ca3af")
    )
}

render_qq_plot <- function(model_obj) {
  resid_vals <- stats::residuals(model_obj)
  qq_base <- stats::qqnorm(resid_vals, plot.it = FALSE)
  
  qq_df <- data.frame(
    theoretical = qq_base$x,
    sample = qq_base$y
  )
  
  resid_mean <- mean(resid_vals)
  resid_sd <- sd(resid_vals)
  
  ggplot2::ggplot(qq_df, ggplot2::aes(x = theoretical, y = sample)) +
    ggplot2::geom_point(color = "steelblue", alpha = 0.8) +
    ggplot2::geom_abline(
      slope = resid_sd,
      intercept = resid_mean,
      linetype = "dashed"
    ) +
    ggplot2::labs(
      title = "Normal Q-Q",
      x = "Theoretical quantiles",
      y = "Sample quantiles"
    ) +
    ta_plot_theme(base_size = 13) +
    ggplot2::theme(
      panel.grid.major = ggplot2::element_blank(),
      panel.grid.minor = ggplot2::element_blank(),
      axis.line = ggplot2::element_line(color = "#9ca3af"),
      axis.ticks = ggplot2::element_line(color = "#9ca3af")
    )
}


assign_download_handler <- function(output, id, engine, response, stratum_display, model_obj) {
  output[[id]] <- downloadHandler(
    filename = function() {
      parts <- c(engine, "results", response)
      if (!is.null(stratum_display)) parts <- c(parts, stratum_display)
      paste0(paste(parts, collapse = "_"), "_", Sys.Date(), ".docx")
    },
    content = function(file) {
      write_lm_docx(model_obj, file)
    }
  )
}

assign_model_outputs <- function(output, engine, response, idx, model_obj, stratum_idx = NULL, stratum_display = NULL) {
  suffix <- if (is.null(stratum_idx)) idx else paste(idx, stratum_idx, sep = "_")
  summary_id <- paste0("summary_", suffix)
  resid_id <- paste0("resid_", suffix)
  qq_id <- paste0("qq_", suffix)
  download_id <- paste0("download_", suffix)

  output[[summary_id]] <- renderPrint({
    render_model_summary(engine, model_obj)
  })

  output[[resid_id]] <- renderPlot({
    render_residual_plot(model_obj)
  })

  output[[qq_id]] <- renderPlot({
    render_qq_plot(model_obj)
  })

  assign_download_handler(output, download_id, engine, response, stratum_display, model_obj)
}

build_diagnostic_block <- function(ns, suffix, tooltip_text) {
  tagList(
    verbatimTextOutput(ns(paste0("summary_", suffix))),
    br(),
    h5("Diagnostics"),
    br(),
    fluidRow(
      column(6, plotOutput(ns(paste0("resid_", suffix)))),
      column(6, plotOutput(ns(paste0("qq_", suffix))))
    ),
    br(),
    helpText(reg_diagnostic_explanation),
    br(),
    br(),
    with_help_tooltip(
      downloadButton(ns(paste0("download_", suffix)), "Download results", style = "width: 100%;"),
      tooltip_text
    )
  )
}

build_response_content <- function(ns, idx, fit_entry) {
  if (!isTRUE(fit_entry$stratified)) {
    return(build_diagnostic_block(ns, idx, "Save the model summary and diagnostics for this response."))
  }

  strata <- fit_entry$strata
  stratum_tabs <- lapply(seq_along(strata), function(j) {
    stratum <- strata[[j]]
    label <- if (!is.null(stratum$display)) stratum$display else paste("Stratum", j)

    content <- if (!is.null(stratum$model)) {
      build_diagnostic_block(ns, paste(idx, j, sep = "_"), "Save the model summary and diagnostics for this stratum.")
    } else {
      tags$pre(format_safe_error_message("Model fitting failed", stratum$error))
    }

    tabPanel(title = label, content)
  })

  do.call(
    tabsetPanel,
    c(list(id = ns(paste0("strata_tabs_", idx))), stratum_tabs)
  )
}

build_model_ui <- function(ns, models_info) {
  success_resps <- models_info$success_responses
  error_resps <- models_info$error_responses
  fits <- models_info$fits

  error_block <- NULL
  if (!is.null(error_resps) && length(error_resps) > 0) {
    error_block <- lapply(error_resps, function(resp) {
      err <- models_info$errors[[resp]]
      tags$pre(
        format_safe_error_message(
          paste("Model fitting failed for", resp),
          if (!is.null(err)) err else ""
        )
      )
    })
  }

  if (is.null(success_resps) || length(success_resps) == 0) {
    if (!is.null(error_block)) return(do.call(tagList, error_block))
    return(NULL)
  }

  panels <- lapply(seq_along(success_resps), function(idx) {
    response <- success_resps[idx]
    fit_entry <- fits[[response]]
    content <- build_response_content(ns, idx, fit_entry)

    if (length(success_resps) > 1) {
      tabPanel(title = response, content)
    } else {
      content
    }
  })

  results_block <- if (length(success_resps) > 1) {
    do.call(tabsetPanel, c(list(id = ns("results_tabs")), panels))
  } else {
    panels[[1]]
  }

  elements <- c(if (!is.null(error_block)) error_block, list(results_block))
  do.call(tagList, elements)
}

render_model_outputs <- function(output, models_info, engine) {
  success_resps <- models_info$success_responses
  fits <- models_info$fits

  if (is.null(success_resps) || length(success_resps) == 0) return()

  for (idx in seq_along(success_resps)) {
    response <- success_resps[idx]
    fit_entry <- fits[[response]]
    strata <- fit_entry$strata

    for (j in seq_along(strata)) {
      stratum <- strata[[j]]
      if (is.null(stratum$model)) next
      is_stratified <- isTRUE(fit_entry$stratified)
      assign_model_outputs(
        output,
        engine,
        response,
        idx,
        stratum$model,
        stratum_idx = if (is_stratified) j else NULL,
        stratum_display = if (is_stratified) stratum$display else NULL
      )
      if (!is_stratified) break
    }
  }
}

regression_ui <- function(id, engine = c("lm", "lmm"), allow_multi_response = FALSE) {
  ns <- NS(id)
  engine <- match.arg(engine)
  allow_multi_response <- isTRUE(allow_multi_response)

  list(
    config = tagList(
      if (allow_multi_response) multi_response_ui(ns("response")) else uiOutput(ns("response_ui")),
      uiOutput(ns("fixed_selector")),
      uiOutput(ns("level_order")),
      uiOutput(ns("covar_selector")),
      if (engine == "lmm") uiOutput(ns("random_selector")),
      uiOutput(ns("interaction_select")),
      uiOutput(ns("formula_preview")),
      br(),
      tags$details(
        tags$summary(strong("Advanced options")),
        stratification_ui("strat", ns)
      ),
      br(),
      fluidRow(
        column(6, with_help_tooltip(
          actionButton(ns("run"), "Show results", width = "100%"),
          "Fit the model using the chosen predictors and options."
        )),
        column(6, with_help_tooltip(
          downloadButton(ns("download_model"), "Download all results", style = "width: 100%;"),
          "Export the model outputs, tables, and summaries to your computer."
        ))
      )
    ),
    results = tagList(
      uiOutput(ns("results_ui"))
    )
  )
}

regression_server <- function(id, data, engine = c("lm", "lmm"), allow_multi_response = FALSE) {
  engine <- match.arg(engine)
  allow_multi_response <- isTRUE(allow_multi_response)

  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    strat_info <- stratification_server("strat", data)

    if (allow_multi_response) {
      selected_responses <- multi_response_server("response", data)
    } else {
      output$response_ui <- renderUI({
        req(data())
        types <- reg_detect_types(data())
        with_help_tooltip(
          selectInput(ns("dep"), "Response variable (numeric)", choices = types$num),
          "Choose the outcome that the model should predict."
        )
      })

      selected_responses <- reactive({
        req(input$dep)
        input$dep
      })
    }

    output$fixed_selector <- renderUI({
      req(data())
      types <- reg_detect_types(data())
      with_help_tooltip(
        selectInput(
          ns("fixed"),
          "Categorical predictors",
          choices = types$fac,
          multiple = TRUE
        ),
        "Pick factor variables that might explain differences in the response."
      )
    })

    output$level_order <- renderUI({
      req(data())
      req(input$fixed)

      df <- data()
      fac_vars <- input$fixed

      if (length(fac_vars) == 0) return(NULL)

      tagList(
        lapply(fac_vars, function(var) {
          values <- df[[var]]
          lvls <- resolve_order_levels(values)
          with_help_tooltip(
            selectInput(
              ns(paste0("order_", var)),
              paste("Order of levels (first = reference)", var),
              choices = lvls,
              selected = lvls,
              multiple = TRUE
            ),
            sprintf("Arrange the levels of %s; the first level becomes the model reference.", var)
          )
        })
      )
    })

    output$covar_selector <- renderUI({
      req(data())
      types <- reg_detect_types(data())
      with_help_tooltip(
        selectInput(
          ns("covar"),
          "Numeric predictors",
          choices = types$num,
          multiple = TRUE
        ),
        "Pick numeric predictors that could help explain the response."
      )
    })

    if (engine == "lmm") {
      output$random_selector <- renderUI({
        req(data())
        types <- reg_detect_types(data())
        with_help_tooltip(
          selectInput(
            ns("random"),
            "Random effect (categorical)",
            choices = types$fac,
            selected = NULL
          ),
          "Choose a grouping factor for random intercepts in the mixed model."
        )
      })
    }

    output$interaction_select <- renderUI({
      req(data())
      types <- reg_detect_types(data())
      reg_interactions_ui(ns, input$fixed, types$fac)
    })

    output$formula_preview <- renderUI({
      responses <- selected_responses()
      req(length(responses) > 0)
      rhs <- reg_compose_rhs(
        input$fixed,
        input$covar,
        input$interactions,
        if (engine == "lmm") input$random else NULL,
        engine = engine
      )
      reg_formula_preview_ui(ns, responses[1], rhs)
    })

    models <- eventReactive(input$run, {
      req(data())
      df <- data()
      responses <- selected_responses()
      req(length(responses) > 0)
      validate_numeric_columns(df, responses, "response variable(s)")

      rhs <- reg_compose_rhs(
        input$fixed,
        input$covar,
        input$interactions,
        if (engine == "lmm") input$random else NULL,
        engine = engine
      )

      strat_details <- strat_info()
      fit_all_models(df, responses, rhs, strat_details, engine, allow_multi_response)
    })

    output$results_ui <- renderUI({
      mod <- models()
      req(mod)
      build_model_ui(ns, mod)
    })

    observeEvent(models(), {
      mod <- models()
      req(mod)
      render_model_outputs(output, mod, engine)
    }, ignoreNULL = FALSE)

    output$download_model <- downloadHandler(
      filename = function() {
        mod <- models()
        if (is.null(mod) || length(mod$flat_models) == 0) {
          return(paste0(engine, "_results_", Sys.Date(), ".docx"))
        }

        if (length(mod$flat_models) == 1) {
          entry <- mod$flat_models[[1]]
          parts <- c(engine, "results", entry$response)
          if (!is.null(entry$stratum)) parts <- c(parts, entry$stratum)
          paste0(paste(parts, collapse = "_"), "_", Sys.Date(), ".docx")
        } else {
          paste0(engine, "_all_results_", Sys.Date(), ".docx")
        }
      },
      content = function(file) {
        mod <- models()
        if (is.null(mod)) stop("No models available. Please run the analysis first.")
        flat_models <- mod$flat_models
        if (length(flat_models) == 0) stop("No models available. Please run the analysis first.")

        if (length(flat_models) == 1) {
          write_lm_docx(flat_models[[1]]$model, file)
        } else {
          doc <- officer::read_docx()
          for (entry in flat_models) {
            tmp <- tempfile(fileext = ".docx")
            sublab <- if (!is.null(entry$stratum)) paste("Stratum:", entry$stratum) else NULL
            
            # pass subtitle so it appears RIGHT under the title
            write_lm_docx(entry$model, tmp, subtitle = sublab)
            
            doc <- officer::body_add_docx(doc, src = tmp)
            doc <- officer::body_add_par(doc, "", style = "Normal")
          }
          print(doc, target = file)
        }
      }
    )

    df_final <- reactive({
      data()
    })

    model_fit <- reactive({
      mod <- models()
      req(mod)
      mod$models
    })

    compiled_results <- reactive({
      mod <- models()
      req(mod)
      compile_regression_results(mod, engine)
    })

    summary_table <- reactive({
      res <- compiled_results()
      req(res)
      res$summary
    })

    effect_table <- reactive({
      res <- compiled_results()
      req(res)
      res$effects
    })

    error_table <- reactive({
      res <- compiled_results()
      req(res)
      res$errors
    })

    reactive({
      mod <- models()
      req(mod)

      data_used <- df_final()

      list(
        analysis_type = if (engine == "lm") "LM" else "LMM",
        type = if (engine == "lm") "lm" else "lmm",
        data_used = data_used,
        model = model_fit(),
        summary = summary_table(),
        posthoc = NULL,
        effects = effect_table(),
        stats = if (!is.null(data_used)) list(n = nrow(data_used), vars = names(data_used)) else NULL,
        responses = mod$responses,
        success_responses = mod$success_responses,
        error_responses = mod$error_responses,
        errors = mod$errors,
        stratification = mod$stratification,
        rhs = mod$rhs,
        allow_multi = mod$allow_multi,
        compiled_errors = error_table(),
        flat_models = mod$flat_models,
        engine = engine
      )
    })
  })
}
