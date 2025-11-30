# ===============================================================
# 🔧 Shared helpers for LM/LMM (UI + server utilities)
# ===============================================================

# ---------------------------------------------------------------
# UI setup
# ---------------------------------------------------------------

compact_chr <- function(x) {
  if (is.null(x) || length(x) == 0) return(character(0))
  x
}

# --------------------------------------------------------------
# Combined LM/LMM docx writer (aligned with ANOVA output layout)
# --------------------------------------------------------------
write_lm_docx_combined <- function(flat_models, file) {
  if (length(flat_models) == 0) stop("No models provided for export.")

  add_blank_line <- function(doc, text = "", style = "Normal") {
    body_add_par(doc, text, style = style)
  }

  format_p <- function(df, p_col) {
    if (is.null(p_col) || !p_col %in% names(df)) return(df)
    p_vals <- as.numeric(df[[p_col]])
    df[[p_col]] <- ifelse(p_vals < 0.0001, "<0.0001", sprintf("%.4f", p_vals))
    df
  }

  format_table <- function(df, p_col = NULL, bold_p = TRUE, header_labels = NULL, merge_cols = NULL) {
    ft <- flextable(df)
    if (!is.null(header_labels)) {
      ft <- set_header_labels(ft, values = header_labels)
    }

    ft <- fontsize(ft, part = "all", size = 10)
    ft <- bold(ft, part = "header", bold = TRUE)
    ft <- color(ft, part = "header", color = "black")

    numeric_cols <- names(df)[sapply(df, is.numeric)]
    text_cols <- setdiff(ft$col_keys, numeric_cols)
    if (length(text_cols) > 0) ft <- align(ft, j = text_cols, align = "left", part = "all")
    if (length(numeric_cols) > 0) {
      ft <- align(ft, j = numeric_cols, align = "right", part = "all")
      ft <- colformat_num(ft, j = numeric_cols, digits = 4)
    }

    ft <- border_remove(ft)
    black <- fp_border(color = "black", width = 1)
    ft <- border(ft, part = "header", border.top = black, border.bottom = black)

    if (!is.null(merge_cols)) {
      ft <- merge_v(ft, j = intersect(merge_cols, ft$col_keys))
    }

    if ("Stratum" %in% names(df)) {
      group_cols <- c(if ("Response" %in% names(df)) "Response", "Stratum")
      strata_factor <- interaction(df[, group_cols, drop = FALSE], drop = TRUE)
      change_rows <- which(diff(as.numeric(strata_factor)) != 0)
      if (length(change_rows) > 0) {
        ft <- border(ft, i = change_rows, part = "body", border.bottom = fp_border(color = "black", width = 0.5))
      }
    } else if ("Response" %in% names(df)) {
      change_rows <- which(diff(as.numeric(factor(df$Response))) != 0)
      if (length(change_rows) > 0) {
        ft <- border(ft, i = change_rows, part = "body", border.bottom = fp_border(color = "black", width = 0.5))
      }
    }

    if (nrow(df) > 0) {
      ft <- border(ft, i = nrow(df), part = "body", border.bottom = black)
    }

    if (bold_p && !is.null(p_col) && p_col %in% ft$col_keys) {
      sig_rows <- suppressWarnings(which(as.numeric(df[[p_col]]) < 0.05))
      lt_rows <- grep("^<", df[[p_col]])
      sig_rows <- unique(c(sig_rows, lt_rows))
      if (length(sig_rows) > 0) {
        ft <- bold(ft, i = sig_rows, j = p_col, bold = TRUE)
      }
    }

    ft <- set_table_properties(ft, layout = "autofit", width = 0.9)
    ft <- autofit(ft)
    ft <- padding(ft, padding.top = 2, padding.bottom = 2, padding.left = 2, padding.right = 2)
    ft
  }

  anova_entries <- list()
  coef_entries <- list()
  rand_entries <- list()
  icc_entries <- list()

  for (entry in flat_models) {
    model <- entry$model
    resp <- entry$response %||% all.vars(formula(model))[1]
    strat <- entry$stratum %||% "None"
    is_lmm <- inherits(model, "merMod")

    anova_tbl <- if (is_lmm) as.data.frame(anova(model, type = 3)) else as.data.frame(car::Anova(model, type = 3))
    anova_tbl <- tibble::rownames_to_column(anova_tbl, "Effect")
    for (col in names(anova_tbl)) {
      if (is.numeric(anova_tbl[[col]])) anova_tbl[[col]] <- round(anova_tbl[[col]], 4)
    }
    p_col <- grep("^Pr", names(anova_tbl), value = TRUE)
    if (length(p_col) > 0) {
      names(anova_tbl)[names(anova_tbl) == p_col[1]] <- "Pr(>F)"
      p_col <- "Pr(>F)"
      anova_tbl <- format_p(anova_tbl, p_col)
    } else {
      p_col <- NULL
    }
    anova_tbl$Response <- resp
    anova_tbl$Stratum <- strat
    anova_entries[[length(anova_entries) + 1]] <- anova_tbl

    coef_tbl <- as.data.frame(summary(model)$coefficients)
    coef_tbl <- tibble::rownames_to_column(coef_tbl, "Term")
    names(coef_tbl)[1] <- "Term"
    names(coef_tbl) <- gsub("Pr\\(>\\|t\\|\\)", "Pr(>|t|)", names(coef_tbl))
    for (col in names(coef_tbl)) {
      if (is.numeric(coef_tbl[[col]])) coef_tbl[[col]] <- round(coef_tbl[[col]], 4)
    }
    p_col_coef <- grep("^Pr", names(coef_tbl), value = TRUE)
    if (length(p_col_coef) > 0) {
      p_col_coef <- p_col_coef[1]
      coef_tbl <- format_p(coef_tbl, p_col_coef)
    } else {
      p_col_coef <- NULL
    }
    coef_tbl$Response <- resp
    coef_tbl$Stratum <- strat
    coef_entries[[length(coef_entries) + 1]] <- coef_tbl

    if (is_lmm) {
      rand_df <- tryCatch(as.data.frame(lme4::VarCorr(model)), error = function(e) NULL)
      if (!is.null(rand_df) && nrow(rand_df) > 0) {
        rand_df <- data.frame(
          Response = resp,
          Stratum = strat,
          Grouping = rand_df$grp,
          Name = rand_df$var1,
          Variance = round(rand_df$vcov, 4),
          StdDev = round(rand_df$sdcor, 4),
          stringsAsFactors = FALSE
        )
        rand_entries[[length(rand_entries) + 1]] <- rand_df
      }

      icc_df <- if (exists("compute_icc") && is.function(compute_icc)) compute_icc(model) else NULL
      if (!is.null(icc_df) && nrow(icc_df) > 0) {
        icc_df <- data.frame(
          Response = resp,
          Stratum = strat,
          Group = icc_df$Group,
          ICC = as.numeric(icc_df$ICC),
          stringsAsFactors = FALSE
        )
        icc_entries[[length(icc_entries) + 1]] <- icc_df
      }
    }
  }

  combined_anova <- dplyr::bind_rows(anova_entries)
  combined_coef <- dplyr::bind_rows(coef_entries)
  combined_rand <- if (length(rand_entries) > 0) dplyr::bind_rows(rand_entries) else NULL
  combined_icc <- if (length(icc_entries) > 0) dplyr::bind_rows(icc_entries) else NULL

  show_strata_anova <- !(length(unique(combined_anova$Stratum)) == 1 && unique(combined_anova$Stratum) == "None")
  show_strata_coef <- !(length(unique(combined_coef$Stratum)) == 1 && unique(combined_coef$Stratum) == "None")
  show_strata_rand <- if (!is.null(combined_rand)) !(length(unique(combined_rand$Stratum)) == 1 && unique(combined_rand$Stratum) == "None") else FALSE
  show_strata_icc <- if (!is.null(combined_icc)) !(length(unique(combined_icc$Stratum)) == 1 && unique(combined_icc$Stratum) == "None") else FALSE

  anova_p_col <- intersect(c("Pr(>F)", "p.value"), names(combined_anova))
  anova_p_col <- if (length(anova_p_col) > 0) anova_p_col[1] else NULL
  coef_p_col <- intersect(c("Pr(>|t|)", "Pr(>|z|)", "p.value"), names(combined_coef))
  coef_p_col <- if (length(coef_p_col) > 0) coef_p_col[1] else NULL

  anova_cols <- c("Response", if (show_strata_anova) "Stratum", "Effect", setdiff(names(combined_anova), c("Response", "Stratum", "Effect")))
  combined_anova <- combined_anova[, anova_cols, drop = FALSE]

  coef_cols <- c("Response", if (show_strata_coef) "Stratum", "Term", setdiff(names(combined_coef), c("Response", "Stratum", "Term")))
  combined_coef <- combined_coef[, coef_cols, drop = FALSE]

  if (!is.null(combined_rand)) {
    rand_cols <- c("Response", if (show_strata_rand) "Stratum", "Grouping", "Name", "Variance", "StdDev")
    combined_rand <- combined_rand[, rand_cols, drop = FALSE]
  }

  if (!is.null(combined_icc)) {
    combined_icc$ICC <- round(combined_icc$ICC, 4)
    icc_cols <- c("Response", if (show_strata_icc) "Stratum", "Group", "ICC")
    combined_icc <- combined_icc[, icc_cols, drop = FALSE]
  }

  header_anova <- setNames(
    c("Response", if (show_strata_anova) "Stratum", "Effect", gsub("\\.", " ", setdiff(names(combined_anova), c("Response", "Stratum", "Effect")))),
    names(combined_anova)
  )
  if (!is.null(anova_p_col)) header_anova[[anova_p_col]] <- "p-value"

  header_coef <- setNames(
    c("Response", if (show_strata_coef) "Stratum", "Term", gsub("\\.", " ", setdiff(names(combined_coef), c("Response", "Stratum", "Term")))),
    names(combined_coef)
  )
  if (!is.null(coef_p_col)) header_coef[[coef_p_col]] <- "p-value"

  merge_cols_anova <- c("Response", if (show_strata_anova) "Stratum")
  merge_cols_coef <- c("Response", if (show_strata_coef) "Stratum")
  merge_cols_rand <- if (!is.null(combined_rand)) c("Response", if (show_strata_rand) "Stratum") else NULL
  merge_cols_icc <- if (!is.null(combined_icc)) c("Response", if (show_strata_icc) "Stratum") else NULL

  header_rand <- if (!is.null(combined_rand)) setNames(
    c("Response", if (show_strata_rand) "Stratum", "Grouping", "Name", "Variance", "Std. Dev."),
    names(combined_rand)
  ) else NULL

  header_icc <- if (!is.null(combined_icc)) setNames(
    c("Response", if (show_strata_icc) "Stratum", "Group", "ICC"),
    names(combined_icc)
  ) else NULL

  doc <- read_docx()
  doc_title <- if (any(vapply(flat_models, function(x) inherits(x$model, "merMod"), logical(1)))) {
    "Linear Mixed Model Results"
  } else {
    "Linear Model Results"
  }
  doc <- body_add_fpar(doc, fpar(ftext(doc_title, prop = fp_text(bold = TRUE, font.size = 12))))
  doc <- add_blank_line(doc)

  doc <- body_add_fpar(doc, fpar(ftext("ANOVA (Type III)", prop = fp_text(bold = TRUE))))
  doc <- add_blank_line(doc)
  doc <- body_add_flextable(
    doc,
    format_table(
      combined_anova,
      p_col = anova_p_col,
      header_labels = header_anova,
      merge_cols = merge_cols_anova
    )
  )
  doc <- add_blank_line(doc)

  if (!is.null(combined_rand) && nrow(combined_rand) > 0) {
    doc <- body_add_fpar(doc, fpar(ftext("Random Effects", prop = fp_text(bold = TRUE))))
    doc <- add_blank_line(doc)
    doc <- body_add_flextable(
      doc,
      format_table(
        combined_rand,
        p_col = NULL,
        bold_p = FALSE,
        header_labels = header_rand,
        merge_cols = merge_cols_rand
      )
    )
    doc <- add_blank_line(doc)
  }

  if (!is.null(combined_icc) && nrow(combined_icc) > 0) {
    doc <- body_add_fpar(doc, fpar(ftext("Intraclass Correlation (ICC)", prop = fp_text(bold = TRUE))))
    doc <- add_blank_line(doc)
    doc <- body_add_flextable(
      doc,
      format_table(
        combined_icc,
        p_col = NULL,
        bold_p = FALSE,
        header_labels = header_icc,
        merge_cols = merge_cols_icc
      )
    )
    doc <- add_blank_line(doc)
  }

  doc <- body_add_fpar(doc, fpar(ftext("Model Coefficients", prop = fp_text(bold = TRUE))))
  doc <- add_blank_line(doc)
  doc <- body_add_flextable(
    doc,
    format_table(
      combined_coef,
      p_col = coef_p_col,
      header_labels = header_coef,
      merge_cols = merge_cols_coef
    )
  )

  doc <- add_blank_line(doc)
  doc <- add_blank_line(doc, "Significance level: p < 0.05 (bold values).")

  print(doc, target = file)
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

trim_output_section <- function(lines, start_pattern, end_pattern = NULL, end_offset = 0) {
  start <- grep(start_pattern, lines)[1]
  if (is.na(start)) return(lines)
  end <- if (is.null(end_pattern)) NA_integer_ else grep(end_pattern, lines)[1]
  if (!is.na(end)) {
    end <- max(start, end - end_offset)
    lines[start:end]
  } else {
    lines[start:length(lines)]
  }
}

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

reg_formula_preview_ui <- function(ns, dep, rhs) {
  if (is.null(dep) || !nzchar(dep)) return(NULL)
  form_txt <- reg_formula_text(dep, rhs)
  wellPanel(
    strong("Model formula: "),
    code(form_txt)
  )
}

# ---------------------------------------------------------------
# Model computation
# ---------------------------------------------------------------

reg_fit_model <- function(dep, rhs, data, engine = c("lm","lmm")) {
  engine <- match.arg(engine)
  form <- as.formula(reg_formula_text(dep, rhs))
  if (engine == "lm") {
    lm(form, data = data)
  } else {
    # LMM: lme4 + lmerTest for p-values
    lmerTest::lmer(form, data = data)
  }
}

# ---------------------------------------------------------------
# Output composition
# ---------------------------------------------------------------

reg_display_summary <- function(model, engine = c("lm", "lmm")) {
  engine <- match.arg(engine)

  format_coefs <- function(coef_mat) {
    if (is.null(coef_mat)) return(NULL)
    df <- data.frame(
      Term = rownames(coef_mat),
      stringsAsFactors = FALSE
    )

    num_cols <- c("Estimate", "Std. Error", "t value", "z value", "Std. error", "Std.Error", "Std error")
    stat_col <- intersect(colnames(coef_mat), c("t value", "z value"))
    p_col <- intersect(colnames(coef_mat), grep("^Pr", colnames(coef_mat), value = TRUE))

    if ("Estimate" %in% colnames(coef_mat)) {
      df$Estimate <- formatC(coef_mat[, "Estimate"], format = "f", digits = 6, drop0trailing = FALSE)
    }
    if ("Std. Error" %in% colnames(coef_mat)) {
      df$Std.Error <- formatC(coef_mat[, "Std. Error"], format = "f", digits = 6, drop0trailing = FALSE)
    }
    if ("Std error" %in% colnames(coef_mat)) {
      df$Std.Error <- formatC(coef_mat[, "Std error"], format = "f", digits = 6, drop0trailing = FALSE)
    }
    if (length(stat_col) > 0) {
      df$Statistic <- formatC(coef_mat[, stat_col[1]], format = "f", digits = 6, drop0trailing = FALSE)
    }
    if (length(p_col) > 0) {
      df$p.value <- format.pval(coef_mat[, p_col[1]], digits = 3, eps = 1e-04)
    }
    capture.output(print(df, row.names = FALSE))
  }

  if (engine == "lm") {
    aout <- capture.output(car::Anova(model, type = 3))
    signif_idx <- grep("^Signif\\. codes", aout)
    if (length(signif_idx) > 0) {
      remove_idx <- c(signif_idx - 1, signif_idx)
      aout <- aout[-remove_idx]
    }
    cat(paste(aout, collapse = "\n"), "\n\n")

    coef_lines <- format_coefs(summary(model)$coefficients)
    cat("Coefficients:\n")
    cat(paste(coef_lines, collapse = "\n"), "\n")
  } else {
    aout <- capture.output(anova(model, type = 3))
    cat(paste(aout, collapse = "\n"), "\n\n")

    rand_var <- tryCatch(lme4::VarCorr(model), error = function(e) NULL)
    cat("Random effects:\n")
    if (!is.null(rand_var)) {
      print(rand_var, comp = c("Variance", "Std.Dev."))
    } else {
      cat("  None\n")
    }
    cat("\n")

    icc_df <- if (exists("compute_icc") && is.function(compute_icc)) compute_icc(model) else NULL
    if (!is.null(icc_df) && nrow(icc_df) > 0) {
      cat("Intraclass Correlation (ICC):\n")
      icc_df$ICC <- formatC(icc_df$ICC, format = "f", digits = 6, drop0trailing = FALSE)
      print(icc_df, row.names = FALSE)
      cat("\n")
    }

    coef_lines <- format_coefs(summary(model)$coefficients)
    cat("Coefficients:\n")
    cat(paste(coef_lines, collapse = "\n"), "\n")
  }
}

reg_display_lm_summary <- function(m) reg_display_summary(m, "lm")

reg_display_lmm_summary <- function(m) reg_display_summary(m, "lmm")

# ---------------------------------------------------------------
# Summaries for standardized regression outputs
# ---------------------------------------------------------------

clean_regression_coef_names <- function(nms) {
  lookup <- c(
    "estimate" = "estimate",
    "std. error" = "std_error",
    "std error" = "std_error",
    "t value" = "statistic",
    "z value" = "statistic",
    "f value" = "statistic",
    "df" = "df",
    "dendf" = "dendf",
    "numdf" = "numdf"
  )

  vapply(nms, function(name) {
    name_trim <- trimws(name)
    key <- tolower(name_trim)
    val <- if (!is.na(key) && key %in% names(lookup)) lookup[[key]] else NULL
    if (!is.null(val)) return(val)
    if (grepl("^Pr\\(>", name_trim)) return("p_value")
    cleaned <- tolower(name_trim)
    cleaned <- gsub("[^[:alnum:]]+", "_", cleaned)
    cleaned <- gsub("^_+|_+$", "", cleaned)
    cleaned <- gsub("_+", "_", cleaned)
    cleaned
  }, character(1), USE.NAMES = FALSE)
}

tidy_regression_model <- function(model, engine) {
  if (is.null(model)) {
    return(list(summary = NULL, effects = NULL))
  }

  coef_mat <- tryCatch(summary(model)$coefficients, error = function(e) NULL)
  coef_df <- NULL
  if (!is.null(coef_mat)) {
    coef_df <- data.frame(
      term = rownames(coef_mat),
      coef_mat,
      row.names = NULL,
      check.names = FALSE,
      stringsAsFactors = FALSE
    )
    original_names <- names(coef_df)
    names(coef_df) <- c("term", clean_regression_coef_names(original_names[-1]))
  }

  if (inherits(model, "lm")) {
    sm <- summary(model)
    metrics <- data.frame(
      metric = c("sigma", "r_squared", "adj_r_squared", "nobs"),
      value = c(sm$sigma, sm$r.squared, sm$adj.r.squared, stats::nobs(model)),
      stringsAsFactors = FALSE
    )
  } else {
    metrics <- data.frame(
      metric = c("sigma", "logLik", "AIC", "BIC", "nobs"),
      value = c(
        stats::sigma(model),
        as.numeric(stats::logLik(model)),
        stats::AIC(model),
        stats::BIC(model),
        stats::nobs(model)
      ),
      stringsAsFactors = FALSE
    )
  }

  anova_tbl <- tryCatch({
    if (engine == "lm") {
      car::Anova(model, type = 3)
    } else {
      anova(model, type = 3)
    }
  }, error = function(e) NULL)

  if (!is.null(anova_tbl)) {
    anova_tbl <- as.data.frame(anova_tbl)
    anova_tbl$Effect <- rownames(anova_tbl)
    rownames(anova_tbl) <- NULL
    anova_tbl <- anova_tbl[, c("Effect", setdiff(names(anova_tbl), "Effect"))]
  }

  effects <- list(metrics = metrics, anova = anova_tbl)
  if (all(vapply(effects, is.null, logical(1)))) effects <- NULL

  list(summary = coef_df, effects = effects)
}

compile_regression_results <- function(model_info, engine) {
  if (is.null(model_info) || is.null(model_info$fits)) return(NULL)

  summary_list <- list()
  effects_list <- list()
  errors_list <- list()

  for (resp in names(model_info$fits)) {
    fit_entry <- model_info$fits[[resp]]
    if (is.null(fit_entry)) next

    store_results <- function(label, stratum) {
      tidy <- tidy_regression_model(stratum$model, engine)
      if (is.null(summary_list[[resp]])) summary_list[[resp]] <<- list()
      if (is.null(effects_list[[resp]])) effects_list[[resp]] <<- list()
      summary_list[[resp]][[label]] <<- tidy$summary
      effects_list[[resp]][[label]] <<- tidy$effects
      if (!is.null(stratum$error)) {
        if (is.null(errors_list[[resp]])) errors_list[[resp]] <<- list()
        errors_list[[resp]][[label]] <<- stratum$error
      }
    }

    if (isTRUE(fit_entry$stratified)) {
      for (stratum in fit_entry$strata) {
        label <- if (!is.null(stratum$display)) stratum$display else stratum$label
        if (is.null(label) || !nzchar(label)) label <- "Stratum"
        store_results(label, stratum)
      }
    } else {
      stratum <- fit_entry$strata[[1]]
      tidy <- tidy_regression_model(stratum$model, engine)
      summary_list[[resp]] <- tidy$summary
      effects_list[[resp]] <- tidy$effects
      if (!is.null(stratum$error)) {
        errors_list[[resp]] <- stratum$error
      }
    }
  }

  list(summary = summary_list, effects = effects_list, errors = errors_list)
}

# ===============================================================
# Results export
# ===============================================================

write_lm_docx <- function(model, file, subtitle = NULL, response_name = NULL, stratum_label = NULL) {

  # Determine model type
  is_lmm <- inherits(model, "merMod")
  dep_var <- all.vars(formula(model))[1]

  add_blank_line <- function(doc, text = "", style = "Normal") {
    body_add_par(doc, text, style = style)
  }

  # Helper: format p-values with labels and significance flag
  format_p <- function(df, p_col) {
    if (is.null(p_col) || !p_col %in% names(df)) return(df)
    p_vals <- as.numeric(df[[p_col]])
    df$sig <- p_vals < 0.05
    df[[p_col]] <- ifelse(p_vals < 0.0001, "<0.0001", sprintf("%.4f", p_vals))
    df
  }

  # Helper for consistent table formatting
  format_table <- function(df, p_col = NULL, bold_p = TRUE, header_labels = NULL) {
    ft <- flextable(df)
    if (!is.null(header_labels)) {
      ft <- set_header_labels(ft, values = header_labels)
    } else {
      ft <- set_header_labels(ft, values = setNames(names(df), names(df)))
    }

    ft <- fontsize(ft, part = "all", size = 10)
    ft <- bold(ft, part = "header", bold = TRUE)
    ft <- color(ft, part = "header", color = "black")

    numeric_cols <- names(df)[sapply(df, is.numeric)]
    if (length(setdiff(ft$col_keys, numeric_cols)) > 0) {
      ft <- align(ft, j = setdiff(ft$col_keys, numeric_cols), align = "left", part = "all")
    }
    if (length(numeric_cols) > 0) {
      ft <- align(ft, j = numeric_cols, align = "right", part = "all")
      ft <- colformat_num(ft, j = numeric_cols, digits = 4)
    }

    ft <- border_remove(ft)
    black <- fp_border(color = "black", width = 1)
    ft <- border(ft, part = "header", border.top = black, border.bottom = black)
    if (nrow(df) > 0) {
      ft <- border(ft, i = nrow(df), part = "body", border.bottom = black)
    }

    if (bold_p && !is.null(p_col) && "sig" %in% names(df) && p_col %in% ft$col_keys) {
      sig_rows <- which(df$sig %in% TRUE)
      if (length(sig_rows) > 0) {
        ft <- bold(ft, i = sig_rows, j = p_col, bold = TRUE)
      }
    }

    ft <- set_table_properties(ft, layout = "autofit", width = 0.9)
    ft <- autofit(ft)
    ft <- padding(ft, padding.top = 2, padding.bottom = 2, padding.left = 2, padding.right = 2)
    ft
  }

  # Create new Word document
  doc <- read_docx()

  # ---- Title ----
  title_parts <- c(
    if (is_lmm) "Linear Mixed Model" else "Linear Model",
    "Results"
  )
  if (!is.null(response_name) && nzchar(response_name)) {
    title_parts <- c(title_parts, paste("—", response_name))
  } else {
    title_parts <- c(title_parts, paste("—", dep_var))
  }
  title_text <- paste(title_parts, collapse = " ")
  doc <- body_add_fpar(
    doc,
    fpar(ftext(title_text, prop = fp_text(bold = TRUE, font.size = 12)))
  )

  # ---- Subtitle (Stratum, if any) ----
  sub_label <- subtitle %||% stratum_label
  if (!is.null(sub_label) && nzchar(sub_label)) {
    subtitle_text <- ftext(
      sub_label,
      prop = fp_text(bold = TRUE, font.size = 11)
    )
    doc <- body_add_fpar(doc, fpar(subtitle_text))
  }

  doc <- add_blank_line(doc)

  # ==========================================================
  # 🔹 ANOVA (Type III)
  # ==========================================================
  doc <- body_add_fpar(doc, fpar(ftext("ANOVA (Type III)", prop = fp_text(bold = TRUE))))
  doc <- add_blank_line(doc)

  if (is_lmm) {
    anova_tbl <- as.data.frame(anova(model, type = 3))
  } else {
    anova_tbl <- as.data.frame(car::Anova(model, type = 3))
  }
  anova_tbl <- tibble::rownames_to_column(anova_tbl, "Effect")

  # Round numeric columns and format p-values
  for (col in names(anova_tbl)) {
    if (is.numeric(anova_tbl[[col]])) anova_tbl[[col]] <- round(anova_tbl[[col]], 4)
  }
  p_col <- grep("^Pr", names(anova_tbl), value = TRUE)
  if (length(p_col) > 0) {
    colnames(anova_tbl)[colnames(anova_tbl) == p_col[1]] <- "Pr(>F)"
    p_col <- "Pr(>F)"
    anova_tbl <- format_p(anova_tbl, p_col)
  } else {
    p_col <- NULL
  }

  ft_anova <- format_table(
    anova_tbl,
    p_col = p_col,
    header_labels = c(
      Effect = "Effect",
      Sum.Sq = "Sum Sq",
      Mean.Sq = "Mean Sq",
      Df = "Df",
      F.value = "F value",
      `Pr(>F)` = "p-value"
    )
  )
  doc <- body_add_flextable(doc, ft_anova)
  doc <- add_blank_line(doc)

  # ==========================================================
  # 🔹 Random Effects & ICC (LMM only)
  # ==========================================================
  if (is_lmm) {
    # ---- Random Effects ----
    doc <- body_add_fpar(doc, fpar(ftext("Random Effects", prop = fp_text(bold = TRUE))))
    doc <- add_blank_line(doc)

    rand_df <- as.data.frame(lme4::VarCorr(model))
    if (nrow(rand_df) > 0) {
      rand_df <- rand_df[, c("grp", "var1", "vcov", "sdcor"), drop = FALSE]
      names(rand_df) <- c("Grouping", "Name", "Variance", "Std. Dev.")
      rand_df$Variance <- round(rand_df$Variance, 4)
      rand_df$`Std. Dev.` <- round(rand_df$`Std. Dev.`, 4)
      ft_rand <- format_table(rand_df, p_col = NULL, bold_p = FALSE)
      doc <- body_add_flextable(doc, ft_rand)
    } else {
      doc <- add_blank_line(doc, "No random-effect variance components were estimated.")
    }

    # ---- ICC ----
    if (exists("compute_icc") && is.function(compute_icc)) {
      icc_df <- compute_icc(model)
    } else {
      icc_df <- NULL
    }
    if (!is.null(icc_df) && nrow(icc_df) > 0) {
      doc <- add_blank_line(doc)
      doc <- body_add_fpar(doc, fpar(ftext("Intraclass Correlation (ICC)", prop = fp_text(bold = TRUE))))
      doc <- add_blank_line(doc)
      icc_df$ICC <- round(icc_df$ICC, 4)
      ft_icc <- format_table(icc_df, p_col = NULL, bold_p = FALSE)
      doc <- body_add_flextable(doc, ft_icc)
    }

    doc <- add_blank_line(doc)
  }

  # ==========================================================
  # 🔹 Model Coefficients
  # ==========================================================
  doc <- body_add_fpar(doc, fpar(ftext("Model Coefficients", prop = fp_text(bold = TRUE))))
  doc <- add_blank_line(doc)

  coef_tbl <- as.data.frame(summary(model)$coefficients)
  coef_tbl <- tibble::rownames_to_column(coef_tbl, "Term")
  names(coef_tbl)[1] <- "Term"
  names(coef_tbl) <- gsub("Pr\\(>\\|t\\|\\)", "Pr(>|t|)", names(coef_tbl))

  for (col in names(coef_tbl)) {
    if (is.numeric(coef_tbl[[col]])) coef_tbl[[col]] <- round(coef_tbl[[col]], 4)
  }
  p_col_coef <- grep("^Pr", names(coef_tbl), value = TRUE)
  if (length(p_col_coef) > 0) {
    p_col_coef <- p_col_coef[1]
    coef_tbl <- format_p(coef_tbl, p_col_coef)
  } else {
    p_col_coef <- NULL
  }

  ft_coef <- format_table(
    coef_tbl,
    p_col = p_col_coef,
    header_labels = c(
      Term = "Term",
      Estimate = "Estimate",
      `Std. Error` = "Std. Error",
      `t value` = "t value",
      `Pr(>|t|)` = "p-value"
    )
  )
  doc <- body_add_flextable(doc, ft_coef)

  # ==========================================================
  # 🔹 Footer
  # ==========================================================
  doc <- add_blank_line(doc)
  doc <- add_blank_line(doc, "Significance level: p < 0.05 (bold values).")

  # Save file
  print(doc, target = file)
}
