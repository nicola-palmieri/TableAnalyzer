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
        selectInput(ns("random"), "Random effect (categorical)", choices = types$fac, selected = NULL),
        "Choose a grouping factor for random intercepts when using mixed models."
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

reg_compose_rhs <- function(fixed, covar, interactions, random = NULL, engine = c("lm","lmm")) {
  engine <- match.arg(engine)
  fixed <- reg_protect_vars(compact_chr(fixed))
  covar <- reg_protect_vars(compact_chr(covar))
  interactions <- reg_protect_interactions(compact_chr(interactions))

  rhs <- c(fixed, covar, interactions)
  if (engine == "lmm" && !is.null(random) && nzchar(random)) {
    rand <- reg_protect_vars(random)
    rhs <- c(rhs, paste0("(1|", rand, ")"))
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

  if (engine == "lm") {
    aout <- capture.output(car::Anova(model, type = 3))
    signif_idx <- grep("^Signif\\. codes", aout)
    if (length(signif_idx) > 0) {
      remove_idx <- c(signif_idx - 1, signif_idx)
      aout <- aout[-remove_idx]
    }
    cat(paste(aout, collapse = "\n"), "\n\n")

    sout <- capture.output(summary(model))
    sout <- trim_output_section(sout, "^Residuals:", "^Signif\\. codes", 2)
    cat(paste(sout, collapse = "\n"))
  } else {
    aout <- capture.output(anova(model, type = 3))
    cat(paste(aout, collapse = "\n"), "\n\n")

    sout <- capture.output(summary(model))
    sout <- trim_output_section(sout, "^Scaled residuals:", "^Correlation of Fixed Effects:", 1)

    icc_df <- compute_icc(model)
    if (!is.null(icc_df) && nrow(icc_df) > 0) {
      icc_line <- paste(paste0("ICC (", icc_df$Group, "): ", icc_df$ICC), collapse = "; ")
      random_idx <- grep("^Random effects:", sout)[1]
      if (!is.na(random_idx)) sout <- append(sout, paste0("\n", icc_line), after = random_idx + 4)
      else sout <- c(sout, icc_line)
    }
    cat(paste(sout, collapse = "\n"))
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

write_lm_docx <- function(model, file, subtitle = NULL) {

  # Determine model type
  is_lmm <- inherits(model, "merMod")
  dep_var <- all.vars(formula(model))[1]

  add_blank_line <- function(doc, text = "", style = "Normal") {
    body_add_par(doc, text, style = style)
  }

  # Helper for consistent table formatting
  format_table <- function(df, bold_p = TRUE) {
    ft <- flextable(df)
    ft <- fontsize(ft, part = "all", size = 10)
    ft <- bold(ft, part = "header", bold = TRUE)
    ft <- color(ft, part = "header", color = "black")
    ft <- align(ft, align = "center", part = "all")
    ft <- border_remove(ft)
    black <- fp_border(color = "black", width = 1)
    ft <- border(ft, part = "header", border.top = black)
    ft <- border(ft, part = "header", border.bottom = black)
    if (nrow(df) > 0) {
      ft <- border(ft, i = nrow(df), part = "body", border.bottom = black)
    }

    # Bold significant p-values
    if (bold_p) {
      p_cols <- names(df)[grepl("Pr", names(df), fixed = TRUE)]
      for (pcol in p_cols) {
        if (is.numeric(df[[pcol]]) || all(grepl("^[0-9.<]+$", df[[pcol]]))) {
          sig_rows <- suppressWarnings(which(as.numeric(df[[pcol]]) < 0.05))
          if (length(sig_rows) == 0) {
            # handle formatted p-values like "<0.001"
            sig_rows <- grep("<0\\.0*1", df[[pcol]])
          }
          if (length(sig_rows) > 0 && pcol %in% ft$col_keys) {
            ft <- bold(ft, i = sig_rows, j = pcol, bold = TRUE)
          }
        }
      }
    }

    ft <- set_table_properties(ft, layout = "autofit", width = 0.9)
    ft <- padding(ft, padding.top = 2, padding.bottom = 2, padding.left = 2, padding.right = 2)
    ft
  }

  # Create new Word document
  doc <- read_docx()

  # ---- Title ----
  title_text <- sprintf(
    "%s Results — %s",
    if (is_lmm) "Linear Mixed Model" else "Linear Model",
    dep_var
  )
  doc <- body_add_fpar(
    doc,
    fpar(ftext(title_text, prop = fp_text(bold = TRUE, font.size = 12)))
  )

  # ---- Subtitle (Stratum, if any) ----
  if (!is.null(subtitle) && nzchar(subtitle)) {
    subtitle_text <- ftext(
      subtitle,
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
    if (is.numeric(anova_tbl[[col]])) anova_tbl[[col]] <- round(anova_tbl[[col]], 3)
  }
  p_col <- grep("^Pr", names(anova_tbl), value = TRUE)
  if (length(p_col) > 0) {
    colnames(anova_tbl)[colnames(anova_tbl) == p_col[1]] <- "Pr(>F)"
  }

  ft_anova <- format_table(anova_tbl)
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
      rand_df <- rand_df[, c("grp", "var1", "var2", "vcov", "sdcor"), drop = FALSE]
      rand_df$var2 <- ifelse(is.na(rand_df$var2), "-", rand_df$var2)
      names(rand_df) <- c("Grouping", "Effect 1", "Effect 2", "Variance", "Std. Dev.")
      rand_df$Variance <- round(rand_df$Variance, 3)
      rand_df$`Std. Dev.` <- round(rand_df$`Std. Dev.`, 3)
      ft_rand <- format_table(rand_df, bold_p = FALSE)
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
      icc_df$ICC <- round(icc_df$ICC, 3)
      ft_icc <- format_table(icc_df, bold_p = FALSE)
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

  ft_coef <- format_table(coef_tbl)
  doc <- body_add_flextable(doc, ft_coef)

  # ==========================================================
  # 🔹 Footer
  # ==========================================================
  doc <- add_blank_line(doc)
  doc <- add_blank_line(doc, "Significance level: p < 0.05 (bold values).")
  doc <- add_blank_line(doc, sprintf("Generated by Table Analyzer on %s", Sys.Date()))

  # Save file
  print(doc, target = file)
}

