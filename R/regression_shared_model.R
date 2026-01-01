# ===============================================================
# ?? Regression shared modeling utilities
# ===============================================================

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
    anova_obj <- car::Anova(model, type = 3)
    raw_anova_lines <- capture.output(anova_obj)
    signif_idx <- grep("^Signif\\. codes", raw_anova_lines)
    if (length(signif_idx) > 0) {
      remove_idx <- c(signif_idx - 1, signif_idx)
      signif_lines <- raw_anova_lines[remove_idx]
    } else {
      signif_lines <- character(0)
    }
    anova_tbl <- as.data.frame(anova_obj)
    formatted_anova <- format_anova_numeric_columns(anova_tbl)
    formatted_anova <- format_anova_f_column(formatted_anova)
    formatted_anova <- format_anova_p_column(formatted_anova)
    aout <- capture.output(print(formatted_anova))
    header_lines <- c(
      "Anova Table (Type III tests)",
      "",
      paste("Response:", all.vars(formula(model))[1]),
      ""
    )
    table_lines <- c(header_lines, aout)
    if (length(signif_lines) > 0) {
      table_lines <- c(table_lines, "", signif_lines)
    }
    cat(paste(table_lines, collapse = "\n"), "\n\n")

    coef_lines <- format_coef_matrix_lines(summary(model)$coefficients)
    cat("Coefficients:\n")
    cat(paste(coef_lines, collapse = "\n"), "\n")
  } else {
    anova_obj <- anova(model, type = 3)
    raw_anova_lines <- capture.output(anova_obj)
    signif_idx <- grep("^Signif\\. codes", raw_anova_lines)
    if (length(signif_idx) > 0) {
      remove_idx <- c(signif_idx - 1, signif_idx)
      signif_lines <- raw_anova_lines[remove_idx]
    } else {
      signif_lines <- character(0)
    }
    anova_tbl <- as.data.frame(anova_obj)
    formatted_anova <- format_anova_numeric_columns(anova_tbl)
    formatted_anova <- format_anova_f_column(formatted_anova)
    formatted_anova <- format_anova_p_column(formatted_anova)
    aout <- capture.output(print(formatted_anova))
    heading_lines <- attr(anova_obj, "heading")
    header_lines <- if (!is.null(heading_lines) && length(heading_lines) > 0) {
      c(heading_lines, "")
    } else {
      character(0)
    }
    table_lines <- c(header_lines, aout)
    if (length(signif_lines) > 0) {
      table_lines <- c(table_lines, "", signif_lines)
    }
    cat(paste(table_lines, collapse = "\n"), "\n\n")

    rand_var <- tryCatch(lme4::VarCorr(model), error = function(e) NULL)
    cat("Random effects:\n")
    if (!is.null(rand_var)) {
      rand_df <- tryCatch(as.data.frame(rand_var), error = function(e) NULL)
      if (!is.null(rand_df) && nrow(rand_df) > 0) {
        rand_df[is.na(rand_df)] <- ""
        rand_df$vcov <- ifelse(
          is.na(rand_df$vcov),
          "",
          sprintf("%.4f", rand_df$vcov)
        )
        rand_df$sdcor <- ifelse(
          is.na(rand_df$sdcor),
          "",
          sprintf("%.4f", rand_df$sdcor)
        )
        print(rand_df, row.names = FALSE)
      } else {
        print(rand_var, comp = c("Variance", "Std.Dev."))
      }
    } else {
      cat("  None\n")
    }
    cat("\n")

    icc_df <- if (exists("compute_icc") && is.function(compute_icc)) compute_icc(model) else NULL
    if (!is.null(icc_df) && nrow(icc_df) > 0) {
      cat("Intraclass Correlation (ICC):\n")
      icc_df$ICC <- formatC(icc_df$ICC, format = "f", digits = 4, drop0trailing = FALSE)
      print(icc_df, row.names = FALSE)
      cat("\n")
    }

    coef_lines <- format_coef_matrix_lines(summary(model)$coefficients)
    cat("Coefficients:\n")
    cat(paste(coef_lines, collapse = "\n"), "\n")
  }
}

format_coef_matrix_lines <- function(coef_mat) {
  if (is.null(coef_mat)) return(NULL)
  df <- data.frame(
    Term = rownames(coef_mat),
    stringsAsFactors = FALSE
  )

  num_cols <- c("Estimate", "Std. Error", "t value", "z value", "Std. error", "Std.Error", "Std error")
  stat_col <- intersect(colnames(coef_mat), c("t value", "z value"))
  p_col <- intersect(colnames(coef_mat), grep("^Pr", colnames(coef_mat), value = TRUE))

  if ("Estimate" %in% colnames(coef_mat)) {
    df$Estimate <- formatC(coef_mat[, "Estimate"], format = "f", digits = 4, drop0trailing = FALSE)
  }
  if ("Std. Error" %in% colnames(coef_mat)) {
    df$Std.Error <- formatC(coef_mat[, "Std. Error"], format = "f", digits = 4, drop0trailing = FALSE)
  }
  if ("Std error" %in% colnames(coef_mat)) {
    df$Std.Error <- formatC(coef_mat[, "Std error"], format = "f", digits = 4, drop0trailing = FALSE)
  }
  if (length(stat_col) > 0) {
    df$Statistic <- formatC(coef_mat[, stat_col[1]], format = "f", digits = 4, drop0trailing = FALSE)
  }
  if (length(p_col) > 0) {
    p_vals <- suppressWarnings(as.numeric(coef_mat[, p_col[1]]))
    df$p.value <- ifelse(
      is.na(p_vals),
      "",
      ifelse(p_vals < 0.0001, "<0.0001", sprintf("%.4f", p_vals))
    )
  }
  capture.output(print(df, row.names = FALSE))
}

format_anova_numeric_columns <- function(anova_tbl) {
  if (is.null(anova_tbl)) return(anova_tbl)

  p_col <- intersect(c("Pr(>F)", "p.value"), names(anova_tbl))
  f_col <- intersect(c("F value", "F.Value", "F"), names(anova_tbl))

  for (col in names(anova_tbl)) {
    if (!is.numeric(anova_tbl[[col]])) next
    if (length(p_col) > 0 && col == p_col[1]) next
    if (length(f_col) > 0 && col == f_col[1]) next
    anova_tbl[[col]] <- sprintf("%.4f", anova_tbl[[col]])
  }

  anova_tbl
}

format_anova_f_column <- function(anova_tbl) {
  if (is.null(anova_tbl)) return(anova_tbl)

  effect_col <- intersect("Effect", names(anova_tbl))
  f_col <- intersect(c("F value", "F.Value", "F"), names(anova_tbl))

  if (length(f_col) > 0) {
    col <- f_col[1]
    f_vals <- suppressWarnings(as.numeric(anova_tbl[[col]]))
    f_chr <- ifelse(is.na(f_vals), "", sprintf("%.4f", f_vals))
    if (length(effect_col) > 0) {
      is_resid <- anova_tbl[[effect_col]] == "Residuals"
      f_chr[is_resid] <- ""
    }
    anova_tbl[[col]] <- f_chr
  }

  anova_tbl
}

format_anova_p_column <- function(anova_tbl) {
  if (is.null(anova_tbl)) return(anova_tbl)

  p_col <- intersect(c("Pr(>F)", "p.value"), names(anova_tbl))

  if (length(p_col) > 0) {
    col <- p_col[1]
    vals <- suppressWarnings(as.numeric(anova_tbl[[col]]))
    anova_tbl[[col]] <- ifelse(
      is.na(vals),
      "",
      ifelse(vals < 0.0001, "<0.0001", sprintf("%.4f", vals))
    )
  }

  anova_tbl
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
