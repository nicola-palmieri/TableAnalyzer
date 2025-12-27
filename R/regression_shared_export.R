# ===============================================================
# ?? Regression shared exports
# ===============================================================

write_lm_docx_combined <- function(flat_models, file) {
  if (length(flat_models) == 0) stop("No models provided for export.")

  add_blank_line <- function(doc, text = "", style = "Normal") {
    body_add_par(doc, text, style = style)
  }

  format_p <- function(df, p_col) {
    if (is.null(p_col) || !p_col %in% names(df)) return(df)
    p_vals <- as.numeric(df[[p_col]])
    df[[p_col]] <- ifelse(p_vals < 0.0001, "<.0001", sprintf("%.4f", p_vals))
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
