#### Section: ANOVA Output Processing ####

#### Section: ANOVA Output Processing ####

prepare_anova_outputs <- function(model_obj, factor_names) {
  old_contrasts <- options("contrasts")
  on.exit(options(old_contrasts), add = TRUE)
  options(contrasts = c("contr.sum", "contr.poly"))
  
  safe_anova <- purrr::safely(function(mod) {
    car::Anova(mod, type = 3)
  })
  
  anova_result <- safe_anova(model_obj)
  if (!is.null(anova_result$error)) {
    return(list(
      error = conditionMessage(anova_result$error),
      anova_object = NULL,
      anova_table = NULL,
      anova_significant = NULL,
      posthoc_details = list(),
      posthoc_table = NULL,
      posthoc_significant = NULL
    ))
  }
  
  anova_obj <- anova_result$result
  anova_df <- as.data.frame(anova_obj)
  anova_df$Effect <- rownames(anova_df)
  rownames(anova_df) <- NULL
  anova_df <- anova_df[, c("Effect", setdiff(names(anova_df), "Effect"))]
  
  # --- round numeric columns and keep raw p-values ---
  p_col <- grep("^Pr", names(anova_df), value = TRUE)
  p_col <- if (length(p_col) > 0) p_col[1] else NULL
  raw_p <- if (!is.null(p_col)) anova_df[[p_col]] else rep(NA_real_, nrow(anova_df))
  
  for (col in names(anova_df)) {
    if (is.numeric(anova_df[[col]])) {
      anova_df[[col]] <- round(anova_df[[col]], 2)
    }
  }
  
  anova_significant <- !is.na(raw_p) & raw_p < 0.05
  if (!is.null(p_col)) {
    names(anova_df)[names(anova_df) == p_col] <- "p.value"
  } else {
    anova_df$p.value <- NA_real_
  }
  
  # --- Post-hoc Tukey for each factor ---
  factor_names <- unique(factor_names[!is.na(factor_names) & nzchar(factor_names)])
  posthoc_details <- list()
  posthoc_combined <- NULL
  posthoc_significant <- numeric(0)
  
  # ONE-WAY POSTHOC (reference vs all)
  if (length(factor_names) == 1) {
    f1 <- factor_names[1]
    f1_spec <- anova_protect_vars(f1)
    
    if (f1 %in% names(model_obj$model)) {
      res <- tryCatch({
        
        emm <- emmeans::emmeans(model_obj, specs = as.formula(paste("~", f1_spec)))
        
        ref_lvl <- levels(model_obj$model[[f1]])[1]
        ref_idx <- which(levels(model_obj$model[[f1]]) == ref_lvl)
        
        # Only reference-vs-others contrasts (clean, robust)
        contrasts <- emmeans::contrast(emm, method = "trt.vs.ctrl", ref = ref_idx)
        res_df <- as.data.frame(summary(contrasts))
        
        res_df$Factor <- f1
        res_df
        
      }, error = function(e) list(error = e$message))
      
      if (is.data.frame(res)) {
        posthoc_details[[f1]] <- list(table = res, error = NULL)
        posthoc_combined <- res
      } else {
        posthoc_details[[f1]] <- list(table = NULL, error = res$error)
      }
    }
  } 
  ### TWO-WAY POSTHOC (nested reference-vs-all)
  else if (length(factor_names) == 2) {
    
    f1 <- factor_names[1]
    f2 <- factor_names[2]
    
    f1_spec <- anova_protect_vars(f1)
    f2_spec <- anova_protect_vars(f2)
    
    # Identify reference level of f2
    f2_levels <- levels(model_obj$model[[f2]])
    ref_lvl <- f2_levels[1]
    ref_idx <- which(f2_levels == ref_lvl)
    
    # Compute nested emmeans: f2 within each f1 level
    res_nested <- tryCatch({
      
      emm_nested <- emmeans::emmeans(
        model_obj,
        specs = as.formula(paste("~", f2_spec, "|", f1_spec))
      )
      
      # Reference-vs-all inside each f1 group
      contrasts_nested <- emmeans::contrast(
        emm_nested,
        method = "trt.vs.ctrl",
        ref = ref_idx
      )
      
      df <- as.data.frame(summary(contrasts_nested))
      df$Factor <- paste0(f2, "_within_", f1)

      # Ensure f1 column exists (grouping variable)
      if (!f1 %in% names(df)) {
        df[[f1]] <- df$comparison # fallback: emmeans puts group there
      }

      # Preserve the user-specified order of the grouping factor
      f1_levels <- levels(model_obj$model[[f1]])
      if (!is.null(f1_levels)) {
        df[[f1]] <- factor(df[[f1]], levels = f1_levels)
        df <- df[order(df[[f1]]), , drop = FALSE]
      }

      df[[f1]] <- as.character(df[[f1]])
      df
      
    }, error = function(e) list(error = e$message))
    
    if (is.data.frame(res_nested)) {
      key <- paste0(f2, "_within_", f1)
      posthoc_details[[key]] <- list(table = res_nested, error = NULL)
      posthoc_combined <- dplyr::bind_rows(posthoc_combined, res_nested)
    } else {
      key <- paste0(f2, "_within_", f1)
      posthoc_details[[key]] <- list(table = NULL, error = res_nested$error)
    }
  }
  
  
  if (!is.null(posthoc_combined)) {
    posthoc_combined <- posthoc_combined[, c("Factor", setdiff(names(posthoc_combined), "Factor"))]
    numeric_cols <- names(posthoc_combined)[sapply(posthoc_combined, is.numeric)]
    if (length(numeric_cols) > 0) {
      for (col in numeric_cols) {
        posthoc_combined[[col]] <- round(posthoc_combined[[col]], 2)
      }
    }
    
    if ("p.value" %in% names(posthoc_combined)) {
      raw_posthoc_p <- posthoc_combined$p.value
      posthoc_significant <- !is.na(raw_posthoc_p) & raw_posthoc_p < 0.05
    } else {
      posthoc_significant <- rep(FALSE, nrow(posthoc_combined))
    }
  }
  
  list(
    error = NULL,
    anova_object = anova_obj,
    anova_table = anova_df,
    anova_significant = anova_significant,
    posthoc_details = posthoc_details,
    posthoc_table = posthoc_combined,
    posthoc_significant = posthoc_significant
  )
}

#### Collate tidy summaries from ANOVA models ####

download_all_anova_results <- function(models_info, file) {
  if (is.null(models_info) || is.null(models_info$models)) {
    stop("No models found to export.")
  }

  combined_anova <- list()
  combined_contrasts <- list()
  factor_names <- unique(unlist(models_info$factors))
  factor_names <- factor_names[!is.na(factor_names) & nzchar(factor_names)]
  errors <- character(0)
  
  # --- Case 1: no stratification
  if (is.null(models_info$strata)) {
    for (resp in models_info$responses) {
      model_entry <- models_info$models[[resp]]
      if (is.null(model_entry) || !is.null(model_entry$error) || is.null(model_entry$model)) {
        if (!is.null(model_entry$error)) {
          errors <- c(errors, paste0(resp, ": ", model_entry$error))
        }
        next
      }
      outputs <- prepare_anova_outputs(model_entry$model, factor_names)
      if (!is.null(outputs$error)) {
        errors <- c(errors, paste0(resp, ": ", outputs$error))
        next
      }
      anova_obj <- outputs$anova_object
      if (is.null(anova_obj)) {
        errors <- c(errors, paste0(resp, ": ANOVA results are unavailable."))
        next
      }
      tbl <- as.data.frame(anova_obj)
      tbl$Response <- resp
      tbl$Stratum <- "None"
      tbl$Term <- rownames(tbl)
      rownames(tbl) <- NULL
      names(tbl) <- sub(" ", "", names(tbl))
      tbl$PrF <- tbl[, grep("^Pr", names(tbl))[1]]
      combined_anova[[length(combined_anova) + 1]] <- tbl

      if (!is.null(outputs$posthoc_table) && nrow(outputs$posthoc_table) > 0) {
        ph_tbl <- outputs$posthoc_table
        ph_tbl$Response <- resp
        ph_tbl$Stratum <- "None"
        combined_contrasts[[length(combined_contrasts) + 1]] <- ph_tbl
      }
    }
  } else {
    # --- Case 2: stratified
    for (stratum in models_info$strata$levels) {
      for (resp in models_info$responses) {
        model_entry <- models_info$models[[stratum]][[resp]]
        if (is.null(model_entry) || !is.null(model_entry$error) || is.null(model_entry$model)) {
          if (!is.null(model_entry$error)) {
            errors <- c(errors, paste0(resp, " (", stratum, "): ", model_entry$error))
          }
          next
        }
        outputs <- prepare_anova_outputs(model_entry$model, factor_names)
        if (!is.null(outputs$error)) {
          errors <- c(errors, paste0(resp, " (", stratum, "): ", outputs$error))
          next
        }
        anova_obj <- outputs$anova_object
        if (is.null(anova_obj)) {
          errors <- c(errors, paste0(resp, " (", stratum, "): ANOVA results are unavailable."))
          next
        }
        tbl <- as.data.frame(anova_obj)
        tbl$Response <- resp
        tbl$Stratum <- stratum
        tbl$Term <- rownames(tbl)
        rownames(tbl) <- NULL
        names(tbl) <- sub(" ", "", names(tbl))
        tbl$PrF <- tbl[, grep("^Pr", names(tbl))[1]]
        combined_anova[[length(combined_anova) + 1]] <- tbl

        if (!is.null(outputs$posthoc_table) && nrow(outputs$posthoc_table) > 0) {
          ph_tbl <- outputs$posthoc_table
          ph_tbl$Response <- resp
          ph_tbl$Stratum <- stratum
          combined_contrasts[[length(combined_contrasts) + 1]] <- ph_tbl
        }
      }
    }
  }

  if (length(combined_anova) == 0) {
    msg <- "No ANOVA models available to export."
    if (length(errors) > 0) {
      msg <- paste0(
        msg,
        " The following issues were reported:\n",
        paste(sprintf("- %s", unique(errors)), collapse = "\n")
      )
    }
    stop(msg)
  }

  write_anova_docx(
    content = list(
      anova_results = combined_anova,
      contrast_results = combined_contrasts
    ),
    file = file
  )
}

write_anova_docx <- function(content, file, response_name = NULL, stratum_label = NULL) {
  if (is.null(content)) stop("No ANOVA results available to export.")

  add_blank_line <- function(doc, text = "", style = "Normal") {
    body_add_par(doc, text, style = style)
  }

  # Helper to format p-values consistently
  format_p <- function(df, p_col) {
    if (is.null(p_col) || !p_col %in% names(df)) return(df)
    p_vals <- as.numeric(df[[p_col]])
    df[[p_col]] <- p_vals
    df[[paste0(p_col, "_label")]] <- ifelse(p_vals < 0.001, "<0.001", sprintf("%.3f", p_vals))
    df$sig <- p_vals < 0.05
    df
  }

  # Shared styling for all tables (aligned with LM/LMM exports)
  format_table <- function(df, visible_cols, header_labels, merge_cols = NULL, p_label_col = NULL, sig_col = "sig") {
    ft <- flextable(df[, visible_cols, drop = FALSE])
    ft <- set_header_labels(ft, values = header_labels)

    if (!is.null(merge_cols)) {
      ft <- merge_v(ft, j = intersect(merge_cols, ft$col_keys))
    }

    ft <- fontsize(ft, part = "all", size = 10)
    ft <- bold(ft, part = "header", bold = TRUE)
    ft <- color(ft, part = "header", color = "black")
    ft <- align(ft, align = "center", part = "all")
    ft <- border_remove(ft)

    black <- fp_border(color = "black", width = 1)
    ft <- border(ft, part = "header", border.top = black, border.bottom = black)

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

    if (!is.null(sig_col) && sig_col %in% names(df) && !is.null(p_label_col) && p_label_col %in% ft$col_keys) {
      sig_rows <- which(df[[sig_col]] %in% TRUE)
      if (length(sig_rows) > 0) {
        ft <- bold(ft, i = sig_rows, j = p_label_col, bold = TRUE)
      }
    }

    ft <- set_table_properties(ft, layout = "autofit", width = 0.9)
    ft <- padding(ft, padding.top = 2, padding.bottom = 2, padding.left = 2, padding.right = 2)
    ft
  }

  # Build unified data frames for ANOVA and contrasts
  is_batch <- !is.null(content$anova_results) || !is.null(content$contrast_results)

  if (is_batch) {
    anova_entries <- content$anova_results
    contrast_entries <- content$contrast_results
  } else {
    anova_entries <- list()
    contrast_entries <- list()

    if (!is.null(content$anova_object)) {
      anova_tbl <- as.data.frame(content$anova_object)
      anova_tbl$Response <- response_name %||% "Response"
      anova_tbl$Stratum <- stratum_label %||% "None"
      anova_tbl$Term <- rownames(anova_tbl)
      rownames(anova_tbl) <- NULL
      names(anova_tbl) <- sub(" ", "", names(anova_tbl))
      anova_tbl$PrF <- anova_tbl[, grep("^Pr", names(anova_tbl))[1]]
      anova_entries <- list(anova_tbl)
    }

    if (!is.null(content$posthoc_table) && nrow(content$posthoc_table) > 0) {
      ph_tbl <- content$posthoc_table
      ph_tbl$Response <- response_name %||% "Response"
      ph_tbl$Stratum <- stratum_label %||% "None"
      contrast_entries <- list(ph_tbl)
    }
  }

  if (is.null(anova_entries) || length(anova_entries) == 0) stop("No ANOVA results available to export.")

  combined_anova <- bind_rows(anova_entries)

  required_cols <- c("Response", "Stratum", "Term", "SumSq", "Df", "Fvalue", "PrF")
  if (!all(required_cols %in% names(combined_anova))) stop("Missing required columns in ANOVA results.")

  combined_anova <- combined_anova %>%
    mutate(
      SumSq = round(SumSq, 3),
      Fvalue = round(Fvalue, 3)
    ) %>%
    format_p("PrF") %>%
    arrange(Response, Stratum, Term)

  show_strata <- !(length(unique(combined_anova$Stratum)) == 1 && unique(combined_anova$Stratum) == "None")

  if (show_strata) {
    anova_visible <- c("Response", "Stratum", "Term", "SumSq", "Df", "Fvalue", "PrF_label")
    anova_merge <- c("Response", "Stratum")
  } else {
    combined_anova$Stratum <- NULL
    anova_visible <- c("Response", "Term", "SumSq", "Df", "Fvalue", "PrF_label")
    anova_merge <- c("Response")
  }

  anova_headers <- c(
    Response = "Response",
    Stratum = if (show_strata) "Stratum" else NULL,
    Term = "Term",
    SumSq = "Sum Sq",
    Df = "Df",
    Fvalue = "F value",
    PrF_label = "Pr(>F)"
  )

  doc <- read_docx()

  title_text <- if (length(unique(combined_anova$Response)) == 1) {
    sprintf("ANOVA Results — %s", unique(combined_anova$Response))
  } else {
    "ANOVA Results"
  }

  doc <- body_add_fpar(doc, fpar(ftext(title_text, prop = fp_text(bold = TRUE, font.size = 12))))

  # Optional subtitle when there is a single stratum label present
  if (!show_strata && !is.null(stratum_label) && nzchar(stratum_label) && !identical(stratum_label, "None")) {
    doc <- body_add_fpar(doc, fpar(ftext(stratum_label, prop = fp_text(bold = TRUE, font.size = 11))))
  }

  doc <- add_blank_line(doc)

  doc <- body_add_fpar(doc, fpar(ftext("ANOVA Table", prop = fp_text(bold = TRUE))))
  doc <- add_blank_line(doc)
  doc <- body_add_flextable(
    doc,
    format_table(
      combined_anova,
      visible_cols = anova_visible,
      header_labels = anova_headers,
      merge_cols = anova_merge,
      p_label_col = "PrF_label"
    )
  )

  if (!is.null(contrast_entries) && length(contrast_entries) > 0) {
    combined_contrasts <- bind_rows(contrast_entries)

    p_col <- intersect(c("p.value", "p.value."), names(combined_contrasts))
    p_col <- if (length(p_col) > 0) p_col[1] else NULL
    combined_contrasts <- format_p(combined_contrasts, p_col)

    has_stratum_contrast <- "Stratum" %in% names(combined_contrasts)
    show_strata_contrast <- has_stratum_contrast && !(length(unique(combined_contrasts$Stratum)) == 1 && unique(combined_contrasts$Stratum) == "None")

    combined_contrasts <- if (has_stratum_contrast) {
      combined_contrasts %>% arrange(Response, Stratum)
    } else {
      combined_contrasts %>% arrange(Response)
    }

    base_cols <- c("Response", if (show_strata_contrast) "Stratum")
    detail_cols <- setdiff(names(combined_contrasts), c(base_cols, "sig", if (!is.null(p_col)) c(p_col, paste0(p_col, "_label"))))
    p_display_col <- if (!is.null(p_col)) paste0(p_col, "_label") else NULL
    contrast_visible <- unique(c(base_cols, detail_cols, p_display_col))

    header_labels <- setNames(gsub("_", " ", contrast_visible), contrast_visible)
    if (!is.null(p_display_col)) header_labels[[p_display_col]] <- "p-value"

    doc <- add_blank_line(doc)
    doc <- body_add_fpar(doc, fpar(ftext("Post-hoc Contrasts", prop = fp_text(bold = TRUE))))
    doc <- add_blank_line(doc)
    doc <- body_add_flextable(
      doc,
      format_table(
        combined_contrasts,
        visible_cols = contrast_visible,
        header_labels = header_labels,
        merge_cols = base_cols,
        p_label_col = p_display_col
      )
    )
  }

  doc <- add_blank_line(doc)
  doc <- add_blank_line(doc, "Significance level: p < 0.05 (bold values).")
  print(doc, target = file)
}

