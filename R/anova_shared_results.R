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

  # Helper for consistent table formatting (matches LM/LMM exports)
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
      p_cols <- names(df)[grepl("Pr|p\\.?value|p-value", names(df), ignore.case = TRUE)]
      for (pcol in p_cols) {
        if (is.numeric(df[[pcol]]) || all(grepl("^[0-9.<]+$", df[[pcol]]))) {
          sig_rows <- suppressWarnings(which(as.numeric(df[[pcol]]) < 0.05))
          if (length(sig_rows) == 0) {
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
      Fvalue = round(Fvalue, 3),
      PrF = as.numeric(PrF),
      `Pr(>F)` = round(PrF, 3)
    ) %>%
    arrange(Response, Stratum, Term)

  show_strata <- !(length(unique(combined_anova$Stratum)) == 1 && unique(combined_anova$Stratum) == "None")

  if (!show_strata) {
    combined_anova$Stratum <- NULL
  }

  anova_cols <- c("Response", if (show_strata) "Stratum", "Term", "SumSq", "Df", "Fvalue", "Pr(>F)")
  combined_anova <- combined_anova[, anova_cols, drop = FALSE]

  # Document shell --------------------------------------------------------
  unique_responses <- unique(combined_anova$Response)
  title_text <- if (!is.null(response_name)) {
    sprintf("ANOVA Results — %s", response_name)
  } else if (length(unique_responses) == 1) {
    sprintf("ANOVA Results — %s", unique_responses)
  } else {
    "ANOVA Results — Multiple Responses"
  }

  doc <- read_docx()
  doc <- body_add_fpar(
    doc,
    fpar(ftext(title_text, prop = fp_text(bold = TRUE, font.size = 12)))
  )

  if (!is.null(stratum_label) && nzchar(stratum_label) && !identical(stratum_label, "None")) {
    doc <- body_add_fpar(
      doc,
      fpar(ftext(stratum_label, prop = fp_text(bold = TRUE, font.size = 11)))
    )
  }

  doc <- add_blank_line(doc)

  # ANOVA table -----------------------------------------------------------
  doc <- body_add_fpar(doc, fpar(ftext("ANOVA (Type III)", prop = fp_text(bold = TRUE))))
  doc <- add_blank_line(doc)
  doc <- body_add_flextable(doc, format_table(combined_anova))
  doc <- add_blank_line(doc)

  # Post-hoc contrasts ----------------------------------------------------
  if (!is.null(contrast_entries) && length(contrast_entries) > 0) {
    combined_contrasts <- bind_rows(contrast_entries)

    p_col <- intersect(c("p.value", "p.value."), names(combined_contrasts))
    p_col <- if (length(p_col) > 0) p_col[1] else NULL
    if (!is.null(p_col)) {
      combined_contrasts[[p_col]] <- as.numeric(combined_contrasts[[p_col]])
      combined_contrasts[[p_col]] <- round(combined_contrasts[[p_col]], 3)
    }

    has_stratum_contrast <- "Stratum" %in% names(combined_contrasts)
    show_strata_contrast <- has_stratum_contrast && !(length(unique(combined_contrasts$Stratum)) == 1 && unique(combined_contrasts$Stratum) == "None")

    combined_contrasts <- if (has_stratum_contrast) {
      combined_contrasts %>% arrange(Response, Stratum)
    } else {
      combined_contrasts %>% arrange(Response)
    }

    base_cols <- c("Response", if (show_strata_contrast) "Stratum")
    contrast_visible <- unique(c(base_cols, setdiff(names(combined_contrasts), base_cols)))

    doc <- body_add_fpar(doc, fpar(ftext("Post-hoc Contrasts", prop = fp_text(bold = TRUE))))
    doc <- add_blank_line(doc)
    doc <- body_add_flextable(doc, format_table(combined_contrasts[, contrast_visible, drop = FALSE]))
    doc <- add_blank_line(doc)
  }

  # Footer ---------------------------------------------------------------
  doc <- add_blank_line(doc, "Significance level: p < 0.05 (bold values).")
  doc <- add_blank_line(doc, sprintf("Generated by Table Analyzer on %s", Sys.Date()))

  print(doc, target = file)
}

