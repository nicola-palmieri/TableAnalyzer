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
  
  # --- Post-hoc Tukey (one-way or two-way specific) ---
    if (length(factor_names) == 1) {
      f1 <- factor_names[1]
      f1_spec <- anova_protect_vars(f1)
      if (f1 %in% names(model_obj$model)) {
        res <- tryCatch({
          emm <- emmeans::emmeans(model_obj, specs = as.formula(paste("~", f1_spec)))
          contrasts <- emmeans::contrast(emm, method = "pairwise", adjust = "tukey")
          as.data.frame(summary(contrasts))
        }, error = function(e) list(error = e$message))
      
      if (is.data.frame(res)) {
        res$Factor <- f1
        posthoc_details[[f1]] <- list(table = res, error = NULL)
        posthoc_combined <- res
      } else {
        posthoc_details[[f1]] <- list(table = NULL, error = res$error)
      }
    }
    
  } else if (length(factor_names) == 2) {
    f1 <- factor_names[1]
    f2 <- factor_names[2]

    f1_spec <- anova_protect_vars(f1)
    f2_spec <- anova_protect_vars(f2)
    
    # --- main-effect Tukey for both factors (averaged) ---
    for (ff in c(f1, f2)) {
      if (ff %in% names(model_obj$model)) {
        ff_spec <- anova_protect_vars(ff)
        res_main <- tryCatch({
          emm_main <- emmeans::emmeans(model_obj, specs = as.formula(paste("~", ff_spec)))
          contrasts_main <- emmeans::contrast(emm_main, method = "pairwise", adjust = "tukey")
          as.data.frame(summary(contrasts_main))
        }, error = function(e) list(error = e$message))
        
        if (is.data.frame(res_main)) {
          res_main$Factor <- ff
          posthoc_details[[ff]] <- list(table = res_main, error = NULL)
          posthoc_combined <- dplyr::bind_rows(posthoc_combined, res_main)
        } else {
          posthoc_details[[ff]] <- list(table = NULL, error = res_main$error)
        }
      }
    }

    # --- nested contrasts of factor2 within each level of factor1 ---
    res_nested <- tryCatch({
      formula_nested <- as.formula(paste("pairwise ~", f2_spec, "|", f1_spec))
      emm_nested <- emmeans::emmeans(model_obj, specs = formula_nested, adjust = "tukey")
      contrasts_df <- as.data.frame(summary(emm_nested$contrasts))
      contrasts_df$Factor <- paste0(f2, "_within_", f1)
      contrasts_df[[f1]] <- as.character(contrasts_df[[f1]])
      contrasts_df
    }, error = function(e) list(error = e$message))
    
    if (is.data.frame(res_nested)) {
      posthoc_details[[paste0(f2, "_within_", f1)]] <- list(table = res_nested, error = NULL)
      posthoc_combined <- dplyr::bind_rows(posthoc_combined, res_nested)
    } else {
      posthoc_details[[paste0(f2, "_within_", f1)]] <- list(table = NULL, error = res_nested$error)
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

  combined_results <- list()
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
      combined_results[[length(combined_results) + 1]] <- tbl
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
        combined_results[[length(combined_results) + 1]] <- tbl
      }
    }
  }

  if (length(combined_results) == 0) {
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

  write_anova_docx(combined_results, file)
}

write_anova_docx <- function(results, file) {

  if (is.null(results) || length(results) == 0) stop("No ANOVA results available to export.")
  combined <- bind_rows(results)
  
  required_cols <- c("Response", "Stratum", "Term", "SumSq", "Df", "Fvalue", "PrF")
  if (!all(required_cols %in% names(combined))) stop("Missing required columns in ANOVA results.")
  
  # Format and sort
  combined <- combined %>%
    mutate(
      SumSq = round(SumSq, 3),
      Fvalue = round(Fvalue, 3),
      PrF_label = ifelse(PrF < 0.001, "<0.001", sprintf("%.3f", PrF)),
      sig = PrF < 0.05
    ) %>%
    arrange(Response, Stratum, Term)
  
  # Hide Stratum column if it's all "None"
  if (length(unique(combined$Stratum)) == 1 && unique(combined$Stratum) == "None") {
    combined$Stratum <- NULL
    visible_cols <- c("Response", "Term", "SumSq", "Df", "Fvalue", "PrF_label")
    merge_cols <- c("Response")
  } else {
    visible_cols <- c("Response", "Stratum", "Term", "SumSq", "Df", "Fvalue", "PrF_label")
    merge_cols <- c("Response", "Stratum")
  }
  
  # Build flextable
  ft <- flextable(combined[, visible_cols])
  
  # Clean header names
  ft <- set_header_labels(
    ft,
    Response = "Response",
    Stratum = if ("Stratum" %in% visible_cols) "Stratum" else NULL,
    Term = "Term",
    SumSq = "Sum Sq",
    Df = "Df",
    Fvalue = "F value",
    PrF_label = "Pr(>F)"
  )
  
  # Merge identical group labels
  ft <- merge_v(ft, j = intersect(merge_cols, ft$col_keys))
  
  # Styling
  ft <- fontsize(ft, part = "all", size = 10)
  ft <- bold(ft, part = "header", bold = TRUE)
  ft <- color(ft, part = "header", color = "black")
  ft <- align(ft, align = "center", part = "all")
  
  # Bold significant p-values (< 0.05)
  if ("sig" %in% names(combined)) {
    sig_rows <- which(combined$sig)
    if (length(sig_rows) > 0 && "PrF_label" %in% ft$col_keys) {
      ft <- bold(ft, i = sig_rows, j = "PrF_label", bold = TRUE)
    }
  }
  
  # ===== Journal-style borders =====
  ft <- border_remove(ft)
  black <- fp_border(color = "black", width = 1)
  thin <- fp_border(color = "black", width = 0.5)
  
  # 1) Top line above header
  ft <- border(ft, part = "header", border.top = black)
  # 2) Line below header
  ft <- border(ft, part = "header", border.bottom = black)
  
  # 3) Thin horizontal lines between different responses
  if ("Response" %in% names(combined)) {
    resp_index <- which(diff(as.numeric(factor(combined$Response))) != 0)
    if (length(resp_index) > 0) {
      ft <- border(ft, i = resp_index, part = "body", border.bottom = thin)
    }
  }
  
  # 4) Final bottom border (last line)
  if (nrow(combined) > 0) {
    ft <- border(ft, i = nrow(combined), part = "body", border.bottom = black)
  }
  
  
  # No side or inner borders
  ft <- set_table_properties(ft, layout = "autofit", width = 0.9)
  ft <- padding(ft, padding.top = 2, padding.bottom = 2, padding.left = 2, padding.right = 2)
  
  # Write to DOCX
  doc <- read_docx()
  doc <- body_add_flextable(doc, ft)
  doc <- body_add_par(doc, "")
  doc <- body_add_par(doc, sprintf("Generated by Table Analyzer on %s", Sys.Date()))
  doc <- body_add_par(doc, "Significant p-values (< 0.05) in bold.", style = "Normal")
  print(doc, target = file)
}


#### Plotting ####

#### Section: Plot Context Initialization ####

