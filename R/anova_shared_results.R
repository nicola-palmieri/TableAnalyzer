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
          df <- as.data.frame(summary(contrasts))
          
          # validate df BEFORE filtering
          if (!is.data.frame(df) || !"contrast" %in% names(df)) {
            stop("Invalid posthoc structure")
          }
          
          # reference filtering
          ref <- levels(model_obj$model[[f1]])[1]
          df <- df[
            grepl(paste0(ref, " - "), df$contrast) |
              grepl(paste0(" - ", ref), df$contrast),
            ,
          ]
          
          df
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

    # --- nested contrasts: keep ONLY comparisons vs the reference level of factor2 ---
    res_nested <- tryCatch({
      formula_nested <- as.formula(paste("pairwise ~", f2_spec, "|", f1_spec))
      emm_nested <- emmeans::emmeans(model_obj, specs = formula_nested, adjust = "tukey")
      contrasts_df <- as.data.frame(summary(emm_nested$contrasts))
      
      # Identify reference level (first level of factor2)
      lev2 <- levels(model_obj$model[[f2]])
      reference <- lev2[1]
      
      # Keep only contrasts involving the reference
      contrasts_df <- contrasts_df[
        grepl(paste0(reference, " - "), contrasts_df$contrast) |
          grepl(paste0(" - ", reference), contrasts_df$contrast),
        ,
      ]
      
      # Add metadata
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
  contrast_results <- list()
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

      if (!is.null(outputs$posthoc_table)) {
        contrast_tbl <- outputs$posthoc_table
        contrast_tbl$Response <- resp
        contrast_tbl$Stratum <- "None"
        contrast_results[[length(contrast_results) + 1]] <- contrast_tbl
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
        combined_results[[length(combined_results) + 1]] <- tbl

        if (!is.null(outputs$posthoc_table)) {
          contrast_tbl <- outputs$posthoc_table
          contrast_tbl$Response <- resp
          contrast_tbl$Stratum <- stratum
          contrast_results[[length(contrast_results) + 1]] <- contrast_tbl
        }
      }
    }
  }

  if (length(combined_results) == 0 && length(contrast_results) == 0) {
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
    file = file,
    content = list(anova = combined_results, contrasts = contrast_results)
  )
}

write_anova_docx <- function(file, content, response_name = NULL, stratum_label = NULL) {

  prep <- prepare_docx_tables(content, response_name, stratum_label)
  combined_anova <- prep$anova
  combined_contrasts <- prep$contrasts

  if (is.null(combined_anova) || length(combined_anova) == 0) {
    stop("No ANOVA results available to export.")
  }

  combined <- bind_rows(combined_anova)

  required_cols <- c("Response", "Stratum", "Term", "SumSq", "Df", "Fvalue", "PrF")
  if (!all(required_cols %in% names(combined))) stop("Missing required columns in ANOVA results.")

  combined <- combined %>%
    mutate(
      SumSq = round(SumSq, 3),
      Fvalue = round(Fvalue, 3),
      PrF_label = ifelse(PrF < 0.001, "<0.001", sprintf("%.3f", PrF)),
      sig = PrF < 0.05
    ) %>%
    arrange(Response, Stratum, Term)

  if (length(unique(combined$Stratum)) == 1 && unique(combined$Stratum) == "None") {
    combined$Stratum <- NULL
    visible_cols <- c("Response", "Term", "SumSq", "Df", "Fvalue", "PrF_label")
    merge_cols <- c("Response")
  } else {
    visible_cols <- c("Response", "Stratum", "Term", "SumSq", "Df", "Fvalue", "PrF_label")
    merge_cols <- c("Response", "Stratum")
  }

  ft <- flextable(combined[, visible_cols])
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

  ft <- apply_publication_style(ft, combined, merge_cols, which(combined$sig), "PrF_label")

  doc <- read_docx()
  doc <- body_add_par(doc, "Type III ANOVA", style = "Normal")
  doc <- body_add_par(doc, "", style = "Normal")
  doc <- body_add_flextable(doc, ft)

  if (!is.null(combined_contrasts) && length(combined_contrasts) > 0) {
    contrast_df <- bind_rows(combined_contrasts)
    contrast_df <- format_contrast_table(contrast_df)

    if (!is.null(contrast_df)) {
      if (!"Stratum" %in% names(contrast_df) ||
        (length(unique(contrast_df$Stratum)) == 1 && unique(contrast_df$Stratum) == "None")) {
        contrast_df$Stratum <- NULL
        c_visible_cols <- c("Response", "Factor", "contrast", "estimate", "SE", "df", "t.ratio", "lower.CL", "upper.CL", "p_label")
        c_merge_cols <- c("Response")
      } else {
        c_visible_cols <- c("Response", "Stratum", "Factor", "contrast", "estimate", "SE", "df", "t.ratio", "lower.CL", "upper.CL", "p_label")
        c_merge_cols <- c("Response", "Stratum")
      }

      c_visible_cols <- intersect(c_visible_cols, names(contrast_df))
      contrast_ft <- flextable(contrast_df[, c_visible_cols])
      contrast_ft <- set_header_labels(
        contrast_ft,
        Response = "Response",
        Stratum = if ("Stratum" %in% c_visible_cols) "Stratum" else NULL,
        Factor = if ("Factor" %in% c_visible_cols) "Factor" else NULL,
        contrast = "Contrast",
        estimate = "Estimate",
        SE = "SE",
        df = "df",
        t.ratio = "t-ratio",
        lower.CL = if ("lower.CL" %in% c_visible_cols) "Lower CL" else NULL,
        upper.CL = if ("upper.CL" %in% c_visible_cols) "Upper CL" else NULL,
        p_label = "p-value"
      )

      contrast_ft <- apply_publication_style(
        contrast_ft,
        contrast_df,
        c_merge_cols,
        which(contrast_df$sig),
        "p_label"
      )

      doc <- body_add_par(doc, "", style = "Normal")
      doc <- body_add_par(doc, "Tukey contrasts", style = "Normal")
      doc <- body_add_par(doc, "", style = "Normal")
      doc <- body_add_flextable(doc, contrast_ft)
    }
  }

  doc <- body_add_par(doc, "")
  doc <- body_add_par(doc, sprintf("Generated by Table Analyzer on %s", Sys.Date()))
  doc <- body_add_par(doc, "Significant p-values (< 0.05) in bold.", style = "Normal")
  print(doc, target = file)
}

prepare_docx_tables <- function(content, response_name = NULL, stratum_label = NULL) {
  anova_tables <- list()
  contrast_tables <- list()

  if (!is.null(content$anova_table)) {
    resp_label <- if (!is.null(response_name)) response_name else "Response"
    stratum_val <- if (!is.null(stratum_label)) stratum_label else "None"

    anova_tbl <- content$anova_table
    anova_tbl$Response <- resp_label
    anova_tbl$Stratum <- stratum_val
    if (!"Term" %in% names(anova_tbl) && "Effect" %in% names(anova_tbl)) {
      anova_tbl$Term <- anova_tbl$Effect
    }

    p_col <- grep("^Pr|p\\.value|p.value", names(anova_tbl), value = TRUE)[1]
    if (!is.null(p_col) && !"PrF" %in% names(anova_tbl)) {
      anova_tbl$PrF <- anova_tbl[[p_col]]
    }

    anova_tables <- list(anova_tbl)

    if (!is.null(content$posthoc_table)) {
      contrast_tbl <- content$posthoc_table
      contrast_tbl$Response <- resp_label
      contrast_tbl$Stratum <- stratum_val
      contrast_tables <- list(contrast_tbl)
    }
  } else if (!is.null(content$anova)) {
    anova_tables <- content$anova
    contrast_tables <- content$contrasts
  } else {
    anova_tables <- content
  }

  list(anova = anova_tables, contrasts = contrast_tables)
}

apply_publication_style <- function(ft, data, merge_cols, sig_rows = integer(0), sig_col = NULL) {
  ft <- merge_v(ft, j = intersect(merge_cols, ft$col_keys))
  ft <- fontsize(ft, part = "all", size = 10)
  ft <- bold(ft, part = "header", bold = TRUE)
  ft <- color(ft, part = "header", color = "black")
  ft <- align(ft, align = "center", part = "all")

  if (!is.null(sig_col) && sig_col %in% ft$col_keys && length(sig_rows) > 0) {
    ft <- bold(ft, i = sig_rows, j = sig_col, bold = TRUE)
  }

  ft <- border_remove(ft)
  black <- fp_border(color = "black", width = 1)
  thin <- fp_border(color = "black", width = 0.5)

  ft <- border(ft, part = "header", border.top = black)
  ft <- border(ft, part = "header", border.bottom = black)

  if ("Response" %in% names(data)) {
    resp_index <- which(diff(as.numeric(factor(data$Response))) != 0)
    if (length(resp_index) > 0) {
      ft <- border(ft, i = resp_index, part = "body", border.bottom = thin)
    }
  }

  if (nrow(data) > 0) {
    ft <- border(ft, i = nrow(data), part = "body", border.bottom = black)
  }

  ft <- set_table_properties(ft, layout = "autofit", width = 0.9)
  ft <- padding(ft, padding.top = 2, padding.bottom = 2, padding.left = 2, padding.right = 2)
  ft
}

format_contrast_table <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(NULL)

  if (!"Response" %in% names(df)) df$Response <- "Response"
  if (!"Stratum" %in% names(df)) df$Stratum <- "None"
  if (!"Factor" %in% names(df) && "factor" %in% names(df)) df$Factor <- df$factor

  if (!"Factor" %in% names(df)) {
    df$Factor <- NA_character_
  }

  numeric_cols <- names(df)[sapply(df, is.numeric)]
  if (length(numeric_cols) > 0) {
    for (col in numeric_cols) {
      df[[col]] <- round(df[[col]], 3)
    }
  }

  p_col <- grep("^p\\.value|^p.value|^p_?value", names(df), value = TRUE)[1]
  if (is.null(p_col)) return(NULL)

  df$p_label <- ifelse(df[[p_col]] < 0.001, "<0.001", sprintf("%.3f", df[[p_col]]))
  df$sig <- df[[p_col]] < 0.05

  df
}

