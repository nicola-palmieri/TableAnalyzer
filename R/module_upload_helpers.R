# Clean names + convert characters to ordered factors
preprocess_uploaded_table <- function(df) {
  df |> mutate(across(where(is.character) | where(is.factor), auto_factor_order))
}

# Convert character/factor to factor with numeric-aware order
auto_factor_order <- function(x) {
  if (!is.factor(x) && !is.character(x)) return(x)

  ordered_levels <- stringr::str_sort(
    if (is.factor(x)) levels(x) else unique(x),
    numeric = TRUE,
    na_last = TRUE
  )

  factor(x, levels = ordered_levels, ordered = is.ordered(x))
}


convert_wide_to_long <- function(path, sheet = 1, replicate_col = "Replicate") {
  headers <- readxl::read_excel(path, sheet = sheet, n_max = 2, col_names = FALSE)
  header1 <- as.character(unlist(headers[1, , drop = TRUE]))
  header2 <- as.character(unlist(headers[2, , drop = TRUE]))
  
  if (all(header1 == "" | is.na(header1))) {
    header1 <- header2
    header2 <- rep("", length(header1))
  }
  
  header1[header1 == ""] <- NA
  header1 <- zoo::na.locf(header1, na.rm = FALSE)
  
  header2[is.na(header2) | header2 == ""] <- ""
  
  if (all(header2 == "")) {
    header2 <- ifelse(grepl("_", header1), sub(".*_", "", header1), "")
  }
  
  clean_names <- ifelse(header2 == "", header1, paste0(header1, "_", header2))
  clean_names <- make.unique(clean_names, sep = "_")
  
  fixed_cols   <- clean_names[header2 == ""]
  measure_cols <- clean_names[header2 != ""]
  
  data <- readxl::read_excel(path, sheet = sheet, skip = 2, col_names = clean_names)
  
  data_long <- data |>
    tidyr::pivot_longer(
      cols = tidyselect::all_of(measure_cols),
      names_to   = c("Variable", replicate_col),
      names_pattern = "^(.*)_([^_]*)$",
      values_to  = "Value"
    )
  
  id_cols <- c(fixed_cols, replicate_col, "Variable")
  
  duplicates <- data_long |>
    dplyr::group_by(dplyr::across(dplyr::all_of(id_cols))) |>
    dplyr::summarise(.n = dplyr::n(), .groups = "drop") |>
    dplyr::filter(.n > 1)
  
  if (nrow(duplicates) > 0) {
    ex <- duplicates[1, , drop = FALSE]
    stop(
      sprintf(
        "Duplicate measurements detected for variable '%s' and replicate '%s'. Ensure header labels are unique before uploading.",
        as.character(ex$Variable),
        as.character(ex[[replicate_col]])
      ),
      call. = FALSE
    )
  }
  
  data_long |>
    tidyr::pivot_wider(
      names_from  = "Variable",
      values_from = "Value"
    ) |>
    tibble::as_tibble()
}




safe_convert_wide_to_long <- purrr::safely(convert_wide_to_long)

safe_preprocess_uploaded_table <- purrr::safely(preprocess_uploaded_table)
