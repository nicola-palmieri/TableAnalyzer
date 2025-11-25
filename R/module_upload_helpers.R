# Clean names + convert characters to ordered factors
validate_uploaded_dataframe <- function(df) {
  if (is.null(df)) {
    stop("Uploaded data is empty. Please select a worksheet that contains data.", call. = FALSE)
  }

  if (!is.data.frame(df)) {
    stop("Uploaded content is not a tabular data frame.", call. = FALSE)
  }

  if (ncol(df) == 0) {
    stop("Uploaded data has no columns. Check the worksheet for content.", call. = FALSE)
  }

  if (nrow(df) == 0) {
    stop("Uploaded data has no rows. Confirm the sheet is populated.", call. = FALSE)
  }

  all_na <- vapply(df, function(col) all(is.na(col)), logical(1))
  if (all(all_na)) {
    stop("Uploaded data only contains missing values. Provide a populated worksheet.", call. = FALSE)
  }

  df
}

# Clean names + convert characters to ordered factors
preprocess_uploaded_table <- function(df) {
  df <- validate_uploaded_dataframe(df)

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
  if (!file.exists(path)) {
    stop("The uploaded file could not be found on disk.", call. = FALSE)
  }

  if (is.null(replicate_col) || !nzchar(trimws(replicate_col))) {
    stop("Provide a non-empty replicate column name when loading wide data.", call. = FALSE)
  }

  headers <- readxl::read_excel(path, sheet = sheet, n_max = 2, col_names = FALSE)

  if (nrow(headers) < 2 || ncol(headers) == 0) {
    stop(
      "Wide-format sheets must include two populated header rows.",
      call. = FALSE
    )
  }
  header1 <- as.character(unlist(headers[1, , drop = TRUE]))
  header2 <- as.character(unlist(headers[2, , drop = TRUE]))

  if (all(is.na(header1)) && all(is.na(header2))) {
    stop("Header rows are empty; add variable and replicate labels before uploading.", call. = FALSE)
  }
  
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

  if (length(measure_cols) == 0) {
    stop(
      "No measurement columns detected under the second header row.",
      call. = FALSE
    )
  }
  
  data <- readxl::read_excel(path, sheet = sheet, skip = 2, col_names = clean_names)

  data <- validate_uploaded_dataframe(data)
  
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
