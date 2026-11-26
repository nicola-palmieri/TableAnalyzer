# Create an app with the same module structure as the maintained applications.
create_app <- function(path, name = "MyAnalyzer", main = "My", accent = "Analyzer", version = "v0.1.0") {
  strings <- list(path = path, name = name, main = main, accent = accent, version = version)
  for (argument in names(strings)) {
    value <- strings[[argument]]
    if (!is.character(value) || length(value) != 1L || is.na(value) || (!nzchar(value) && argument != "accent"))
      stop("`", argument, "` must be one valid string (only `accent` may be empty).", call. = FALSE)
  }
  path <- path.expand(path)
  if (file.exists(path) && !dir.exists(path)) stop("Destination is an existing file.", call. = FALSE)
  if (dir.exists(path) && length(list.files(path, all.files = TRUE, no.. = TRUE)))
    stop("Destination must be empty; existing files are never overwritten.", call. = FALSE)
  template <- system.file("templates", package = "NicoShinyAppTemplate", mustWork = TRUE)
  files <- list.files(template, recursive = TRUE, all.files = TRUE, no.. = TRUE)
  substitutions <- c(APP_NAME_R = encodeString(name, quote = '"'),
    BRAND_MAIN_R = encodeString(main, quote = '"'), BRAND_ACCENT_R = encodeString(accent, quote = '"'),
    APP_VERSION_R = encodeString(version, quote = '"'))
  if (!dir.exists(path) && !dir.create(path, recursive = TRUE))
    stop("Could not create destination directory.", call. = FALSE)
  for (file in files) {
    text <- paste(readLines(file.path(template, file), encoding = "UTF-8", warn = FALSE), collapse = "\n")
    for (key in names(substitutions)) text <- gsub(paste0("{{", key, "}}"), substitutions[[key]], text, fixed = TRUE)
    target <- file.path(path, file)
    dir.create(dirname(target), recursive = TRUE, showWarnings = FALSE)
    writeLines(text, target, useBytes = TRUE)
  }
  invisible(normalizePath(path, winslash = "/", mustWork = TRUE))
}
