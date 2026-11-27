# Identical startup bridge in each app; common implementation lives in dependencies/.
.vetmeduni_load_dependency <- function() {
  package <- "VetmeduniAppTemplate"
  source_files <- vapply(sys.frames(), function(frame) {
    value <- frame$ofile
    if (is.character(value) && length(value) == 1L) value else ""
  }, character(1))
  source_files <- source_files[nzchar(source_files) & file.exists(source_files)]
  override <- Sys.getenv("VETMEDUNI_APP_TEMPLATE_PATH", "")
  starts <- unique(c(if (nzchar(override)) dirname(override),
    if (length(source_files)) dirname(normalizePath(tail(source_files, 1), winslash = "/")), getwd()))
  for (start in starts) {
    directory <- normalizePath(start, winslash = "/", mustWork = TRUE)
    repeat {
      loader <- file.path(directory, "dependencies", "load_VetmeduniAppTemplate.R")
      if (file.exists(loader)) {
        source(loader, local = TRUE)$value(file.path(dirname(loader), package))
        return(invisible(NULL))
      }
      parent <- dirname(directory)
      if (identical(parent, directory)) break
      directory <- parent
    }
  }
  if (!requireNamespace(package, quietly = TRUE)) stop("VetmeduniAppTemplate is required; see README.md.", call. = FALSE)
  suppressPackageStartupMessages(library(package, character.only = TRUE))
}
.vetmeduni_load_dependency()
rm(.vetmeduni_load_dependency)
