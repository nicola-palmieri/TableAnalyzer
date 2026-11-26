# Run once with source("scripts/install_dependencies.R") or Rscript.
local({
  sources <- vapply(sys.frames(),function(frame) {
    value <- frame$ofile
    if(is.character(value) && length(value)==1L && file.exists(value)) value else ""
  },character(1))
  sources <- sources[nzchar(sources)]
  script <- if(length(sources)) tail(sources,1L) else sub("^--file=", "", grep("^--file=",commandArgs(),value=TRUE)[1])
  if (is.na(script) || !file.exists(script)) script <- "scripts/install_dependencies.R"
  app_dir <- normalizePath(file.path(dirname(script),".."),winslash="/",mustWork=TRUE)
  packages <- c("bslib","car","commonmark","dplyr","DT","emmeans","fitdistrplus","flextable",
    "GGally","ggplot2","ggrepel","htmltools","janitor","lme4","lmerTest","officer","patchwork",
    "purrr","ragg","readxl","rlang","scales","shiny","skimr","stringr","tibble","tidyr",
    "tidyselect","xml2","zoo")
  missing <- packages[!vapply(packages,requireNamespace,logical(1),quietly=TRUE)]
  if (length(missing)) install.packages(missing,repos="https://cloud.r-project.org")
  install.packages(file.path(app_dir,"dependencies","NicoShinyAppTemplate"),repos=NULL,type="source")
})
