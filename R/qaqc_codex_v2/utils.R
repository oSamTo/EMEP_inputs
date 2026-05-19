## Load the packages used across the QAQC v2 helper files.
qaqc_v2_load_packages <- function() {
  suppressPackageStartupMessages({
    library(data.table)
    library(ggplot2)
    library(terra)
    library(sf)
  })
}

qaqc_v2_load_packages()

## Find the repository root so later code can build stable file paths.
qaqc_v2_project_root <- function() {
  root <- "/gws/ssde/j25b/ceh_generic/samtom/EMEP_inputs"
  if (file.exists(file.path(root, "run.R"))) {
    return(root)
  }
  root <- normalizePath(getwd(), mustWork = TRUE)
  while (!file.exists(file.path(root, "run.R"))) {
    parent <- dirname(root)
    if (identical(parent, root)) {
      stop("Could not find project root containing run.R")
    }
    root <- parent
  }
  root
}

## Convert the different domain spellings used by scripts into one standard value.
qaqc_v2_normalise_domain <- function(domain) {
  domain <- toupper(domain)
  if (domain %in% c("UK", "UKEIRE", "UK/EIRE")) {
    return("UKEIRE")
  }
  if (domain %in% c("EU", "EMEP-EU", "EMEPEU")) {
    return("EU")
  }
  if (domain %in% c("GLOBAL", "GLOBE", "WORLD")) {
    return("GLOBAL")
  }
  stop("domain must be one of UKEIRE, EU or GLOBAL")
}

## Make relative paths safe by resolving them from the project root.
qaqc_v2_abs_path <- function(path, root = qaqc_v2_project_root()) {
  path <- as.character(path)[1]
  if (is.na(path) || path == "") {
    return(NA_character_)
  }
  if (grepl("^/", path)) {
    return(path)
  }
  file.path(root, gsub("^(\\.\\./)+", "", path))
}

## Translate the CEH pollutant name to the EMEP model name when a lookup table is available.
qaqc_v2_poll_model <- function(species, dt_poll = NULL) {
  if (is.null(dt_poll)) {
    dt_poll <- get0("dt_poll", inherits = TRUE)
  }
  if (!is.null(dt_poll) &&
      all(c("ceh_poll", "emep_model") %in% names(dt_poll)) &&
      species %in% dt_poll[, ceh_poll]) {
    return(dt_poll[ceh_poll == species, emep_model][1])
  }
  species
}

## Read a CSV if it exists; otherwise return an empty data.table so callers can keep going.
qaqc_v2_read_csv <- function(path) {
  if (is.null(path) || is.na(path) || !file.exists(path)) {
    return(data.table::data.table())
  }
  data.table::fread(path)
}

## Source the QAQC v2 files in dependency order.
qaqc_v2_source <- function(root = qaqc_v2_project_root()) {
  files <- c("utils.R", "data.R", "plots.R", "render.R")
  for (file in files) {
    path <- file.path(root, "R", "qaqc_codex_v2", file)
    if (!file.exists(path)) {
      stop("Missing QAQC v2 source file: ", path)
    }
    source(path)
  }
  invisible(TRUE)
}
