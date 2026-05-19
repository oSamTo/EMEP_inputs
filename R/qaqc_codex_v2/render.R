## Main entry point: assemble QAQC inputs, generate plots, and optionally render the PDF.
create_qaqc_v2_codex <- function(
  array_id,
  project,
  scenario,
  y,
  species,
  domain,
  folname,
  inv,
  data_source = NA,
  map_yr_uk = NA,
  time_dim,
  emep_version,
  v_EMEP_sec,
  agg_schema = NA,
  tp_scheme = NA,
  dt_sec = get0("dt_sec", inherits = TRUE),
  dt_poll = get0("dt_poll", inherits = TRUE),
  render_pdf = TRUE
) {
  root <- qaqc_v2_project_root()
  domain <- qaqc_v2_normalise_domain(domain)
  folname <- qaqc_v2_abs_path(folname, root)
  ## If data_source was not passed in, infer it from the folder structure after the domain name.
  if (is.na(data_source) || data_source == "") {
    parts <- strsplit(folname, "/", fixed = TRUE)[[1]]
    i_domain <- which(parts == domain)[1]
    data_source <- if (!is.na(i_domain) && length(parts) >= i_domain + 1) {
      parts[i_domain + 1]
    } else {
      NA
    }
  }

  dir.create(
    file.path(folname, "qaqc", paste0("e", y)),
    recursive = TRUE,
    showWarnings = FALSE
  )

  ## Keep dt_poll visible for helper functions that still use the existing global lookup pattern.
  if (!is.null(dt_poll)) {
    assign("dt_poll", dt_poll, envir = .GlobalEnv)
  }
  ## Gather all source tables, rasters, and generated plot paths before rendering.
  summary_paths <- qaqc_v2_summary_paths(
    domain,
    y,
    species,
    folname,
    inv,
    map_yr_uk,
    data_source
  )
  raster_paths <- qaqc_v2_raster_paths(y, species, folname)
  plot_paths <- qaqc_v2_write_plots(
    domain = domain,
    y = y,
    species = species,
    folname = folname,
    summary_paths = summary_paths,
    raster_paths = raster_paths,
    dt_sec = dt_sec,
    data_source = data_source,
    inv = inv
  )

  ## Bundle everything the R Markdown report needs into one params object.
  params <- list(
    project = project,
    scenario = scenario,
    y = y,
    species = species,
    domain = domain,
    folname = folname,
    inv = inv,
    data_source = data_source,
    map_yr_uk = map_yr_uk,
    time_dim = time_dim,
    emep_version = emep_version,
    v_EMEP_sec = v_EMEP_sec,
    agg_schema = agg_schema,
    tp_scheme = tp_scheme,
    dt_sec = dt_sec,
    dt_poll = dt_poll,
    summary_paths = summary_paths,
    raster_paths = raster_paths,
    plot_paths = plot_paths
  )

  ## Name the output PDF consistently with the existing inventory/domain/year convention.
  output_file <- paste0(
    qaqc_v2_poll_model(species, dt_poll),
    "_",
    domain,
    "_",
    y,
    "emis_",
    inv,
    "inv_QAQC_v2_codex.pdf"
  )

  latex_log_dir <- file.path(root, "latex_logs", paste0("array_", array_id))

  dir.create(latex_log_dir, recursive = TRUE, showWarnings = FALSE)

  ## Rendering can be disabled when callers only want paths/data for testing or inspection.
  if (render_pdf) {
    rmarkdown::render(
      input = file.path(root, "R", "QAQC_v2_codex.Rmd"),
      output_file = output_file,
      output_dir = file.path(folname, "qaqc", paste0("e", y)),
      intermediates_dir = latex_log_dir,
      params = params,
      envir = new.env(parent = globalenv())
    )
  }

  invisible(params)
}
