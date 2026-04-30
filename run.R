source("R/run_setup.R")
source("R/workspace.R")

##########################################################
####                                                  ####
####  THIS RUN FILE WILL TAKE EMISSIONS FILES ALREADY ####
####    CONVERTED TO LATLONG @ 0.01 or 0.1 DEGREE     ####
####    AND OUTPUT EMEP NETCDF FILES. USE FILE        ####
###   R/RUN_SETUP.R TO MAKE CHOICES FOR DATA INPUTS   ####
####                                                  ####
##########################################################

## For the UK, they are derived from NAEI maps/points, in SNAP sectors.
## For EIRE, they are GNFR maps from the MapEire project, with EMEP data.
## For the EU, they are derived from EMEP/CEIP maps, in GNFR sectors.
## For the globe, they are;
# - derived from global HTAP_v32 sector maps, to GNFR, for each country
# - derived from

# (see /gws/nopw/j04/ceh_generic/inventory_processor)

### CHOOSE OPTIONS FOR RUN IN 'run_setup.R' ###
i_a <<- as.numeric(commandArgs(trailingOnly = TRUE)[1]) # array number
# i_a <<- 1

# create a full table of individual runs. These will form array jobs.
# make output directories based on project name and scenarios (if applicable).
dt_cj <- CJ(
  run_domain,
  run_source,
  v_years,
  time_dim,
  tp_scheme,
  agg_schema,
  output_project,
  v_scenarios
)

if (run_source == "NAEI") {
  dir_inv <- naei_inv
} else if (run_source == "EMEP") {
  dir_inv <- emep_inv
} else if (run_source == "HTAP") {
  dir_inv <- htap_inv
} else {
  stop("Choose a valid data source.")
}

dt_cj[
  ,
  output_dir := paste0(
    "/gws/ssde/j25b/ceh_generic/samtom/EMEP_inputs/outputs/", # nolint
    output_project,
    "/",
    v_scenarios,
    "/",
    run_domain,
    "/",
    run_source,
    "/",
    dir_inv,
    "/EMEP4UK",
    emep_version,
    "/",
    time_dim,
    "/",
    "TP",
    tp_scheme,
    "/",
    agg_schema
  )
]

# subset specifics for array run number
data_source <- dt_cj[i_a, run_source]
y <- dt_cj[i_a, v_years]
time_dim <- dt_cj[i_a, time_dim]
tp_scheme <- dt_cj[i_a, tp_scheme]
agg_schema <- dt_cj[i_a, agg_schema]
project <- dt_cj[i_a, output_project]
scenario <- dt_cj[i_a, v_scenarios]
dir_inv <- dir_inv
folname <- dt_cj[i_a, output_dir]

#######################
#### RUN FUNCTIONS ####

if (run_domain == "UKEIRE") {
  ## UK & EIRE ##
  UKEIRE_functions <- paste0("EMEP_UKEIRE_", emep_version)

  get(UKEIRE_functions)(
    data_source = data_source,
    y = y,
    v_pollutants = v_pollutants,
    time_dim = time_dim,
    v_EMEP_sec = v_EMEP_sec,
    naei_inv = dir_inv,
    map_yr_uk = map_yr_uk,
    map_yr_ie = map_yr_ie,
    folname = folname,
    project = project,
    scenario = scenario,
    tp_scheme = tp_scheme,
    uk_agg_schema = agg_schema,
    dt_alt_emis = dt_alt_emis
  )
} else if (run_domain == "EU") {
  ## EU ##
  # if the tp_scheme is not "annual", "test", "EMEP4UKv4.45" or
  # "EMEP4UKv5.0", skip EU
  if (tp_scheme %in% c("EMEP4UKv4.45", "EMEP4UKv5.0", "annual", "test")) {
    # only the function set sourced by choice of version should be available
    # (e.g. "EU_v5.0_functions.R").
    # still, using a variable function name to be sure.
    EU_functions <- paste0("EMEP_EU_", emep_version)

    # this does mean the arguments need to be the same in each one.
    get(EU_functions)(
      data_source = data_source,
      y = y,
      v_pollutants = v_pollutants,
      time_dim = time_dim,
      v_EMEP_sec = v_EMEP_sec,
      emep_inv = dir_inv,
      folname = folname,
      tp_scheme = tp_scheme,
      eu_agg_schema = agg_schema
    )
  }
} else if (run_domain == "GLOBAL") {
  ## GLOBAL ##
  GLOBAL_functions <- paste0("EMEP_GLOBAL_", emep_version)

  get(GLOBAL_functions)(
    data_source = data_source,
    y = y,
    v_pollutants = v_pollutants,
    time_dim = time_dim,
    v_EMEP_sec = v_EMEP_sec,
    glob_inv = dir_inv,
    folname = folname,
    tp_scheme = tp_scheme,
    global_agg_schema = agg_schema
  )
} else {
  stop("Choose a valid run domain.")
}


## QAQC ##
# this should be lapply over v_pollutants for a separate file.
if (output_QAQC) {
  for (species in v_pollutants) {
    create_qaqc(
      project = project,
      scenario = scenario,
      y = y,
      species = species,
      uk_folname = uk_folname,
      eu_folname = eu_folname,
      map_yr_uk = map_yr_uk,
      naei_inv = naei_inv,
      emep_inv = emep_inv,
      time_dim = time_dim,
      emep_version = emep_version,
      v_EMEP_sec = v_EMEP_sec,
      uk_agg_schema = uk_agg_schema,
      eu_agg_schema = eu_agg_schema,
      tp_scheme = tp_scheme
    )
  }
}
