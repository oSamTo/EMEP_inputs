#### --------------------------------------------- ####
##--                                               --##
##--    FILE TO SET RUN CHOICES, E.G THE YEAR,     --##
##--  THE POLLUTANT VECTOR, EMEP MODEL VERSION ETC --##
##--                                               --##
#### --------------------------------------------- ####
suppressPackageStartupMessages({
  require(data.table)
})

# ---------------- #
# OUTPUT LOCATIONS #

# choose a project name and any scenarios.
# This will form the start of the output location, i.e.
# "ceh_generic/samtom/EMEP_inputs/outputs/", project,"/", scenario
output_project <- "EDGAR"
v_scenarios <- "BASE" # paste0("SGS",6) # names or 'BASE'

# ------------------ #
# EMEP MODEL DOMAINS #
# set which domains to run.
run_domain <<- c("GLOBAL") # "UKEIRE", "EU", "GLOBAL"

# then nominate the data sources.
# ! THESE NEED TO BE IDENTICAL TO THE FOLDER NAMES IN 'inventory_processor' ! #
# UKEIRE file = "NAEI" (actually uses MapEire & EMEP as well)
# EU file     = "EMEP" (to be expanded to EDGAR, CAMS)
# GLOBAL      = "HTAP" or "EGDAR"
run_source <<- c("EDGAR")

# For now the run_source is linked to the domain, and only one is chosen,
# but we could have multiple data sources for the same domain.
## possibility to create blended source inputs?

# ------------------ #
# EMEP MODEL VERSION #
# this makes the input to the model version.
emep_version <<- "v5.0" # v4.36, v4.45 , v5.0

# ----------------- #
# OUTPUT QAQC FILES #
output_QAQC <<- TRUE

# ------------------------------------ #
# EMISSIONS & INVENTORY YEARS/VERSIONS #

## vectors of emissions years and pollutants to run ##
v_years <- c(2022) # what emissions years to process
v_pollutants <- c("nox", "nh3", "sox", "pm25", "pmco", "co", "voc")
# "nox","nh3","sox","pm25","pmco","co","voc", "hcl",
# "cd", "cu", "ni", "pb", "zn" - CEH names, not EMEP model

# choose whether to use a static map or dynamic map year (i.e. the latest
# map scaled or the map pertinent to the emissions year)
dynamic_map_uk <<- c(FALSE)

# choose what SNAP sectors to split out to GNFR in the UK data.
# specifically UK, specifically SNAPs.
# REMEMBER, in processed data:
#      * SNAP08 is assigned to I_Offroad and H_Aviation & G_Shipping are empty
#      * SNAP10 is assigned to K_AgriLivestock and L_AgriOther is empty
v_snap_split <- NULL

# The following inventory choice is effectively the sub-folder of data choice.

# UK & Eire emission years
naei_inv <- 2025 # naei_inv  = which inventory compilation year to use
map_yr_uk <- 2023 # map_yr_uk = year of NAEI spatial dist. for the data
map_yr_ie <- 2019 # map_yr_ie = year of MapEire spatial dist. for the data

if (run_source == "NAEI" && naei_inv < 2025 && map_yr_uk != (naei_inv - 2)) {
  stop("If NAEI inventory is before 2025, the map year must be the latest.")
}

# EMEP EU emission years
emep_inv <- 2025 # emep_inv  = which inventory compilation year to use
# emep_map_yr <- 2021

# Zhang et al (2022) years
zhang_inv <- 2022 # always 2022. Made in 2022. Map comes from emissions year.

# HTAP emissions version
htap_inv <- "v32" # only option at the moment is 'v32'

# EDGAR emissions version
edgar_inv <- "v81" # only option at the moment is 'v81'

# ------------------- #
# TEMPORAL PARAMETERS #

## time dimension to process the data into ##
time_dim <- "annual" # annual, month, yday

## EMEP sectors to put into the netcdf - see dt_sec for choice (standard = 1:13)
# anything before v4.45 will default to SNAPs 1:11
if (emep_version == "v4.36") {
  v_EMEP_sec <- 1:11 # SNAPS
} else {
  v_EMEP_sec <- 1:13 # GNFR index
}

## temporal profile schema ##
# UK can be: "EMEP4UKv4.36", "EMEP4UKv4.45" , "EMEP4UKv5.0", "ukem_genYr", "ukem_2017:2023", "test"
# EU can be: "EMEP4UKv4.36", "EMEP4UKv4.45" , "EMEP4UKv5.0", "test"  (EDGAR in the future)
# HTAP can be: "EMEP4UKv5.5"
tp_scheme <- c("EMEP4UKv5.0")

# however, reset the tp_scheme if time_dim is 'annual' - either UK or EU
# we dont use temporal profiling for the annual total inputs
if (time_dim == "annual") {
  tp_scheme <- "annual"
}

# -------------------- #
# COUNTRY AGGREGATIONS #

## aggregation schema
agg_schema <- "allISO" # allISO = separate ISO inpus. oneGRID = one file.

# ----------- #
# STOP CHECKS #

# stop of run domains are wrong
if (length(run_domain) > 1) {
  stop("Choose exactly one domain to construct.")
}

if (!(run_domain %in% c("UKEIRE", "EU", "GLOBAL"))) {
  stop("Choose a valid domain to construct.")
}

# only choose 1 EMEP version at a time.
if (length(emep_version) > 1) {
  stop("Choose exactly one EMEP model version to run.")
}

# stop if Zhang is not 2022
if (zhang_inv != 2022) {
  stop("Zhang inventory has to be 2022. Change!")
}

# stop if HTAP is not v32 (only one processed as of June 2026)
if (htap_inv != "v32") {
  stop("Change HTAP inventory version. Only v32 is available at the moment.")
}

# stop if EDGAR is not v81 (only one processed as of June 2026)
if (edgar_inv != "v81") {
  stop("Change EDGAR inventory version. Only v81 is available at the moment.")
}

# break if EU is monthly AND ISO - the files are too big. THIS WILL CHANGE
if (run_domain == "EU" && tp_scheme != "annual" && agg_schema == "allISO") {
  stop("Don't run sub-annual EU on ISO codes. Files too large.")
}
# break if EU is annual and agg schema is one EU - nothing accepts it.
if (run_domain == "EU" && tp_scheme == "annual" && agg_schema == "oneGRID") {
  stop("Don't run annual EU with one aggregated surface.")
}

# break if GLOBAL is not EDGAR or HTAP
if (run_domain == "GLOBAL" && !(run_source %in% c("HTAP", "EDGAR"))) {
  stop("Need to choose either HTAP or EDGAR as source data for GLOBAL domain.")
}

# break if GLOB is monthly AND ISO - the files are too big. THIS WILL CHANGE
if (run_domain == "GLOBAL" && tp_scheme != "annual" && agg_schema == "allISO") {
  stop("Don't run sub-annual GLOBAL on ISO codes. Files too large.")
}
# break if domain is GLOBAL and there is more than 1 year - run time cant cope.
if (run_domain == "GLOBAL" && length(v_years) > 1) {
  stop("Run max 1 year for GLOBAL domain. Run time too long. Not enough cores.")
}
# break if GLOB is HTAP_v32 and the year is outside of 2000:2020
if (
  run_source == "HTAP" && htap_inv == "v32" && any(!c(v_years %in% 2000:2020))
) {
  stop("HTAP_v32 GLOBAL only runs from 2000 to 2020. Check year chosen!")
}

# break if GLOB is EDGAR_v81 and the year is outside of 2000:2022
if (
  run_source == "EDGAR" && edgar_inv == "v81" && any(!c(v_years %in% 2000:2022))
) {
  stop("EDGAR_v81 GLOBAL only runs from 2000 to 2022. Check year chosen!")
}

# break if SNAP split is anything but NULL, 8 or 10.

if (any(v_snap_split %in% c(1:7, 9, 11))) {
  stop("Splitting SNAP sectors can only apply to SN08 & SNAP10. Check!")
}

# --------------------- #
# ALTERNATIVE EMISSIONS #
# a table to nominate file locations for different emissions ;
# e.g. older years, different projects, and so on.

fol_alt_emis <- paste0(
  "/gws/ssde/j25b/ceh_generic/samtom/EMEP_inputs/data/alt_emis/",
  output_project
)

if (dir.exists(fol_alt_emis)) {
  dt_alt_emis <- fread(file.path(fol_alt_emis, "alternate_emissions.csv"))
} else {
  dt_alt_emis <- fread(
    "/gws/ssde/j25b/ceh_generic/samtom/EMEP_inputs/data/alt_emis/alternate_emissions.csv" #nolint
  )
}

# use empty structure when no alt emissions
# dt_alt_emis <- data.table(projectName = character(), scenarioName = character(),
#                          poll = character(), iso = character(),
#                          diff_or_pt = character(), sector = character(),
#                          fname = character(), loc = character())

# do some checks on the alternative emissions files.
if (nrow(dt_alt_emis) > 0) {
  if (any(!(dt_alt_emis[, iso] %in% c("GB", "IE")))) {
    stop("ISO code error in alternative emissions")
  }
  if (any(!(dt_alt_emis[, poll] %in% v_pollutants))) {
    stop("Pollutant error in alternative emissions")
  }
  if (any(!(dt_alt_emis[, diff_or_pt] %in% c("diff", "pt")))) {
    stop("Alternative emissions need to be 'diff' or 'pt'")
  }
  # if(any(!(dt_alt_emis[,sector] %in% dt_sec[, GNFRlong]))) stop("Sector name error in alternative emissions")
} else {
  print("No alternative emissions files nominated.")
}
