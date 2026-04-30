#######################################################
####                                               ####
####    FILE TO SET RUN CHOICES, E.G THE YEAR,     ####
####  THE POLLUTANT VECTOR, EMEP MODEL VERSION ETC ####
####                                               ####
#######################################################
require(data.table)

########################
## EMEP MODEL DOMAINS ##
# set which domains to run.
run_domain <<- c("GLOBAL") # "UKEIRE", "EU", "GLOBAL"

if (length(run_domain) > 1) {
  stop("Choose exactly one domain to construct.")
}

if (!(run_domain %in% c("UKEIRE", "EU", "GLOBAL"))) {
  stop("Choose a valid domain to construct.")
}

# then nominate the data sources.
# ! THESE NEED TO BE IDENTICAL TO THE FOLDER NAMES IN 'inventory_processor' ! #
# UKEIRE file = "NAEI" (actually uses MapEire & EMEP as well)
# EU file     = "EMEP" (to be expanded to EDGAR, CAMS)
# GLOBAL      = "HTAP" (to be expanded to EDGAR)
run_source <<- c("HTAP")

# For now the run_source is linked to the domain, and only one is chosen,
# but we could have multiple data sources for the same domain.
## possibility to create blended source inputs?

########################
## EMEP MODEL VERSION ##

## EMEP model version
# this makes the input to the model version.
emep_version <<- "v5.0" # v4.36, v4.45 , v5.0

if (length(emep_version) > 1) {
  stop("Choose exactly one EMEP model version to run.")
}

#######################
## OUTPUT QAQC FILES ##
output_QAQC <<- FALSE

#################################
## EMISSIONS & INVENTORY YEARS ##

## vectors of emissions years and pollutants to run ##
v_years <- c(2020) # what emissions years to process
v_pollutants <- c("nox", "nh3", "sox", "pm25", "pmco", "co", "voc")
# "nox","nh3","sox","pm25","pmco","co","voc", "hcl",
# "cd", "cu", "ni", "pb", "zn" - CEH names, not EMEP model

# The following inventory choice is effectively the sub-folder of data choice.

# UK & Eire emission years
naei_inv <- 2025 # naei_inv  = which inventory compilation year to use
map_yr_uk <- 2023 # map_yr_uk = year of NAEI spatial dist. for the data
map_yr_ie <- 2019 # map_yr_ie = year of MapEire spatial dist. for the data

# EMEP EU emission years
emep_inv <- 2025 # emep_inv  = which inventory compilation year to use
# emep_map_yr <- 2021

# Zhang et al (2022) years
zhang_inv <- 2022 # always 2022. Made in 2022. Map comes from emissions year.

if (zhang_inv != 2022) {
  stop("Zhang inventory has to be 2022. Change!")
}

# HTAP emissions version
htap_inv <- "v32" # only option at the moment is 'v32'

if (htap_inv != "v32") {
  stop("Change HTAP inventory version. Only v32 is available at the moment.")
}

#########################
## TEMPORAL PARAMETERS ##

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

##########################
## COUNTRY AGGREGATIONS ##

## aggregation schema
agg_schema <- "allISO" # allISO = separate ISO inpus. oneGRID = one file.

#################
## STOP CHECKS ##

# break if EU is monthly AND ISO - the files are too big. THIS WILL CHANGE
if (run_domain == "EU" && tp_scheme != "annual" && agg_schema == "allISO") {
  stop("Don't run sub-annual EU on ISO codes. Files too large.")
}
# break if EU is annual and agg schema is one EU - nothing accepts it.
if (run_domain == "EU" && tp_scheme == "annual" && agg_schema == "oneGRID") {
  stop("Don't run annual EU with one aggregated surface.")
}
# break if GLOB is monthly AND ISO - the files are too big. THIS WILL CHANGE
if (run_domain == "GLOBAL" && tp_scheme != "annual" && agg_schema == "allISO") {
  stop("Don't run sub-annual GLOBAL on ISO codes. Files too large.")
}
# break if domain is GLOBAL and there is more than 1 year - run time cant cope.
if (run_domain == "GLOBAL" && length(v_years) > 1) {
  stop("Run max 1 year for GLOBAL domain. Run time too long. Not enough cores.")
}
# break if GLOB is HTAP_32 and the year is outside of 2000:2020
if (
  run_source == "HTAP" && htap_inv == "v32" && any(!c(v_years %in% 2000:2020))
) {
  stop("HTAP_v32 GLOBAL only runs from 2000 to 2020. Check year chosen!")
}


######################
## OUTPUT LOCATIONS ##

## choose output directory for the EMEP input files
# STANDARD/NFC = paste0("/gws/nopw/j04/ceh_generic/samtom/EMEP_inputs/outputs/EMEP4UK", # nolint
# 					    emep_version,"/inv",naei_inv)

output_project <- "HTAP_32"
v_scenarios <- "BASE" # paste0("SGS",6) # names or 'BASE'

# if there are errors re no "alternate_emissions.csv" file, make sure this
# has been considered in the set up. Make an empty one.

##########################
## ALTERNATIVE EMISSIONS ##
# a table to nominate file locations for different emissions ;
# e.g. older years, different projects, and so on.

dt_alt_emis <- fread(paste0(
  "/gws/ssde/j25b/ceh_generic/samtom/EMEP_inputs/data/alt_emis/",
  output_project,
  "/alternate_emissions.csv"
))

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
