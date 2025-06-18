#######################################################
####                                               ####
####    FILE TO SET RUN CHOICES, E.G THE YEAR,     ####
####  THE POLLUTANT VECTOR, EMEP MODEL VERSION ETC ####
####                                               ####
#######################################################
require(data.table)

########################
## EMEP MODEL VERSION ##

## EMEP model version
# this makes the input to the model version.
emep_version <- "v4.36" # v4.36, v4.45 , v5.0

#######################
## OUTPUT QAQC FILES ##
output_QAQC <- TRUE

#################################
## EMISSIONS & INVENTORY YEARS ##

## vectors of emissions years and pollutants to run ##
v_years <- c(2021) # what emissions years to process
v_pollutants <- c("nox", "nh3", "sox", "pm25", "pmco", "co", "voc") # "nox","nh3","sox","pm25","pmco","co","voc", "cd", "cu", "ni", "pb", "zn" - CEH names, not EMEP model

# UK & Eire emission years
naei_inv <- 2023 # naei_inv  = which inventory compilation year to use
map_yr_uk <- 2021 # map_yr_uk = what year is the NAEI spatial distribution for the data
map_yr_ie <- 2019 # map_yr_ie = what year is the MapEire spatial distribution for the data: 2019

# EMEP EU emission years
emep_inv <- 2023 # emep_inv  = which inventory compilation year to use
#emep_map_yr <- 2021

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
tp_scheme <- c("EMEP4UKv4.36")

# however, reset the tp_scheme if time_dim is 'annual' - either UK or EU
# we dont use temporal profiling for the annual total inputs
if (time_dim == "annual") {
  tp_scheme <- "annual"
}

##########################
## COUNTRY AGGREGATIONS ##

## aggregation schema
uk_agg_schema <- "allISO" # allISO = separate ISO inpus for UK/IE/SEA. oneUKIE = one file.
eu_agg_schema <- "allISO" # allISO = separate ISO inputs. oneEU = one EU file.

# break if EU is monthly AND ISO - the files are too big. THIS WILL CHANGE
if (tp_scheme != "annual" & eu_agg_schema == "allISO") {
  stop("Don't run EU monthly on ISO codes.")
}
# break if EU is annual and agg schema is one EU - nothing accepts it.
if (tp_scheme == "annual" & eu_agg_schema == "oneEU") {
  stop("Don't run EU annual with one aggregated surface.")
}

######################
## OUTPUT LOCATIONS ##

## choose output directory for the EMEP input files
# STANDARD/NFC = paste0("/gws/nopw/j04/ceh_generic/samtom/EMEP_inputs/outputs/EMEP4UK",
#					    emep_version,"/inv",naei_inv)

output_project <- "EPA_4.36"
v_scenarios <- "BASE" # paste0("SGS",6) # names or 'BASE'

###########################
## ALTERNATIVE EMISSIONS ##
# a table to nominate file locations for different emissions - e.g. older years, different projects, and so on.

dt_alt_emis <- fread(paste0(
  "/gws/nopw/j04/ceh_generic/samtom/EMEP_inputs/data/alt_emis/",
  output_project,
  "/alternate_emissions.csv"
))
# use empty structure when no alt emissions
#dt_alt_emis <- data.table(projectName = character(), scenarioName = character(),
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
