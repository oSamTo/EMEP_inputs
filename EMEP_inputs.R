source("R/workspace.R")

###########################################################
####                                                   ####
####    THIS MASTER SCRIPT WILL TAKE NAEI EMISSIONS    ####
#### FILES ALREADY CONVERTED TO LATLONG FOR THE UK     ####
####    0.01 DEGREE AND OUTPUT EMEP NETCDF FILES       ####
####                                                   ####
###########################################################

### CHOOSE WHICH YEARS AND WHICH POLLUTANTS/GHGS TO PUT IN THE NETCDF ### (NO pm10, only pmco)
#y <- 2019
#species <- "sox"

i_a <- as.numeric(commandArgs(trailingOnly = TRUE)[1]) # array number
# i_a <- 1
v_years <- c(2021:2022) # what emissions years to process
v_pollutants <- c("nox","nh3","sox","pm25","pmco","co","nmvoc") # "nox","nh3","sox","pm25","pmco","co","nmvoc", "cd", "cu", "ni", "pb", "zn" - CEH names, not EMEP model
species <- v_pollutants[i_a]

time_dim <- "month" # annual, month, yday
tp_scheme <- "test" # EMEP4UKv4.45 / EMEP4UKv5.0, genYr, 2017:2021, ukem_genYr, ukem_2017:2023 
eu_tp_scheme <- "EMEP4UKv5.0" # EMEP4UKv4.45 / EMEP4UKv5.0 / EDGAR (no EDGAR at the mo)

# UK & Eire emission years
naei_inv = 2024
map_yr_uk = 2022
map_yr_ie = 2019

# EMEP EU emission years
emep_inv <- 2024
#emep_map_yr <- 2021

eu_agg_schema <- "oneEU"
uk_agg_schema <- "NA"

## choose output directory for the EMEP input files
output_dir <- paste0("/gws/nopw/j04/ceh_generic/samtom/EMEP_inputs/outputs/EMEP4UK/inv",naei_inv)

## a table to nominate file locations for different emissions - e.g. older years, different projects, and so on. 
dt_alt_emis <- data.table(poll = c("nh3"), 
                          loc = c("/gws/nopw/j04/ceh_generic/inventory_processor/data"))

# alt_emis = dt_alt_emis

#### PROCESSING ####
# map_yr_uk = what year is the NAEI spatial distribution for the data: 2018/2019/2020
# map_yr_ie = what year is the MapEire spatial distribution for the data: 2016/2019
# naei_inv = which inventory compilation year to use

## UK & EIRE ##
EMEPinputUK(v_years, species, uk_agg_schema = uk_agg_schema, time_dim = time_dim, tp_scheme = tp_scheme, alt_emis = dt_alt_emis,
            naei_inv = naei_inv, map_yr_uk = map_yr_uk, map_yr_ie = map_yr_ie, output_dir)

# EU ##
#EMEPinputEU(v_years, species, eu_agg_schema = eu_agg_schema, time_dim = time_dim,
#            eu_tp_scheme, emep_inv = emep_inv, output_dir)
