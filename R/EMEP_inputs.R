source("R/workspace.R")

###########################################################
####                                                   ####
####    THIS MASTER SCRIPT WILL TAKE NAEI EMISSIONS    ####
#### FILES ALREADY CONVERTED TO LATLONG FOR THE UK     ####
####    0.01 DEGREE AND OUTPUT EMEP NETCDF FILES       ####
####                                                   ####
###########################################################

## choose output directory for the EMEP input files
output_dir <- "C:/FastProcessingSam/EMEP_new_grid/grid_sep22/trial_input"

### CHOOSE WHICH YEARS AND WHICH POLLUTANTS/GHGS TO PUT IN THE NETCDF ### (NO pm10, only pmco)
v_years <- 2020 # what emissions years to process
v_pollutants <- c("nox") # "nox","nh3","sox","pm2.5","pmco","co","nmvoc", "cd", "cu", "ni", "pb", "zn"

#### PROCESSING ####
# map_yr_uk = what year is the NAEI spatial distribution for the data: 2018/2019/2020
# map_yr_ie = what year is the MapEire spatial distribution for the data: 2016/2019
# naei_inv = which inventory compilation year to use

EMEPinputUK(v_years, v_pollutants, time_scale = "month",
            naei_inv = 2020, map_yr_uk = 2020, map_yr_ie = 2019, output_dir)


EMEPinputEU(v_years, v_pollutants, time_scale = "month",
            emep_inv = 2020, map_yr = 2020, output_dir)


downloadGriddedEMEPemissions(pollutant = "sox", report_year = 2022, emis_year = 2020)
