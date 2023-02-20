source("R/workspace.R")
source("R/emissions_functions.R")

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
v_years <- 2019
v_pollutants <- c("nox") 
# "nox","nh3","sox","pm2.5","pmco","co","nmvoc", "cd", "cu", "ni", "pb", "zn"
map_yr <- 2019 # what year is the NAEI spatial distribution for the data: 2018/2019/2020

# Oct '22: there is no choice for sector classification anymore.

#### PROCESSING ####
creatEMEPinput(v_years, v_pollutants, time_scale = "month", map_yr, output_dir)
