#########################################
#### PARAMETERS for targets workflow ####
#########################################

## e.g. for map/point data years, inventory year, sectors to aggregate to and so forth. 

## choose output directory for the EMEP input files
output_dir <- "sam_test"

### CHOOSE WHICH YEARS AND WHICH POLLUTANTS/GHGS TO PUT IN THE NETCDF ### (NO pm10, only pmco)
#v_years <- 2020 # what emissions years to process


time_dim <- "month"
tp_fol <- "pre_TEMREG"

naei_inv = 2022
map_yr_uk = 2020
map_yr_ie = 2019

res_crs <- "0.01_LL"
