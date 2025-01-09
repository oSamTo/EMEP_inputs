##############################################################################################################
packs <- c("sf","terra","stringr","dplyr","ggplot2","data.table","stats","readxl","ncdf4","lubridate","patchwork")

lapply(packs, require, character.only = TRUE)
##############################################################################################################
options(datatable.showProgress = FALSE)

source("R/UK_functions.R")
source("R/EU_functions.R")

###########################################################
#### SETTING UP WORKSPACE FOR MAKING EMPE MODEL INPUTS ####
###########################################################

run_clock <<- format(now(), "%Y_%m_%d_%H%M%S") # this is for archiving, time of run

# new extended domain to include both UK & Eire
r_dom_1km <<- rast(xmin = -230000, xmax = 750000, ymin = -50000, ymax = 1300000, 
                  res = 1000, crs = "epsg:27700", vals = NA)

# This is the lat long equivalent raster of the UK domain at 1km in BNG
r_dom_0.1 <<- rast(xmin = -13.8, xmax = 4.6, ymin = 49, ymax = 61.5, 
                  res = 0.01, crs = "epsg:4326", vals = NA)

# EU domain
r_dom_EU <<- rast(xmin = -30, xmax = 90, ymin = 30, ymax = 82, 
                   res = 0.1, crs = "epsg:4326", vals = NA)

# the emissions need to be masked to terrestrial cells (plus some coastal cells) - Massimo wants EMEP emissions data on the sea
# the mask is in 0.1 degree, disaggregate to 0.01 so masking can be done
r_dom_terr_10km <<- crop(extend(disagg(rast("data/spatial/Emissions_mask_10km.tif"), fact=10), r_dom_0.1), r_dom_0.1)
r_dom_terr      <<- rast("data/spatial/terrestrial_mask.tif")

# lookup file for sector mapping
dt_sec <<- fread("data/lookups/EMEP_sectors.csv")[!is.na(sec)]

# lookup file for pollutant names
dt_poll <<- fread("data/lookups/pollutant_names.csv")

# lookup file for EMEP country names
dt_iso <<- fread("data/lookups/EMEP_territories.csv")

# EMEP input missing value
EMEP_fillval <<- 9.96920996838687e+36

# EMEP yday numbers, to represent month central days, as taken from an EMEP input file - fixed (?)
v_mday <<- c(14,45,73,104,134,165,195,226,257,287,318,348)
v_yday <<- 1:365

