##############################################################################################################
list.of.packages <- c("sf","terra","stringr","dplyr","ggplot2","data.table","stats","readxl","ncdf4","lubridate")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)
lapply(list.of.packages, require, character.only = TRUE)
##############################################################################################################
options(datatable.showProgress = FALSE)
###########################################################
#### SETTING UP WORKSPACE FOR MAKING EMPE MODEL INPUTS ####
###########################################################

# coordinate reference systems required #
#LL <<- st_crs(4326) # For EMEP European data
#BNG <<- st_crs(27700)  # NAEI data in British National Grid

# new extended domain to include both UK & Eire
r_dom_1km <<- rast(xmin = -230000, xmax = 750000, ymin = -50000, ymax = 1300000, 
                  res = 1000, crs = "epsg:27700", vals = NA)

# This is the lat long equivalent raster of the UK domain at 1km in BNG
r_dom_0.1 <<- rast(xmin = -13.8, xmax = 4.6, ymin = 49, ymax = 61.5, 
                  res = 0.01, crs = "epsg:4326", vals = NA)

# the emissions need to be masked to terrestrial cells (plus some coastal cells) - Massimo wants EMEP emissions data on the sea
# the mask is in 0.1 degree, disaggregate to 0.01 so masking can be done
r_dom_terr_10km <<- crop(extend(disagg(rast("data/spatial/Emissions_mask_10km.tif"), fact=10), r_dom_0.1), r_dom_0.1)
r_dom_terr      <<- rast("data/spatial/terrestrial_mask.tif")

# lookup file for sector mapping
dt_sec <<- fread("data/lookups/EMEP_sectors.csv")[!is.na(EMEP_sec)]

# EMEP input missing value
EMEP_fillval <<- 9.96920996838687e+36

# EMEP yday numbers as taken from an EMEP input file - fixed (?)
v_yday <<- c(14,45,73,104,134,165,195,226,257,287,318,348)
