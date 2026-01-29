######################################################
####                                              ####
####    FILE TO SET PACKAGES, SOURCE FUNCTIONS,   ####
####  SET GLOBAL OBJECTS NEEDED THROUGHOUT - ETC. ####
####                                              ####
######################################################

###############################################################################
packs <- c(
  "sf",
  "terra",
  "stringr",
  "dplyr",
  "ggplot2",
  "data.table",
  "purrr",
  "cowplot",
  "abind",
  "stats",
  "readxl",
  "ncdf4",
  "lubridate",
  "patchwork",
  "tidyterra",
  "knitr",
  "kableExtra",
  "janitor"
)

lapply(packs, require, character.only = TRUE)

###############################################################################
options(datatable.showProgress = FALSE)

source(paste0("R/", emep_version, "/UK_", emep_version, "_functions.R"))
source(paste0("R/", emep_version, "/EU_", emep_version, "_functions.R"))
source("R/QAQC_functions.R")

###########################################################
#### SETTING UP WORKSPACE FOR MAKING EMEP MODEL INPUTS ####
###########################################################

run_clock <<- format(now(), "%Y_%m_%d") # this is for archiving, time of run

# new extended domain to include both UK & Eire
r_dom_1km <<- rast(
  xmin = -230000,
  xmax = 750000,
  ymin = -50000,
  ymax = 1300000,
  res = 1000,
  crs = "epsg:27700",
  vals = NA
)

# This is the lat long equivalent raster of the UK domain at 1km in BNG
# good for v4.36, v4.45, v5.0
r_dom_UKIE <<- rast(
  xmin = -13.8,
  xmax = 4.6,
  ymin = 49,
  ymax = 61.5,
  res = 0.01,
  crs = "epsg:4326",
  vals = NA
)

# plot domain for UK, slightly larger.
r_dom_ukplot <<- rast(
  xmin = -13.8,
  xmax = 5.4,
  ymin = 48.1,
  ymax = 62,
  res = 0.01,
  crs = "epsg:4326",
  vals = NA
)

# EU domain
r_dom_EU <<- rast(
  xmin = -30,
  xmax = 90,
  ymin = 30,
  ymax = 82,
  res = 0.1,
  crs = "epsg:4326",
  vals = NA
)

# shape for plotting.
# disable some spherical geometry in sf() that causes plot issues
suppressWarnings(sf::sf_use_s2(FALSE))

sf_world <<- st_read("data/spatial/world/TM_WORLD_BORDERS-0.3.shp")
st_crs(sf_world) <- "epsg:4326"
sf_world <- st_make_valid(sf_world)
sf_uk <<- st_crop(sf_world, ext(r_dom_ukplot))
sf_eu <<- st_crop(sf_world, ext(r_dom_EU))
sf_ie <<- sf_uk[sf_uk$NAME == "Ireland", ]

# reinstate spherical geometry in sf()
suppressWarnings(sf::sf_use_s2(TRUE))

# the emissions need to be masked to terrestrial cells (plus some coastal cells)
# Massimo wants EMEP emissions data on the sea
# the mask is in 0.1 degree, disaggregate to 0.01 so masking can be done
# UK data does not have IOM, but EMEP only has shipping - use the
# mask with no IOM.
r_dom_terr_10km <<- crop(
  extend(
    disagg(rast("data/spatial/Emissions_mask_10km_noIOM.tif"), fact = 10),
    r_dom_UKIE
  ),
  r_dom_UKIE
)
r_dom_terr <<- rast("data/spatial/terrestrial_mask.tif")

# lookup files for sector mapping
dt_sec <<- fread("data/lookups/EMEP_sectors.csv")[!is.na(sec)]
dt_SNAPGNFR <<- fread("data/lookups/SNAP_to_GNFR.csv")
dt_GNFRSNAP <<- fread("data/lookups/GNFR_to_SNAP.csv")

# lookup file for pollutant names
dt_poll <<- fread("data/lookups/pollutant_names.csv")

# lookup file for EMEP country names - taken from EMEPv5.0 file
dt_iso <<- readRDS("data/lookups/dt_iso.rds")
dt_iso <<- dt_iso[!is.na(ISO_char)]
r_iso <<- rast("data/spatial/iso_map.tif") # use this to summarise other data
r_iso <- crop(extend(r_iso, r_dom_EU), r_dom_EU)
# Alter Kazakhstan from code 92 --> 53
r_iso[r_iso == 92] <- 53
# codes 57 (Malta) & 62 (Monaco) are not in the ISO raster (too small)

# EMEP input missing value
EMEP_fillval <<- 9.96920996838687e+36

# EMEP yday numbers, to represent month central days,
# as taken from an EMEP input file - fixed (?)
v_mday <<- c(14, 45, 73, 104, 134, 165, 195, 226, 257, 287, 318, 348)
v_yday <<- 1:365

###############################################################################
#### function to return molecular weight of the species being processed
get_mol_weight <- function(species) {
  if (species == "") {
    c(,, "sox", "pm25", "pmco", "co", "voc")
  } else if (species == "nox") {
    mw <- 46
  } else if (species == "nh3") {
    mw <- 17
  } else if (species == "sox") {
    mw <- 64
  } else if (species == "pm25") {
    mw <- NA
  } else if (species == "pmco") {
    mw <- NA
  } else if (species == "co") {
    mw <- 28
  } else if (species == "voc") {
    mw <- NA
  } else if (species == "pm10") {
    mw <- NA
  } else if (species == "cu") {
    # metals etc....
    mw <- NA
  } else if (species == "hcl") {
    # metals etc....
    mw <- 36.5
  }

  return(mw)
}
