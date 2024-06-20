
source("R/check_nc_funcs.R")

# array number
i_a <- as.numeric(commandArgs(trailingOnly = TRUE)[1]) # array number

# vectors of species, years
v_pollutants <- c("nox", "nh3", "sox", "pm25", "pmco", "voc", "co")
species <- v_pollutants[i_a]

v_years <- c(seq(1960,2005,5),2006:2021)

# UK & Eire emission years
naei_inv = 2023
map_yr_uk = 2021
map_yr_ie = 2019

# EMEP EU emission years
emep_inv <- 2023

# lookup file for sector mapping
dt_sec <- fread("data/lookups/EMEP_sectors.csv")[!is.na(sec)]

################################################
## ONE FUNCTION WITH OPTIONS FOR UK, EIRE, EU ##
################################################

# it is easier than separate functions below.

evaluateNC(species, v_years, naei_inv, map_yr_uk, map_yr_ie, emep_inv, tp_scheme = "genYr", dt_sec,
		   summarise_UK = TRUE, plot_UK = TRUE, summarise_EIRE = FALSE, 
		   plot_EIRE = FALSE, summarise_EU = FALSE, plot_EU = FALSE)


