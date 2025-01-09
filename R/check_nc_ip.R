
source("R/check_nc_funcs.R")

# array number
i_a <- as.numeric(commandArgs(trailingOnly = TRUE)[1]) # array number

# vectors of species, years
v_pollutants <- c("nox", "nh3", "sox", "pm25", "pmco", "voc", "co")
species <- v_pollutants[i_a]

v_years <- c(2021:2022)

# UK & Eire emission years
naei_inv = 2024
map_yr_uk = 2022
map_yr_ie = 2019

# EMEP EU emission years
emep_inv <- 2024

# lookup file for sector mapping
dt_sec <- fread("data/lookups/EMEP_sectors.csv")[!is.na(sec)]

################################################
## ONE FUNCTION WITH OPTIONS FOR UK, EIRE, EU ##
################################################

# it is easier than separate functions below.

for(tp in c("EMEP4UKv4.45","EMEP4UKv5.0","genYr","ukem_genYr")){

evaluateNC(species, v_years, naei_inv, map_yr_uk, map_yr_ie, emep_inv, tp_scheme = tp, dt_sec,
		   summarise_UK = TRUE, plot_UK = TRUE, summarise_EIRE = TRUE, 
		   plot_EIRE = TRUE, summarise_EU = TRUE, plot_EU = TRUE)

}


