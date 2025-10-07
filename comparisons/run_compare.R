source("comparisons/compare_functions.R")

##########################################################
####                                                  ####
####     THIS RUN FILE WILL COMPARE TWO DIFFERENT     ####
####    INPUT FILES FOR EMEP; RELATIVE AND ABSOLUTE   ####
####                                                  ####
##########################################################

## Need to nominate 2 runs to compare.
# as input files have changing folder structure and filename vocabularly over
# the years, it's not possible to year variables etc so just use the whole
# folder name and find the required file(s).

# need to make sure the folder name is changed in line with the desired
# emissions year (if relevant, for example for older v4.45 inputs).
folname_1 <- "outputs/NFC/BASE/EMEP4UKv5.0/inv2024/UKEIRE/annual/TPannual_allISO"
emis_yr_1 <- 2022

folname_2 <- "outputs/NFC/BASE/EMEP4UKv5.0/inv2025/UKEIRE/annual/TPannual_allISO"
emis_yr_2 <- 2022

## Do an output for one nominated pollutant.
# (as from: dt_poll[, emep_model]
v_pollutant <- c("nox", "nh3", "sox", "pm25")

for (p in v_pollutant) {
  print(paste0("Processing pollutant: ", p))
  output_comparison(pollutant = p, folname_1, emis_yr_1, folname_2, emis_yr_2)
}
