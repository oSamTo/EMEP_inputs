#### set up the run ####

v_years <- c(2022) # what emissions years to process
v_pollutants <- c("nox","nh3","sox","pm25","pmco","co","voc") # "nox","nh3","sox","pm25","pmco","co","voc", "cd", "cu", "ni", "pb", "zn" - CEH names, not EMEP model

time_dim <- "annual" # annual, month, yday
tp_scheme <- c("ukem_genYr") # c("EMEP4UKv4.45" , "EMEP4UKv5.0", "genYr", 2017:2021, "test", "ukem_genYr", ukem_2017:2023 
#eu_tp_scheme <- "EMEP4UKv5.0" # EMEP4UKv4.45 / EMEP4UKv5.0 / EDGAR (no EDGAR at the mo)

# reset the tp_scheme if time_dim is 'annual'
# we dont use temporal profiling for the annual total inputs
if(time_dim == "annual") tp_scheme <- "annual"

# UK & Eire emission years
naei_inv  = 2024 # naei_inv  = which inventory compilation year to use
map_yr_uk = 2022 # map_yr_uk = what year is the NAEI spatial distribution for the data
map_yr_ie = 2019 # map_yr_ie = what year is the MapEire spatial distribution for the data: 2019

# EMEP EU emission years
emep_inv <- 2024 # emep_inv  = which inventory compilation year to use
#emep_map_yr <- 2021

## aggregation schema
uk_agg_schema <- "NA"    # none made yet. 
eu_agg_schema <- "oneEU" # oneEU = one EU file. ISO = separate ISO inputs

# break if EU is monthly AND ISO - the files are too big. 
# if(eu_tp_scheme != "annual" & eu_agg_schema == "ISO") stop("Don't run EU monthly on ISO codes.") 

## choose output directory for the EMEP input files
output_dir <- paste0("/gws/nopw/j04/ceh_generic/samtom/EMEP_inputs/outputs/EMEP4UK/inv",naei_inv)
