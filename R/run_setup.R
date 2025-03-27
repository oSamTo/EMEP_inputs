
#######################################################
####                                               ####
####    FILE TO SET RUN CHOICES, E.G THE YEAR,     ####
####  THE POLLUTANT VECTOR, EMEP MODEL VERSION ETC ####
####                                               ####
#######################################################

## EMEP model version
# this makes the input to the model version. 
emep_version <- "v5.0" # v4.34, v4.36, v4.45 , v5.0

## vectors of emissions years and pollutants to run ##
v_years <- c(2010,2015,2020) # what emissions years to process
v_pollutants <- c("nox","nh3","sox","pm25","pmco","co","voc") # "nox","nh3","sox","pm25","pmco","co","voc", "cd", "cu", "ni", "pb", "zn" - CEH names, not EMEP model

## time dimension to process the data into ##
time_dim <- "annual" # annual, month, yday

## EMEP sectors to put into the netcdf - see dt_sec for choice (standard = 1:13)
v_EMEP_sec <- 1:13

## temporal profile schema ##
# UK can be: "EMEP4UKv4.45" , "EMEP4UKv5.0", "ukem_genYr", "ukem_2017:2023", "test"
# EU can be: "EMEP4UKv4.45" , "EMEP4UKv5.0", "test"  (EDGAR in the future)
tp_scheme <- c("EMEP4UKv4.45")

# however, reset the tp_scheme if time_dim is 'annual' - either UK or EU
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
uk_agg_schema <- "allISO" # allISO = separate ISO inpus for UK/IE/SEA. oneUKIE = one file. 
eu_agg_schema <- "allISO" # allISO = separate ISO inputs. oneEU = one EU file. 

# break if EU is monthly AND ISO - the files are too big. THIS WILL CHANGE 
if(tp_scheme != "annual" & eu_agg_schema == "allISO") stop("Don't run EU monthly on ISO codes.")
# break if EU is annual and agg schema is one EU - nothing accepts it. 
if(tp_scheme == "annual" & eu_agg_schema == "oneEU") stop("Don't run EU annual with one aggregated surface.")

## choose output directory for the EMEP input files
output_dir <- paste0("/gws/nopw/j04/ceh_generic/samtom/EMEP_inputs/outputs/EMEP4UK",
					 emep_version,"/inv",naei_inv)
