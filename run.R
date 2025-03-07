source("R/run_setup.R")
source("R/workspace.R")

##########################################################
####                                                  ####
####    THIS RUN FILE WILL TAKE NAEI EMISSIONS FILES  ####
####  ALREADY CONVERTED TO LATLONG FOR THE UK @ 0.01  ####
####  DEGREE AND OUTPUT EMEP NETCDF FILES. USE FILE   ####
###   RUN_SETUP.R TO MAKE CHOICES FOR DATA CREATION   ####
####                                                  ####
##########################################################

## For the UK, they are derived from NAEI maps/points, in SNAP sectors. 
## For EIRE, they are GNFR maps from the MapEire project, with EMEP data.
## For the EU, they are derived from EMEP/CEIP maps, in GNFR sectors. 

# (see /gws/nopw/j04/ceh_generic/inventory_processor)

### CHOOSE WHICH YEARS AND WHICH POLLUTANTS/GHGS TO PUT IN THE NETCDF ### (NO pm10, only pmco)
i_a <- as.numeric(commandArgs(trailingOnly = TRUE)[1]) # array number
# i_a <- 1

# create a full table of individual runs. These will form array jobs (product of).
# as every pollutant is require in one file, this is likely the same length as emissions years.
dt_cj <- CJ(v_years, time_dim, tp_scheme, uk_agg_schema, eu_agg_schema)

y <- dt_cj[i_a, v_years]
time_dim <- dt_cj[i_a, time_dim]
tp_scheme <- dt_cj[i_a, tp_scheme]
uk_agg_schema <- dt_cj[i_a, uk_agg_schema]
eu_agg_schema <- dt_cj[i_a, eu_agg_schema]

## a table to nominate file locations for different emissions - e.g. older years, different projects, and so on. 
dt_alt_emis <- data.table(poll = c("nh3"), 
                          loc = c("/gws/nopw/j04/ceh_generic/inventory_processor/data"))

# set the root folder name, for writing. 
uk_folname <- paste0(output_dir, "/UKEIRE/", time_dim, "/TP", 
                     tp_scheme, "_", uk_agg_schema)

eu_folname <- paste0(output_dir, "/EU/", time_dim, "/TP", 
                     tp_scheme, "_", eu_agg_schema)
								
# folname = uk_folname
# folname = eu_folname

#######################
#### RUN FUNCTIONS ####
# uk and eu should be both ran prior to QAQC (as some maps need both)

## UK & EIRE ##
UKEIRE_functions <- paste0("EMEP_UKEIRE_",emep_version)

get(UKEIRE_functions)(y = y, v_pollutants = v_pollutants, time_dim = time_dim, 
                      v_EMEP_sec = v_EMEP_sec, naei_inv = naei_inv, map_yr_uk = map_yr_uk, 
                      map_yr_ie = map_yr_ie, folname = uk_folname, tp_scheme = tp_scheme, 
                      uk_agg_schema = uk_agg_schema, dt_alt_emis = dt_alt_emis)


## EU ##
# if the tp_scheme is not "annual", "test", "EMEP4UKv4.45" or "EMEP4UKv5.0", skip EU
if(tp_scheme %in% c("EMEP4UKv4.45", "EMEP4UKv5.0", "annual", "test")){
  
  # only the function set sourced by choice of version should be available (e.g. "EU_v5.0_functions.R"). 
  # still, using a variable function name to be sure. 
  EU_functions <- paste0("EMEP_EU_",emep_version)
  
  # this does mean the arguments need to be the same in each one. 
  get(EU_functions)(y = y, v_pollutants = v_pollutants, time_dim = time_dim,
                    v_EMEP_sec = v_EMEP_sec, emep_inv = emep_inv, folname = eu_folname,
			        tp_scheme = tp_scheme, eu_agg_schema = eu_agg_schema)

}


## QAQC ##
# this should be lapply over v_pollutants for a separate file. 
create_qaqc(y, species, uk_folname, eu_folname, map_yr_uk, naei_inv, 
            emep_inv, time_dim, emep_version, v_EMEP_sec,
			uk_agg_schema, eu_agg_schema, tp_scheme)


