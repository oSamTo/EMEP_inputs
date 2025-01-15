source("R/workspace.R")
source("R/run_setup.R")

###########################################################
####                                                   ####
####    THIS RUN FILE WILL TAKE NAEI EMISSIONS FILES   ####
####  ALREADY CONVERTED TO LATLONG FOR THE UK @ 0.01   ####
####       DEGREE AND OUTPUT EMEP NETCDF FILES         ####
####                                                   ####
###########################################################

## For the UK, they are derived from NAEI maps/points, in SNAP sectors. 
## For EIRE, they are GNFR maps from the MapEire project, with EMEP data.
## For the EU, they are derived from EMEP/CEIP maps, in GNFR sectors. 

# (see /gws/nopw/j04/ceh_generic/inventory_processor)

### CHOOSE WHICH YEARS AND WHICH POLLUTANTS/GHGS TO PUT IN THE NETCDF ### (NO pm10, only pmco)
i_a <- as.numeric(commandArgs(trailingOnly = TRUE)[1]) # array number
# i_a <- 3

# create a full table of individual runs. These will form array jobs. 
dt_cj <- CJ(v_years, time_dim, tp_scheme, v_pollutants, uk_agg_schema, eu_agg_schema)

y <- dt_cj[i_a, v_years]
species = dt_cj[i_a, v_pollutants]
time_dim <- dt_cj[i_a, time_dim]
tp_scheme <- dt_cj[i_a, tp_scheme]
uk_agg_schema <- dt_cj[i_a, uk_agg_schema]

## a table to nominate file locations for different emissions - e.g. older years, different projects, and so on. 
dt_alt_emis <- data.table(poll = c("nh3"), 
                          loc = c("/gws/nopw/j04/ceh_generic/inventory_processor/data"))

# set the root folder name, for writing. 
uk_folname <- paste0(output_dir,"/emis",y,
	        		 "/UKEIRE/",time_dim,"/TP",tp_scheme,
				     "_AGG",uk_agg_schema)
					 
# folname = uk_folname

#### PROCESSING ####
# uk and eu should be both ran prior to QAQC (as some maps need both)
## UK & EIRE ##
EMEP_input_UK(y = y, species = species, time_dim = time_dim,
              naei_inv = naei_inv, map_yr_uk = map_yr_uk, map_yr_ie = map_yr_ie, folname = uk_folname,
	          tp_scheme = tp_scheme, uk_agg_schema = uk_agg_schema, dt_alt_emis = dt_alt_emis)

## EU ##
#EMEPinputEU(y = y, species = species, eu_agg_schema = eu_agg_schema, time_dim = time_dim,
#              eu_tp_scheme, emep_inv = emep_inv, output_dir)

## QAQC ##
create_qaqc(y, species, uk_folname, map_yr_uk, 
            naei_inv, emep_inv, time_dim)