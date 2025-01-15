#### this returns the legnth of the array required ####
## this is determined by choices made in run_setup.R
require(data.table)
source("R/run_setup.R")

dt_cj <- CJ(v_years, tp_scheme, v_pollutants, uk_agg_schema, eu_agg_schema)

i <- nrow(dt_cj)

print(paste0("array size required: ",i))