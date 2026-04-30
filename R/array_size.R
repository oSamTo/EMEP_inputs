#### this returns the legnth of the array required ####
## this is determined by choices made in run_setup.R
require(data.table)
source("R/run_setup.R")

dt_cj <- CJ(
    run_domain,
  run_source,
  v_years,
  time_dim,
  tp_scheme,
  agg_schema,
  output_project,
  v_scenarios
)

i <- data.table(nrow(dt_cj))

fwrite(i, "R/array.size", col.names = FALSE)
