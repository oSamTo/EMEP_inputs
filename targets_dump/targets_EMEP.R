library(here) # construct file paths relative to project root
here::i_am("./targets_EMEP.R")

###########################################################
####                                                   ####
####    THIS MASTER SCRIPT WILL TAKE NAEI EMISSIONS    ####
#### FILES ALREADY CONVERTED TO LATLONG FOR THE UK     ####
####    0.01 DEGREE AND OUTPUT EMEP NETCDF FILES       ####
####                                                   ####
###########################################################

source("PARAMETERS.R")

# EMEP input missing value
EMEP_fillval <<- 9.96920996838687e+36
emis_loc <- "/gws/nopw/j04/ceh_generic/inventory_processor/data"

#####################
#### Load package dependencies 

library(targets)

tar_option_set(
  packages = c("dplyr","terra","data.table","sf","readxl","stringr", "ggplot2", "vctrs", "stats", "ncdf4", "lubridate")
)
options(datatable.showProgress = FALSE)
# the above loads these packages at a global level for all targets. You can also choose to load them separately 

#####################
#### Load the functions to be used in targets

# Functions used by the targets.
source(here::here("R", "UK_functions.R"))
#source(here::here("R", "EU_functions.R"))

#####################
#### Define pipeline

# List of target objects.
list(
    
  #############
  ## Domains ##
  tar_target(r_dom_UKEIRE_BNG, setDomain(area = "UKEIRE", crs = "BNG") ), # set UK NAEI domain in BNG
  tar_target(r_dom_UKEIRE_LL,  setDomain(area = "UKEIRE", crs = "LL") ), # set UK NAEI domain in LL
  # need EU domain
  
  ###########
  ## Masks ##
  tar_target(r_dom_terr_10km , crop(extend(disagg(rast("data/spatial/Emissions_mask_10km.tif"), fact=10), r_dom_UKEIRE_LL), r_dom_UKEIRE_LL))
  tar_target(r_dom_terr , rast("data/spatial/terrestrial_mask.tif"))
    
  ####################
  ## Look-up tables ##
  tar_target(lookup_sectors,  "data/lookups/EMEP_sectors.csv", format = "file"),
  #tar_target(lookup_SIC,  "data/lookups/points_sectors_to_SNAP.csv", format = "file"),
  #tar_target(lookup_CRF,  "data/lookups/CRF_to_SNAP.csv", format = "file"),
  tar_target(lookup_ISO,  "data/lookups/EMEP_territories.csv", format = "file"),
  tar_target(lookup_PID,  "../../inventory_processor/data/lookups/pollutants.xlsx", format = "file"),
      
  tar_target(dt_sec,  fread(lookup_sectors)[!is.na(sec)]),
  #tar_target(dt_NFR,  as.data.table(read_excel(lookup_sectors, sheet = "NAEI_lookup"))),
  #tar_target(dt_SIC,  fread(lookup_SIC)),
  #tar_target(dt_CRF,  fread(lookup_CRF)),
  tar_target(dt_ISO,  fread(lookup_ISO)),
  tar_target(dt_PID,  as.data.table(read_excel(lookup_PID, sheet = "NAEI_pollutants"))),
  
  #tar_target(dt_SNAPGNFR,   as.data.table(read_excel(lookup_sectors, sheet = "SNAPtoGNFR"))),
  #tar_target(dt_GNFRSNAP,   as.data.table(read_excel(lookup_sectors, sheet = "GNFRtoSNAP"))),
  #tar_target(dt_SNAPnames,  as.data.table(read_excel(lookup_sectors, sheet = "SNAPnames"))),
  
  # EMEP yday numbers, to represent month central days, as taken from an EMEP input file - fixed (?)
  tar_target(v_yday, c(14,45,73,104,134,165,195,226,257,287,318,348)),
  
  
  #################################
  ## vectors for dynamic targets ##
  # create a target of aggregation types (required for dynamic targets later)
  # https://books.ropensci.org/targets/dynamic.html
  tar_target(v_yr, c(2020)),
  tar_target(v_poll_ceh, vectorPolls(dt_PID, class = "ceh")),
  tar_target(v_poll_emep, vectorPolls(dt_PID, class = "emep"))
  
  ###########################################
  ## file targets for formatted input data ##
  # ensures re-read if they are recreated at any point (using v_sources above)
  
  # check whether EMEP wants SNAP or GNFR and whether this should be an argument 
  
  
  tar_target(v_fname_sourceFiles_hour,     checkFiles(),
                                           pattern = cross(v_aggregations, v_pollutant, v_yr), format = "file"),
  
  ###################
  ## UK PROCESSING ##
  ###################
  
  ##################
  ## Collect Data ##
  EMEPinputUK <- function(v_years, v_pollutants, time_scale = c("year","month"), time_dim, tp_fol
  naei_inv, map_yr_uk, map_yr_ie, output_dir,
  pattern = cross(v_aggregations, v_pollutant, v_yr), format = "file")
  
  
  
  
  # collect UK data
  # process to time splits
  # summarise
  # make netcdfs
  # summarise
  
  
  # repeat for EU
  
  
  
  
  
  
  ######################
  ## Downloading data ##
  # NAEI UK totals (.csv format)
  # 
  #tar_target(v_fname_NAEItotals, downloadNAEItotals(species = v_poll_naei, invYear_NAEI, dt_PID), pattern = map(v_poll_naei)),
    
  # NAEI UK points data (.xlsx format)
  # can only obtain the latest pubslihed data from NAEI website
  # special target to copy across 1990 - 2019 point data. 
  #tar_target(v_fname_NAEIpoints, downloadNAEIpoints(ptsYear_NAEI, invYear_NAEI)),
  #tar_target(v_fname_NAEIallpoints, copyNAEIpoints(fname = v_fname_NAEIpoints, invYear_NAEI)),
    
  # NAEI UK gridded data (.asc format)
  # AT THE MOMENT ADDING A POLLUTANT TO THE VECTOR WILL RUN EVERYTHING AGAIN. FIX.
  #tar_target(v_folname_NAEImaps, downloadNAEImaps(species = v_poll_naei, mapYear_NAEI, invYear_NAEI, dt_PID), pattern = map(v_poll_naei)),
   
  ##############################
  ## Formatting download data ##
  # NAEI UK totals
  # NAEI: produce ONE table of emissions inventory totals at NFR19 (all pollutants/years)
  # After NFR19, aggregations via sector mapping vector  
  #tar_target(fname_NAEItotalsNFR,  formatNAEItotals(v_fnames = v_fname_NAEItotals, invYear_NAEI, dt_PID), format = "file"),
  #tar_target(v_fname_NAEItotalsAgg,  aggNAEItotals(fname = fname_NAEItotalsNFR, classification = v_aggregations, invYear_NAEI, dt_NFR, dt_PID), pattern = map(v_aggregations), format = "file"),
  
  # NAEI UK points data, including LL conversion
  # NAEI: Format the UK points into one large table, expanding year on year, all pollutants.
  # Write with other classifications in. 
  # Write out a csv with LL conversion - have to do this over two separate targets at the moment.
  #tar_target(fname_NAEIpointsNFR, formatNAEIpoints(v_fname_NAEIallpoints, v_poll_ceh, invYear_NAEI, dt_PID, dt_SIC), format = "file"),
  #tar_target(v_fname_NAEIpointsAgg,  aggNAEIpoints(fname = fname_NAEIpointsNFR, classification = v_aggregations, invYear_NAEI, dt_NFR, dt_SIC, dt_SNAPGNFR), pattern = map(v_aggregations), format = "file"),
  #tar_target(v_fname_NAEIpointsNFRLL,  transformUKpts(fname = fname_NAEIpointsNFR, invYear_NAEI), format = "file"),
  #tar_target(v_fname_NAEIpointsAggLL,  transformUKpts(fname = v_fname_NAEIpointsAgg, invYear_NAEI), pattern = map(v_fname_NAEIpointsAgg), format = "file"),
  
  # NAEI UK gridded data (which come in SNAP), including LL conversion
  #tar_target(v_folname_NAEImapsSNAP, formatNAEImaps(fname = v_folname_NAEImaps, r_dom = r_dom_naei_BNG, mapYear_NAEI, invYear_NAEI, dt_PID, dt_SNAPnames), pattern = map(v_folname_NAEImaps), format = "file"),
  #tar_target(v_folname_NAEImapsGNFR, gnfrNAEImaps(fname = v_folname_NAEImapsSNAP, r_dom = r_dom_naei_BNG, mapYear_NAEI, invYear_NAEI, dt_SNAPGNFR), pattern = map(v_folname_NAEImapsSNAP), format = "file"),
  
  #tar_target(v_folname_NAEImapsSNAPLL,  transformUKmaps(fname = v_folname_NAEImapsSNAP, r_dom = r_dom_naei_LL, mapYear_NAEI, invYear_NAEI), pattern = map(v_folname_NAEImapsSNAP), format = "file"),
  #tar_target(v_folname_NAEImapsGNFRLL,  transformUKmaps(fname = v_folname_NAEImapsGNFR, r_dom = r_dom_naei_LL, mapYear_NAEI, invYear_NAEI), pattern = map(v_folname_NAEImapsGNFR), format = "file"),
    
  ###############
  ## PM coarse ##
  
  # no PMcoarse info in NAEI, so use PM10 - PM2.5
  # NAEI UK totals
  #tar_target(fname_NAEItotalsNFRpmco, NAEItotalsPMcoarse(fname = fname_NAEItotalsNFR, classification = "NFR", invYear_NAEI), format = "file"),
  #tar_target(v_fname_NAEItotalsAggpmco, NAEItotalsPMcoarse(fname = v_fname_NAEItotalsAgg, classification = v_aggregations, invYear_NAEI), pattern = map(v_aggregations), format = "file"),
  
  # NAEI UK points data, including LL conversion
  #tar_target(fname_NAEIpointsNFRpmco, NAEIpointsPMcoarse(fname = fname_NAEIpointsNFR, classification = "NFR", invYear_NAEI), format = "file"),
  #tar_target(v_fname_NAEIpointsNFRLLpmco, NAEIpointsPMcoarse(fname = v_fname_NAEIpointsNFRLL, classification = "NFR", invYear_NAEI), format = "file"),
  
  #tar_target(v_fname_NAEIpointsAggpmco, NAEIpointsPMcoarse(fname = v_fname_NAEIpointsAgg, classification = v_aggregations, invYear_NAEI), pattern = map(v_aggregations), format = "file"),
  #tar_target(v_fname_NAEIpointsAggLLpmco, NAEIpointsPMcoarse(fname = v_fname_NAEIpointsAggLL, classification = v_aggregations, invYear_NAEI), pattern = map(v_aggregations), format = "file"),
  
  # NAEI UK gridded data, including LL conversion
  # uses vectorised targets, but keeping BNG & LL separate
  #tar_target(v_folname_NAEImapsSNAPpmco, NAEImapsPMcoarse(v_folname = v_folname_NAEImapsSNAP, to_crs = "BNG", invYear_NAEI), format = "file"),
  #tar_target(v_folname_NAEImapsGNFRpmco, NAEImapsPMcoarse(v_folname = v_folname_NAEImapsGNFR, to_crs = "BNG", invYear_NAEI), format = "file"),
  
  #tar_target(v_folname_NAEImapsSNAPpmcoLL, NAEImapsPMcoarse(v_folname = v_folname_NAEImapsSNAPLL, to_crs = "LL", invYear_NAEI), format = "file"),
  #tar_target(v_folname_NAEImapsGNFRpmcoLL, NAEImapsPMcoarse(v_folname = v_folname_NAEImapsGNFRLL, to_crs = "LL", invYear_NAEI), format = "file"),
  
  ###############
  ## Summaries ##
  
  #tar_target(v_fname_NAEIsummaries,  summariseNAEI())
 
   
  
)



