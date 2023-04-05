##############################################################################################################
packs <- c("sf","terra","stringr","dplyr","ggplot2","data.table","stats","readxl","ncdf4","lubridate")

lapply(packs, require, character.only = TRUE)
##############################################################################################################
options(datatable.showProgress = FALSE)

source("R/UK_functions.R")
#source("R/EU_functions.R")

###########################################################
#### SETTING UP WORKSPACE FOR MAKING EMPE MODEL INPUTS ####
###########################################################

# new extended domain to include both UK & Eire
r_dom_1km <<- rast(xmin = -230000, xmax = 750000, ymin = -50000, ymax = 1300000, 
                  res = 1000, crs = "epsg:27700", vals = NA)

# This is the lat long equivalent raster of the UK domain at 1km in BNG
r_dom_0.1 <<- rast(xmin = -13.8, xmax = 4.6, ymin = 49, ymax = 61.5, 
                  res = 0.01, crs = "epsg:4326", vals = NA)

# EU domain
r_dom_EU <<- rast(xmin = -30, xmax = 90, ymin = 30, ymax = 82, 
                   res = 0.01, crs = "epsg:4326", vals = NA)

# the emissions need to be masked to terrestrial cells (plus some coastal cells) - Massimo wants EMEP emissions data on the sea
# the mask is in 0.1 degree, disaggregate to 0.01 so masking can be done
r_dom_terr_10km <<- crop(extend(disagg(rast("data/spatial/Emissions_mask_10km.tif"), fact=10), r_dom_0.1), r_dom_0.1)
r_dom_terr      <<- rast("data/spatial/terrestrial_mask.tif")

# lookup file for sector mapping
dt_sec <<- fread("data/lookups/EMEP_sectors.csv")[!is.na(sec)]

# lookup file for pollutant names
dt_poll <<- fread("data/lookups/pollutants.csv")

# lookup file for EMEP country names
dt_iso <<- fread("data/lookups/EMEP_territories.csv")

# EMEP input missing value
EMEP_fillval <<- 9.96920996838687e+36

# EMEP yday numbers, to represent month central days, as taken from an EMEP input file - fixed (?)
v_yday <<- c(14,45,73,104,134,165,195,226,257,287,318,348)

######################################################################################################
#### function to fetch gridded emissions for a given year from NAEI website. 
#griddedNAEIemissions <- function(pollutant = c("cd","co","hg","nh3","nmvoc","nox","pb","pm25","pm10","pmco","sox"), #emis_year){
#  
#  pollutant <- match.arg(pollutant)
#  
#  if(!is.numeric(emis_year))   stop ("Year vector is not numeric")
#  
#  suppressWarnings(dir.create(paste0("./data/gridded/UK/",pollutant), recursive = T))
#  
#  #####
#  
#  print(paste0(Sys.time(),": GRIDDED ",pollutant," emissions from NAEI for ",emis_year))
#  
#  # create URL
#  naei_num <- dt_poll[ceh_poll == pollutant, naei_poll]
#  
#  poll_url <- paste0("https://naei.beis.gov.uk/mapping/mapping_",emis_year,"/",naei_num,".zip")
#  
#  # create a name for your download
#  poll_file_name <- paste0(pollutant,"_maps_",emis_year,".zip")
#  
#  # download to local
#  download.file(url = poll_url,
#                destfile = paste0("./data/gridded/UK/",pollutant,"/",poll_file_name),
#                quiet = T)
#  
#  # unzip download to local
#  unzip(paste0("./data/gridded/UK/",pollutant,"/",poll_file_name), overwrite = T,  exdir = paste0("./data/gridded#/UK/",pollutant,"/",pollutant,"_maps_",emis_year))
#  
#  # list the files and write as .tif
#  
#  poll_ascs <- list.files(paste0("./data/gridded/UK/",pollutant,"/",pollutant,"_maps_",emis_year), pattern=".asc$", #full.names=T)
#  
#  for(f in poll_ascs){
#    
#    r <- rast(f)
#    
#    suppressWarnings(writeRaster(r, paste0("./data/gridded/UK/",pollutant,"/",pollutant,"_maps_",emis_year,"/",names(r),".tif"), overwrite=T))
#    
#  }
#  
#  # Tidy up
#  j <- list.files(paste0("./data/gridded/UK/",pollutant,"/",pollutant,"_maps_",emis_year,"/"), pattern="^.*\\.(?!tif$)[^.]+$", full.names = T)
#  do.call(file.remove, list(j))
#  
#  # delete the zip file
#  file.remove(paste0("./data/gridded/UK/",pollutant,"/",poll_file_name))
#  
#  
#}





