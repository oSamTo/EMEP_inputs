#########################################################
#### FUNCTIONS FOR CREATION OF EMEP - UK INPUT FILES ####
#########################################################

######################################################################################################
#### function to set the combined UK/EIRE domain
setDomain <- function(area = c("UKEIRE","EU_EMEP"), crs = c("BNG","LL")){
  
  area  <- match.arg(area)  # area to process
  crs   <- match.arg(crs)   # crs to return in
  
  # if statements to return correct domain
  if(area == "UKEIRE"){
    
    if(crs == "BNG"){
      
      r <- rast(xmin = -230000, xmax = 750000, ymin = -50000, ymax = 1300000,
                res = 1000, crs = "epsg:27700", vals = NA)
      
    }else{
     
      r <- rast(xmin = -13.8, xmax = 4.6, ymin = 49, ymax = 61.5,
                              res = 0.01, crs = "epsg:4326", vals = NA)
       
    }
    
  }else if(area == "EU_EMEP"){
    
    if(crs == "BNG"){
      
      stop(print("Cannot have BNG crs for the EU EMEP domain"))
      
    }else{
      
      r <- rast(xmin = -30, xmax = 90, ymin = 30, ymax = 82, 
                        res = 0.1, crs = "epsg:4326", vals = NA)
      
    }
    
  }
  
  return(r)
}


vectorPolls <- function(dt_PID, class = c("ceh","emep","mapeire","naei")){
  
  class <- match.arg(class)
    
  if(class == "naei"){
    dt_PID <- dt_PID[ceh_poll != "pmco"]
	v <- dt_PID[,PollutantID]
  }else{
    dt_PID <- dt_PID[ceh_poll != "pmco"]
    v <- dt_PID[,get(paste0(class,"_poll"))]
  }
  
  return(v)
  
}

######################################################################################################
#### function to return filenames of extant emissions files for UK and EIRE or EU
## this feeds into another target to update chain if the files change, otherwise it is not used. 

checkFiles <- function(y, pollutant, region = c("UKEIRE","EU")){

  ## read in UK files
  list.files("", pattern = paste0(), full.name=T)




}




dump <- function(){

require(terra)
require(data.table)
require(dplyr)
require(ncdf4)
require(readxl)

y <- 2019
pollutant <- "nox"

EMEP_fillval <<- 9.96920996838687e+36
emis_loc <- "/gws/nopw/j04/ceh_generic/inventory_processor/data"

output_dir <- "sam_test"

time_dim <- "month"
tp_fol <- "pre_TEMREG"

naei_inv = 2023
map_yr_uk = 2021
map_yr_ie = 2019

res_crs <- "0.01_LL"

v_yday <- c(14,45,73,104,134,165,195,226,257,287,318,348)

lookup_sectors <- "data/lookups/EMEP_sectors.csv"
lookup_ISO <- "data/lookups/EMEP_territories.csv"
lookup_PID <- "../../inventory_processor/data/lookups/pollutants.xlsx"
      
dt_sec <- fread(lookup_sectors)[!is.na(sec)]
dt_ISO <- fread(lookup_ISO)
dt_PID <- as.data.table(read_excel(lookup_PID, sheet = "NAEI_pollutants"))




}
######################################################################################################

######################################################################################################

######################################################################################################
#### function to take NAEI emissions, make ready to EMEP format and create netCDFs for UK & Eire
EMEPinputUK <- function(v_years, species, uk_agg_schema, time_dim = c("year","month"), tp_scheme, 
                        alt_emis, naei_inv, map_yr_uk, map_yr_ie, output_dir){
  
  time_dim <- match.arg(time_dim)
  
  # v_years  = vector, *numeric*, year to process.
  if(!is.numeric(v_years)) stop ("Year vector is not numeric")
  
  # naei_inv  = *numeric*, year of inventory release data.
  if(!is.numeric(naei_inv)) stop ("Inventory release year is not numeric")
  
  # map_yr = *numeric*, year of spatial dist. from NAEI. 
  if(map_yr_uk < 2018) stop ("UK spatial distribution must be 2018 or later")
  if(!(map_yr_ie %in% c(2016,2019))) stop ("Eire spatial distribution must be 2016 or 2019")
  
  # For the years & pollutants, take the regional emissions in Lat Long and;
  #   i) convert point emissions (.csv) into a raster
  #  ii) combine the points with the diffuse (.tif) data as required for the model
  # iii) MASK THE DATA to fit *inside* the EU emissions data
  #  iv) For SNAP 1: ONLY point sources go into A_PublicPower as this is treated with 200m injection. All other into B_Industry.
  #   v) split into monthly emissions, if needed
  #  vi) create netCDF
    	
	res_crs <- "0.01_LL"
 
  ## loop through years and pollutants listed and make a netCDF input file for each.
  ## years before 1970 (1980 for NH3) will always use the SPEED time series - /gws/nopw/j04/ceh_generic/samtom/SPEED
  ## SNAP 1 prior to 1990 will use point data from the same folder above. 
  
  ## Data to be made with a monthly time attribute
      # for the month, incorporate the new DUKEMs temporal data 
  
  for(y in v_years){
    
    #for(species in v_pollutants){
      
      ######################################################################################
      if(!(species %in% c("ch4","co2","n2o","bap","bz","hcl","nox","sox","nh3", "co", "nmvoc","cd","cu","pb","hg","ni","zn", "pm0_1","pm1","pm25","pm10","pmco")) ) stop ("Species must be in: 
                                            AP:    bap, bz, co, hcl, nh3, nox, sox, voc
                                            PM:    pm0_1, pm1, pm2_5, pm10, pmco
                                            GHG:   ch4, co2, n2o
                                            Metal: cd, cu, hg, ni, pb, zn")
      ######################################################################################
      
      print(paste0(Sys.time(),": Creating EMEP4UK UK/EIRE input netCDF for ",species," in ",y,"..."))
           
	  
	  # set emissions location. 
	  # use the default emissions location for all processed emissions from inventory, if not set in the alternative table
	  # THIS ISN'T REALLY IN USE AT THE MOMENT
	  if(species %in% dt_alt_emis[,poll]){
	  
	    emis_loc <- dt_alt_emis[poll == species, loc]
		loc_text <- "alternative"
	  
	  }else{
	  
	    emis_loc <- "/gws/nopw/j04/ceh_generic/inventory_processor/data"
		loc_text <- "standard"
	  
	  }
	
	  
	  # set up blank stacks ready for data of different regions
      l_uk_allSec  <- list()
      l_ie_allSec  <- list()
      l_sea_allSec <- list()
      
      # sector totals list
      l_sum_allSec <- list()
      
      # sector names to loop through - this is netcdf sector names (sec01 etc.)
      v_sectors <- dt_sec[,unique(sec)]
      
      print(paste0(Sys.time(),":      Collecting and creating all emissions input data from ",loc_text," location..."))
      
      for(i in v_sectors){
        
        if(i == "") next # not interested in currently blank EMEP-named sectors
        
        print(paste0(Sys.time(),":         ",i))
        
        #########################################################
        #### OBTAIN 12 MONTHS OF DATA FOR EACH NETCDF SECTOR ####
        
        ## At this point, we are using GNFR maps for UK and Eire
            # these have already been generated: 
                # for the UK, they are derived from SNAP maps. 
                # for EIRE, they are GNFR maps from the MapEire project. 
        ## There is one GNFR per EMEP sector name
        
        #sc <- dt_sec[name == i, EMEP_sec]
        #sc_pad <- str_pad(sc, 2, "0", side = "left")
        
        ####################################
        #### LISTS OF EMISSION SURFACES ####
        
        l_uk <- UKIEsectorEmissions(emis_loc, species, y, i, res_crs, map_yr = map_yr_uk, naei_inv, country = "uk")
        l_ie <- UKIEsectorEmissions(emis_loc, species, y, i, res_crs, map_yr = map_yr_ie, naei_inv, country = "eire")
        
        ########################
        #### TEMPORAL SPLIT ####
        # if the time_dim is year, the data stays as 1 annual total.
        # if the time_dim is month, the data needs to be split into 12 layers
            # this is either the temporal profiles from inside EMEP4UK
            # or the newly generated stuff that has come out of DUKEMs
        # the sea layer needs to be split based on the country it came from (i.e. UK or Eire)
        
        l_uk_prof   <- splitUKIEannual(species = species, time_dim, tp_scheme, l_annual = l_uk, i = i, country = "uk")
        l_ie_prof   <- splitUKIEannual(species = species, time_dim, tp_scheme, l_annual = l_ie, i = i, country = "ie")
        
        # create three stacks for UK, Eire and the SEA buffer (annual or monthly)
        s_uk  <- l_uk_prof[["terrestrial"]]
        s_ie  <- l_ie_prof[["terrestrial"]]
        		
		if(time_dim == "year"){ v_in <- 1 }else if(time_dim == "month"){ v_in <- 1:12 } 
		
		s_sea <- tapp(c(l_uk_prof[["sea"]], l_ie_prof[["sea"]]), index = v_in, sum, na.rm=T)
		names(s_sea) <- paste0(species,"_uk_sea_",i,"_",str_pad(1:12, 2, "0", side = "left"))
        # need an if clause for s_sea in-case it's annual
        
        ###################
        #### COLLATING ####
        # add temporal raster stacks to lists
        
        l_uk_allSec[[paste0("uk_", species, "_", i,"_",time_dim)]]   <- s_uk
        l_ie_allSec[[paste0("ie_", species, "_", i,"_",time_dim)]]   <- s_ie
        l_sea_allSec[[paste0("sea_", species, "_", i,"_",time_dim)]] <- s_sea
        
        ####################
        #### STATISTICS ####
        
        dt_totals <- summariseUKIEemissions(y, species, i,
                                        l_uk = l_uk, s_uk = s_uk,
                                        l_ie = l_ie, s_ie = s_ie,
                                        sea = s_sea)
        
        l_sum_allSec[[paste0("totals_", species, "_", i,"_", time_dim)]] <- dt_totals
        
         
      } # sector loop
	  
	  ####################################################
      #### AGGREGATION BY SECTOR OR ISO (IF REQUIRED) ####
	  	  
	  dt_emis_summary <- rbindlist(l_sum_allSec, use.names=T)[order(Region, GNFR)]
	  
	  #print(paste0(Sys.time(),":               Aggregating by sector and/or ISO code..."))
	  #l_eu_agg <- aggregateEU(y, species, schema, time_dim, l_eu_allSec)
	  ## code to go here for any aggregations, e.g. ISO code ##
	  # will need to create 'l_uk_agg' object, like in EU code
      
      ############################################################
      #### CREATE AND POPULATE NETCDF ON POLLUTANT/YEAR BASIS ####
      
      print(paste0(Sys.time(),":      Creating and populating netcdf..."))
      
      createNETCDFuk(y, naei_inv, species, map_yr_uk, map_yr_ie, output_dir, time_dim, tp_scheme, uk_agg_schema,
                     l_uk_allSec, l_ie_allSec, l_sea_allSec, dt_emis_summary)
      
    #} # pollutant loop
        
	# write some metadata
	print(paste0(Sys.time(),": Writing metadata for ",species," in ",y,"..."))
    writeMetadataUK(y, species, naei_inv, map_yr_uk, map_yr_ie, output_dir, time_dim, tp_scheme, uk_agg_schema, alt_emis)
	
	# make some year-specific plots
	print(paste0(Sys.time(),": Plotting netCDF emissions for ",species," in ",y,"..."))
	plotYearUK(species, output_dir, y, tp_scheme, uk_agg_schema, naei_inv, map_yr_uk)
	
	print(paste0(Sys.time(),": DONE..."))
	
  } # year loop
  
  # one more function to plot data IF > 1 year
  
  
} # end of function
        

######################################################################################################
#### function to collect sector data for diffuse and points, based on country and sector
UKIEsectorEmissions <- function(emis_loc, species, y, i, res_crs, map_yr, naei_inv, country = c("uk", "eire")){
  
  country <- match.arg(country)
  
  # v_years  = vector, *numeric*, year to process.
  if(!is.numeric(y)) stop ("Year is not numeric")
    
  # set the diffuse filename - this will be the latest year according to inventory year chosen
  # e.g. if choosing NAEI inventory year 2022, the emissions map will be the latest in that series, 2020. 
  # point data is either NAEI series, SPEED data or none for Eire :
  
  
  ## UK ##
  # SN01:        SPEED historical power data                NAEI point data
  #          <----------------------------------><-----------------------------------> ...
  #
  # Year:  1960        1970        1980        1990        2000        2010        2020...
  #          |-----------|-----------|-----------|-----------|-----------|-----------|
  #
  # !SN01:   <----------------------------------------------><-----------------------> ...
  #                 NAEI point data at year = 2000                NAEI point data
  
  ## EIRE ##
  #  ALL:           CEDS historical data                      EMEP data
  #          <----------------------------------><-----------------------------------> ...
  #
  # Year:  1960        1970        1980        1990        2000        2010        2020...
  #          |-----------|-----------|-----------|-----------|-----------|-----------|
  
    
  
  # read both points files, easier to subset. 
  if(country == "uk"){
    
	# diffuse filename (current year)
	f_diff <- paste0(emis_loc,"/NAEI/inv",naei_inv,"/maps/NAEI_",species,"_DIFFUSE_inv",naei_inv,"_emis_",naei_inv-2,"/GNFR/NAEI_",
	                 species,"_DIFFUSE_inv",naei_inv,"_emis_",naei_inv-2,"_GNFR_",dt_sec[sec == i, GNFRlong],"_t_LL.tif")
  
    # set the points filename - the NAEI data from 1990 to current year
    f_pt <- c(paste0(emis_loc,"/NAEI/inv",naei_inv,"/points/NAEI_AllPoll_POINTS_inv",naei_inv,"_emis_1990_",naei_inv-2,"_GNFR_t_LL.csv"),
	          paste0(emis_loc,"/../../samtom/SPEED/power_station_emissions_ALL_1950-2000_GNFR_t_LL.csv"))
      
  }else{
  
    # diffuse filename - Eire inventory is currently set to 2021 with 2019 maps
	f_diff <- paste0(emis_loc,"/MapEire/inv2021/maps/tif/MapEire_",species,"_DIFFUSE_inv2021_emis_2019/GNFR/MapEire_",
	                 species,"_DIFFUSE_inv2021_emis_2019_GNFR_",dt_sec[sec == i, GNFRlong],"_t_LL.tif")
  
    # set the points filename
    f_pt <- "no_file"
  
  }
  
  ### read in the data; if diff/pts are empty or don't exist, set as blank domain
  ### read diffuse data ###
  if(file.exists(f_diff)){
    
    r_diff <- crop(extend(rast(f_diff), r_dom_0.1), r_dom_0.1)
  
  }else{
    
    r_diff <- r_dom_0.1
    
  } # end of read diffuse
    
  
  ### read point data ###
  # this is decided by country, UK should always have point files to read
  if(country == "uk"){
    	
    # get the table of points first and assess if any entries at all
	#     PUBLIC POWER: use NAEI point data back to 1990, then the UKSCAPE/SPEED time series to 1960
	# NOT PUBLIC POWER: years prior to 2000 will use 2000 !! (see theory/graphs on combining points, diffuse, scaling etc)
	
	# read both files, subset the historical to < 1990 and bind together, then perform subsetting. 
	l_pts <- lapply(f_pt, fread)
	l_pts[[2]] <- l_pts[[2]][Year < 1990]
		
	dt_pts <- rbindlist(l_pts, use.names = T)
	
    dt_pts <- dt_pts[GNFR == dt_sec[sec == i, GNFRlong] & AREA == toupper(country) & Pollutant == species]
	
	if(i == "sec01"){
	  
	  dt_pts <- dt_pts[Year == y]
	
	}else if(i != "sec01" & y >= 2000){
	  
	  dt_pts <- dt_pts[Year == y]
	
	}else{
	
	  dt_pts <- dt_pts[Year == 2000]
	
	}
	
    
    # if there are no points, use a blank raster, otherwise rasterize
    if(nrow(dt_pts) == 0){
      
      r_pt <- r_dom_0.1
      
    }else{
      
      v_pt <- vect(dt_pts, geom=c("Easting", "Northing"), crs = "EPSG:4326")
      r_pt <- terra::rasterize(v_pt, r_dom_0.1, field = "emis_t", fun = sum)
      
    } # end of read points
    
  }else{
    
    r_pt <- r_dom_0.1
    
  }
  
  ## combine the surfaces, whether they have data in or not. 
  s <- c(r_diff, r_pt) ; names(s) <- c("diffuse","point")
  r <- app(s, sum, na.rm = T) ; names(r) <- "total"
  
  ###############
  ### SCALING ###
    
  if(country == "uk"){
  
    # read in the SNAP time series;
         # SNAP maps were put into GNFR maps, so there will be 0 data in some GNFRs 
		 # but there may be data in equiv inventory GNFRs, as it's just sector totalling, so data will be lost to maps of 0
	# use the ACTUAL amounts, not the alpha - this is due to the complex relationship of point & 
	#                                         diffuse data, data completeness and relative scaling causing error 
    
	# anything prior to 1970 (or 1980 for NH3) needs the SPEED totals.
	if(y >= 1980){
	  
	  f_alpha <- paste0(emis_loc,"/NAEI/inv",naei_inv,"/alpha/NAEI_AllPoll_TOTALS_inv",naei_inv,"_emis_1970-",naei_inv-2,"_SNAP_alpha.csv")
      dt_alpha <- fread(f_alpha)
	
	}else if(y >= 1970 & species != "nh3"){
	  
	  f_alpha <- paste0(emis_loc,"/NAEI/inv",naei_inv,"/alpha/NAEI_AllPoll_TOTALS_inv",naei_inv,"_emis_1970-",naei_inv-2,"_SNAP_alpha.csv")
      dt_alpha <- fread(f_alpha)
	
	}else{
	  
	  f_alpha <- paste0(emis_loc,"/../../samtom/SPEED/SPEED_AllPoll_TOTALS_invNA_emis_1960-1970_SNAP_alpha.csv")
      dt_alpha <- fread(f_alpha)
	
	}
	
	# subset the value required. Remember;
			# to scale industry by SNAPs 3 & 4 together
			# all of SNAP 8 is in I_Offroad. While G_Shipping and H_Aviation are empty, shouldn't take the risk using SN08 > once
			# all of SNAP 10 is in K_AgriLivestock. While L_AgriOther is empty, shouldn't take the risk using SN10 > once
	if(i == "sec02"){
	  scaling_value <- dt_alpha[Pollutant == species & Year == y & AREA == toupper(country) & SNAP %in% c(3:4), sum(tot_emis_t, na.rm=T)]
	}else if(i %in% c("sec07","sec08","sec12")){
	  scaling_value <- NA
	}else{
	  scaling_value <- dt_alpha[Pollutant == species & Year == y & AREA == toupper(country) & SNAP == dt_sec[sec == i, SNAP], tot_emis_t]
	}
	
	if(length(scaling_value) == 0) scaling_value <- 0
	if(is.na(scaling_value)) scaling_value <- 0
  
  }else if(country == "eire"){
  
   # the base year for Eire is 2019, from a 2021 inventory. Alpha factor needed if y != 2019. Data from EMEP/CEDS. 
   # EMEP data is centred on inv-2 though, so need to re-work data to be relative to 2019
      
   ## this should mirror the EU method for 1970 - 1989 & 1990 - y, apart from I_Offroad. 
   #		1990 to present: 
   # 				use EMEP alpha value against the MapEire map
   # 		Prior to 1990, CEDS values: 
   #				In EU, I_Offroad is actual, but here it stays alpha.
   #				Aviation: use the global CEDS value (alpha)
   #				ANY other: use ISO alpha
		 
   
   if(y >= 1990){
     
	 f_alpha <- paste0(emis_loc,"/EMEP/inv",naei_inv,"/alpha/EMEP_AllPoll_TOTALS_inv",naei_inv,"_emis_1970-",naei_inv-2,"_GNFR_alpha.csv")
     dt_alpha <- fread(f_alpha)
	 
	 scaling_value <- dt_alpha[Pollutant == species & Year == y  & ISO2 == "IE" & GNFR == dt_sec[sec == i, GNFRlong], tot_emis_t]
   
   }else{
     
	 # if before 1990, find the 1990 EMEP value and scale by EMEP scalar, apply to 2019 map
     dt_ceds_alpha <- fread(paste0(emis_loc,"/../../samtom/SPEED/CEDS_for_EMEP/",species,"_CEDS_1950_1990_ISO_GNFR_kt.csv"))
	 dt_emep_alpha <- fread(paste0(emis_loc,"/EMEP/inv",naei_inv,"/alpha/EMEP_AllPoll_TOTALS_inv",naei_inv,"_emis_1970-",naei_inv-2,"_GNFR_alpha.csv"))
	 
	 if(i == "sec08"){	 
	   emep_alpha <- dt_ceds_alpha[Year == y & ISO2 == "XX" & GNFR == dt_sec[sec == i, GNFRlong], alpha]
	 }else{	 
       emep_alpha <- dt_ceds_alpha[Year == y & ISO2 == "IE" & GNFR == dt_sec[sec == i, GNFRlong], alpha]
	 }
	 
	   emep90_t  <- dt_emep_alpha[Pollutant == species & Year == 1990 & ISO2 == "IE" & GNFR == dt_sec[sec == i, GNFRlong], tot_emis_t]
	   scaling_value <- (emep_alpha * emep90_t)	 
	 
   }
   		
	if(length(scaling_value) == 0) scaling_value <- 0
	if(is.na(scaling_value)) scaling_value <- 0
   
  }else{
    
	# error catch
    print("no SCALAR for this year/pollutant/sector")
    stop
  }
  
  # multiply by the alpha only in Eire, before 1990 (i.e. relative CEDS). Everything else goes on totals.
    
  # re-scale the mapped data
  r <- (r / global(r, sum, na.rm=T)$sum) * scaling_value 
  

  ###############
  ### MASKING ###
  
  # EMEP needs zero value, not NA, in emissions
  r[is.na(r)] <- 0 ; r[is.nan(r)] <- 0 ; r[is.infinite(r)] <- 0 
  
  # Mask to the EMEP input restrictions
  r_t   <- mask(r,     r_dom_terr)                 # emissions on UK&EIRE land territory
  r_t[is.na(r_t)] <- 0 ; r_t[is.nan(r_t)] <- 0
  
  r_t10 <- mask(r,     r_dom_terr_10km)            # emissions on UK&EIRE land territory + 10km sea buffer
  r_t10[is.na(r_t10)] <- 0 ; r_t10[is.nan(r_t10)] <- 0
  
  r_ow  <- mask(r,     r_dom_terr_10km, inverse=T) # emissions outwith UK&EIRE land + 10km buffer (i.e. rest of domain)
  r_ow[is.na(r_ow)] <- 0 ; r_ow[is.nan(r_ow)] <- 0
  
  r_sea <- mask(r_t10, r_dom_terr, inverse=T)      # emissions only in the 10km sea buffer
  r_sea[is.na(r_sea)] <- 0 ; r_sea[is.nan(r_sea)] <- 0
  
  l <- list(r, r_t, r_t10, r_ow, r_sea)
  names(l) <- c("total", "terrestrial","terrestrial_10km","outwith_10km","sea")
  
  return(l)
  
} # end of function

######################################################################################################
#### function to split annual emissions out into months (or keep as annual)
splitUKIEannual <- function(species, time_dim = c("year","month"), tp_scheme, l_annual, i, country = c("uk","ie","sea")){
  
  country    <- match.arg(country)
  time_dim <- match.arg(time_dim)
  
  if(time_dim == "year"){ i_time <- 1 }else{ i_time <- 1:12 }
  
  if(time_dim == "year"){
    
    l_s <- c(l_annual[["terrestrial"]], l_annual[["sea"]])
    names(l_s) <- c("terrestrial","sea")
    
	names(l_s[["terrestrial"]]) <- paste0(species, "_", country,"_","terrestrial", "_", i,"_",str_pad(i_time, 2, "0", side = "left"))
    names(l_s[["sea"]]) <- paste0(species, "_", country,"_","sea", "_", i,"_",str_pad(i_time, 2, "0", side = "left"))
        
  }else{
    
    ## Use the nominated temporal schema to split the data to monthly layers
	
	## If the tp_scheme = version of EMEP4UK (e.g. 'EMEP4UKv4.45');
	     # this is temporal data of the EMEP model, for that version (e.g.: EMEP4UKv4.45 = July '23)
    ## HOWEVER if the tp_scheme != version of EMEP4UK;
	     # we can use the GNFR generated monthly profiles, instead of the SNAP level ones in the model input
		 # the GNFR ones pertain more to the GNFR sector, instead of being combined to SNAP
		 # the hour/wday profiles will be done internally at SNAP level. month model inputs (SNAP) are just ignored. 
		
	## !! CONSIDER INTRODUCING EIRE PROFILES
    	
	if(tp_scheme %in% c("EMEP4UKv4.45", "EMEP4UKv5.0")){
	# temporal schema in EMEP4UKv4.45 / EMEP4UKv5.0
	  
	  country_id <- ifelse(country == "uk", 27, 14) # to extract correct temporal file factors; 27 = UK, 14 = IE (Eire)
      snap_id <- dt_sec[sec == i, as.numeric(SNAP)] # set the SNAP to read from the temporal file. 
    
      # read in temporal file for legacy temporal splits (subset to Eire or UK - SEA needs to match parent country)
      if(species == "nmvoc"){
	    dt_tempro <- fread(paste0("/gws/nopw/j04/ceh_generic/samtom/TEMREG/output/model_inputs/EMEP4UK/",tp_scheme,"/MonthlyFacs.voc"))
	  }else{
	    dt_tempro <- fread(paste0("/gws/nopw/j04/ceh_generic/samtom/TEMREG/output/model_inputs/EMEP4UK/",tp_scheme,"/MonthlyFacs.",species))
	  }
	  	  
	  names(dt_tempro) <- c("ISO","SNAP",month.abb[1:12])
      dt_tempro_m <- melt(dt_tempro, id.vars = c("ISO","SNAP"), variable.name = "MON", value.name = "FAC")
    
      # extract required temporal data - Use 1s if the SNAP sector in dt_dec is "NA"
      if(is.na(snap_id)){
        v_tempro <- rep(1,12)
      }else if(snap_id %in% dt_tempro_m[,SNAP]){
	    v_tempro <- dt_tempro_m[ISO == country_id & SNAP == snap_id][["FAC"]] # vector of monthly splits 
	    v_tempro <- v_tempro/mean(v_tempro)# not always adding to exactly 12 in the temporal file; ensure
	  }else{
	    v_tempro <- rep(1,12)	          
      }	
	
		
	}else{
	# IF the tp_scheme is something generated by TEMREG model, use the SNAP csv output;
	# HAS to be SNAP output for the UK, as the data correspond to UK NAEI SNAP emissions
	# EIRE can stay as GNFR timing
	# GAMs can produce -ve values in profiles which will cause -ve emissions
		# MUST NOT have -ve emissions as it will crash EMEP4UK
		# thought about setting to 0, but set to 2% and re-normalise everything (divide by mean)
	
	  if(dt_sec[sec == i, GNFRlong] == ""){
	  # IF the GNFR code is blank, just use a series of 1s against 0 data;
	    v_tempro <- rep(1,12)
	  
	  }else if(i == "sec13"){
	  # IF it is M_Other, set to 1s
	    v_tempro <- rep(1,12)
		
	  }else{
	  
	    # choose SNAP for UK and GNFR for Eire
		if(country == "uk"){
		
		  dt_tempro <- fread(paste0("/gws/nopw/j04/ceh_generic/samtom/TEMREG/output/coeff_sector/SNAP/",species,"/",tp_scheme,
							   "/SNAP_",dt_sec[sec == i, str_pad(SNAP, 2, "0", side = "left")],"_month_",species,"_",tp_scheme,".csv"))
	  
	      v_tempro <- dt_tempro[,N] # vector of monthly splits 
		  v_tempro[v_tempro < 0] <- 0.02 # rare occasion factor goes below 0, set to 0.02 (i.e. some emissions, but tiny)
	      v_tempro <- v_tempro/mean(v_tempro)# not always adding to 12 in the tempro file, adjust slightly
		
		}else{
          		
		  dt_tempro <- fread(paste0("/gws/nopw/j04/ceh_generic/samtom/TEMREG/output/coeff_sector/GNFR/",species,"/",tp_scheme,
							   "/GNFR_",dt_sec[sec == i, GNFRlong],"_month_",species,"_",tp_scheme,".csv"))
	  
	      v_tempro <- dt_tempro[,N] # vector of monthly splits 
		  v_tempro[v_tempro < 0] <- 0.02 # rare occasion factor goes below 0, set to 0.02 (i.e. some emissions, but tiny)
	      v_tempro <- v_tempro/mean(v_tempro)# not always adding to 12 in the tempro file, adjust slightly
		
		}
	    	  	  
	  }
		
	}
        
    # make a standard 1 month raster. Annual/12.
    
    l_s_month <- lapply(l_annual[c("terrestrial","sea")], function(x) rep(x/12, 12))
    
    # adjust with temporal profile
    
    l_s <- lapply(l_s_month, function(x) x * v_tempro)
    
	# error if any negative values remain
	if(any(global(l_s[["terrestrial"]], min, na.rm=T) < 0)) stop("there are negative emissions values (land)")
	if(any(global(l_s[["sea"]], min, na.rm=T) < 0)) stop("there are negative emissions values (sea)")
	
    names(l_s[["terrestrial"]]) <- paste0(species, "_", country,"_","terrestrial", "_", i,"_",str_pad(i_time, 2, "0", side = "left"))
    names(l_s[["sea"]]) <- paste0(species, "_", country,"_","sea", "_", i,"_",str_pad(i_time, 2, "0", side = "left"))
       
	
  }
  
  return(l_s) # return the monthly/annual emissions
  
} # end function

######################################################################################################
#### function to summarise input emissions
summariseUKIEemissions <- function(y, species, i, l_uk, s_uk, l_ie, s_ie, sea){
  
  # summarise the emissions totals
  dt <- data.table(Year = y,
                   Pollutant = species,
                   Region = c("uk","ie","sea"),
                   GNFR = dt_sec[sec == i, GNFRlong],
                   SNAP = dt_sec[sec == i, SNAP],
                   EMEP_Sector = dt_sec[sec == i, str_pad(EMEP_sec, 2, "0", side = "left")],
                   sec_long = i,
                   long_name = dt_sec[sec == i, name],
				   Data_source = "Emissions_files",
                   Incoming_annual_kt      = unlist(c(global(l_uk[["total"]], sum, na.rm=T)/1000, global(l_ie[["total"]], sum, na.rm=T)/1000, 0)), 
                   Terres_annual_kt   = unlist(c(global(l_uk[["terrestrial"]], sum, na.rm=T)/1000, global(l_ie[["terrestrial"]], sum, na.rm=T)/1000, 0)), 
                   Terres_10_annual_kt = unlist(c(global(l_uk[["terrestrial_10km"]], sum, na.rm=T)/1000, global(l_ie[["terrestrial_10km"]], sum, na.rm=T)/1000, sum(global(sea, sum, na.rm=T))/1000)),  
                   SEA_annual_kt           = unlist(c(global(l_uk[["sea"]], sum, na.rm=T)/1000, global(l_ie[["sea"]], sum, na.rm=T)/1000, sum(global(sea, sum, na.rm=T))/1000)), 
                   Out_of_Mask_annual_kt   = unlist(c(global(l_uk[["outwith_10km"]], sum, na.rm=T)/1000, global(l_ie[["outwith_10km"]], sum, na.rm=T)/1000, 0)) )
  
  
  # sum terrestrial monthly, for output stats. 
  dt_month <- data.table(Region = c("uk","ie","sea"))
    
  month_cols <- paste0("Terres_M",1:12)
  dt_month[, (month_cols) := lapply(1:12, function(x) c((global(s_uk, sum, na.rm=T)$sum/1000)[x], 
                                                               (global(s_ie, sum, na.rm=T)$sum/1000)[x],
							                                   (global(sea, sum, na.rm=T)$sum/1000)[x])) ]
														
  dt_month[, Terres_ann := rowSums(.SD, na.rm=T), .SDcols = month_cols]
  
  # join to main table. 
  dt <- dt[dt_month, on = "Region"]
  
  return(dt)  
  
} # end of function

######################################################################################################
#### function to create a netCDF and input the data
createNETCDFuk <- function(y, naei_inv, species, map_yr_uk, map_yr_ie, output_dir, time_dim, tp_scheme, uk_agg_schema,
                           l_uk_allSec, l_ie_allSec, l_sea_allSec, dt_emis_summary){
  
  # create output directory
  folname <- paste0(output_dir,"/emis",y,"/UKEIRE/TP",tp_scheme,"_AGG",uk_agg_schema)
  dir.create(file.path(folname), showWarnings = FALSE, recursive = T)   
  
  # create netcdf name
  nc_filename <- paste0(folname,"/", dt_poll[ceh_poll == species, emep_model],"_UKEIRE_",
                        y,"emis_",map_yr_uk,"map_",naei_inv,"inv_0.01.nc")
 
  
  # if the file already exists, just delete and rewrite
  if(file.exists(nc_filename)){
    
    print(paste0("NetCDF already exists for ",species, " in ", y,"; DELETING & REPLACING..."))
    file.remove(nc_filename)
    
  }else{}
  
  # netCDF variables are done on sector name and attributes determine sector, country etc
  # in this file, UK (27), EIRE (14) and SEA (171) remain separate
  # there are 12 month layers, or just one annual layer (dependent on time_dim)
  # the names, e.g. sec01, are taken from the lookup table 'dt_sec' and are currently what EMEP4UK requires
  
  # Set up the dimensions: latlong, time, sectors
  
  v_lon <- as.array(seq(xmin(r_dom_0.1) + 0.01/2, xmax(r_dom_0.1) - 0.01/2, 0.01))
  n_lon <- length(v_lon)
  v_lat <- as.array(seq(ymin(r_dom_0.1) + 0.01/2, ymax(r_dom_0.1) - 0.01/2, 0.01))
  n_lat <- length(v_lat)
  #timevals <- as.numeric(difftime(paste0(y,"-01-01"), dmy("01-01-1850")))
  if(time_dim == "year"){ v_time <- 1 }else{ v_time <- v_yday }
  n_time <- length(v_time)
  
  ## !! need some code above to incorporate possibility of annual data for v_time !! ##
  
  # create dimensions
  dimlon <- ncdim_def(name = "lon", longname= "longitude", units = "degrees_east", vals = v_lon)
  dimlat <- ncdim_def(name = "lat", longname= "latitude", units = "degrees_north", vals = v_lat)
  #dimtime <- ncdim_def(name = "time", units = "days since 1850-01-01 00:00", longname = "days", vals = timevals)
  dimtime <- ncdim_def(name = "time", units = "days since 2019-01-01 00:00", vals = v_time)
  
  # Create names and variables for UK, Eire and SEA SEPARATELY
  ## Ireland: ISO:  IE: country 14
  ## UK:      ISO:  GB: country 27
  ## buffer:  ISO: SEA: country 171
  v_sectors_uk   <- paste0("Emis_UK_", dt_sec[sec != "",name]) 
  v_sectors_ie   <- paste0("Emis_IE_", dt_sec[sec != "",name])
  v_sectors_sea  <- paste0("Emis_SEA_",dt_sec[sec != "",name]) 
  
  v_sectors <- c(v_sectors_uk, v_sectors_ie, v_sectors_sea)
  v_country <- c(rep(27, length(v_sectors_uk)), rep(14, length(v_sectors_ie)), rep(171, length(v_sectors_sea)))
  
  # for each sector in the given year, create a new netcdf var
  l_variables <- lapply(X = 1:length(v_sectors), function(s){
    ncvar_def(name = v_sectors[s],
              missval = EMEP_fillval, # _FillValue ?
              longname = str_split(v_sectors[s],"_")[[1]][3], # long_name?
              units = ifelse(time_dim == "year", "tonnes yr-1" , "tonnes month-1"), 
              dim = list(dimlon,dimlat,dimtime), 
              compression = 4,
              prec = "float")})
  
  ## Create the new netcdf
  ncnew <- nc_create(nc_filename, l_variables, force_v4=T)
  
  # now extract the data from the raster Stack and insert
  print(paste0(Sys.time(),":      Inserting data..."))
  
  l <- list()
  
  for(v in 1:length(v_sectors)){
    
    sec_name  <- v_sectors[v]
    sec_desc  <- substr(sec_name, str_locate_all(sec_name,"_")[[1]][2,2] + 1, nchar(sec_name))
    sec_EMEP  <- dt_sec[name == sec_desc, str_pad(EMEP_sec, 2, "0", side = "left")]
    sec_long  <- dt_sec[name == sec_desc, sec]
    sec_GNFR  <- dt_sec[name == sec_desc, GNFRlong]
    # if(sec_desc == "OtherStationaryComb") sec_GNFR <- ""
	
    ISO <- tolower(str_split(sec_name, "_")[[1]][2])
    ISO_num  <- v_country[v]
    
    l_insert <- get(paste0("l_",ISO,"_allSec"))
    s_insert <- l_insert[[paste0(ISO,"_",species, "_", sec_long,"_",time_dim)]]
    
	# FINAL check for -ve values
	if(any(global(s_insert, min, na.rm=T)$min < 0)) stop(paste0("NEG VALUES in ",species," in ",y,"!!"))
	
    # extract the year and pollutant and put in
    a <- array(s_insert, dim = c(n_lon, n_lat, n_time))
    a <- a[,1250:1,] # need to reverse the rows, for a reason i have not worked out. 
    
    ncvar_put(ncnew, sec_name, a)
    
    # few extra variable attributes
    ncatt_put(ncnew, varid = sec_name, attname = "long_name"  , attval = sec_desc, prec="char")
    ncatt_put(ncnew, varid = sec_name, attname = "description", attval = sec_desc, prec="char")
    ncatt_put(ncnew, varid = sec_name, attname = "sector"     , attval = as.integer(sec_EMEP),   prec="short")
    ncatt_put(ncnew, varid = sec_name, attname = "species"    , attval = ifelse(species=="nmvoc","voc",species) , prec="char")
    ncatt_put(ncnew, varid = sec_name, attname = "country"    , attval = ISO_num , prec="int")
    
    # summary of data going into netcdf
    dt <- data.table(Pollutant = species, Year = y, Region = ISO, long_name = sec_desc, Data_source = "NetCDF",
	                 EMEP_Sector = sec_EMEP, GNFR = sec_GNFR, time_res = time_dim, time_unit = 1:n_time, 
					 emis_kt = round(global(s_insert, sum, na.rm=T)[,1]/1000, 6))
    dtw <- dcast(dt, Pollutant+Year+Region+long_name+Data_source+EMEP_Sector+GNFR ~ time_res+time_unit, value.var = "emis_kt")
    if(time_dim == "year"){ v_in <- 1 }else if(time_dim == "month"){ v_in <- 1:12 }
	if(time_dim == "month"){
	  setnames(dtw, paste0("month_",1:12), paste0("Terres_M",1:12))
	  dtw[, Terres_ann := rowSums(.SD, na.rm=T), .SDcols = paste0("Terres_M",v_in)]
	}
	
    	
    l[[v_sectors[v]]] <- dtw
    
    remove(a)
    gc()
    
  }
  
  
  ## Finally the global attributes
  ncatt_put(ncnew, 0, "description","UK NAEI & MapEire", prec="char")
  ncatt_put(ncnew, 0, "Conventions","CF-1.0", prec="char")
  ncatt_put(ncnew, 0, "projection","lon lat", prec="char")
  ncatt_put(ncnew, 0, "Grid_resolution", "0.01", prec="char")
  ncatt_put(ncnew, 0, "Created_with",R.Version()$version.string, prec="char")
  ncatt_put(ncnew, 0, "ncdf4_version", packageDescription("ncdf4")$Version, prec="char")
  ncatt_put(ncnew, 0, "Created_by","Sam Tomlinson samtom@ceh.ac.uk", prec="char")
  ncatt_put(ncnew, 0, "Created_date", as.character(Sys.time()), prec="char")
  #ncatt_put(ncnew, 0, "Sector_names", class, prec="char")
  #ncatt_put(ncnew, 0, "sec01", "publicpower", prec="char")
  #ncatt_put(ncnew, 0, "sec02", "domestic", prec="char")
  #ncatt_put(ncnew, 0, "sec03", "industrialcombustion", prec="char")
  #ncatt_put(ncnew, 0, "sec04", "industrialprocessing", prec="char")
  #ncatt_put(ncnew, 0, "sec05", "fugitive", prec="char")
  #ncatt_put(ncnew, 0, "sec06", "solvents", prec="char")
  #ncatt_put(ncnew, 0, "sec07", "roadtransport", prec="char")
  #ncatt_put(ncnew, 0, "sec08", "othertransport", prec="char")
  #ncatt_put(ncnew, 0, "sec09", "waste", prec="char")
  #ncatt_put(ncnew, 0, "sec10", "agrilivestock", prec="char")
  #ncatt_put(ncnew, 0, "sec11", "natureother", prec="char")
  
  ncatt_put(ncnew, 0, "periodicity", ifelse(time_dim=="month","monthly","annual"), prec="char")
  ncatt_put(ncnew, 0, "NCO","netCDF Operators version 4.9.8 (Homepage = http://nco.sf.net, Code = http://github.com/nco/nco)", prec = "char")
  
  ncatt_put(ncnew, 0, "emissions_year",y, prec="int")
  ncatt_put(ncnew, 0, "inventory_year",naei_inv, prec="int")
  ncatt_put(ncnew, 0, "UK_map_year",map_yr_uk, prec="int")
  ncatt_put(ncnew, 0, "EIRE_map_year",map_yr_ie, prec="int")
  
  nc_close(ncnew)
  
  dt_ncdf_summary <- rbindlist(l, use.names = T)
  
  dt_final_summary <- rbindlist(list(dt_emis_summary, dt_ncdf_summary), use.names = T, fill = T)
  keycols <- c("Region","EMEP_Sector","long_name","Data_source")
  setorderv(dt_final_summary, keycols)
  #dt_ncdf_summary <- dt_ncdf_summary[dt_emis_summary, on = c("Pollutant","Year","Region","long_name","EMEP_Sector","GNFR")]
  
  if(species == "nmvoc"){
    fwrite(dt_final_summary, paste0(folname,"/voc_UKEIRE_",y,"emis_",map_yr_uk,"map_",naei_inv,"inv_SUMMARY.csv"))
  }else{
    fwrite(dt_final_summary, paste0(folname,"/",species,"_UKEIRE_",y,"emis_",map_yr_uk,"map_",naei_inv,"inv_SUMMARY.csv"))
  }
    
  
} # end of function

######################################################################################################
#### function to write out some metadata for the run

writeMetadataUK <- function(y, species, naei_inv, map_yr_uk, map_yr_ie, output_dir, time_dim, tp_scheme, uk_agg_schema, alt_emis){

  # create some metadata for the run
  fileName <- paste0(output_dir,"/emis",y,"/UKEIRE/TP",tp_scheme,"_AGG",uk_agg_schema,"/Metadata_UKEIRE_",format(now(), "%Y%m%d_%H%M%S"),".txt")
  
  suppressMessages(file.create(fileName))
  
  file_conn <- file(fileName)  

  # temporal profile text
  if(tp_scheme == "pre_TEMREG"){
    tp_scheme_text <- paste0("/gws/nopw/j04/ceh_generic/samtom/TEMREG/output/model_inputs/EMEP4UK/",tp_scheme)
  }else{
    tp_scheme_text <- paste0("/gws/nopw/j04/ceh_generic/samtom/TEMREG/output/coeff_sector/GNFR/",species,"/",tp_scheme)
  }
      
  # alternative emissions source text
  if("nh3" %in% alt_emis[,poll]){nh3_loc_text <- alt_emis[poll == "nh3", loc]}else{nh3_loc_text  <- "/gws/nopw/j04/ceh_generic/inventory_processor/data"}
  if("nox" %in% alt_emis[,poll]){nox_loc_text <- alt_emis[poll == "nox", loc]}else{nox_loc_text  <- "/gws/nopw/j04/ceh_generic/inventory_processor/data"}
  if("sox" %in% alt_emis[,poll]){sox_loc_text <- alt_emis[poll == "sox", loc]}else{sox_loc_text  <- "/gws/nopw/j04/ceh_generic/inventory_processor/data"}



  writeLines(c(paste0("File creation timestamp: ", Sys.time()),
               paste0("Folder name: ", output_dir),
               paste0("Pollutants generated: ", species),
			   paste0("Year of emissions generated: ", y),
			   paste0("NAEI inventory used: ", naei_inv),
			   paste0("NAEI UK map year used: ", map_yr_uk),
			   paste0("MapEire Eire map year used: ", map_yr_ie),
			   paste0("Aggregation schema: ", uk_agg_schema),
			   paste0("Periodicity of netcdf data: ", time_dim),
			   paste0("Schema for temporal profiles: ", tp_scheme_text),
			   paste0("UK temporal profiles: SNAP onto GNFR"),
			   paste0(""),
			   if("nh3" %in% alt_emis[,poll])paste0("Location of NH3 emissions: ", alt_emis[poll == "nh3", loc]),
			   if("nox" %in% alt_emis[,poll])paste0("Location of NOx emissions: ", alt_emis[poll == "nox", loc]),
			   if("sox" %in% alt_emis[,poll])paste0("Location of SOx emissions: ", alt_emis[poll == "sox", loc]),
			   "Location of NAEI/EIRE emissions: /gws/nopw/j04/ceh_generic/inventory_processor/data"), file_conn)
			   
  close(file_conn)





}

######################################################################################################
#### function to plot some data for the year of emissions

plotYearUK <- function(species, output_dir, y, tp_scheme, uk_agg_schema, naei_inv, map_yr_uk){
  
  # set output directory and create plots folder
  folname <- paste0(output_dir,"/emis",y,"/UKEIRE/TP",tp_scheme,"_AGG",uk_agg_schema)
  dir.create(file.path(folname,"plots"), showWarnings = FALSE, recursive = T)   
  
  # set filename to read
  nc_file <- paste0(folname, "/", dt_poll[ceh_poll == species, emep_model],"_UKEIRE_",y,"emis_",map_yr_uk,"map_",naei_inv,"inv_0.01.nc")
  
  #nc <- nc_open(paste0(folname, "/", nc_file))
  #nc_close(nc)
  
  # gather sector info
  v_sectors <- dt_sec[,unique(sec)]
  
  l <- list()
  
  for(i in v_sectors){
  
    if(i == "") next # not interested in currently blank EMEP-named sectors
    if(dt_sec[sec == i, GNFRlong]  == "" ) next
    print(paste0(Sys.time(),":         ",i))
	
	nc_sec <- dt_sec[sec == i, name]
		
    # stack and raster total
    s_UK <- rast(nc_file, subds = paste0("Emis_UK_",nc_sec))
	s_IE <- rast(nc_file, subds = paste0("Emis_IE_",nc_sec))
        
    # annual total rasters
    r_UK <- app(s_UK, sum, na.rm=T)
	r_IE <- app(s_IE, sum, na.rm=T)
	
	# summarise all 
	dt_uk_mon <- data.table(Pollutant = dt_poll[ceh_poll == species, emep_model], Year = y, Sector = nc_sec, ISO = "UK", month = 1:12, nc_emis_t = global(s_UK, sum, na.rm=T)$sum)
	dt_uk_ann <- data.table(Pollutant = dt_poll[ceh_poll == species, emep_model], Year = y, Sector = nc_sec, ISO = "UK", month = 0, nc_emis_t = global(r_UK, sum, na.rm=T)$sum)
	dt_ie_mon <- data.table(Pollutant = dt_poll[ceh_poll == species, emep_model], Year = y, Sector = nc_sec, ISO = "IE", month = 1:12, nc_emis_t = global(s_IE, sum, na.rm=T)$sum)
	dt_ie_ann <- data.table(Pollutant = dt_poll[ceh_poll == species, emep_model], Year = y, Sector = nc_sec, ISO = "IE", month = 0, nc_emis_t = global(r_IE, sum, na.rm=T)$sum)
	
	dt <- rbindlist(list(dt_uk_mon, dt_uk_ann, dt_ie_mon, dt_ie_ann), use.names = T)
	
	l[[i]] <- dt  
  
  }
    
  dt_ncdf <- rbindlist(l, use.names=T)
  
  #fwrite(dt_ncdf, paste0(folname, "/plots/nox_data.csv"))
  
  # plot up the data in several ways
  # annual data has month = 0
  
  # plot of monthly 
  dt_mon <- dt_ncdf[month != 0]
  
  g_mon <- ggplot(dt_mon, aes(x = month, y = nc_emis_t/1000, group = Sector, colour = Sector))+
    geom_line()+
    scale_x_continuous(breaks = 1:12)+
    guides(color = "none")+
    labs(y = bquote(kt~month^-1))+
    facet_wrap(~ISO, ncol=1, scales = "free_y")+
    theme_bw()+
    theme(strip.text = element_text(size = 20),
          #legend.title = element_blank(), 
          legend.position = "none",
          #legend.text = element_text(size = 16),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size = 14))
		
  # plot of annual
  dt_ann <- dt_ncdf[month == 0]
   
  g_ann <- ggplot(dt_ann, aes(x = Sector, y = nc_emis_t/1000, fill = Sector))+
    geom_bar(stat="identity")+
	#scale_x_continuous(breaks = 1:12)+
    labs(y = bquote(kt~month^-1))+
    facet_wrap(~ISO, ncol=1, scales = "free_y")+
    theme_bw()+
    theme(strip.text = element_text(size = 20),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size = 14),
          legend.title = element_blank(),
          legend.text = element_text(size = 16))

  # combine and write
  p <- g_mon + g_ann + 
             plot_layout(widths = c(2, 1), guides = "collect") & 
			 theme(legend.position = 'bottom')

  fname <- paste0(folname,"/plots/",dt_poll[ceh_poll == species, emep_model],"_UKEIRE_",y,"emis_",map_yr_uk,"map_",naei_inv,"inv.png")

  ggsave(fname, p, width = 14, height = 10)
 
}
















