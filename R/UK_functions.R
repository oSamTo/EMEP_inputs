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

######################################################################################################
#### function to take NAEI emissions, make ready to EMEP format and create netCDFs for UK & Eire
EMEPinputUK <- function(v_years, species, uk_agg_schema, time_dim = c("annual","month","yday"), tp_scheme, 
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
      
      ######################################################################################
      if(!(species %in% c("ch4","co2","n2o","bap","bz","hcl","nox","sox","nh3", "co", "nmvoc","cd","cu","pb","hg","ni","zn", "pm0_1","pm1","pm25","pm10","pmco")) ) stop ("Species must be in: 
                                            AP:    bap, bz, co, hcl, nh3, nox, sox, voc
                                            PM:    pm0_1, pm1, pm2_5, pm10, pmco
                                            GHG:   ch4, co2, n2o
                                            Metal: cd, cu, hg, ni, pb, zn")
      ######################################################################################
      
      print(paste0(Sys.time(),": Creating EMEP4UK UK/EIRE input netCDF for ",dt_poll[ceh_poll == species, emep_model]," in ",y,"..."))
           
	  
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
	  l_ow_allSec  <- list()
      
      # sector totals list
      l_inv_summary <- list()
	  l_mask_summary <- list()
	  l_group_summary <- list()
      
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
        
		# we do not make a new sea list here, because the temporal profiles don't exist.
		# i.e. we need to keep the sea buffer emissions associated with country of origin. 
        l_uk <- UKIEsectorEmissions(emis_loc, species, y, i, time_dim, res_crs, map_yr = map_yr_uk, naei_inv, country = "uk")
        l_ie <- UKIEsectorEmissions(emis_loc, species, y, i, time_dim, res_crs, map_yr = map_yr_ie, naei_inv, country = "ie")
        
        ########################
        #### TEMPORAL SPLIT ####
        # if the time_dim is year, the data stays as 1 annual total.
        # if the time_dim is month, the data needs to be split into 12 layers
            # this is either the temporal profiles from inside EMEP4UK
            # or the newly generated stuff that has come out of DUKEMs
        # the sea & outwith layers need to be split based on the country it came from (i.e. UK or Eire)
        
        l_uk_prof   <- splitUKIEannual(species = species, time_dim, tp_scheme, l_annual = l_uk, i = i, country = "uk")
        l_ie_prof   <- splitUKIEannual(species = species, time_dim, tp_scheme, l_annual = l_ie, i = i, country = "ie")
        
        ###########################
		#### AREA BASED STACKS ####
		# create stacks for UK, Eire, SEA buffer and outwith UK domain (annual or monthly), separate & total
        
		l_s_uk  <- stack_data(l_uk_prof, l_ie_prof, mask = "uk")
		l_s_ie  <- stack_data(l_uk_prof, l_ie_prof, mask = "ie")
		l_s_sea <- stack_data(l_uk_prof, l_ie_prof, mask = "sea")
		l_s_ow  <- stack_data(l_uk_prof, l_ie_prof, mask = "ow")
				        	
        ###################
        #### COLLATING ####
        # add temporal raster stacks to lists that will go into NetCDF (so just certain ones)
        
        l_uk_allSec[[paste0("uk_", dt_poll[ceh_poll == species, emep_model], "_", i,"_",time_dim)]]   <- l_s_uk
        l_ie_allSec[[paste0("ie_", dt_poll[ceh_poll == species, emep_model], "_", i,"_",time_dim)]]   <- l_s_ie
        l_sea_allSec[[paste0("sea_", dt_poll[ceh_poll == species, emep_model], "_", i,"_",time_dim)]] <- l_s_sea
		l_ow_allSec[[paste0("ow", dt_poll[ceh_poll == species, emep_model], "_", i,"_",time_dim)]]    <- l_s_ow
        
        ####################
        #### STATISTICS ####        
		# annual inventory summary for UK & IE
		dt_inv_summary <- rbindlist(list(l_uk$ann_summary, l_ie$ann_summary), use.names = T)
		
		# make annual summaries for all masked areas
		l_maskgroup_summary <- summarise_UKIE_emissions(y, naei_inv, species, i, time_dim,
                                                        l_uk_inv = l_uk, l_ie_inv = l_ie, 
														l_s_uk = l_s_uk, l_s_ie = l_s_ie,
														l_s_sea = l_s_sea, l_s_ow = l_s_ow)
														
		dt_mask_summary <- l_maskgroup_summary[["mask"]]
		dt_group_summary <- l_maskgroup_summary[["group"]]
        
        l_inv_summary[[paste0("totals_", dt_poll[ceh_poll == species, emep_model], "_", i,"_", time_dim)]] <- dt_inv_summary
        l_mask_summary[[paste0("totals_", dt_poll[ceh_poll == species, emep_model], "_", i,"_", time_dim)]] <- dt_mask_summary
		l_group_summary[[paste0("totals_", dt_poll[ceh_poll == species, emep_model], "_", i,"_", time_dim)]] <- dt_group_summary
         
      } # sector loop
	  
	  ####################################################
      #### AGGREGATION BY SECTOR OR ISO (IF REQUIRED) ####
	  	  
	  #print(paste0(Sys.time(),":               Aggregating by sector and/or ISO code..."))
	  #l_eu_agg <- aggregateEU(y, species, schema, time_dim, l_eu_allSec)
	  ## code to go here for any aggregations, e.g. ISO code ##
	  # will need to create 'l_uk_agg' object, like in EU code
      
      ############################################################
      #### CREATE AND POPULATE NETCDF ON POLLUTANT/YEAR BASIS ####
      
      print(paste0(Sys.time(),":      Creating and populating netcdf..."))
      
	  target_folname <- archive_data(y, tp_scheme, uk_agg_schema, output_dir)
	  
      dt_ncinput_summary <- createNETCDFuk(y, naei_inv, species, map_yr_uk, map_yr_ie, 
	                                       time_dim, folname = target_folname, tp_scheme, uk_agg_schema,
                                           l_uk_allSec, l_ie_allSec, l_sea_allSec)
      	  
	  ####################################
      #### COLLATE/WRITE SUMMARY DATA ####	  
	  print(paste0(Sys.time(),": Summarising netCDF files for ",dt_poll[ceh_poll == species, emep_model]," in ",y,"..."))
	  
	  dt_inv_summary  <- rbindlist(l_inv_summary, use.names=T)
	  dt_mask_summary <- rbindlist(l_mask_summary, use.names=T)
	  dt_group_summary <- rbindlist(l_group_summary, use.names=T)
	  # dt_ncinput_summary # as above
	  
	  # another nc summary from file, post writing. Double checker. 
	  dt_ncoutput_summary <- summarise_nc_file(species, y, folname = target_folname, 
	                                           map_yr_uk, naei_inv, time_dim)	  	    
	  
	  write_summaries(naei_inv, species, map_yr_uk, folname = target_folname,
					  dt_inv = dt_inv_summary, dt_mask = dt_mask_summary, dt_group = dt_group_summary,
					  dt_ncinp = dt_ncinput_summary, dt_ncout = dt_ncoutput_summary)
  
	  ###################
      #### PLOT DATA ####	  
	  # make some year-specific plots
	  print(paste0(Sys.time(),": Plotting emissions data for ",dt_poll[ceh_poll == species, emep_model]," in ",y,"..."))
	  
	  #   plotYearUK(species, output_dir, y, tp_scheme, uk_agg_schema, naei_inv, map_yr_uk)
	  
	  
	  
	  ##################
      #### QAQC DOC ####	  
	  


	  ###########################
	  #### METADATA - DELETE ####
	  
	  # write some metadata - ON HOLD - this can go in QAQC
	  #print(paste0(Sys.time(),": Writing metadata for ",species," in ",y,"..."))
      #writeMetadataUK(y, species, naei_inv, map_yr_uk, map_yr_ie, output_dir, time_dim, tp_scheme, uk_agg_schema, alt_emis)
		
	  print(paste0(Sys.time(),": DONE..."))
	
  } # year loop
  
} # end of function
        

######################################################################################################
#### function to collect sector data for diffuse and points, based on country and sector
UKIEsectorEmissions <- function(emis_loc, species, y, i, time_dim, res_crs, map_yr, naei_inv, country = c("uk", "ie")){
  
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
	
	# subset the value required. GNFR UK maps are forced from SNAP maps. Remember;
			# to scale industry by SNAPs 3 & 4 together (inventory processor combines 3 & 4 to B_Industry)
			# all of SNAP 8 is in I_Offroad (inventory_processor).
				# While G_Shipping and H_Aviation are empty, shouldn't take the risk using SN08 > once
			# all of SNAP 10 is in K_AgriLivestock (inventory_processor).
				# While L_AgriOther is empty, shouldn't take the risk using SN10 > once
			# set secs 14 to 19 as NA, as they are not used yet, and causes double counting in summary
				
	if(i == "sec02"){
	  scaling_value <- dt_alpha[Pollutant == species & Year == y & AREA == toupper(country) & SNAP %in% c(3:4), sum(tot_emis_t, na.rm=T)]
	}else if(i %in% c("sec07","sec08","sec12")){
	  scaling_value <- NA
	}else if(i %in% c("sec14","sec15","sec16","sec17","sec18","sec19")){
	  scaling_value <- NA
	}else{
	  scaling_value <- dt_alpha[Pollutant == species & Year == y & AREA == toupper(country) & SNAP == dt_sec[sec == i, SNAP], tot_emis_t]
	}
	
	if(length(scaling_value) == 0) scaling_value <- 0
	if(is.na(scaling_value)) scaling_value <- 0
  
  }else if(country == "ie"){
  
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
  rs <- (r / global(r, sum, na.rm=T)$sum) * scaling_value 
  

  ###############
  ### MASKING ###
    
  # EMEP needs zero value, not NA, in emissions
  rs[is.na(r)] <- 0 ; rs[is.nan(r)] <- 0 ; rs[is.infinite(r)] <- 0 
  
  # Mask to the EMEP input restrictions
  r_t   <- mask(rs,     r_dom_terr)                 # emissions on UK&EIRE land territory
  r_t[is.na(r_t)] <- 0 ; r_t[is.nan(r_t)] <- 0
  
  r_t10 <- mask(rs,     r_dom_terr_10km)            # emissions on UK&EIRE land territory + 10km sea buffer
  r_t10[is.na(r_t10)] <- 0 ; r_t10[is.nan(r_t10)] <- 0
  
  r_ow  <- mask(rs,     r_dom_terr_10km, inverse=T) # emissions outwith UK&EIRE land + 10km buffer (i.e. rest of domain)
  r_ow[is.na(r_ow)] <- 0 ; r_ow[is.nan(r_ow)] <- 0
  
  r_sea <- mask(r_t10, r_dom_terr, inverse=T)      # emissions only in the 10km sea buffer
  r_sea[is.na(r_sea)] <- 0 ; r_sea[is.nan(r_sea)] <- 0
  
  ###################
  ### RETURN DATA ###
  
  # also make a scaling/inventory data table - no summary of masked etc. only inventory
  dt_inv <- data.table(Area = country, 
					   Pollutant = ifelse(species=="nmvoc","voc",species), 
					   data_source = "inventory",
					   emis_y = y, 
					   inv_y = naei_inv, 
					   sec_EMEP = i,
					   sec_GNFR = dt_sec[sec == i, GNFRlong], 
					   sec_SNAP = dt_sec[sec == i, SNAP],
					   sec_long = dt_sec[sec == i, name],
					   time_res = time_dim,
                       emis_t_diffmap = global(r_diff, sum, na.rm=T)$sum, 
					   emis_t_pt = global(r_pt, sum, na.rm=T)$sum,
					   emis_t_inv_spatial = global(r, sum, na.rm=T)$sum, 
					   emis_t_inv_table = scaling_value,
					   emis_t_spatial_scaled = global(rs, sum, na.rm=T)$sum)
  
  l <- list(r, r_t, r_t10, r_ow, r_sea, dt_inv)
  names(l) <- c("total", "terrestrial","terrestrial_10km","outwith_10km","sea", "ann_summary")
  
  return(l)
  
} # end of function


######################################################################################################
#### function to split annual emissions out into months (or keep as annual)
splitUKIEannual <- function(species, time_dim = c("annual","month","yday"), tp_scheme, l_annual, i, country = c("uk","ie","sea")){
  
  country    <- match.arg(country)
  time_dim <- match.arg(time_dim)
  
  if(time_dim == "annual"){ i_time <- 1 }else if(time_dim == "month"){ i_time <- 1:12 }else{ i_time <- 1:365 }
  
  if(time_dim == "annual"){
    
    l_s <- c(l_annual[["terrestrial"]], l_annual[["sea"]])
    names(l_s) <- c("terrestrial","sea")
    
	names(l_s[["terrestrial"]]) <- paste0(dt_poll[ceh_poll == species, emep_model], "_", country,"_","terrestrial", "_", i,"_",str_pad(i_time, 2, "0", side = "left"))
    names(l_s[["sea"]]) <- paste0(dt_poll[ceh_poll == species, emep_model], "_", country,"_","sea", "_", i,"_",str_pad(i_time, 2, "0", side = "left"))
        
  }else if(time_dim == "month"){
    
    ## Use the nominated temporal schema to split the data to monthly layers
	
	## If the tp_scheme = version of EMEP4UK (e.g. 'EMEP4UKv4.45');
	     # this is temporal data of the EMEP model, for that version (e.g.: EMEP4UKv4.45 = July '23)
    ## HOWEVER if the tp_scheme != version of EMEP4UK;
	     # we can use the GNFR generated monthly profiles, instead of the SNAP level ones in the model input
		 # the GNFR ones pertain more to the GNFR sector, instead of being combined to SNAP
		 # this is ok re emissions, as it's now able to take more maps, but sadly we are constrained to SNAP. 
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
	
		
	}else if(grepl("ukem_", tp_scheme)){
	# IF the tp_scheme is generated by ukem_pro, use the SNAP csv output;
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
		
		  dt_tempro <- fread(paste0("/gws/nopw/j04/ukem/test/sam/ukem_pro/output/coeff_sector/SNAP/",species,
		                            "/",strsplit(tp_scheme,"_")[[1]][2], "/SNAP_",dt_sec[sec == i, str_pad(SNAP, 2, "0", side = "left")],
							        "_month_",species,"_",strsplit(tp_scheme,"_")[[1]][2],".csv"))
	  
	      v_tempro <- dt_tempro[,N] # vector of monthly splits 
		  v_tempro[v_tempro < 0] <- 0.02 # rare occasion factor goes below 0, set to 0.02 (i.e. some emissions, but tiny)
	      v_tempro <- v_tempro/mean(v_tempro)# not always adding to 12 in the tempro file, adjust slightly
		
		}else{
          		
		  dt_tempro <- fread(paste0("/gws/nopw/j04/ukem/test/sam/ukem_pro/output/coeff_sector/GNFR19/",species,
		                            "/",strsplit(tp_scheme,"_")[[1]][2],"/GNFR19_",dt_sec[sec == i, GNFRlong],
									"_month_",species,"_",strsplit(tp_scheme,"_")[[1]][2],".csv"))
	  
	      v_tempro <- dt_tempro[,N] # vector of monthly splits 
		  v_tempro[v_tempro < 0] <- 0.02 # rare occasion factor goes below 0, set to 0.02 (i.e. some emissions, but tiny)
	      v_tempro <- v_tempro/mean(v_tempro)# not always adding to 12 in the tempro file, adjust slightly
		
		}
	    	  	  
	  }
		
	}else if(tp_scheme == "test"){
	# test bed ability
	  v_tempro <- runif(12,0.5,1.5)
	  v_tempro <- v_tempro/mean(v_tempro)
	
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
    
    l_s_month <- lapply(l_annual[c("terrestrial","sea","outwith_10km")], function(x) rep(x/12, 12))
    
    # adjust with temporal profile
    
    l_s <- lapply(l_s_month, function(x) x * v_tempro)
    
	# error if any negative values remain
	if(any(global(l_s[["terrestrial"]], min, na.rm=T) < 0)) stop("there are negative emissions values (land)")
	if(any(global(l_s[["sea"]], min, na.rm=T) < 0)) stop("there are negative emissions values (sea)")
	if(any(global(l_s[["outwith_10km"]], min, na.rm=T) < 0)) stop("there are negative emissions values (outwith_10km)")
	
	names(l_s)[3] <- "outwith"
	
    names(l_s[["terrestrial"]]) <- paste0(dt_poll[ceh_poll == species, emep_model], "_", country,"_","terrestrial", "_", i,"_",str_pad(i_time, 2, "0", side = "left"))
    names(l_s[["sea"]]) <- paste0(dt_poll[ceh_poll == species, emep_model], "_", country,"_","sea", "_", i,"_",str_pad(i_time, 2, "0", side = "left"))
    names(l_s[["outwith"]]) <- paste0(dt_poll[ceh_poll == species, emep_model], "_", country,"_","outwith", "_", i,"_",str_pad(i_time, 2, "0", side = "left"))
	
  } # end of month if. Will need a yday 'if' in future. 
  
  
  return(l_s) # return the monthly/annual emissions
  
} # end function

######################################################################################################
#### function to create stacks of emissions for different mask areas
stack_data <- function(l_uk_prof, l_ie_prof, mask = c("uk","ie","sea","ow")){

  s_blank <- l_uk_prof[[1]] %>% setValues(.,0)

  mask <- match.arg(mask)

  if(time_dim == "annual"){ v_in <- 1 }else if(time_dim == "month"){ v_in <- 1:12 }else{v_in <- 1:365} 
  
  if(mask %in% c("uk","ie")){
  
    l_prof <- get(paste0("l_",mask,"_prof"))
	
	s_all <- tapp(c(l_prof[["terrestrial"]], l_prof[["sea"]], l_prof[["outwith"]]), v_in, sum, na.rm=T)
	names(s_all) <- paste0(dt_poll[ceh_poll == species, emep_model],"_",mask,"_all_",i,"_",str_pad(v_in, 2, "0", side = "left"))
	s_ter <- l_prof[["terrestrial"]]
    s_sea <- l_prof[["sea"]]
    s_ow <- l_prof[["outwith"]]
  
  }else if (mask == "sea"){
    
	s_all <- s_blank
    names(s_all) <- paste0(dt_poll[ceh_poll == species, emep_model],"_",mask,"_all_",i,"_",str_pad(v_in, 2, "0", side = "left"))	
	s_ter <- s_blank
    names(s_ter) <- paste0(dt_poll[ceh_poll == species, emep_model],"_",mask,"_terrestrial_",i,"_",str_pad(v_in, 2, "0", side = "left"))	
    s_sea <- tapp(c(l_uk_prof[["sea"]], l_ie_prof[["sea"]]), v_in, sum, na.rm=T)
    names(s_sea) <- paste0(dt_poll[ceh_poll == species, emep_model],"_",mask,"_sea_",i,"_",str_pad(v_in, 2, "0", side = "left"))	
    s_ow <- s_blank
	names(s_ow) <- paste0(dt_poll[ceh_poll == species, emep_model],"_",mask,"_outwith_",i,"_",str_pad(v_in, 2, "0", side = "left"))	
  
  }else if (mask == "ow"){
    
	s_all <- s_blank
    names(s_all) <- paste0(dt_poll[ceh_poll == species, emep_model],"_",mask,"_all_",i,"_",str_pad(v_in, 2, "0", side = "left"))	
	s_ter <- s_blank
    names(s_ter) <- paste0(dt_poll[ceh_poll == species, emep_model],"_",mask,"_terrestrial_",i,"_",str_pad(v_in, 2, "0", side = "left"))	
    s_sea <- s_blank
    names(s_sea) <- paste0(dt_poll[ceh_poll == species, emep_model],"_",mask,"_sea_",i,"_",str_pad(v_in, 2, "0", side = "left"))	
    s_ow <- tapp(c(l_uk_prof[["outwith"]], l_ie_prof[["outwith"]]), v_in, sum, na.rm=T)
	names(s_ow) <- paste0(dt_poll[ceh_poll == species, emep_model],"_",mask,"_outwith_",i,"_",str_pad(v_in, 2, "0", side = "left"))
  
  }
  
  l <- list("all" = s_all, "terrestrial" = s_ter, "sea" = s_sea, "outwith" = s_ow)
  
  return(l)
}

######################################################################################################
#### function to summarise input emissions
summarise_UKIE_emissions <- function(y, naei_inv, species, i, time_dim, 
                                     l_uk_inv = l_uk, l_ie_inv = l_ie, 
									 l_s_uk = l_s_uk, l_s_ie = l_s_ie,
									 l_s_sea = l_s_sea, l_s_ow = l_s_ow){
    
	
  if(time_dim == "annual"){ i_time <- 1 }else if(time_dim == "month"){ i_time <- 1:12 }else{ i_time <- 1:365 }
  time_cols <- paste0("t",i_time)
  
  ## MASK SUMMARY for UK & IE
  # basic info
  dt_mask <- data.table(Area = c("uk","uk","uk","ie","ie","ie"), 
                   mask = c("terrestrial","sea","outwith","terrestrial","sea","outwith"),
                   Pollutant = ifelse(species=="nmvoc","voc",species),
				   data_source = "masked",
	       	       emis_y = y, 
				   inv_y = naei_inv, 
				   sec_EMEP = i,
				   sec_GNFR = dt_sec[sec == i, GNFRlong], 
				   sec_SNAP = dt_sec[sec == i, SNAP],
				   sec_long = dt_sec[sec == i, name],
				   time_res = time_dim)
    
  # add in annual totals - in country order as above
  dt_mask[, emis_t_tot_masked := c(global(l_uk_inv[["terrestrial"]], sum, na.rm=T)[,1],
                              global(l_uk_inv[["sea"]],              sum, na.rm=T)[,1],
							  global(l_uk_inv[["outwith_10km"]],     sum, na.rm=T)[,1],
							  global(l_ie_inv[["terrestrial"]],      sum, na.rm=T)[,1],
							  global(l_ie_inv[["sea"]],              sum, na.rm=T)[,1],
							  global(l_ie_inv[["outwith_10km"]],     sum, na.rm=T)[,1]) ]
  
  # summarise the monthly emissions totals
  dt_mask[, (time_cols) := lapply(i_time, function(x) c((global(l_s_uk[["terrestrial"]], sum, na.rm=T)[,1])[x], 
                                                        (global(l_s_uk[["sea"]],         sum, na.rm=T)[,1])[x],
							                            (global(l_s_uk[["outwith"]],     sum, na.rm=T)[,1])[x],
												        (global(l_s_ie[["terrestrial"]], sum, na.rm=T)[,1])[x], 
                                                        (global(l_s_ie[["sea"]],         sum, na.rm=T)[,1])[x],
							                            (global(l_s_ie[["outwith"]],     sum, na.rm=T)[,1])[x])) ]
  														
  dt_mask[, tsum := rowSums(.SD, na.rm=T), .SDcols = time_cols]
  dt_mask[, tot_tres_ratio := emis_t_tot_masked / tsum]
    
  ## GROUPED SUMMARIES for IE, UK, SEA, OW
  # basic info
  dt_group <- data.table(Area = c("uk","ie","sea","ow"),                   
                         Pollutant = ifelse(species=="nmvoc","voc",species),
				         data_source = "grouped",
	       	             emis_y = y, 
				         inv_y = naei_inv, 
				         sec_EMEP = i,
				         sec_GNFR = dt_sec[sec == i, GNFRlong], 
				         sec_SNAP = dt_sec[sec == i, SNAP],
				         sec_long = dt_sec[sec == i, name],
				         time_res = time_dim)
    
  # add in annual totals - in country order as above
  dt_group[, emis_t_tot_grouped := c(sum(global(l_s_uk[["terrestrial"]],  sum, na.rm=T)[,1], na.rm=T),
                                     sum(global(l_s_ie[["terrestrial"]],  sum, na.rm=T)[,1], na.rm=T),
								 	 sum(global(l_s_sea[["sea"]],         sum, na.rm=T)[,1], na.rm=T),
								 	 sum(global(l_s_ow[["outwith"]],      sum, na.rm=T)[,1], na.rm=T)) ]
  
  # summarise the monthly emissions totals
  dt_group[, (time_cols) := lapply(i_time, function(x) c((global(l_s_uk[["terrestrial"]], sum, na.rm=T)[,1])[x], 
                                                         (global(l_s_ie[["terrestrial"]], sum, na.rm=T)[,1])[x],
							                             (global(l_s_sea[["sea"]],        sum, na.rm=T)[,1])[x],
												         (global(l_s_ow[["outwith"]],     sum, na.rm=T)[,1])[x])) ]
  														
  dt_group[, tsum := rowSums(.SD, na.rm=T), .SDcols = time_cols]
  dt_group[, tot_tres_ratio := emis_t_tot_grouped / tsum]
	
  l <- list("mask" = dt_mask, "group" = dt_group)	
  
  return(l)  
  
} # end of function

######################################################################################################
#### function to create directory and archive anything that exists in the target directory. 
archive_data <- function(y, tp_scheme, uk_agg_schema, output_dir){

  print(paste0("Creating new directory and archiving previously run data..."))

  folname <- paste0(output_dir,"/emis",y,"/UKEIRE/TP",tp_scheme,"_AGG",uk_agg_schema)
  dir.create(file.path(folname), showWarnings = FALSE, recursive = T)  
  
  # collect all files and folders named 'plots','tables' and 'qaqc' to move to archive folder.
  v_files <- list.files(folname, full.name = FALSE, recursive = FALSE, include.dirs = FALSE, pattern = ".nc$")
  v_fols  <- c("plots","tables","qaqc")
  
  if(length(v_files) > 0){
  
    archive_folname <- paste0(folname,"/",run_clock)
    dir.create(file.path(archive_folname), showWarnings = FALSE, recursive = TRUE)
	
	sapply(v_files, function(x) file.rename(from = file.path(folname, x), to = file.path(folname, run_clock, x)))
	  
  }
  
  if(sum(dir.exists(paste0(folname,"/",v_fols))) > 0){
  
    archive_folname <- paste0(folname,"/",run_clock)
    dir.create(file.path(archive_folname), showWarnings = FALSE, recursive = TRUE)
		
	sapply(which(dir.exists(paste0(folname,"/",v_fols))), function(x) file.rename(from = file.path(folname, v_fols[x]), to = file.path(folname, run_clock, v_fols[x])))
	  
  }
  
  
  return(folname)
  
  
}

######################################################################################################
#### function to create a netCDF and input the data
createNETCDFuk <- function(y, naei_inv, species, map_yr_uk, map_yr_ie, time_dim, folname,
                           tp_scheme, uk_agg_schema, l_uk_allSec, l_ie_allSec, l_sea_allSec){
    
  # create netcdf name
  fname <- paste0(dt_poll[ceh_poll == species, emep_model],"_UKEIRE_",
                  y,"emis_",map_yr_uk,"map_",naei_inv,"inv_0.01.nc")
  nc_filename <- paste0(folname,"/", fname)
   
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
  if(time_dim == "annual"){ v_time <- 1 }else if(time_dim == "month"){ v_time <- v_mday }else{ v_time <- v_yday }
  n_time <- length(v_time)
  i_time <- 1:n_time
  
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
  
  if(time_dim == "annual"){ 
    unit_name <- "tonnes yr-1"
  }else if(time_dim == "month"){
    unit_name <- "tonnes month-1"
  }else{
    unit_name <- "tonnes yday-1"
  }
  
  # for each sector in the given year, create a new netcdf var
  l_variables <- lapply(X = 1:length(v_sectors), function(s){
    ncvar_def(name = v_sectors[s],
              missval = EMEP_fillval, # _FillValue ?
              longname = str_split(v_sectors[s],"_")[[1]][3], # long_name?
              units = unit_name, 
              dim = list(dimlon,dimlat,dimtime), 
              compression = 4,
              prec = "float")})
  
  ## Create the new netcdf
  ncnew <- nc_create(nc_filename, l_variables, force_v4=T)
  
  # now extract the data from the raster Stack and insert
  print(paste0(Sys.time(),":      Inserting data..."))
  
  l <- list()
  
  for(v in 1:length(v_sectors)){
    
    sect_name  <- v_sectors[v]
    sect_desc  <- substr(sect_name, str_locate_all(sect_name,"_")[[1]][2,2] + 1, nchar(sect_name))
    sect_EMEP  <- dt_sec[name == sect_desc, str_pad(EMEP_sec, 2, "0", side = "left")]
    sect_long  <- dt_sec[name == sect_desc, sec]
    sect_GNFR  <- dt_sec[name == sect_desc, GNFRlong]
    # if(sect_desc == "OtherStationaryComb") sect_GNFR <- ""
	
    ISO <- tolower(str_split(sect_name, "_")[[1]][2])
    ISO_num  <- v_country[v]
    
	# get data list (all sectors, all masks)
    l_insert <- get(paste0("l_",ISO,"_allSec"))
	
	# selected correct sector
    l_s_insert <- l_insert[[paste0(ISO,"_",dt_poll[ceh_poll == species, emep_model], "_", sect_long,"_",time_dim)]]
	
	# select correct stack (dependent on iso - terrestrial for uk/ie, sea for sea. Outwith not used for .nc)
	if(ISO == "sea"){ s_insert <- l_s_insert[["sea"]] }else{ s_insert <- l_s_insert[["terrestrial"]] }
    
	# FINAL check for -ve values
	if(any(global(s_insert, min, na.rm=T)$min < 0)) stop(paste0("NEG VALUES in ",dt_poll[ceh_poll == species, emep_model]," in ",y,"!!"))
	
    # extract the year and pollutant and put in
    a <- array(s_insert, dim = c(n_lon, n_lat, n_time))
    a <- a[,1250:1,] # need to reverse the rows, for a reason i have not worked out. 
    
    ncvar_put(ncnew, sect_name, a)
    
    # few extra variable attributes
    ncatt_put(ncnew, varid = sect_name, attname = "long_name"  , attval = sect_desc, prec="char")
    ncatt_put(ncnew, varid = sect_name, attname = "description", attval = sect_desc, prec="char")
    ncatt_put(ncnew, varid = sect_name, attname = "sector"     , attval = as.integer(sect_EMEP),   prec="short")
    ncatt_put(ncnew, varid = sect_name, attname = "species"    , attval = ifelse(species=="nmvoc","voc",species) , prec="char")
    ncatt_put(ncnew, varid = sect_name, attname = "country"    , attval = ISO_num , prec="int")
    
    # summary of data going into netcdf
	# basic table
    dt <- data.table(Area = ISO, 
	                 Pollutant = ifelse(species=="nmvoc","voc",species),
					 data_source = "NetCDF_input",
					 emis_y = y, 
					 inv_y = naei_inv, 
					 sec_EMEP = sect_long,
				     sec_GNFR = sect_GNFR, 
				     sec_SNAP = dt_sec[EMEP_sec == as.numeric(sect_EMEP), SNAP],
				     sec_long = sect_desc,
					 sec_ncdf = sect_name,
					 time_res = time_dim)
	
	# add some summarised data from netCDF surface
	time_cols <- paste0("t",i_time)
	
	# add in annual totals - emissions coming in, array and the newly input ncdf data
    dt[, emis_t_tot_ncinput := sum(global(s_insert, sum, na.rm=T)[,1], na.rm=T) ]
	dt[, emis_t_tot_array := sum(a, na.rm=T) ]
	dt[, emis_t_tot_ncfile  := sum(global(rast(nc_filename, sect_name), sum, na.rm=T)[,1], na.rm=T) ]
		
	# summarise the monthly emissions totals put into ncdf
    dt[, (time_cols) := lapply(i_time, function(x) (global(rast(nc_filename, sect_name),  sum, na.rm=T)[,1])[x]) ]
	
	# summarise the monthly emissions totals
    dt[, tsum := rowSums(.SD, na.rm=T), .SDcols = time_cols]
	dt[, tot_tres_ratio := emis_t_tot_ncinput / tsum]

    l[[v_sectors[v]]] <- dt
    
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
  
  ncatt_put(ncnew, 0, "periodicity",    time_dim, prec="char")
  ncatt_put(ncnew, 0, "NCO","netCDF Operators version 4.9.8 (Homepage = http://nco.sf.net, Code = http://github.com/nco/nco)", prec = "char")  
  ncatt_put(ncnew, 0, "emissions_year", y, prec="int")
  ncatt_put(ncnew, 0, "inventory_year", naei_inv, prec="int")
  ncatt_put(ncnew, 0, "UK_map_year",    map_yr_uk, prec="int")
  ncatt_put(ncnew, 0, "EIRE_map_year",  map_yr_ie, prec="int")
  
  nc_close(ncnew)
  
  dt_ncdf_summary <- rbindlist(l, use.names = T)
  
  return(dt_ncdf_summary)
  
} # end of function


######################################################################################################
#### function to write out the summary tables into a new folder
write_summaries <- function(naei_inv, species, map_yr_uk, folname,
                            dt_inv, dt_mask, dt_group, dt_ncinp, dt_ncout){
    
  dir.create(file.path(folname, "tables"), showWarnings = FALSE, recursive = T)  
	
  fname_route <- paste0(dt_poll[ceh_poll == species, emep_model],"_UKEIRE_",y,"emis_",map_yr_uk,"map_",naei_inv,"inv")
    
  
  fwrite(dt_inv,   paste0(folname, "/tables/", fname_route, "_INVENTORY.csv"))
  fwrite(dt_mask,  paste0(folname, "/tables/", fname_route, "_MASKED.csv"))
  fwrite(dt_group, paste0(folname, "/tables/", fname_route, "_PROCESSED.csv"))
  fwrite(dt_ncinp, paste0(folname, "/tables/", fname_route, "_NETCDFINP.csv"))
  fwrite(dt_ncout, paste0(folname, "/tables/", fname_route, "_NETCDFOUT.csv"))
  
}

######################################################################################################
#### function to read and summarise nc file fresh. Post writing. 
summarise_nc_file <- function(species, y, folname, map_yr_uk,
                              naei_inv, time_dim){
    
  # set filename to read
  nc_file <- paste0(folname, "/", dt_poll[ceh_poll == species, emep_model],"_UKEIRE_",y,"emis_",map_yr_uk,"map_",naei_inv,"inv_0.01.nc")
  
  # time dims
  if(time_dim == "annual"){ i_time <- 1 }else if(time_dim == "month"){ i_time <- 1:12 }else{ i_time <- 1:365 }
  #time_cols <- paste0("t",i_time)
  
  # gather sector info direct from input file. Yet another safety check. 
  v_sectors <- dt_sec[,unique(sec)]
  
  l <- list()
  
  for(i in v_sectors){
  
    if(i == "") next # not interested in currently blank EMEP-named sectors
    if(dt_sec[sec == i, GNFRlong]  == "" ) next
    print(paste0(Sys.time(),":         ",i))
	
	nc_sec <- dt_sec[sec == i, name]
	
	# cycle through uk, ie and sea, summarising emissions
	for(area in c("uk", "ie", "sea")){
	  
	  # stack and raster total
      s <- rast(nc_file, subds = paste0("Emis_",toupper(area),"_", nc_sec))
	        
      # annual total rasters
      r <- app(s,  sum, na.rm=T)
	  
	  # summarise all 
	  dt_time <- data.table(Area = area, 
	                        Pollutant = dt_poll[ceh_poll == species, emep_model],
			                data_source = "NetCDF_output",
				            emis_y = y, 
				 		    inv_y = naei_inv,
							sec_EMEP = i,
							sec_GNFR = dt_sec[sec == i, GNFRlong], 
							sec_SNAP = dt_sec[sec == i, SNAP], 
							sec_long = dt_sec[sec == i, name],
							time_res = time_dim,							   
							t = i_time,
							emis_t_ncfile = global(s, sum, na.rm=T)[,1])
	
	  dt_tot  <- data.table(Area = area, 
	                        Pollutant = dt_poll[ceh_poll == species, emep_model],
					        data_source = "NetCDF_output",
					        emis_y = y, 
							inv_y = naei_inv,
							sec_EMEP = i,
							sec_GNFR = dt_sec[sec == i, GNFRlong], 
							sec_SNAP = dt_sec[sec == i, SNAP], 
							sec_long = dt_sec[sec == i, name],
							time_res = "annual",							   
							t = 1,
							emis_t_ncfile = global(r, sum, na.rm=T)[,1])
	
	  dt <- rbindlist(list(dt_time, dt_tot), use.names = T)
	
	  l[[paste0(area,"_",i)]] <- dt  
	
	} # area
  
  } # sector
  
  dt_ncfile_summary <- rbindlist(l, use.names = T)
  
  return(dt_ncfile_summary)

}

######################################################################################################
#### function to plot annual 
plotYearUK <- function(species, output_dir, y, tp_scheme, uk_agg_schema, naei_inv, map_yr_uk){
  
  dir.create(file.path(folname, "plots"), showWarnings = FALSE, recursive = T)  
  
  fname_route <- paste0(dt_poll[ceh_poll == species, emep_model],"_UKEIRE_",y,"emis_",map_yr_uk,"map_",naei_inv,"inv")
  
  # read written summaries  
  dt_inv   <- paste0(folname, "/", fname_route, "_INVENTORY.csv"))
  dt_mask  <- paste0(folname, "/", fname_route, "_MASKED.csv"))
  dt_group <- paste0(folname, "/", fname_route, "_PROCESSED.csv"))
  dt_ncinp <- paste0(folname, "/", fname_route, "_NETCDFINP.csv"))
  dt_ncout <- paste0(folname, "/", fname_route, "_NETCDFOUT.csv"))
  
  
  dt_inv_total <- dt_inv[, lapply(.SD, sum, na.rm=T), by = .(Area, Pollutant, data_source, emis_y, inv_y), .SDcols = c("emis_t_inv_spatial","emis_t_inv_table","emis_t_spatial_scaled")]
  
  dt_mask_total <- dt_mask[, lapply(.SD, sum, na.rm=T), by = .(Area, mask, Pollutant, data_source, emis_y, inv_y), .SDcols = c("emis_t_tot_masked","tsum")]
  
  dt_group_total <- dt_group[, lapply(.SD, sum, na.rm=T), by = .(Area, Pollutant, data_source, emis_y, inv_y), .SDcols = c("emis_t_tot_grouped","tsum")]
  
  dt_ncinp_total <- dt_ncinp[, lapply(.SD, sum, na.rm=T), by = .(Area, Pollutant, data_source, emis_y, inv_y), .SDcols = c("emis_t_tot_ncinput","emis_t_tot_array","emis_t_tot_ncfile","tsum")]
  
  dt_ncout_total <- dt_ncout[time_res == "annual", lapply(.SD, sum, na.rm=T), by = .(Area, Pollutant, data_source, emis_y, inv_y), .SDcols = c("emis_t_ncfile")]

	
  dt_inv  <- copy(dt_inv_summary)
  dt_mask <- copy(dt_mask_summary)
  dt_ncinp <- copy(dt_ncinput_summary)
  dt_ncout <- copy(dt_ncoutput_summary)
  
  
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


create_qaqc <- function(){

  dir.create(file.path(folname, "qaqc"), showWarnings = FALSE, recursive = T)  



}





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














