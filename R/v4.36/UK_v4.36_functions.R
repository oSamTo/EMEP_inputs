#########################################################
#### FUNCTIONS FOR CREATION OF EMEP - UK INPUT FILES ####
#########################################################

vectorPolls <- function(dt_PID, class = c("ceh","emep","mapeire","naei")) {
  
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

###############################################################################

###############################################################################
#### function to take NAEI emissions, make ready to EMEP format and create netCDFs for UK & Eire
EMEP_UKEIRE_v4.36 <- function(y, v_pollutants, time_dim = c("annual","month","yday"), 
			                 v_EMEP_sec, naei_inv, map_yr_uk, map_yr_ie, folname, 
							 project, scenario, tp_scheme, uk_agg_schema, dt_alt_emis){
	
  time_dim <- match.arg(time_dim)
  
  # v_years  = vector, *numeric*, year to process.
  if(!is.numeric(y)) stop ("Year vector is not numeric")
  
  # naei_inv  = *numeric*, year of inventory release data.
  if(!is.numeric(naei_inv)) stop ("Inventory release year is not numeric")
  
  # map_yr = *numeric*, year of spatial dist. from NAEI. 
  if(map_yr_uk < 2018) stop ("UK spatial distribution must be 2018 or later")
  if(!(map_yr_ie %in% c(2016,2019))) stop ("Eire spatial distribution must be 2016 or 2019")
  
  print(paste0(format(Sys.time(), "%F %T"),": Creating ",project," EMEP4UK UKEIRE inputs (",time_dim,") for ",y,"..."))
  
  # For the years & pollutants, take the regional emissions in Lat Long and;
  #   i) convert point emissions (.csv) into a raster
  #  ii) combine the points with the diffuse (.tif) data as required for the model
  # iii) MASK THE DATA to fit *inside* the EU emissions data
  #  iv) For SNAP 1: ONLY point sources go into A_PublicPower as this is treated with 200m injection. All other into B_Industry.
  #   v) split into monthly emissions, if needed
  #  vi) create netCDF
    	
  res_crs <- "0.01_LL"
 
  ####################################################
  #### CREATE THE NETCDF FILE TO PUT EMISSIONS IN
    
  ## loop through pollutants listed and populate the netCDF input file.
  ## years before 1970 (1980 for NH3) will always use the SPEED time series - /gws/nopw/j04/ceh_generic/samtom/SPEED
  ## SNAP 1 prior to 1990 will use point data from the same folder above. 
  
  for(species in v_pollutants){
    
    # each pollutant has a netCDF in v4.36
    fname_ncdf <- create_NETCDF_uk(y, species, folname, naei_inv,
                                 v_EMEP_sec, time_dim, uk_agg_schema)

    ## Data can be made with a monthly time attribute
     # for the month, profile is chosen in run_setup.R 
        
    ######################################################################################
    if(!(species %in% c("nox","sox","nh3", "co", "voc","pm25","pm10","pmco")) ) stop ("Species must be in: 
                                            AP:    co, nh3, nox, sox, voc
                                            PM:    pm25, pm10, pmco")
    ######################################################################################
      
    print(paste0(format(Sys.time(), "%F %T"),":        ",species," data:"))        
		
	  # create folder, move old netCDF files. ABANDONED THIS FOR NOW. 
	  # archive_data(species, folname)	
	 
	  # set up blank stacks ready for data of different regions
    l_uk_emis  <- list()
    l_ie_emis  <- list()
    l_sea_emis <- list()
	  l_ow_emis  <- list()
     
    # sector totals list
    l_inv_summary <- list()
	  l_mask_summary <- list()
	  l_group_summary <- list()
     
    # sector names to loop through
    # in v4.36 this is sec01:sec11
    v_sectors <- paste0("sec", str_pad(1:11, side = "left", width = 2, 0)) 
	
    print(paste0(format(Sys.time(), "%F %T"),":            gathering emissions..."))
     
    for(i in v_sectors){
       
      if(i == "") next # not interested in currently blank EMEP-named sectors
      if(dt_sec[sec == i, GNFRlong]  == "" ) next # not interested in blank named GNFR sectors either
      # print(paste0(format(Sys.time(), "%F %T"),":         ",i))
      	  
      ####################################
      #### LISTS OF EMISSION SURFACES ####
       
	    # we do not make a new sea list here, because the temporal data doesn't exist.
	    # i.e. we need to keep the sea buffer emissions associated with country of origin. 
      l_uk <- UKIE_sector_Emissions(dt_alt_emis, species, y, i, project, scenario,
	                                time_dim, res_crs, map_yr = map_yr_uk, 
									naei_inv, country = "uk")
      l_ie <- UKIE_sector_Emissions(dt_alt_emis, species, y, i, project, scenario, 
	                                time_dim, res_crs, map_yr = map_yr_ie, 
									naei_inv, country = "ie")
      
      ########################
      #### TEMPORAL SPLIT ####
      # if the time_dim is 'annual', the data stays as 1 annual total.
      # if the time_dim is 'month',  the data needs to be split into 12 layers.
	    # if the time_dim is 'yday',   the data needs to be split into 365 layers.
		  # this is either the temporal profiles from inside EMEP4UKv4.45 / v5.0
      # or newly generated data (e.g. ukem_pro)
      # the sea & outwith layers need to be split based on the country it came from (i.e. UK or Eire)
      l_uk_prof   <- split_UKIE_annual(species = species, time_dim, tp_scheme, 
	                                   l_annual = l_uk, i = i, country = "uk")
      l_ie_prof   <- split_UKIE_annual(species = species, time_dim, tp_scheme, 
	                                   l_annual = l_ie, i = i, country = "ie")
       
      ###########################
	    #### AREA BASED STACKS ####
	    # create stacks for UK, Eire, SEA buffer and outwith UK domain (annual or monthly); separate & total
      l_s_uk  <- stack_data(species, l_uk_prof, l_ie_prof, i, mask = "uk")
	    l_s_ie  <- stack_data(species, l_uk_prof, l_ie_prof, i, mask = "ie")
	    l_s_sea <- stack_data(species, l_uk_prof, l_ie_prof, i, mask = "sea")
	    l_s_ow  <- stack_data(species, l_uk_prof, l_ie_prof, i, mask = "ow")
			        	
      ###################
      #### COLLATING ####
      # add temporal raster stacks to lists that will go into NetCDF
      l_uk_emis[[paste0("uk_", dt_poll[ceh_poll == species, emep_model], "_", i,"_",time_dim)]]   <- l_s_uk
      l_ie_emis[[paste0("ie_", dt_poll[ceh_poll == species, emep_model], "_", i,"_",time_dim)]]   <- l_s_ie
      l_sea_emis[[paste0("sea_", dt_poll[ceh_poll == species, emep_model], "_", i,"_",time_dim)]] <- l_s_sea
	    l_ow_emis[[paste0("ow", dt_poll[ceh_poll == species, emep_model], "_", i,"_",time_dim)]]    <- l_s_ow
       
      ####################
      #### STATISTICS ####        
	    # inventory summary for UK & IE
	    dt_inv_summary <- rbindlist(list(l_uk$ann_summary, l_ie$ann_summary), use.names = T)
	
	    # make summaries for all masked areas
	    l_maskgroup_summary <- summarise_UKIE_emissions(project, scenario, y, naei_inv, species, i, time_dim,
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
	 	  
	  #print(paste0(format(Sys.time(), "%F %T"),":               Aggregating by sector and/or ISO code..."))
	  #l_eu_agg <- aggregateEU(y, species, schema, time_dim, l_eu_allSec)
	  ## code to go here for any aggregations, e.g. ISO code ##
	  # will need to create 'l_uk_agg' object, like in EU code
     
    ###################################################
    #### INPUT DATA TO NETCDF TO SPECIES VARIABLES ####
    print(paste0(format(Sys.time(), "%F %T"),":            populating netcdf..."))
     
	  # input data and summarise what's going in. 
	  l_ukiesea_emis <- list("GB" = l_uk_emis, "IE" = l_ie_emis, "SEA" = l_sea_emis) # use GB, not UK
	 
    dt_ncinput_summary <- input_data_NETCDF_uk(project, scenario, y, species,
                                               naei_inv, map_yr_uk, map_yr_ie,
                                               time_dim, v_EMEP_sec, fname_ncdf,
                                               uk_agg_schema, l_ukiesea_emis)
     	  
	 ##############################
     #### QAQC; TABLES & PLOTS ####
     print(paste0(format(Sys.time(), "%F %T"),":            summaries..."))
	 
	 dt_inv_summary  <- rbindlist(l_inv_summary, use.names=T)
	 dt_mask_summary <- rbindlist(l_mask_summary, use.names=T)
	 dt_group_summary <- rbindlist(l_group_summary, use.names=T)
	 # dt_ncinput_summary # as above
	 
	 # another nc summary from file, post writing. Double checker. 
	 dt_ncoutput_summary <- summarise_nc_file_uk(project, scenario, fname_ncdf, y, species, 
	                                             naei_inv, time_dim, v_EMEP_sec)
	 	  
	 write_summaries_uk(y, species, naei_inv, map_yr_uk, folname,
				        dt_inv = dt_inv_summary, dt_mask = dt_mask_summary, dt_group = dt_group_summary,
				        dt_ncinp = dt_ncinput_summary, dt_ncout = dt_ncoutput_summary)
  	
	 print(paste0(format(Sys.time(), "%F %T"),":            pollutant complete."))
	 
	 # tidy
     remove(l_ukiesea_emis)
     remove(l_uk_emis)
     remove(l_ie_emis)
	 remove(l_sea_emis)
     gc()
	 
	} # pollutant loop
  
  print(paste0(format(Sys.time(), "%F %T"),": DONE."))
  
} # end of function
        

###############################################################################
#### function to collect sector data for diffuse and points, based on country and sector
UKIE_sector_Emissions <- function(dt_alt_emis, species, y, i, project, scenario, 
                                  time_dim, res_crs, map_yr, naei_inv, 
								  country = c("uk", "ie")){
  
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
  
    
  ## emissions file locations: 
  # check the dt_alt_emis for any surfaces - where present, use them. 
  # if not present, use the default location;
		# "/gws/nopw/j04/ceh_generic/inventory_processor/data"
    
  # read both points files, easier to subset. 
  ## UK
  if(country == "uk"){
    
	dt_alt_sub <- dt_alt_emis[projectName == project & scenarioName == scenario & 
	                          poll == species & diff_or_pt == "diff" & 
	                          sector == i & iso == "GB"]
	
	alt_checker <- nrow(dt_alt_sub)
		
	if(alt_checker > 1) stop("Alternative emissions has too many surfaces for poll/sector/iso")
	
	## Diffuse UK
	if(alt_checker == 1){
 
      fol_emis <- dt_alt_sub[, loc]
	    fname_emis <- dt_alt_sub[, fname]
	  
	    f_diff <- file.path(fol_emis, fname_emis)
	    if(!(file.exists(f_diff))) stop("file for alternate emissions does not exist.")
	  
      loctext_diff <- "alt_file"
 
    }else{
 
      fol_emis <- "/gws/nopw/j04/ceh_generic/inventory_processor/data"
	    
      # v4.36 needs SNAP maps from the inventory processor      
	    f_diff <- paste0(fol_emis,"/NAEI/inv",naei_inv,"/maps/NAEI_",
                       dt_poll[ceh_poll == species, invProc],
					             "_DIFFUSE_inv",naei_inv,"_emis_",naei_inv-2,
                       "/SNAP/NAEI_", dt_poll[ceh_poll == species, invProc],
                       "_DIFFUSE_inv",naei_inv,"_emis_",naei_inv-2,
                       "_SNAP_",substr(i, 4, 5),"_t_LL.tif")
	  	  
      loctext_diff <- "inventory"
 
    }
	
	## Points UK
	# (this is more unlikely, but could happen, e.g. power)
	
	dt_alt_sub <- dt_alt_emis[projectName == project & scenarioName == scenario & 
	                          poll == species & diff_or_pt == "pt" & 
	                          sector == i & iso == "GB"]
	
	alt_checker <- nrow(dt_alt_sub)
	
    if(alt_checker > 1) stop("Alternative emissions has too many surfaces for poll/sector/iso")
	
    if(alt_checker == 1){
 
      fol_emis <- dt_alt_sub[, loc]
	    fname_emis <- dt_alt_sub[, fname]
	  
	    ## WE NEED TO READ IN 1990+ FILE AND 1950-2000 FILE AND REMOVE THE NECESSARY EMISSIONS
	  
	    stop("make code to replace existing points in time series data")
	  
	    f_pt <- file.path(fol_emis, fname_emis)
	    if(!(file.exists(f_pt))) stop("file for alternate emissions does not exist.")
	  
        loctext_pt <- "alt_file"
 
    }else{
 
      fol_emis <- "/gws/nopw/j04/ceh_generic/inventory_processor/data"
	  	  
	    f_pt <- paste0(fol_emis,"/NAEI/inv",naei_inv,
                     "/points/NAEI_AllPoll_POINTS_inv",naei_inv,
	                   "_emis_1990_",naei_inv-2,"_SNAP_t_LL.csv")
			# i have removed pre 2000 power from this. 
	  	  
      loctext_pt <- "inventory"
 
    }
  
  ## EIRE
  }else if(country == "ie"){
    
	  dt_alt_sub <- dt_alt_emis[projectName == project & scenarioName == scenario & 
	                            poll == species & diff_or_pt == "diff" & 
	                            sector == i & iso == "IE"]
							  
    alt_checker <- nrow(dt_alt_sub)
	
    if(alt_checker > 1) stop("Alternative emissions has too many surfaces for poll/sector/iso")
	  
    ## Diffuse Eire
	  if(alt_checker == 1){
 
      fol_emis <- dt_alt_sub[, loc]
	    fname_emis <- dt_alt_sub[, fname]
	  
	    f_diff <- file.path(fol_emis, fname_emis)
	  
      if(!(file.exists(f_diff))) stop("file for alternate emissions does not exist.")
	   
      loctext_diff <- "alt_file"
 
    }else{
 
      fol_emis <- "/gws/nopw/j04/ceh_generic/inventory_processor/data"
	  
	  # diffuse filename - Eire inventory is currently set to 2021 with 2019 maps
	    f_diff <- paste0(fol_emis,"/MapEire/inv2021/maps/tif/MapEire_",
                       dt_poll[ceh_poll == species, invProc],
	                     "_DIFFUSE_inv2021_emis_2019/SNAP/MapEire_", 
                       dt_poll[ceh_poll == species, invProc],
					             "_DIFFUSE_inv2021_emis_2019_SNAP_",
                       substr(i, 4, 5),"_t_LL.tif")
	  	  
      loctext_diff <- "inventory"
 
    }
  
  ## Points Eire
  # set the points filename
	# you cant replace Eire points at the moment, as they are not separate in source data
  f_pt <- "no_file"
	loctext_pt <- "no_file"
  
  } # country ifelse
  
  ### read in the data; if diff/pts are empty or don't exist, set as blank domain
  ### read diffuse data ###
  if(file.exists(f_diff)){
    
    r_diff <- crop(extend(rast(f_diff), r_dom_UKIE), r_dom_UKIE)
  
  }else{
    
    r_diff <- r_dom_UKIE
    
  } # end of read diffuse    
  
  ### read point data ###
  # this is decided by country, UK should always have point files to read
  if(country == "uk"){
    	
    # get the table of points first and assess if any entries at all
	  #     PUBLIC POWER: use NAEI point data back to 1990, then the UKSCAPE/SPEED time series to 1960
	  # NOT PUBLIC POWER: years prior to 2000 will use 2000 !! (see theory/graphs on combining points, diffuse, scaling etc)
	  
	  # read both files, subset the historical to < 1990 and bind together, then perform subsetting. 
	  l_pts <- lapply(f_pt, fread)
	  #l_pts[[2]] <- l_pts[[2]][Year < 1990]
	  	
	  dt_pts <- rbindlist(l_pts, use.names = T)
	  
    dt_pts <- dt_pts[SNAP == as.numeric(substr(i, 4, 5)) & AREA == toupper(country) & Pollutant == dt_poll[ceh_poll == species, invProc]]
	  
	  if(i == "sec01"){
	    
	    dt_pts <- dt_pts[Year == y]
	  
	  }else if(i != "sec01" & y >= 2000){
	    
	    dt_pts <- dt_pts[Year == y]
	  
	  }else{
	  
	    dt_pts <- dt_pts[Year == 2000]
	  
	  }
	  
      
    # if there are no points, use a blank raster, otherwise rasterize
    if(nrow(dt_pts) == 0){
        
      r_pt <- r_dom_UKIE
        
    }else{
        
      v_pt <- vect(dt_pts, geom=c("Easting", "Northing"), crs = "EPSG:4326")
      r_pt <- terra::rasterize(v_pt, r_dom_UKIE, field = "emis_t", fun = sum)
        
    } # end of read points
    
  }else if(country == "ie"){
    
    r_pt <- r_dom_UKIE
    
  }
  
  ## combine the surfaces, whether they have data in or not. 
  s <- c(r_diff, r_pt) ; names(s) <- c("diffuse","point")
  r <- app(s, sum, na.rm = T) ; names(r) <- "total"
  
  ###############
  ### SCALING ###
  
  # Scale the mapped data to a chosen year - scale by emissions, not an alpha factor
  # BUT, if alternative emission supplied, do not scale that surface; use as is.
  
  if(country == "uk"){
    
	# read in the SNAP time series;
           # SNAP maps were put into GNFR maps, so there will be 0 data in some GNFRs 
	  	   # but there may be data in equiv inventory GNFRs, as it's just sector totalling, so data will be lost to maps of 0
	  # use the ACTUAL amounts, not the alpha - this is due to the complex relationship of point & 
	  #                                         diffuse data, data completeness and relative scaling causing error 
    
	  # anything prior to 1970 (or 1980 for NH3) needs the SPEED totals.
	  if (y >= 1980) {
	  
	    f_alpha <- paste0("/gws/nopw/j04/ceh_generic/inventory_processor/data/NAEI/inv",naei_inv,"/alpha/NAEI_AllPoll_TOTALS_inv",naei_inv,"_emis_1970-",naei_inv-2,"_SNAP_alpha.csv")
    
	  } else if(y >= 1970 & species != "nh3") {
	  
	    f_alpha <- paste0("/gws/nopw/j04/ceh_generic/inventory_processor/data/NAEI/inv",naei_inv,"/alpha/NAEI_AllPoll_TOTALS_inv",naei_inv,"_emis_1970-",naei_inv-2,"_SNAP_alpha.csv")
    
	  } else {
	  
	    f_alpha <- paste0("/gws/nopw/j04/ceh_generic/samtom/SPEED/SPEED_AllPoll_TOTALS_invNA_emis_1960-1970_SNAP_alpha.csv")
    
    }

    dt_alpha <- fread(f_alpha)
	
	  # subset the value required. GNFR UK maps are just SNAP maps re-assigned. Remember;
		  	  # to scale industry by SNAPs 3 & 4 together (inventory processor combines 3 & 4 to B_Industry)
			  # all of SNAP 8 is in I_Offroad (inventory_processor).
			  	  # While G_Shipping and H_Aviation are empty, shouldn't take the risk using SN08 > once
			  # all of SNAP 10 is in K_AgriLivestock (inventory_processor).
				  # While L_AgriOther is empty, shouldn't take the risk using SN10 > once
			  # set secs 14 to 19 as NA, as they are not used yet, and causes double counting in summary

    scaling_value <- dt_alpha[Pollutant == dt_poll[ceh_poll == species, invProc] & 
                              Year == y & AREA == toupper(country) & 
                              SNAP == as.numeric(substr(i, 4, 5)), tot_emis_t]
 	
	  if(length(scaling_value) == 0) scaling_value <- 0
	  if(is.na(scaling_value)) scaling_value <- 0
	  
	  # This is the same value irrespective of whether alt_emis were used or not. 
	  inventory_table_value <- copy(scaling_value) 	  
	  
    # if the data are alternative emissions, use that value as scalar, i.e. OVERWRITE.
	  if(loctext_diff == "alt_file" & loctext_pt == "alt_file"){
	  
	    if(i %in% c("sec02", "sec07", "sec08", "sec09")) stop("Blended sectors have not been coded for scaling yet.")
	    
	    # if both diffuse AND points are supplied as alternative files, no scaling	
	    scaling_value <- global(r, sum, na.rm=T)[,1]
	    scalar_used <- "no"
	    
	    replacement_value <- scaling_value - inventory_table_value
	  
	  # if the data are alternative emissions, use that value as scalar, i.e. OVERWRITE.
	  }else if(loctext_diff == "alt_file" | loctext_pt == "alt_file"){
	  
	    if(i %in% c("sec02", "sec07", "sec08", "sec09")) stop("Blended sectors have not been coded for scaling yet.")
	    # if either diffuse OR points are supplied as alternative files;
		  # still no scaling, as assumption is the year/inventory is not changing.
	    scaling_value <- global(r, sum, na.rm=T)[,1]
	    scalar_used <- "no"
	    
	    replacement_value <- scaling_value - inventory_table_value
	  
    # if the data are NOT alternative emissions, use scalars
	  }else{
	  
      scalar_used <- "yes"
	  
	    replacement_value <- NA
	
	  } # ifelse for scalar needed or not. 
  
  } else if (country == "ie") {
  	
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
     
	    f_alpha <- paste0("/gws/nopw/j04/ceh_generic/inventory_processor/data/EMEP/inv",naei_inv,"/alpha/EMEP_AllPoll_TOTALS_inv",naei_inv,"_emis_1970-",naei_inv-2,"_SNAP_alpha.csv")
      dt_alpha <- fread(f_alpha)
	 
  	  scaling_value <- dt_alpha[Pollutant == dt_poll[ceh_poll == species, invProc] &
                                Year == y  & ISO2 == "IE" &
                                SNAP == as.numeric(substr(i, 4, 5)), tot_emis_t]
   
    }else{
     
	    # if before 1990, find the 1990 EMEP value and scale by EMEP scalar, apply to 2019 map
      dt_ceds_alpha <- fread(paste0("/gws/nopw/j04/ceh_generic/samtom/SPEED/CEDS_for_EMEP/",dt_poll[ceh_poll == species, SPEED],"_CEDS_1950_1990_ISO_GNFR_kt.csv"))
	    dt_emep_alpha <- fread(paste0("/gws/nopw/j04/ceh_generic/inventory_processor/data/EMEP/inv",naei_inv,"/alpha/EMEP_AllPoll_TOTALS_inv",naei_inv,"_emis_1970-",naei_inv-2,"_SNAP_alpha.csv"))
	    
      ## THIS WILL ERROR AS CEDS ONLY HAS GNFR at this point (EMEP has SNAP) ##
  
	    if(i == "sec08"){	 
	      emep_alpha <- dt_ceds_alpha[Year == y & ISO2 == "XX" & GNFR == dt_sec[sec == i, GNFRlong], alpha]
	    }else{	 
        emep_alpha <- dt_ceds_alpha[Year == y & ISO2 == "IE" & GNFR == dt_sec[sec == i, GNFRlong], alpha]
	    }
	    
	    emep90_t  <- dt_emep_alpha[Pollutant == dt_poll[ceh_poll == species, SPEED] & Year == 1990 & ISO2 == "IE" & GNFR == dt_sec[sec == i, GNFRlong], tot_emis_t]
	    scaling_value <- (emep_alpha * emep90_t)
	 
    }
   		
	  if(length(scaling_value) == 0) scaling_value <- 0
	  if(is.na(scaling_value)) scaling_value <- 0
	  
	  # This is the same value irrespective of whether alt_emis were used or not. 
	  inventory_table_value <- copy(scaling_value)
	    
	
	  # if the data are alternative emissions, use that value as scalar, i.e. OVERWRITE.
	  # points are always 'no_file' for Ireland
	  if(loctext_diff == "alt_file"){
	  
	    if(i %in% c("sec02", "sec07", "sec08", "sec09")) stop("Blended sectors have not been coded for scaling yet.")
	    # if both diffuse AND points are supplied as alternative files, no scaling	
	    scaling_value <- global(r, sum, na.rm=T)[,1]
	    scalar_used <- "no"
		  
	    replacement_value <- scaling_value - inventory_table_value
      # if the data are NOT alternative emissions, use scalars
	    # points are always 'no_file' for Ireland
	  
    }else{
	
      scalar_used <- "yes"
      
	    replacement_value <- NA

    } # ifelse for scalar needed or not. 
   
   
  # no country
  } else {
    
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
  dt_inv <- data.table(Project = project, 
                       Scenario = scenario,
                       Area = country,
                       mask = "all",  
					             Pollutant = species, 
					             data_source_diff = loctext_diff,
					             data_source_pt = loctext_pt,
					             emis_y = y, 
					             inv_y = naei_inv, 
					             sec_EMEP = i,
					             sec_GNFR = dt_SNAPGNFR[snap_id == as.numeric(substr(i, 4, 5)), GNFR],
					             sec_SNAP = as.numeric(substr(i, 4, 5)),
					             sec_long = dt_SNAPGNFR[snap_id == as.numeric(substr(i, 4, 5)), snap_name],
					             time_res = time_dim,
                       emis_t_diffmap = global(r_diff, sum, na.rm=T)$sum, 
					             emis_t_pt = global(r_pt, sum, na.rm=T)$sum,
					             emis_t_spatial = global(r, sum, na.rm=T)$sum, 
					             emis_t_spatial_inv = inventory_table_value,
					             replacement_t = replacement_value,
					             scaling = scalar_used,
					             emis_t_scalar = scaling_value,
					             emis_t_spatial_scaled = global(rs, sum, na.rm=T)$sum)
  
  l <- list(r, r_t, r_t10, r_ow, r_sea, dt_inv)
  names(l) <- c("total", "terrestrial","terrestrial_10km","outwith_10km","sea", "ann_summary")
  
  return(l)
  
} # end of function


######################################################################################################
#### function to split annual emissions out into months (or keep as annual)
split_UKIE_annual <- function(species, time_dim = c("annual","month","yday"), 
                              tp_scheme, l_annual, i, 
							  country = c("uk","ie","sea")){
  
  country    <- match.arg(country)
  time_dim <- match.arg(time_dim)
  
  if(time_dim == "annual"){ i_time <- 1 }else if(time_dim == "month"){ i_time <- 1:12 }else{ i_time <- 1:365 }
  
  if(time_dim == "annual"){
         
	l_s <- l_annual[c("terrestrial","sea","outwith_10km")]
	    
	# error if any negative values remain
	if(any(global(l_s[["terrestrial"]], min, na.rm=T) < 0)) stop("there are negative emissions values (land)")
	if(any(global(l_s[["sea"]], min, na.rm=T) < 0)) stop("there are negative emissions values (sea)")
	if(any(global(l_s[["outwith_10km"]], min, na.rm=T) < 0)) stop("there are negative emissions values (outwith_10km)")
	
	names(l_s)[3] <- "outwith"
	
    names(l_s[["terrestrial"]]) <- paste0(dt_poll[ceh_poll == species, emep_model], "_", country,"_","terrestrial", "_", i,"_",str_pad(i_time, 2, "0", side = "left"))
    names(l_s[["sea"]]) <- paste0(dt_poll[ceh_poll == species, emep_model], "_", country,"_","sea", "_", i,"_",str_pad(i_time, 2, "0", side = "left"))
    names(l_s[["outwith"]]) <- paste0(dt_poll[ceh_poll == species, emep_model], "_", country,"_","outwith", "_", i,"_",str_pad(i_time, 2, "0", side = "left"))
		
		
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
    	
	if(tp_scheme %in% c("EMEP4UKv4.45", "EMEP4UKv5.0")) {
	# temporal schema in EMEP4UKv4.45 / EMEP4UKv5.0
	  
	  country_id <- ifelse(country == "uk", 27, 14) # to extract correct temporal file factors; 27 = UK, 14 = IE (Eire)
    snap_id <- as.numeric(substr(i, 4, 5)) # set the SNAP to read from the temporal file. 
    
    # read in temporal file for legacy temporal splits (subset to Eire or UK - SEA needs to match parent country)
    dt_tempro <- fread(paste0("/gws/nopw/j04/ceh_generic/samtom/TEMREG/output/model_inputs/EMEP4UK/",
                              tp_scheme,"/MonthlyFacs.",
                              dt_poll[ceh_poll == species, emep_model]))
	  	  	  
	  names(dt_tempro) <- c("ISO","SNAP",month.abb[1:12])
    dt_tempro_m <- melt(dt_tempro, id.vars = c("ISO","SNAP"), 
                        variable.name = "MON", value.name = "FAC")
    
    # extract required temporal data - Use 1s if the SNAP sector in dt_dec is "NA"
    if(is.na(snap_id)){
      v_tempro <- rep(1,12)
    }else if(snap_id %in% dt_tempro_m[,SNAP]){
	    v_tempro <- dt_tempro_m[ISO == country_id & SNAP == snap_id][["FAC"]] # vector of monthly splits 
	    v_tempro <- v_tempro/mean(v_tempro)# not always adding to exactly 12 in the temporal file; ensure
	  }else{
	    v_tempro <- rep(1,12)	          
    }		
		
	}else if(grepl("ukem_", tp_scheme)) {
	# IF the tp_scheme is generated by ukem_pro, use the SNAP gam output;
	# Use yday gams as they take into account joining up the year start/end. Subset by v_mday
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
		# use yday GAMs and subset
		if(country == "uk"){
		  
		  # gam
		  m_gam <- readRDS(paste0("/gws/nopw/j04/ukem/test/sam/ukem_pro/output/GAM_sector/SNAP/",
								  dt_poll[ceh_poll == species, ukem_pro],"/",strsplit(tp_scheme,"_")[[1]][2],"/SNAP_",
								  dt_sec[sec == i, str_pad(SNAP, 2, "0", side = "left")],
								  "_yday_",dt_poll[ceh_poll == species, ukem_pro],"_",strsplit(tp_scheme,"_")[[1]][2],".rds"))
				  		  
		  # set up blank table, extract gam yday values 
		  dt_tempro <- data.table(time = v_mday, N = 0)
		  dt_tempro[, "N" := mgcv::predict.gam(m_gam, newdata = list(time = time)), by = seq_len(nrow(dt_tempro))]
		  
          # create vector		  
	      v_tempro <- dt_tempro[,N] # vector of monthly splits 
		  v_tempro[v_tempro < 0] <- 0.02 # rare occasion factor goes below 0, set to 0.02 (i.e. some emissions, but tiny)
	      v_tempro <- v_tempro/mean(v_tempro)# not always adding to 12 in the tempro file, adjust slightly
		
		}else{
          
		  # gam
		  m_gam <- readRDS(paste0("/gws/nopw/j04/ukem/test/sam/ukem_pro/output/GAM_sector/SNAP/",
								  dt_poll[ceh_poll == species, ukem_pro],"/",strsplit(tp_scheme,"_")[[1]][2],"/SNAP_",
								  substr(i, 4, 5),"_yday_",dt_poll[ceh_poll == species, ukem_pro],"_",
								  strsplit(tp_scheme,"_")[[1]][2],".rds"))
								  
		  # set up blank table, extract gam yday values 
		  dt_tempro <- data.table(time = v_mday, N = 0)
		  dt_tempro[, "N" := mgcv::predict.gam(m_gam, newdata = list(time = time)), by = seq_len(nrow(dt_tempro))]
		  
          # create vector	
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
		
		  dt_tempro <- fread(paste0("/gws/nopw/j04/ceh_generic/samtom/TEMREG/output/coeff_sector/SNAP/",dt_poll[ceh_poll == species, TEMREG],"/",tp_scheme,
							   "/SNAP_",dt_sec[sec == i, str_pad(SNAP, 2, "0", side = "left")],"_month_",dt_poll[ceh_poll == species, TEMREG],"_",tp_scheme,".csv"))
	  
	      v_tempro <- dt_tempro[,N] # vector of monthly splits 
		  v_tempro[v_tempro < 0] <- 0.02 # rare occasion factor goes below 0, set to 0.02 (i.e. some emissions, but tiny)
	      v_tempro <- v_tempro/mean(v_tempro)# not always adding to 12 in the tempro file, adjust slightly
		
		}else{
          		
		  dt_tempro <- fread(paste0("/gws/nopw/j04/ceh_generic/samtom/TEMREG/output/coeff_sector/SNAP/",dt_poll[ceh_poll == species, TEMREG],"/",tp_scheme,
							   "/SNAP_",substr(i, 4, 5),"_month_",dt_poll[ceh_poll == species, TEMREG],"_",tp_scheme,".csv"))
	  
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
stack_data <- function(species, l_uk_prof, l_ie_prof, i, mask = c("uk","ie","sea","ow")){

  s_blank <- l_uk_prof[[1]] %>% setValues(.,0)

  mask <- match.arg(mask)

  if(time_dim == "annual"){ v_in <- 1 }else if(time_dim == "month"){ v_in <- 1:12 }else{v_in <- 1:365} 
  
  if(mask %in% c("uk","ie")){
  
    l_prof <- get(paste0("l_",mask,"_prof"))
	
	s_all <- suppressWarnings(tapp(c(l_prof[["terrestrial"]], l_prof[["sea"]], l_prof[["outwith"]]), 
							  v_in, sum, na.rm=T))
	names(s_all) <- paste0(dt_poll[ceh_poll == species, emep_model],"_",mask,"_all_",i,"_",str_pad(v_in, 2, "0", side = "left"))
	s_ter <- l_prof[["terrestrial"]]
    s_sea <- l_prof[["sea"]]
    s_ow <- l_prof[["outwith"]]
  
  }else if (mask == "sea"){
    
	s_all <- s_blank
    names(s_all) <- paste0(dt_poll[ceh_poll == species, emep_model],"_",mask,"_all_",i,"_",str_pad(v_in, 2, "0", side = "left"))	
	s_ter <- s_blank
    names(s_ter) <- paste0(dt_poll[ceh_poll == species, emep_model],"_",mask,"_terrestrial_",i,"_",str_pad(v_in, 2, "0", side = "left"))	
    s_sea <- suppressWarnings(tapp(c(l_uk_prof[["sea"]], l_ie_prof[["sea"]]), v_in, sum, na.rm=T))
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
    s_ow <- suppressWarnings(tapp(c(l_uk_prof[["outwith"]], l_ie_prof[["outwith"]]), v_in, sum, na.rm=T))
	names(s_ow) <- paste0(dt_poll[ceh_poll == species, emep_model],"_",mask,"_outwith_",i,"_",str_pad(v_in, 2, "0", side = "left"))
  
  }
  
  l <- list("all" = s_all, "terrestrial" = s_ter, "sea" = s_sea, "outwith" = s_ow)
  
  return(l)
}

######################################################################################################
#### function to summarise input emissions
summarise_UKIE_emissions <- function(project, scenario, y, naei_inv, species, i, time_dim, 
                                     l_uk_inv = l_uk, l_ie_inv = l_ie, 
									 l_s_uk = l_s_uk, l_s_ie = l_s_ie,
									 l_s_sea = l_s_sea, l_s_ow = l_s_ow){
    
	
  if(time_dim == "annual"){ i_time <- 1 }else if(time_dim == "month"){ i_time <- 1:12 }else{ i_time <- 1:365 }
  time_cols <- paste0("t",i_time)
  
  ## MASK SUMMARY for UK & IE
  # basic info
  dt_mask <- data.table(Project = project, Scenario = scenario, 
                        Area = c("uk","uk","uk","ie","ie","ie"), 
                        mask = c("terrestrial","sea","outwith","terrestrial","sea","outwith"),
                        Pollutant = species,
				                data_source = "masked",
	       	              emis_y = y, 
				                inv_y = naei_inv, 
				                sec_EMEP = i,
				                sec_GNFR = dt_SNAPGNFR[snap_id == as.numeric(substr(i, 4, 5)), GNFR],
					              sec_SNAP = as.numeric(substr(i, 4, 5)),
					              sec_long = dt_SNAPGNFR[snap_id == as.numeric(substr(i, 4, 5)), snap_name],
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
  dt_group <- data.table(Project = project, Scenario = scenario, 
                         Area = c("uk","ie","sea","ow"),                   
                         Pollutant = species,
				                 data_source = "grouped",
	       	               emis_y = y, 
				                 inv_y = naei_inv, 
				                 sec_EMEP = i,
				                 sec_GNFR = dt_SNAPGNFR[snap_id == as.numeric(substr(i, 4, 5)), GNFR],
					               sec_SNAP = as.numeric(substr(i, 4, 5)),
					               sec_long = dt_SNAPGNFR[snap_id == as.numeric(substr(i, 4, 5)), snap_name],
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
archive_data <- function(species, folname){

  print(paste0(format(Sys.time(), "%F %T"),":      Creating new directory and archiving previously run data..."))
  
  dir.create(file.path(folname), showWarnings = FALSE, recursive = T)  
  
  # collect all files and folders named 'plots','tables' and 'qaqc' to move to archive folder.
  v_files <- list.files(folname, full.name = FALSE, recursive = FALSE, include.dirs = FALSE, 
					    pattern = paste0("^",species,".*\\.nc$"))
  
  v_plots <- list.files(file.path(folname,"plots"), full.name = FALSE, recursive = FALSE, include.dirs = FALSE, 
					    pattern = paste0("^",species,".*\\.png$"))
						
  v_tables <- list.files(file.path(folname,"tables"), full.name = FALSE, recursive = FALSE, include.dirs = FALSE, 
					    pattern = paste0("^",species,".*\\.csv$"))
						
  v_qaqc <- list.files(file.path(folname,"qaqc"), full.name = FALSE, recursive = FALSE, include.dirs = FALSE, 
					    pattern = paste0("^",species,".*\\.html$"))
  
 
  
  if(length(v_files) > 0){
  
    archive_folname <- paste0(folname,"/",run_clock)
    dir.create(file.path(archive_folname), showWarnings = FALSE, recursive = TRUE)
	
	sapply(v_files, function(x) file.rename(from = file.path(folname, x), to = file.path(archive_folname, x)))
	  
  }
  
  if(length(v_plots) > 0){
  
    archive_folname <- paste0(folname,"/",run_clock,"/plots")
    dir.create(file.path(archive_folname), showWarnings = FALSE, recursive = TRUE)
	
	sapply(v_plots, function(x) file.rename(from = file.path(folname, x), to = file.path(archive_folname, x)))
	  
  }
  
  if(length(v_tables) > 0){
  
    archive_folname <- paste0(folname,"/",run_clock,"/tables")
    dir.create(file.path(archive_folname), showWarnings = FALSE, recursive = TRUE)
	
	sapply(v_tables, function(x) file.rename(from = file.path(folname, x), to = file.path(archive_folname, x)))
	  
  }
  
  if(length(v_qaqc) > 0){
  
    archive_folname <- paste0(folname,"/",run_clock,"/qaqc")
    dir.create(file.path(archive_folname), showWarnings = FALSE, recursive = TRUE)
	
	sapply(v_qaqc, function(x) file.rename(from = file.path(folname, x), to = file.path(archive_folname, x)))
	  
  }
 # if(sum(dir.exists(paste0(folname,"/",v_fols))) > 0){
 # 
 #   archive_folname <- paste0(folname,"/",run_clock)
 #   dir.create(file.path(archive_folname), showWarnings = FALSE, recursive = TRUE)
 #	
 #	sapply(which(dir.exists(paste0(folname,"/",v_fols))), function(x) file.rename(from = file.path(folname, v_fols[x]), to = file.path(folname, run_clock, v_fols[x])))
 #	  
 # }
   
  
}

######################################################################################################
#### function to create a netCDF and input the data - simple routine chooser
create_NETCDF_uk <- function(y, species, folname, naei_inv,
                             v_EMEP_sec, time_dim, uk_agg_schema){
  
  if(time_dim == "annual"){
    
    fname <- create_NETCDF_uk_annual(y, species, folname, naei_inv, 
                                     v_EMEP_sec, time_dim, eu_agg_schema)
    
  }else if(time_dim == "month"){
    
    fname <- create_NETCDF_uk_month()
    
  }
  
  return(fname)
  
}

######################################################################################################
#### function to create a netCDF and input the data - this is the ANNUAL/MONTHLY input (EMEPv5.0)
create_NETCDF_uk_annual <- function(y, species, folname, naei_inv,
                                    v_EMEP_sec, time_dim, uk_agg_schema){
  
  if(time_dim != "annual") stop("time choice has to be annual to make an annual netcdf.")
  
  # create output directory
  dir.create(file.path(folname), showWarnings = FALSE, recursive = T)   
  
  # create netcdf name
  nc_filename <- paste0(folname,"/UKEIRE_",species,"_", naei_inv,"inv_", y,"emis_0.01.nc")
  
  # if the file already exists, just delete and rewrite
  if(file.exists(nc_filename)){
    
    print(paste0("NetCDF already exists in this folder location; DELETING & REPLACING..."))
    file.remove(nc_filename)
    
  }else{}
  
  ## EMEPv4.36 file creation. 
  # netCDF variables are done on sector name and attributes determine sector, country etc
  # in this file, UK (27), EIRE (14) and SEA (171) remain separate
  # dt_iso determines the iso codes.
  # for UKEIRE we have UK, IE and SEA, as ISOs. 
  
  # Set up the dimensions: latlong, time, sectors  
  v_lon    <- as.array(seq(xmin(r_dom_UKIE) + 0.01/2, xmax(r_dom_UKIE) - 0.01/2, 0.01))
  n_lon    <- length(v_lon)
  v_lat    <- as.array(seq(ymin(r_dom_UKIE) + 0.01/2, ymax(r_dom_UKIE) - 0.01/2, 0.01))
  n_lat    <- length(v_lat)  
  v_sector <- v_EMEP_sec
  n_sector <- length(v_sector)
  v_time   <- as.duration(dmy('01-01-1850') %--% dmy(paste0('01-01-',y))) / ddays(1) # n.days for v4.36.
  n_time   <- length(v_time)
  
  # create dimensions (no sector dim in v4.36)
  dimlon  <- ncdim_def(name = "lon",  longname = "longitude", 
                       units = "degrees_east",  vals = v_lon, unlim = FALSE)
  dimlat  <- ncdim_def(name = "lat",  longname = "latitude", 
                       units = "degrees_north", vals = v_lat, unlim = FALSE)  
  dimtime <- ncdim_def(name = "time", longname = "days", 
                       units = "days since 1850-01-01 00:00", vals = v_time, unlim = FALSE)
  
  # Create names and variables for all pollutants_ISOs SEPARATELY
  if(uk_agg_schema == "oneUKIE") {
    
    stop("v4.36 requires UK / IE / SEA to be split")
    
  } else if (uk_agg_schema == "allISO") {
    
    ## Ireland: ISO:  IE: country 14
    ## UK:      ISO:  GB: country 27
    ## buffer:  ISO: SEA: country 171
  
    v_sectors_uk   <- paste0("Emis:GB:snap:", str_pad(1:11, side = "left", width = 2, 0)) 
    v_sectors_ie   <- paste0("Emis:IE:snap:", str_pad(1:11, side = "left", width = 2, 0)) 
    v_sectors_sea  <- paste0("Emis:SEA:snap:", str_pad(1:11, side = "left", width = 2, 0)) 
  
    v_vars <- c(v_sectors_uk, v_sectors_ie, v_sectors_sea)
    v_country <- c(rep(27, length(v_sectors_uk)), rep(14, length(v_sectors_ie)), rep(171, length(v_sectors_sea)))

  }
  
  # for each pollutant_ISO variable, create a new netcdf var
  l_iso_var <- lapply(X = 1:length(v_vars), function(s){
    ncvar_def(name = v_vars[s],
              longname = v_vars[s],
              units = "Mg cell-1 yr-1", 							 
              missval = EMEP_fillval, # _FillValue ?
              dim = list(dimlon, dimlat, dimtime), 
              compression = 4,
              prec = "float")})
  
  ## Create the new netcdf
  nc_new <- nc_create(nc_filename, l_iso_var, force_v4 = T)
      
  # Finally the global attributes (as in v4.36)
  ncatt_put(nc_new, 0, "Conventions", "CF-1.0", prec = "char")
  ncatt_put(nc_new, 0, "projection", "lon lat", prec = "char")
  ncatt_put(nc_new, 0, "Grid_resolution", "0.01", prec = "char")
  ncatt_put(nc_new, 0, "Created_with", R.Version()$version.string, prec = "char")
  ncatt_put(nc_new, 0, "ncdf4_version", packageDescription("ncdf4")$Version, prec = "char")
  ncatt_put(nc_new, 0, "Created_by", "Sam Tomlinson samtom@ceh.ac.uk", prec = "char")
  ncatt_put(nc_new, 0, "Created_date", format(Sys.Date(), "%Y%m%d"), prec = "int")
  ncatt_put(nc_new, 0, "Sector_names", "SNAP", prec = "char")
  
  ncatt_put(nc_new, 0, "sec01", "publicpower", prec = "char")
  ncatt_put(nc_new, 0, "sec02", "domestic", prec = "char")
  ncatt_put(nc_new, 0, "sec03", "industrialcombustion", prec = "char")
  ncatt_put(nc_new, 0, "sec04", "industrialprocessing", prec = "char")
  ncatt_put(nc_new, 0, "sec05", "fugitive", prec = "char")
  ncatt_put(nc_new, 0, "sec06", "solvents", prec = "char")
  ncatt_put(nc_new, 0, "sec07", "roadtransport", prec = "char")
  ncatt_put(nc_new, 0, "sec08", "othertransport", prec = "char")
  ncatt_put(nc_new, 0, "sec09", "waste", prec = "char")
  ncatt_put(nc_new, 0, "sec10", "agrilivestock", prec = "char")
  ncatt_put(nc_new, 0, "sec11", "natureother", prec = "char")
  
  ncatt_put(nc_new, 0, "periodicity", "yearly", prec = "char")
  ncatt_put(nc_new, 0, "NCO","netCDF Operators version 4.9.3-alpha03 (Homepage = http://nco.sf.net, Code = http://github.com/nco/nco)", prec = "char")
  
  #close connection  
  nc_close(nc_new)  
  
  return(nc_filename)
  
} # end of function


######################################################################################################
#### function to create a netCDF and input the data - this is the MONTHLY input (EMEPv4.45)
create_NETCDF_uk_month <- function(){
  
}



######################################################################################################
#### function to create a netCDF and input the data
input_data_NETCDF_uk <- function(project, scenario, y, species, naei_inv, 
                                 map_yr_uk, map_yr_ie, time_dim, 
                                 v_EMEP_sec, fname_ncdf, uk_agg_schema, l_ukiesea_emis){
  
  # netCDF variables are done on ISO then sector name 
  # in this file, UK (27), EIRE (14) and SEA (171) remain separate
  # there are 12 month layers, or just one annual layer (dependent on time_dim)
  # the names, e.g. sec01, are taken from the lookup table 'dt_sec' and are currently what EMEP4UK requires
    
  if(length(l_ukiesea_emis) != 3) stop("There are not 3 ISO sector lists.")
  
  if( unique(unlist(lapply(l_ukiesea_emis, function(x) length(x)))) != length(v_EMEP_sec) ){
    stop("Some ISOs do not have the same sector length as nominated.")
  }
  
  # set time length based on choice of time dimension
  if(time_dim == "annual"){ i_time <- 1 }else{ i_time <- 1:12 }
  
  # open netcdf
  nc <- nc_open(fname_ncdf, write = T)      
  
  # create the vector of variables (needs to match ncdf created above, or it wil fail). 
  # pollutant_iso only, not the total summary variable. 
  if(uk_agg_schema == "oneUKIE"){ # THIS HAS NOT BEEN RUN, TO DATE
    
    #v_vars <- unlist(lapply(species, function(x) paste0(x,"_UKIE")))
    
  }else if(uk_agg_schema == "allISO"){
    
    v_sectors_uk   <- paste0("Emis:GB:snap:", str_pad(1:11, side = "left", width = 2, 0)) 
    v_sectors_ie   <- paste0("Emis:IE:snap:", str_pad(1:11, side = "left", width = 2, 0)) 
    v_sectors_sea  <- paste0("Emis:SEA:snap:", str_pad(1:11, side = "left", width = 2, 0)) 
  
    v_vars <- c(v_sectors_uk, v_sectors_ie, v_sectors_sea)
        
  }
  
  # array dimensions (identical to ncdf)
  n_lon    <- nc$dim$lon$len
  n_lat    <- nc$dim$lat$len
  n_time   <- nc$dim$time$len
  
  if(length(i_time) != n_time) stop("time_dim choice and time dim inside netCDF are not the same.")
  
  l <- list()  # for summary data
  l_for_sum <- list() # a new list for stack subsets for summary surface  
  
  for(v in v_vars){
    
	  # set some variables for inputting. 
    var_iso <- strsplit(v,":")[[1]][2]
    if(uk_agg_schema == "allISO")   var_code <- ifelse(var_iso == "GB", 27, ifelse(var_iso == "IE", 14, 171)) 
    if(uk_agg_schema == "oneUKIE")  var_code <- 64 # DON'T KNOW!!
        
    # make a few extra attributes for the variables. 
    ncatt_put(nc, varid = v, attname = "species", attval = species, prec = "char")    
    ncatt_put(nc, varid = v, attname = "sector", attval = as.numeric(strsplit(v,":")[[1]][4]), prec = "int")    
    ncatt_put(nc, varid = v, attname = "country_ISO", attval = var_iso, prec = "char")
    ncatt_put(nc, varid = v, attname = "CountryNum", attval = var_code, prec = "int")
    
	  # collect the data for inserting - list of sector stacks. nlyr = v_time (from time_dim)
	  l_sec_v_multi <- l_ukiesea_emis[[var_iso]]
	  
	  # we have to extract terrestrial or sea based on ISO
    emis_partition <- ifelse(var_iso == "SEA", "sea", "terrestrial")
	  l_sec_v <- lapply(l_sec_v_multi, function(x) rast(x[grep(emis_partition, names(x))]) )
	  if(unique(unlist(lapply(l_sec_v, function(x) length(x)))) != 1) stop("Too many partitions selected pre-input.")

    # select the specific sector
    v_sec <- paste0("sec", strsplit(v,":")[[1]][4])
    s_sec <- l_sec_v[[grep(paste0("_",v_sec,"_"), names(l_sec_v))]]

	  # also, flip the data at this stage: netCDF and R terra have different start points for data.
    s_sec <- flip(s_sec, direction="vertical")
	  l_for_sum[[v]] <- l_sec_v
    
    # convert to array - use lapply over the sectors. 
    # if it's annual, you could stack the sector list and just make the array; 
      # a <- array(rast(l_sec_v), dim = c(n_lon, n_lat, n_sector, n_time))
    # but we want flexibility for month layers. Making a huge sector-month stack wont work,
    # the array will 'fill-up' the sector dim with the time layers from sector 1 and so on. 
    a <- array(s_sec, dim = c(n_lon, n_lat, n_time))
        
    # last check for NA/NaN/Inf and set to 0
    a[is.na(a)] <- 0 ; a[is.nan(a)] <- 0 ; a[is.infinite(a)] <- 0 	
    
    # insert data
    ncvar_put(nc, v, a)
	
	  ## summary of data going into netcdf ##
    # basic table
    dt <- data.table(Project = project, 
                     Scenario = scenario, 
	                   Area = var_iso,
                     iso_code = var_code,
                     Pollutant = species, 
                     data_source = "NetCDF_input",
                     emis_y = y, 
                     inv_y = naei_inv, 
                     agg = uk_agg_schema,					 
                     time_res = time_dim,
                     layer_name = v)
    
	  dt[, sec_num := as.numeric(strsplit(v,":")[[1]][4])]
    dt[, sec_name := dt_SNAPGNFR[snap_id == sec_num, snap_name]]
    
    # add some summarised data from netCDF surface
    time_cols <- paste0("t",i_time)
    
    # add in annual totals - emissions coming in, array and the newly input ncdf data
    dt[, emis_t_tot_ncinput := sum(global(s_sec, sum, na.rm=T)[,1]) ]
    dt[, emis_t_tot_array := sum(a[,,1]) ]
    
    # summarise the monthly emissions totals put into ncdf
    dt[, (time_cols) := sum(global(s_sec, sum, na.rm=T)[,1])]
    
    # summarise the monthly emissions totals
    dt[, tsum := rowSums(.SD, na.rm=T), .SDcols = time_cols]
    dt[, tot_tres_ratio := emis_t_tot_ncinput / tsum]	
    
    # list
    l[[v]] <- dt
    
    # tidy
    remove(l_sec_v_multi)
    remove(a)
    gc()	 
    
  }
  
  #########
  ## NO - see Janice Scheffler email 28/01/25
  
  # For the species, there needs to be one sum surface, all ISO all sector. 
  # set some variables for inputting. 
  # mol_weight <- get_mol_weight(species)
  
  # make a few extra attributes for the variables.
  # v <- species
  # ncatt_put(nc, varid = v, attname = "species", attval = species, prec = "char")
  # if(!is.na(mol_weight)) ncatt_put(nc, varid = v, attname = "molecular_weight"  , attval = mol_weight, prec = "int")
  # if(!is.na(mol_weight)) ncatt_put(nc, varid = v, attname = "molecular_weight_units"  , attval = "g mole-1", prec = "char")
  
  # collect the data for inserting - sum all ISO-sectors, over time dim
  # in annual data this will be 13 layers for sectors, per ISO
  # in other data, this will be 13 x i_time layers, per ISO
  # THIS WILL NEED A FUTURE CHECK THAT SUM OVER MONTH IS CORRECT
  # l_iso <- lapply(l_for_sum, function(x) rast(x))
  # l_iso <- lapply(l_iso, function(x) suppressWarnings(tapp(x, i_time, sum, na.rm=T)) )
  # s_iso <- rast(l_iso)
  # s_all_time <- suppressWarnings(tapp(s_iso, i_time, sum, na.rm=T))
  # NO FLIP - taken from processed loop above. 
  # s_all_time <- lapply(s_all_time, function(x) flip(x, direction="vertical"))
  # s_all_time <- rast(s_all_time) # comes out as list after the flip. 
  
  # convert to array - use lapply over the sectors. 
  # as it's summary, just make the array
  # a <- array(s_all_time, dim = c(n_lon, n_lat, n_time))
  
  # last check for NA/NaN/Inf and set to 0
  # a[is.na(a)] <- 0 ; a[is.nan(a)] <- 0 ; a[is.infinite(a)] <- 0 	
  
  # insert data
  # ncvar_put(nc, v, a)
  
  
  ## summary of data going into netcdf ##
  # basic table
  # dt <- data.table(Area = "SUM_ALL",
  #                 iso_code = 0,
  #                 Pollutant = species, 
  #                 data_source = "NetCDF_input",
  #                 emis_y = y, 
  #                 inv_y = naei_inv, 
  #                 agg = eu_agg_schema,					 
  #                 time_res = time_dim,
  #                 sec_num = "TOTAL")
  
  # dt[, sec_name := "TOTAL" ]
  
  # add some summarised data from netCDF surface
  # time_cols <- paste0("t",i_time)
  
  ## THE FOLLOWING WILL NEED A REVIEW WHEN RUNNING MONTHLY
  
  # add in annual totals - emissions coming in, array and the newly input ncdf data
  # dt[, emis_t_tot_ncinput := sum(global(s_all_time, sum, na.rm=T)[,1]) ]
  # dt[, emis_t_tot_array := sum(a) ]
  
  # summarise the emissions totals put into ncdf
  # dt[, (time_cols) := unlist(global(s_all_time, sum, na.rm=T)[,1]) ]
  
  # summarise the monthly emissions totals
  # dt[, tsum := rowSums(.SD, na.rm=T), .SDcols = time_cols]
  # dt[, tot_tres_ratio := emis_t_tot_ncinput / tsum]	
  
  # list
  # l[[paste0(v,"_sum")]] <- dt
  #########
  
  
  # close connection  
  nc_close(nc)
  
  # combine summaries and write
  dt_ncdf_summary <- rbindlist(l, use.names = T, fill = T)
  
  return(dt_ncdf_summary)
  
} # end of function

######################################################################################################
#### function to write out the summary tables into a new folder
write_summaries_uk <- function(y, species, naei_inv, map_yr_uk, folname,
                            dt_inv, dt_mask, dt_group, dt_ncinp, dt_ncout){
										
  dir.create(file.path(folname, "tables", paste0("e",y)), showWarnings = FALSE, recursive = T)  
	
  fname_route <- paste0(dt_poll[ceh_poll == species, emep_model],"_UKEIRE_",y,"emis_",map_yr_uk,"map_",naei_inv,"inv")
    
  # write
  fwrite(dt_inv,   paste0(folname, "/tables/e", y, "/", fname_route, "_INVENTORY.csv"))
  fwrite(dt_mask,  paste0(folname, "/tables/e", y, "/", fname_route, "_MASKED.csv"))
  fwrite(dt_group, paste0(folname, "/tables/e", y, "/", fname_route, "_PROCESSED.csv"))
  fwrite(dt_ncinp, paste0(folname, "/tables/e", y, "/", fname_route, "_NETCDFINP.csv"))
  fwrite(dt_ncout, paste0(folname, "/tables/e", y, "/", fname_route, "_NETCDFOUT.csv"))
  
}

######################################################################################################
#### function to read and summarise nc file fresh. Post writing. 
summarise_nc_file_uk <- function(project, scenario, fname_ncdf, y, species, 
	                             naei_inv, time_dim, v_EMEP_sec){
    
  # time dims
  if(time_dim == "annual"){ i_time <- 1 }else if(time_dim == "month"){ i_time <- 1:12 }else{ i_time <- 1:365 }
  
  # gather sector info direct from input file. Yet another safety check. 
  nc <- nc_open(fname_ncdf)
  v_var <- names(nc$var)
  nc_close(nc)
  
  # list for summary data
  l <- list()  
      
  for(v in v_var){
  
    ## if it's the full sum layer, change the v_EMEP_sec to 1. 
    if(v == species) v_EMEP_sec_now <- 1
    if(v != species) v_EMEP_sec_now <- v_EMEP_sec
    
    ## extract and make stack
    # simply using rast() will read in all layers across sector & time dims, into 1 big stack (can't handle 4D). 
    # We can certainly work with this, but it might need to go via an array etc in the future. 
    
    # terra cant handle the variable names of 4.36
    # use raster() for the summary
    require(raster)
    s_nc <- suppressWarnings(stack(fname_ncdf, varname = v))
        
    # this ensures the sectors are split out into their own stacks (important for non-annual)
    #if(v == species){
    #  l_out <- list(s_nc)
    #}else{
    #  l_out <- lapply(v_EMEP_sec_now, function(x){ s_nc[[grep(paste0("sector=",x,"$"), names(s_nc))]]})	
    #}	
    
    # some variables for table.
    # var_spec <- strsplit(v,":")[[1]][1]
    var_iso <- strsplit(v,":")[[1]][2]
            
    if(uk_agg_schema == "allISO")   var_code <- ifelse(var_iso == "GB", 27, ifelse(var_iso == "IE", 14, 171)) 
    if(uk_agg_schema == "oneUKIE")  var_code <- 64 # DON'T KNOW!!
    
    # summarise all     
    dt <- data.table(Project = project, 
                     Scenario = scenario, 
	                   Area = var_iso,
                     iso_code = var_code,
                     Pollutant = species, 
                     data_source = "NetCDF_output",
                     emis_y = y, 
                     inv_y = naei_inv, 
                     agg = uk_agg_schema,					 
                     time_res = time_dim)
    
    dt[, sec_num := as.numeric(strsplit(v,":")[[1]][4])]
    dt[, sec_name := dt_SNAPGNFR[snap_id == sec_num, snap_name]]
    
    # add some summarised data from netCDF surface
    time_cols <- paste0("t",i_time)
    
    # summarise the emissions totals put into ncdf (totals, ignore time)
    dt[, emis_t_tot_ncoutput := suppressWarnings(sum(cellStats(s_nc, sum, na.rm=T))) ]
    
    # summarise nc emissions data in time splits
    ## AGAIN - THE FOLOWING WILL NOT WORK FOR MONTHLY DATA, WRONG STRUCTURE
    dt[, (time_cols) := suppressWarnings(sum(cellStats(s_nc, sum, na.rm=T)))]
    
    # summarise the emissions totals
    dt[, tsum := rowSums(.SD, na.rm=T), .SDcols = time_cols]
    dt[, tot_tres_ratio := emis_t_tot_ncoutput / tsum ]
    
    ## plus a totals table
    #
    #
    # HERE
    # dt <- rbindlist(list(dt_time, dt_tot), use.names = T)
    
    l[[v]] <- dt  
  
    
    } # var name
   
  dt_ncfile_summary <- rbindlist(l, use.names = T)
  
  return(dt_ncfile_summary)

}