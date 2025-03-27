
#########################################################
#### FUNCTIONS FOR CREATION OF EMEP - EU INPUT FILES ####
#########################################################

######################################################################################################
#### function to take EMEP emissions, make ready to EMEP format and create netCDFs for EU

EMEP_EU_v5.0 <- function(y, v_pollutants, time_dim = c("annual","month","yday"), 
                         v_EMEP_sec, emep_inv, folname, tp_scheme, eu_agg_schema){
  
  # v_years  = vector, *numeric*, year to process.
  if(!is.numeric(v_years)) stop ("Year vector is not numeric")
  
  # emissions year = *numeric*
  if(!is.numeric(emep_inv)) stop ("Reporting year is not numeric")
  
  print(paste0(format(Sys.time(), "%F %R"),": Creating EMEP4UK EU inputs (",time_dim,") for ",y,"..."))
    
  # For the years & pollutants, take the EU emissions in csv format and;
  #   i) convert every country/sector into a raster
  #  ii) MASK THE DATA to fit *around* the UK emissions data
  # iii) split into monthly emissions, if needed
  #  iv) create netCDF
    
  # Make a new UK mask to the EMEP input restrictions - need this for processing
  r_uk_EU_ext <- aggregate(extend(r_dom_terr_10km, ext(r_dom_EU)), fact = 10) # converts UK territory+10km mask to EU size
  
  res_crs <- "0.1_LL" # not currently used. 
  
  ####################################################
  #### CREATE THE NETCDF FILE TO PUT EMISSIONS IN
  fname_ncdf <- create_NETCDF_eu(y, v_pollutants, folname, emep_inv,
                                 v_EMEP_sec, time_dim, eu_agg_schema)
    
  for(species in v_pollutants){
  
      ######################################################################################
      if(!(species %in% c("nox","sox","nh3", "co", "voc","pm25","pm10","pmco")) ) stop ("Species must be in: 
                                            AP:    co, nh3, nox, sox, voc
                                            PM:    pm25, pm10, pmco")
      ######################################################################################
      
      print(paste0(format(Sys.time(), "%F %R"),":        ",species," data:"))
      
      # we are using the EMEP csv emissions NOT the netcdf emissions
          # https://www.ceip.at/the-emep-grid/gridded-emissions/nox
      # this is because we need a variable *per country* per sector - the netcdf does not have country IDs
      
	  # There is no *alternative* emissions source, only the processed EMEP data (yet!)
	  emis_loc <- paste0("/gws/nopw/j04/ceh_generic/inventory_processor/data/EMEP/inv",emep_inv,"/maps/csv")
	  
      # set up blank stack ready for data of different countries
      l_eu_emis  <- list()
      
	  # Unprocessed EMEP sector totals list
      l_EMEP_tots <- list()
      
      # Processed sector totals list
      l_eu_summary <- list()
      
      # sector names to loop through - this is netcdf sector names (sec01 etc.)
      v_sectors <- dt_sec[EMEP_sec %in% v_EMEP_sec,unique(sec)]
      
      print(paste0(format(Sys.time(), "%F %R"),":            gathering emissions..."))
	  
      for(i in v_sectors){
        
        if(i == "") next # not interested in currently blank EMEP-named sectors
        if(dt_sec[sec == i, GNFRlong]  == "" ) next # not interested in blank named GNFR sectors either
                        
        #######################################################
        #### OBTAIN 12 MONTHS OF DATA FOR EACH GNFR SECTOR ####
        
        ## There is one GNFR sector per EMEP sector name
        # read in the GNFR csv from EMEP/CEIP
        # create a raster for every country in that sector (with UK masked out)
        # fill in missing countries/sectors with blanks
        # there are no EMEP csv files for Intl Ships (in shipping), Avi Cruise (??) or LULUCF
        
        ###################################
        #### LIST OF EMISSION SURFACES ####
        # evaluate the emissions input file only. 
		l_EMEP_file <- summarise_EMEP_file(emis_loc, species, y, i, emep_inv, r_uk_EU_ext)
		
		l_EMEP_tots[[i]] <- l_EMEP_file[[2]]
		
		# bring in all emissions into stacks
        l_eu <- EMEP_sector_Emissions(fname = l_EMEP_file[[1]], i, y, species, r_uk_EU_ext)
    		
        ########################
        #### TEMPORAL SPLIT ####
        # if the time_dim is year, the data stays as 1 annual total.
        # if the time_dim is month, the data needs to be split into 12 layers
        # this is either; 
                         # the default (up to 2019) timing files in EMEP. <CURRENTLY THIS>
                         # or something newer like EDGAR?                
        l_eu_prof <- split_EU_annual(species = species, time_dim, eu_tp_scheme, l_annual = l_eu, i = i)
        		
        ###################
        #### COLLATING ####
        # add temporal raster stacks to lists - this is now a list (sectors) of lists (countries by month)         
        l_eu_emis[[i]] <- l_eu_prof        
		
        ####################
        #### STATISTICS ####
        # summarising the processed emissions files
        dt_totals <- summarise_EU_emissions(y, species, i, l_s = l_eu_prof, time_dim)
        
        l_eu_summary[[i]] <- dt_totals
        
        
      } # sector loop
      
	  ###########################################################
	  #### summary files - emissions and processed emissions ####
	  
	  dt_emep_emis <- rbindlist(l_EMEP_tots, use.names=T)[order(ISO, GNFR)]
	  
	  dt_proc_emis <- rbindlist(l_eu_summary, use.names=T)[order(ISO, GNFR)]
	  
	  #######################################################
      #### AGGREGATE/RESHAPE BY SECTOR/ISO (IF REQUIRED) ####
	  
	  # The netcdf needs to have every sector at annual/month, per ISO/EU
	  # tp_schema already taken care of above. Incoming data is;
		# l_eu_emis ---> list of 13 x sectors pollutant-1 a-1 
	    #           ---> each of which list of 60 ISO sector-1 
		#           ---> each of which stack of month/annual ISO-1
	  # this structure means the large emissions files are read in once, not every time per ISO
	  # However the data need to be `pollutant_iso`, with the sectors satcked as a dimension. 
		# Reshape here.
	  
	  # if agg_schema is; 
		# EU: one EU territory file for month/annual data (i.e. no ISO). 
		# ISO = separate ISO inputs for month/annual data.

	  print(paste0(format(Sys.time(), "%F %R"),":            Reshaping..."))
	  l_eu_toInp <- reshape_EU(y, species, eu_agg_schema, time_dim, l_eu_emis)
	  
      ###################################################
      #### INPUT DATA TO NETCDF TO SPECIES VARIABLES ####
      print(paste0(format(Sys.time(), "%F %R"),":            populating netcdf..."))
      	
	  # input data and summarise what's going in. 
      dt_ncinput_summary <- input_data_NETCDF_eu(y, species, emep_inv, time_dim, v_EMEP_sec,
	                                             eu_agg_schema, l_eu = l_eu_toInp, fname_ncdf)
      	  
      ##############################
	  #### QAQC; TABLES & PLOTS ####
	  print(paste0(format(Sys.time(), "%F %R"),":            summaries..."))
	  
	  # summarise the nc file itself, post writing. Double checker. (bit of a time sap)
	  dt_ncoutput_summary <- summarise_nc_file_eu(fname_ncdf, y, species, emep_inv, time_dim, v_EMEP_sec)
	  
	  # write out polluatnt level tables. These can be used for plots etc. 
	  write_summaries_eu(y, species, emep_inv, folname, dt_emep_emis = dt_emep_emis, 
	                     dt_proc_emis = dt_proc_emis, dt_ncinp = dt_ncinput_summary, 
					     dt_ncout = dt_ncoutput_summary)
	  
      # need clever plot function for all summaries;
	  # dt_in_file
	  # dt_emis_summary
	  # dt_ncdf_summary
	  
      print(paste0(format(Sys.time(), "%F %R"),":            pollutant complete."))
	  
   } # pollutant loop
   
  
  print(paste0(format(Sys.time(), "%F %R"),": DONE."))
  
} # end of function
 
######################################################################################################
#### function to summarise the emissions file, before anything is done to it. 

summarise_EMEP_file <- function(emis_loc, species, y, i, emep_inv, r_uk_EU_ext){

  # v_years  = vector, *numeric*, year to process.
  if(!is.numeric(y)) stop ("Year vector is not numeric")
  
  # set the diffuse filename - the inventory year determines the year of production
  # but the emissions are produced in csv lat/lon files back to 1990 (see Inventory Processor)
  # https://www.ceip.at/the-emep-grid/gridded-emissions/nox
  # e.g. if choosing EMEP inventory year 2023, all of the emissions will have been produced that year
  #! Scaling by reported timeline DITCHED. too many errors/artefacts/oddities.
  
  # if the year is earlier than 1990, use 1990 here;
  
  if(y >= 1990){
    
	f_diff <- paste0(emis_loc,"/",y,"/EMEP_",dt_poll[emep_model == species, invProc],"_DIFFUSE_inv",emep_inv,"_emis_",y,"/GNFR/EMEP_",
                   dt_poll[emep_model == species, invProc],"_DIFFUSE_inv",emep_inv,"_emis_",y,"_GNFR_",dt_sec[sec == i, GNFRlong],"_t_LL.csv")
  
  }else{
    
	f_diff <- paste0(emis_loc,"/1990/EMEP_",dt_poll[emep_model == species, invProc],"_DIFFUSE_inv",emep_inv,"_emis_1990/GNFR/EMEP_",
                   dt_poll[emep_model == species, invProc],"_DIFFUSE_inv",emep_inv,"_emis_1990_GNFR_",dt_sec[sec == i, GNFRlong],"_t_LL.csv")
  
  }
      
  dt_emis <- fread(f_diff)
  
  emep_y <- ifelse(y >= 1990, y, 1990)
    
  if(nrow(dt_emis) != 0){
  
    dt_emis[, EMEP_data := emep_y] 
  
    dt_tot <- dt_emis[,.(emis_t = sum(emis_t, na.rm=T)), by = .(ISO2, Year, EMEP_data, GNFR, Pollutant) ]
	
	dt_tot[, c("SNAP", 
	           "Region", 
			   "EMEP_Sector", 
			   "sec_long", 
			   "long_name", 
			   "Data_source") := 
	       list(dt_sec[sec == i, SNAP], 
		       "eu", 
			   dt_sec[sec == i, EMEP_sec], 
			   i, 
			   dt_sec[sec == i, name], 
			   "EMEP_unprocessed") ]
			   
	setnames(dt_tot, "ISO2", "ISO")
  
  }else{
   
    # summarise the emissions totals
	
    dt_tot <- data.table(ISO = NA,
	                     Year = y,
					     EMEP_data = emep_y,
                         GNFR = dt_sec[sec == i, GNFRlong],
					     Pollutant = species,
					     emis_t = 0,
                         SNAP = dt_sec[sec == i, SNAP],
					     Region = "eu",
                         EMEP_Sector = dt_sec[sec == i, str_pad(EMEP_sec, 2, "0", side = "left")],
                         sec_long = i,
                         long_name = dt_sec[sec == i, name],
				         Data_source = "EMEP_unprocessed")
     
  }
  
  # Check if the ISO codes in the emissions data are present in the v_iso from the model
  # If not, something needs looking at - is the latest model version being used?
  v_iso_emis <- unique(dt_emis[,ISO2])
  if(any(!(v_iso_emis %in% dt_iso[,ISO_char]))) stop("There are extra ISO codes in the emissions - check the model version!")
    
  return(list(f_diff, dt_tot))
  
}

######################################################################################################
#### function to collect EMEP sector data, per country, for diffuse emissions
EMEP_sector_Emissions <- function(fname, i, y, species, r_uk_EU_ext){
   
  dt_emis <- fread(fname)
    
  #####################
  ### ALPHA SCALING ###
  # EMEP mapped data != EMEP reported values in some countries (esp. E.Europe).
  # using alpha values DEPRECATED. to cumbersome and files are too different. 
    
  # EMEP mapped data (above) is emep_inv - 2, so if year != emep_inv - 2, we need the alpha values for scaling
  #f_alpha <- paste0(emis_loc,"/../../alpha/EMEP_AllPoll_TOTALS_inv",emep_inv,"_emis_1970-",emep_inv-2,"_GNFR_alpha.csv")
  #dt_alpha <- fread(f_alpha)
	
  #dt_alpha <- dt_alpha[Pollutant == species & Year == y & GNFR == dt_sec[sec == i, GNFRlong] ]
  
  ################
  ### STACKING ###
  
  ### for every country present in the data; 
    # rasterise its data to the whole extent and add to list
	# There are 60 unique ISO codes in the EMEP data (in 2022 inventory)
   
  # lapply using a small function below  - which includes masking to UK boundary and setting NA to 0
  # use all iso codes in model, make a blank surface if needed. 
  l_r <- lapply(dt_iso[,ISO_char], function(x) ISO_sector_Raster(x, dt_emis = dt_emis, i = i, species = species,
                                                   y = y, UKmask = r_uk_EU_ext))
  names(l_r) <- dt_iso[,ISO_char]
  
  return(l_r)
  
} # end of function

######################################################################################################
#### function to create raster from ISO information in EMEP emissions csv

 ISO_sector_Raster <- function(iso, dt_emis, i, species, y, UKmask){
	 	 
	 # subset the data to the ISO
	 dt <- dt_emis[ISO2 == iso, c("Lon","Lat","emis_t")]
	 
	 # rasterise using a point matrix and value field
	 if(nrow(dt) == 0){	 
	   r <- r_dom_EU
	 }else{
       r <- rasterize(as.matrix(dt[,c("Lon","Lat")]), y = r_dom_EU, values = dt[["emis_t"]], fun = sum, na.rm=T)	 
	 }
	 	 
	 # For data prior to 1990, the 1990 file read in above needs to be adjusted
	 # obtain scaling factor from CEDS and apply - this is for EU data prior to 1990 only
	 if(y < 1990){
	   
	   # data is derived from CEDS, indexed to 1990 at the sector/ISO level. 
	   # /gws/nopw/j04/ceh_generic/samtom/SPEED/CEDS_for_EMEP
	   dt_ceds_data <- fread(paste0("../SPEED/CEDS_for_EMEP/",species,"_CEDS_1950_1990_ISO_GNFR_kt.csv"))
	   dt_ceds_90   <- dt_ceds_data[Year == 1990 & GNFR == dt_sec[sec == i, GNFRlong]] # this is for tricky ISOs (below)
	   dt_ceds      <- dt_ceds_data[Year == y & GNFR == dt_sec[sec == i, GNFRlong]]
	 
	 # it gets complicated: There are 51 ISO2 codes in the CEDS data - 60 ISO2 codes in the EMEP spatial data
	 #                    : 50/51 ISO2 codes in CEDS data exist in the EMEP data: 'XX' does not (represents GLOBAL)
	 #                    : however they are all territories, countries, i.e. no ocean/sea areas or other generalities
	 #                    : codes in EMEP not in CEDS: 'AST', 'MC',   'NOA',    'RUE',        'ATL',    'BAS',   'BLS',    'CAS',  'MED',     'NOS'
	 #                                                 Asia  Monaco  N.Africa  Russia.Ext  NE.Atlantic  Baltic  BlackSea  Caspian  Med.Sea  North.Sea
	 #
	 # This makes Shipping (especially) very hard. 
	 # CEDS DOES have data for Dom & Intl Shipping, for each ISO and the entire globe respectively. 
	 # CEDS DOES have data for Dom & Intl Aviation, but only for the entire globe (for both). 
	 #
	 # FIXes;
	 # use CEDS G_Shipping for sec07 for each ISO. Where the ISO does not exist in CEDS (10 codes above) - use P_IntShipping for all
	 # do not use O_AviCruise data, use H_Aviation (only exists for 'GLOBAL') for aviation for all ISO codes
	 # non-Aviation or Ships for 10 missing codes: use the change (1990 to y) of whole sector for all 50 ISO2 codes in CEDS
	 # do not use alpha values for I_Offroad (sec09) - crazy results. Just have to accept absolute values from CEDS.
	 
	   v_CEDS_missing <- c("AST","MC","NOA","RUE","ATL","BAS","BLS","CAS","MED","NOS")
	   
	   if(i == "sec07"){
	     
		 # Shipping: if it's a missing sector, use P_IntShip, otherwise use the ISO G_Shipping
		 if(iso %in% v_CEDS_missing){	       
		   # subset to P_IntShipping
		   alpha_fac <- dt_ceds_data[Year == y & GNFR == "P_IntShipping" & ISO2 == "XX", alpha]
		   
	     }else{
	       # simply take the CEDS value
		   alpha_fac <- dt_ceds[ISO2 == iso, alpha]
		 		 
	     }  
	   
	   }else if(i == "sec08"){
	   
	     # Aviation: doesn't matter if it's a missing ISO or not, simply take the global CEDS value
		 alpha_fac <- dt_ceds[ISO2 == "XX", alpha] # this is the same Global H_Aviation value for every ISO
	   
       }else if(i == "sec09"){

         # I_Offroad: due to some LARGE disparities between 1990 emissions and pre-1990 emissions in the
		 #            CEDS data for this sector, the alpha values applied to EMEP 1990 data are massive. Use CEDS actual values. 
		 alpha_fac <- dt_ceds[ISO2 == iso, emis_kt] # !! THIS IS EMISSION VALUE IN KT !!
		 
 	   
	   }else{
	     
		 # ANY other: if it's a missing sector, use whole EU sector change, otherwise use the ISO
		 if(iso %in% v_CEDS_missing){
	       # make a mean across all ISO codes for the tricky areas not in CEDS
		   all_iso_tot_y  <- dt_ceds[ISO2 %in% dt_emis[,unique(ISO2)], sum(emis_kt, na.rm = T)]
		   all_iso_tot_90 <- dt_ceds_90[ISO2 %in% dt_emis[,unique(ISO2)], sum(emis_kt, na.rm = T)]
		   alpha_fac <- all_iso_tot_y/all_iso_tot_90
		   
	     }else{
	       # simply take the CEDS value
		   alpha_fac <- dt_ceds[ISO2 == iso, alpha]
		 		 
	     }
		 		 		 
	   }
	   
	 # reset some NAs, if they exist	 
	 if(length(alpha_fac) == 0) alpha_fac <- 0
	 if(is.na(alpha_fac))       alpha_fac <- 0
	 
	 # multiply by the alpha scalar
	 if(i != "sec09"){
	   r <- r * alpha_fac 
	 }else{ # it is important that I_Offroad is treated separately; using actual kt value
	   r <- (r / global(r, sum, na.rm=T)$sum) * alpha_fac 
	 }
	 	 
	 
	 } # end of pre1990 scaling, IF NEEDED
	  
	 
	 # Mask to the UK extent
	 r <- mask(r, UKmask, inverse=T)
	 
	 # set NA to 0 (for EMEP), and NaN, and Infinite
	 r[is.na(r)] <- 0 ; r[is.nan(r)] <- 0 ; r[is.infinite(r)] <- 0 	
	 
	 # name
	 names(r) <- paste0(species,"_",iso,"_sector=",dt_sec[sec == i, EMEP_sec])

	 return(r)
	  
}

######################################################################################################
#### function to split annual emissions out into months (or keep as annual)
split_EU_annual <- function(species, time_dim = c("annual","month"), eu_tp_scheme, l_annual, i){
    
  time_dim <- match.arg(time_dim)
  
  # set time splits
  if(time_dim == "annual"){ i_time <- 1 }else{ i_time <- 1:12 }
  
  # if time dim is a annual, no split
  if(time_dim == "annual"){
    
    l_s <- copy(l_annual)
	names(l_s) <- names(l_annual)
        
  }else{
    
    ## Use the nominated temporal schema to split the data to monthly layers
	
	# NOT INCORPORATED: 
	## If the eu_tp_scheme = "pre_TEMREG";
	     # this is EMEP4UK temporal data as of July 2023. Monthly splits, for 5 SNAPs, per ISO
    ## Option to change this to use EDGAR
	     # we could use the EDGAR generated regional profiles. 
    	
	if(eu_tp_scheme %in% c("EMEP4UKv4.45", "EMEP4UKv5.0")){
	
	# If the tp_scheme = version of EMEP4UK (e.g. 'EMEP4UKv4.45'); use EMEP defaults of that version
	# read in timing file for legacy temporal splits (subset to Eire or UK - SEA needs to match parent country)
	dt_timing <- fread(paste0("/gws/nopw/j04/ukem/test/sam/ukem_pro/output/model_inputs/EMEP4UK/",tp_scheme,"/MonthlyFacs.",species))
		   
    names(dt_timing) <- c("ISO","SNAP",month.abb[1:12])
    dt_profs <- melt(dt_timing, id.vars = c("ISO","SNAP"), variable.name = "MON", value.name = "FAC")
    
	# as there are up to 60 ISO codes in the list/stack, best to use lapply
	l_s <- lapply(names(l_annual), function(x) emep_ISO_profile(x, species, dt_profs, i_time, l_annual, i))
	
	names(l_s) <- names(l_annual)
		
	}else{
	
	  # THIS IS FOR POTENTIAL EDGAR USAGE
	
	}
	
  } # end of time_dim = month 
  
  return(l_s) # return the monthly/annual emissions
  
} # end function

######################################################################################################
#### function to apply temporal profile splits by ISO ID
emep_ISO_profile <- function(iso, species, dt_profs, i_time, l_annual, i){
    
  # subset list
  r <- l_annual[[iso]]
  
  # collect the country code and the SNAP ID
  country_id <- dt_iso[EMEP_iso == iso, EMEP_code]
  snap_id <- dt_sec[sec == i, as.numeric(SNAP)] # set the SNAP to read from the timing.     
  
  # extract required timing data - Use 1s if the SNAP sector in dt_dec is "NA"
  if(is.na(snap_id)){ # If snap is NA in the sector file
    v_timing <- rep(1,12)
  }else if(!(snap_id %in% dt_profs[ISO == country_id, SNAP])){ # if SNAP is not in the timing file
    v_timing <- rep(1,12)
  }else if(!(country_id %in% dt_profs[,ISO])){ # if country ISO is not in timing file
    v_timing <- rep(1,12)	
	
  }else{
	v_timing <- dt_profs[ISO == country_id & SNAP == snap_id][["FAC"]] # vector of monthly splits 
	v_timing <- v_timing/mean(v_timing)# not always adding to 12 in the timing file, adjust slightly
  }
 
  # make a standard 1 month raster. Annual/12.
  s_month <- rast(lapply(r, function(x) rep(x/12, 12)))
    
  # adjust with temporal profile
  s <- s_month * v_timing
 
  names(s) <- paste0(iso,"_",i,"_emis_t_",str_pad(i_time, 2, "0", side = "left"))
  
  return(s)

}

######################################################################################################
#### function to summarise input emissions
summarise_EU_emissions <- function(y, species, i, l_s, time_dim){
  
  # set time splits
  if(time_dim == "annual"){ i_time <- 1 }else{ i_time <- 1:12 }
  
  # there are some instances where there are no emissions from the sector and it will error
  if(length(l_s) != 0){
  
    l <- lapply(l_s, function(x) global(x, sum, na.rm=T))
    l_dt <- lapply(l, function(x) as.data.table(x))
	l_dt <- lapply(names(l_s), function(x) l_dt[[x]][, layer_name := names(l_s[[x]])]) ; names(l_dt) <- names(l)
	l_dt <- lapply(1:length(l_dt), function(x) l_dt[[x]][, ISO := names(l_dt)[x]])
	l_dt <- lapply(l_dt, function(x) x[, step := paste0("t",i_time)])
    dt_emis <- rbindlist(l_dt, use.names=T)
    setnames(dt_emis, "sum", "emis_t")
    dt_w <- dcast(dt_emis, ISO+layer_name~step, value.var = "emis_t")
  
  }else{
   
    dt_emis <- data.table(ISO = NA, 
						  layer_name = paste0(species,"_NA_sector=",dt_sec[sec == i, EMEP_sec]), 
						  step := paste0("t",i_time), 
						  emis_t = 0)
						  
	dt_w <- dcast(dt_emis, ISO+layer_name~step, value.var = "emis_t")
  
  }
   
  
  # summarise the emissions totals
  dt <- data.table(Year = y,
                   Pollutant = species,
                   Region = "eu",
                   GNFR = dt_sec[sec == i, GNFRlong],
                   SNAP = dt_sec[sec == i, SNAP],
                   EMEP_Sector = dt_sec[sec == i, EMEP_sec],
                   sec_long = i,
                   long_name = dt_sec[sec == i, name],
				   Data_source = "EMEP_processed")
  
  dt_i <- cbind(dt, dt_w)
  dt_i[, ann_emis_kt := rowSums(.SD, na.rm=T)/1000, .SDcols = paste0("t",i_time)]  
   
  return(dt_i)  
  
} # end of function

######################################################################################################
#### function to aggregate the emissions in a nominated way; e.g. by sector, by ISO grouping etc
reshape_EU <- function(y, species, eu_agg_schema = c("allISO", "oneEU"), time_dim, l_eu_emis){
	
	eu_agg_schema <- match.arg(eu_agg_schema)
	
	if(time_dim == "annual"){ i_time <- 1 }else{ i_time <- 1:12 }
    n_time <- length(i_time)
	
	if(eu_agg_schema == "allISO"){
	
	  # no need to aggregate to one EU - only rehsape to stacks of sectors ISO-1
	  l_reshaped <- purrr::list_transpose(l_eu_emis)
	  
	  # but do not collapse sectoral list (per ISO) into stacks, as;
		# a) that overwrites names. 
		# b) we can have time disaggregated stacks per sector.
	  	
	  return(l_reshaped)
	
	}else if(eu_agg_schema == "oneEU"){
		
	  # this schema aggregates all the data into one EU surface, per sector per time step.
	  # this heavily reduces the variables to read (from ISO level)
	  # but retains the separation by sector
	  	  	  	  
	  # convert all list elements in rast stacks (12 x n.ISO) - skip if empty list element
	  l_s <- lapply(l_eu_emis, function(x) aggToEU(x) )
		
	  # the new rasts are months 1 to 12, by ISO code. 
	  # the layer names suggest it is one month of ~60 ISOs, repeated, but this is WRONG
	  	  
	  l_s_sum <- lapply(l_s, function(x) sumMonths(x, n_time) )
	
	  return(l_s_sum)
	
	}
		
}

#### sub-functions for aggregating EU data, specifically EU

# quick function to allow for empty list elements (e.g. CO in AgriLivestock) - can't be aggregated
aggToEU <- function(l){
	  
	    if(length(l) == 0){
		  return(l)
		}else{
		  l2 <- rast(l)
		  return(l2)
		}	  
}

sumMonths <- function(s, n_time){
	     
		 if(length(s) == 0){
		  return(s)
		 }else{
		  s12 <- tapp(s, index = 1:n_time, sum, na.rm=T)
		  return(s12)
		 }	   
}


######################################################################################################
#### function to create a netCDF and input the data - simple routine chooser
create_NETCDF_eu <- function(y, v_pollutants, folname, emep_inv,
                             v_EMEP_sec, time_dim, eu_agg_schema){
						   
  if(time_dim == "annual"){
  
    fname <- create_NETCDF_eu_annual(y, v_pollutants, folname, emep_inv, 
                                     v_EMEP_sec, time_dim, eu_agg_schema)
								 
  }else if(time_dim == "month"){
  
    fname <- create_NETCDF_eu_month()
  
  }
  
  
  
  return(fname)

}

######################################################################################################
#### function to create a netCDF and input the data - this is the ANNUAL/MONTHLY input (EMEPv5.0)
create_NETCDF_eu_annual <- function(y, v_pollutants, folname, emep_inv, 
                                    v_EMEP_sec, time_dim, eu_agg_schema){
  
  if(time_dim != "annual") stop("time choice has to be annual to make an annual netcdf.")
    
  # create output directory
  dir.create(file.path(folname), showWarnings = FALSE, recursive = T)   
    
  # create netcdf name
  nc_filename <- paste0(folname,"/EU_", emep_inv,"inv_", y,"emis_0.1.nc")
      
  # if the file already exists, just delete and rewrite
  if(file.exists(nc_filename)){
    
    print(paste0("NetCDF already exists in this folder location; DELETING & REPLACING..."))
    file.remove(nc_filename)
    
  }else{}
  
  ## EMEPv5.0 file creation. 
  # this contains every pollutant_iso as variables (60 * 7). There is also a sum sector/iso variable (7). 
  # each pollutant_iso variable has sector dim of length(sectors) - determined by dt_sec
  # the sum surface is all isos and sectors all summed. 
  # dt_iso determines the iso codes.
    
  # Set up the dimensions: latlong, time, sectors  
  v_lon    <- as.array(seq(xmin(r_dom_EU) + 0.1/2, xmax(r_dom_EU) - 0.1/2, 0.1))
  n_lon    <- length(v_lon)
  v_lat    <- as.array(seq(ymin(r_dom_EU) + 0.1/2, ymax(r_dom_EU) - 0.1/2, 0.1))
  n_lat    <- length(v_lat)  
  v_sector <- v_EMEP_sec
  n_sector <- length(v_sector)
  v_time   <- 1 # this has to be 1 for this annual function. 
  n_time   <- length(v_time)
    
  # create dimensions
  dimlon  <- ncdim_def(name = "lon",  longname = "longitude", 
					   units = "degrees_east",  vals = v_lon, unlim = FALSE)
  dimlat  <- ncdim_def(name = "lat",  longname = "latitude", 
                       units = "degrees_north", vals = v_lat, unlim = FALSE)  
  dimsecs <- ncdim_def(name = "sector", longname = "GNFR sector index",  
                       units = "", vals = v_sector, unlim = FALSE)
  dimtime <- ncdim_def(name = "time", longname = "time", 
                       units = "", vals = v_time, unlim = FALSE)
  
  # create the dims as variables. 
  #ncdim_lon <- ncvar_def(name = "lon", longname = "longitude", units = "degrees_east",
#				  	  		   dim = list(dimlon), compression = 4, prec = "float")
#  ncdim_lat <- ncvar_def(name = "lat", longname = "latitude", units = "degrees_north",
#				  	  		   dim = list(dimlat), compression = 4, prec = "float")
#  ncdim_sec <- ncvar_def(name = "sector", longname = "GNFR sector index", units = "",
#				  	  		   dim = list(dimsecs), compression = 4, prec = "float")
#  ncdim_tim <- ncvar_def(name = "time", longname = "time index", units = "",
#				  	  		   dim = list(dimtime), compression = 4, prec = "float")
#							   
#  l_dim_var <- list(ncdim_lon, ncdim_lat, ncdim_sec, ncdim_tim)
  
  
  # Create names and variables for all pollutants_ISOs SEPARATELY
  if(eu_agg_schema == "oneEU"){
  
    v_vars <- unlist(lapply(v_pollutants, function(x) paste0(x,"_EUR")))
	
  }else if(eu_agg_schema == "allISO"){
    
	v_iso_emep <- sort(dt_iso[,ISO_char])
    v_vars <- unlist(lapply(v_pollutants, function(x) paste0(x,"_",v_iso_emep)))
  
  }
    
  # for each pollutant_ISO variable, create a new netcdf var
  l_iso_var <- lapply(X = 1:length(v_vars), function(s){
                     ncvar_def(name = v_vars[s],
                               units = "tonnes/year", 							 
				  	  		   #missval = EMEP_fillval, # _FillValue ?
				               dim = list(dimlon, dimlat, dimsecs, dimtime), 
                               compression = 4,
                               prec = "float")})
  
  # combine the variable lists   
#  l_var	<- c(l_dim_var, l_iso_var)
	
  ## Create the new netcdf
  nc_new <- nc_create(nc_filename, l_iso_var, force_v4 = T)
  
  ###############
  ## NO - see Janice Scheffler email 28/01/25
  
  # now ADD the summary variables, that dont have a sector dim. 
  #l_sum_var <- lapply(X = 1:length(v_pollutants), function(s){
  #                   ncvar_def(name = v_pollutants[s],
  #                             units = "tonnes/year", 							 
  #				  	  		   #missval = EMEP_fillval, # _FillValue ?
  #				               dim = list(dimlon, dimlat, dimtime), 
  #                             compression = 4,
  #                             prec = "float")})

  # can var_add list of new variables, so loop
  #for(j in 1:length(l_sum_var)){
  #  nc_new <- ncvar_add(nc_new, l_sum_var[[j]])
  #}
  ###############
    
  # Finally the global attributes
  ncatt_put(nc_new, 0, "description", "EU_EMEP", prec = "char")
  ncatt_put(nc_new, 0, "Conventions", "CF-1.6 for coordinates", prec = "char")
  ncatt_put(nc_new, 0, "created_date", format(Sys.Date(), "%Y%m%d"), prec = "int")
  ncatt_put(nc_new, 0, "created_hour",  gsub(":","",format(Sys.time(), "%F %R")), prec = "double") 
  ncatt_put(nc_new, 0, "projection", "lon lat", prec = "char")
  ncatt_put(nc_new, 0, "periodicity", "yearly", prec = "char")
  
  # 3 extras by me
  ncatt_put(nc_new, 0, "Grid_resolution", "0.1", prec = "char")
  ncatt_put(nc_new, 0, "Created_with", R.Version()$version.string, prec = "char")
  ncatt_put(nc_new, 0, "ncdf4_version", packageDescription("ncdf4")$Version, prec = "char")
  
  # sectors - this might have to change if the amount of sectors input changes
  ncatt_put(nc_new, 0, "SECTORS_NAME", "GNFR", prec = "char")
  
  for(i in v_EMEP_sec){
  
    glob_att_name <- dt_sec[GNFRlong != "" & EMEP_sec == i, sec]
	glob_att_val  <- dt_sec[GNFRlong != "" & EMEP_sec == i, GNFRlong]
  
    ncatt_put(nc_new, 0, glob_att_name, glob_att_val, prec = "char")
  
  }
  
  #ncatt_put(nc_new, 0, "NCO","netCDF Operators version 4.9.8 (Homepage = http://nco.sf.net, Code = http://github.com/nco/nco)", prec = "char")
  
  #close connection  
  nc_close(nc_new)  
  
  return(nc_filename)
  
} # end of function


######################################################################################################
#### function to create a netCDF and input the data - this is the MONTHLY input (EMEPv4.45)
create_NETCDF_eu_month <- function(){

}

######################################################################################################
#### function to put data into the pre-made netcdf file
input_data_NETCDF_eu <- function(y, species, emep_inv, time_dim, v_EMEP_sec,
                                 eu_agg_schema, l_eu, fname_ncdf){
  
  if(length(l_eu) != 60) stop("There is not 60 ISO sector lists.")
  
  if( unique(unlist(lapply(l_eu, function(x) length(x)))) != length(v_EMEP_sec) ){
    stop("Some ISOs do not have the same sector length as nominated.")
  }
  
  # set time length based on choice of time dimension
  if(time_dim == "annual"){ i_time <- 1 }else{ i_time <- 1:12 }
  
  # open netcdf
  nc <- nc_open(fname_ncdf, write = T)      
    
  # create the vector of variables (needs to match ncdf created above, or it wil fail). 
  # pollutant_iso only, not the total summary variable. 
  if(eu_agg_schema == "oneEU"){
  
    v_vars <- unlist(lapply(species, function(x) paste0(x,"_EUR")))
	
  }else if(eu_agg_schema == "allISO"){
    
	v_iso_emep <- sort(dt_iso[,ISO_char])
    v_vars <- unlist(lapply(species, function(x) paste0(x,"_",v_iso_emep)))
  
  }
  
  # array dimensions (identical to ncdf)
  n_lon    <- nc$dim$lon$len
  n_lat    <- nc$dim$lat$len
  n_sector <- nc$dim$sector$len
  n_time   <- nc$dim$time$len
  
  if(length(i_time) != n_time) stop("time_dim choice and time dim inside netCDF are not the same.")
  
  l <- list()  # for summary data
    
  for(v in v_vars){
  
    # set some variables for inputting. 
	var_iso <- strsplit(v,"_")[[1]][2]
	if(eu_agg_schema == "allISO") var_code <- dt_iso[ISO_char == var_iso, ISO_num]
	if(eu_agg_schema == "oneEU")  var_code <- 64
	mol_weight <- get_mol_weight(species)
	
	# make a few extra attributes for the variables. 
	ncatt_put(nc, varid = v, attname = "species", attval = species, prec = "char")
	if(!is.na(mol_weight)) ncatt_put(nc, varid = v, attname = "molecular_weight"  , attval = mol_weight, prec = "int")
    if(!is.na(mol_weight)) ncatt_put(nc, varid = v, attname = "molecular_weight_units"  , attval = "g mole-1", prec = "char")
	ncatt_put(nc, varid = v, attname = "country_ISO", attval = var_iso, prec = "char")
    ncatt_put(nc, varid = v, attname = "countrycode", attval = var_code, prec = "int")
	
	# collect the data for inserting - list of sector stacks. nlyr = v_time (from time_dim)
	# also, flip the data at this stage: netCDF and R terra have different start points for data.
	l_sec_v <- l_eu[[var_iso]]
	l_sec_v <- lapply(l_sec_v, function(x) flip(x, direction="vertical"))
		
	# convert to array - use lapply over the sectors. 
		# if it's annual, you could stack the sector list and just make the array
	# a <- array(rast(l_sec_v), dim = c(n_lon, n_lat, n_sector, n_time))
		# but we want flexibility for month layers. Making a huge sector-month stack wont work,
		# the array will 'fill-up' the sector dim with the time layers from sector 1 and so on. 
	l_a <- lapply(l_sec_v, function(x) array(x, dim = c(n_lon, n_lat, 1, n_time)))
	a <- abind(l_a, along = 3) # combine on the sector dimension
	
	# last check for NA/NaN/Inf and set to 0
	a[is.na(a)] <- 0 ; a[is.nan(a)] <- 0 ; a[is.infinite(a)] <- 0 	
	  
    # insert data
	ncvar_put(nc, v, a)
	    
	## summary of data going into netcdf ##
	# basic table
    dt <- data.table(iso_char = var_iso,
					 iso_code = var_code,
					 Pollutant = species, 
					 Data_source = "NetCDF_input",
	                 emis_y = y, 
					 inv_y = emep_inv, 
					 agg = eu_agg_schema,					 
					 time_res = time_dim,
					 sec_num = names(l_sec_v))
	
	dt[, sec_name := dt_sec$GNFRlong[match(names(l_sec_v), dt_sec[,sec])] ]
					 
	# add some summarised data from netCDF surface
	time_cols <- paste0("t",i_time)

    # add in annual totals - emissions coming in, array and the newly input ncdf data
	dt[, emis_t_tot_ncinput := unlist(lapply(l_sec_v, function(x) sum(global(x, sum, na.rm=T)[,1]))) ]
	dt[, emis_t_tot_array := unlist(lapply(1:n_sector, function(x) sum(a[,,x,]))) ]
	
	# summarise the monthly emissions totals put into ncdf
    dt[, (time_cols) := unlist(lapply(1:n_sector, function(x) global(l_sec_v[[x]],  sum, na.rm=T)[,1])) ]
	
	# summarise the monthly emissions totals
    dt[, tsum := rowSums(.SD, na.rm=T), .SDcols = time_cols]
	dt[, tot_tres_ratio := emis_t_tot_ncinput / tsum]	
	
	# list
	l[[v]] <- dt
    
	# tidy
	remove(l_sec_v)
	remove(a)
    gc()	 
  
  }
   
  ###############
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
  # in other data, this will be 13 x i_time layers, per ISO
  # THIS WILL NEED A FUTURE CHECK THAT SUM OVER MONTH IS CORRECT
  # l_iso <- lapply(l_eu, function(x) rast(x))
  # l_iso <- lapply(l_iso, function(x) suppressWarnings(tapp(x, i_time, sum, na.rm=T)) )
  # s_iso <- rast(l_iso)
  # s_all_time <- suppressWarnings(tapp(s_iso, i_time, sum, na.rm=T))
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
  # dt <- data.table(iso_char = "SUM_ALL",
  #  			   iso_code = 0,
  #				   Pollutant = species, 
  #				   Data_source = "NetCDF_input",
  #	               emis_y = y, 
  #				   inv_y = emep_inv, 
  #				   agg = eu_agg_schema,					 
  #				   time_res = time_dim,
  #				   sec_num = "TOTAL")
	
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
  ###############
  
    
  #close connection  
  nc_close(nc)
  
  # combine summaries and write
  dt_ncdf_summary <- rbindlist(l, use.names = T)
  
  return(dt_ncdf_summary)
  
} # end of function

######################################################################################################
#### function to read and summarise nc file fresh. Post writing. 
summarise_nc_file_eu <- function(fname_ncdf, y, species, emep_inv, time_dim, v_EMEP_sec){
    
  # time dims
  if(time_dim == "annual"){ i_time <- 1 }else if(time_dim == "month"){ i_time <- 1:12 }else{ i_time <- 1:365 }

  # gather sector info direct from input file. Yet another safety check. 
  nc <- nc_open(fname_ncdf)
  v_var <- names(nc$var)
  v_var <- v_var[grep(paste0("^",species), v_var)] # as all variables exist in .nc, restrict to species. 
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
    s_nc <- suppressWarnings(terra::rast(fname_ncdf, subds = v))
    
	# this ensures the sectors are split out into their own stacks (important for non-annual)
	if(v == species){
	  l_out <- list(s_nc)
	}else{
	  l_out <- lapply(v_EMEP_sec_now, function(x){ s_nc[[grep(paste0("sector=",x,"$"), names(s_nc))]]})	
	}	
	    
    # some variables for table.
    var_spec <- strsplit(v,"_")[[1]][1]
    if(v != species) var_iso <- strsplit(v,"_")[[1]][2]
    if(v == species) var_iso <- "SUM_ALL"
    
    if(eu_agg_schema == "allISO") var_code <- dt_iso[ISO_char == var_iso, ISO_num]
    if(v == species) var_code <- 0
    if(eu_agg_schema == "oneEU")  var_code <- 64
    	
	## unsure how this will behave if the data is monthly - presume it'' be too long
    v_secs <- as.numeric(unlist(lapply(l_out, function(x) strsplit(names(x),"=")[[1]][2])))
	v_secs <- paste0("sec",str_pad(v_secs, width = 2, side = "left", 0))
       
    # summarise all     
    dt <- data.table(iso_char = var_iso,
                     iso_code = var_code,
                     Pollutant = species, 
                     Data_source = "NetCDF_output",
                     emis_y = y, 
                     inv_y = emep_inv, 
                     agg = eu_agg_schema,					 
                     time_res = time_dim,
                     sec_num = v_secs)
    
    dt[, sec_name := dt_sec$GNFRlong[match(v_secs, dt_sec[,sec])] ]
    if(v == species) dt[, sec_name := "TOTAL"]
    
    # add some summarised data from netCDF surface
    time_cols <- paste0("t",i_time)
    
    # summarise the emissions totals put into ncdf (totals, ignore time)
	dt[, emis_t_tot_ncoutput := suppressWarnings(unlist(lapply(l_out, function(x) sum(global(x, sum, na.rm=T)[,1]))) ) ]
		
	# summarise nc emissions data in time splits
	## AGAIN - THE FOLOWING WILL NOT WORK FOR MONTHLY DATA, WRONG STRUCTURE
    dt[, (time_cols) := suppressWarnings(unlist(lapply(v_EMEP_sec_now, function(x) global(l_out[[x]],  sum, na.rm=T)[,1]))) ]
    
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

######################################################################################################
#### function to write out the summary tables into a new folder
write_summaries_eu <- function(y, species, emep_inv, folname, dt_emep_emis, 
	                           dt_proc_emis, dt_ncinp, dt_ncout){
    
  dir.create(file.path(folname, "tables", paste0("e",y)), showWarnings = FALSE, recursive = T)  
	
  fname_route <- paste0(dt_poll[ceh_poll == species, emep_model],"_EU_",y,"emis_",naei_inv,"inv")
    
  
  fwrite(dt_emep_emis, paste0(folname, "/tables/e", y, "/", fname_route, "_INVENTORY.csv"))
  fwrite(dt_proc_emis, paste0(folname, "/tables/e", y, "/", fname_route, "_PROCESSED.csv"))
  fwrite(dt_ncinp,     paste0(folname, "/tables/e", y, "/", fname_route, "_NETCDFINP.csv"))
  fwrite(dt_ncout,     paste0(folname, "/tables/e", y, "/", fname_route, "_NETCDFOUT.csv"))
  
}

######################################################################################################
#### function to