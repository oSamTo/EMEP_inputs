
#########################################################
#### FUNCTIONS FOR CREATION OF EMEP - EU INPUT FILES ####
#########################################################


######################################################################################################
#### function to take EMEP emissions, make ready to EMEP format and create netCDFs for EU

EMEPinputEU <- function(v_years, species, eu_agg_schema, time_dim = "month", 
                        eu_tp_scheme, emep_inv, output_dir){
  
  # v_years  = vector, *numeric*, year to process.
  if(!is.numeric(v_years)) stop ("Year vector is not numeric")
  
  # emissions year = *numeric*
  if(!is.numeric(emep_inv)) stop ("Reporting year is not numeric")
  
  # For the years & pollutants, take the EU emissions in csv format and;
  #   i) convert every country/sector into a raster
  #  ii) MASK THE DATA to fit *around* the UK emissions data
  # iii) split into monthly emissions, if needed
  #  iv) create netCDF
    
  # Make a new UK mask to the EMEP input restrictions - need this for processing
  r_uk_EU_ext <- aggregate(extend(r_dom_terr_10km, ext(r_dom_EU)), fact = 10) # converts UK territory+10km mask to EU size
  
  res_crs <- "0.1_LL"
  
  ## loop through years and pollutants listed and make a netCDF input file for each.
  ## This is to be made with a monthly time attribute
      # for the month, incorporate the EDGAR temporal data 
  
  for(y in v_years){
          
      ######################################################################################
      if(!(species %in% c("nox","sox","nh3", "co", "nmvoc","pm25","pm10","pmco")) ) stop ("Species must be in: 
                                            AP:    co, nh3, nox, sox, nmvoc
                                            PM:    pm25, pm10, pmco")
      ######################################################################################
      
      print(paste0(Sys.time(),": Creating EMEP4UK EU input netCDF for ",species," in ",y,"..."))
      
      # we are using the EMEP csv emissions NOT the netcdf emissions
          # https://www.ceip.at/the-emep-grid/gridded-emissions/nox
      # this is because we need a variable *per country* per sector - the netcdf does not have country IDs
      
	  # There is no *alternative* emissions source, only the processed EMEP data (yet!)
	  emis_loc <- paste0("/gws/nopw/j04/ceh_generic/inventory_processor/data/EMEP/inv",emep_inv,"/maps/csv")
	  
      # set up blank stack ready for data of different countries
      l_eu_allSec  <- list()
      
	  # Unprocessed EMEP sector totals list
      l_EMEP_tots <- list()
      
      # Processed sector totals list
      l_sum_allSec <- list()
      
      # sector names to loop through - this is netcdf sector names (sec01 etc.)
      v_sectors <- dt_sec[,unique(sec)]
      
      print(paste0(Sys.time(),":      Collecting and creating all emissions input data from inventory processor..."))
      
      for(i in v_sectors){
        
        if(i == "") next # not interested in currently blank EMEP-named sectors
        if(dt_sec[sec == i, GNFRlong]  == "" ) next # not interested in blank named GNFR sectors either
        # this isnt the same as UK, which uses blank placeholders, but there are many countries so ignore
        
        print(paste0(Sys.time(),":         ",i))
        
        #######################################################
        #### OBTAIN 12 MONTHS OF DATA FOR EACH GNFR SECTOR ####
        
        ## There is one GNFR per EMEP sector name
        # read in the GNFR csv for EMEP
        # create a raster for every country and every sector (with UK masked out)
        # fill in missing countries/sectors with blanks
        # there are no EMEP csv files for Intl Ships (in shipping), Avi Cruise (??) or LULUCF
        
        ###################################
        #### LIST OF EMISSION SURFACES ####
        # evaluate the emissions input file only. 
		l_EMEP_file <- summariseEMEPfile(emis_loc, species, y, i, emep_inv)
		
		l_EMEP_tots[[paste0("EMEP_totals_", species, "_", i)]] <- l_EMEP_file[[2]]
		
		# bring in all emissions into stacks
        l_eu <- EMEPsectorEmissions(fname = l_EMEP_file[[1]], i, y, r_uk_EU_ext)
                
        ########################
        #### TEMPORAL SPLIT ####
        # if the time_dim is year, the data stays as 1 annual total.
        # if the time_dim is month, the data needs to be split into 12 layers
        # this is either; 
                         # the default (up to 2019) timing files in EMEP. <CURRENTLY THIS>
                         # or something newer like EDGAR?        
        
        l_eu_prof <- splitEUannual(species = species, time_dim, eu_tp_scheme, l_annual = l_eu, i = i)
        		
        ###################
        #### COLLATING ####
        # add temporal raster stacks to lists - this is now a list (sectors) of lists (countries by month) 
        
        l_eu_allSec[[paste0("eu_", species, "_", i,"_",time_dim)]] <- l_eu_prof
        
		
        ####################
        #### STATISTICS ####
        # summarising the processed emissions files
        dt_totals <- summariseEUemissions(y, species, i, l_s = l_eu_prof)
        
        l_sum_allSec[[paste0("eu_totals_", species, "_", i,"_", time_dim)]] <- dt_totals
        
        
      } # sector loop
      
	  ###########################################################
	  #### summary files - emissions and processed emissions ####
	  
	  dt_in_file <- rbindlist(l_EMEP_tots, use.names=T)[order(ISO, GNFR)]
	  
	  lapply(l_EMEP_tots, names)
	  
	  dt_emis_summary <- rbindlist(l_sum_allSec, use.names=T)[order(ISO, GNFR)]
	  
	  ####################################################
      #### AGGREGATION BY SECTOR OR ISO (IF REQUIRED) ####
	  	  
	  print(paste0(Sys.time(),":               Aggregating by sector and/or ISO code..."))
	  l_eu_agg <- aggregateEU(y, species, eu_agg_schema, time_dim, l_eu_allSec)
	  	  
      ############################################################
      #### CREATE AND POPULATE NETCDF ON POLLUTANT/YEAR BASIS ####
            
      print(paste0(Sys.time(),":               Creating and populating netcdf..."))
      
      dt_ncdf_summary <- createNETCDFeu(y, species, emep_inv, output_dir, time_dim, eu_tp_scheme, 
                                        eu_agg_schema, l_eu = l_eu_agg)
      
      
      # need clever plot function for all summaries;
	  # dt_in_file
	  # dt_emis_summary
	  # dt_ncdf_summary
    
  } # year
  
  
} # end of function
 
######################################################################################################
#### function to summarise the emissions file, before anything is done to it. 

summariseEMEPfile <- function(emis_loc, species, y, i, emep_inv){

  # v_years  = vector, *numeric*, year to process.
  if(!is.numeric(y)) stop ("Year vector is not numeric")
  
  # set the diffuse filename - the inventory year determines the year of production
  # but the emissions are produced in csv lat/lon files back to 1990 (see Inventory Processor)
  # https://www.ceip.at/the-emep-grid/gridded-emissions/nox
  # e.g. if choosing EMEP inventory year 2023, all of the emissions will have been produced that year
  #! Scaling by reported timeline DITCHED. too many errors/artefacts/oddities.
  
  # if the year is earlier than 1990, use 1990 here;
  
  if(y >= 1990){
    
	f_diff <- paste0(emis_loc,"/",y,"/EMEP_",species,"_DIFFUSE_inv",emep_inv,"_emis_",y,"/GNFR/EMEP_",
                   species,"_DIFFUSE_inv",emep_inv,"_emis_",y,"_GNFR_",dt_sec[sec == i, GNFRlong],"_t_LL.csv")
  
  }else{
    
	f_diff <- paste0(emis_loc,"/1990/EMEP_",species,"_DIFFUSE_inv",emep_inv,"_emis_1990/GNFR/EMEP_",
                   species,"_DIFFUSE_inv",emep_inv,"_emis_1990_GNFR_",dt_sec[sec == i, GNFRlong],"_t_LL.csv")
  
  }
      
  dt_emis <- fread(f_diff)
  
  emep_y <- ifelse(y >= 1990, y, 1990)
    
  if(nrow(dt_emis) != 0){
  
    dt_emis[, EMEP_data := emep_y] 
  
    dt_tot <- dt_emis[,.(emis_t = sum(emis_t, na.rm=T)), by = .(ISO2, Year, EMEP_data, GNFR, Pollutant) ]
	dt_tot[, c("Year","SNAP", "Region", "EMEP_Sector", "sec_long", "long_name", "Data_source") := 
	      list(y, dt_sec[sec == i, SNAP], "eu", dt_sec[sec == i, str_pad(EMEP_sec, 2, "0", side = "left")], i, dt_sec[sec == i, name], "EMEP_unprocessed") ]
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
				     Data_source = "EMEP_mapped_time_series")
     
  }
  
  
  return(list(f_diff, dt_tot))
  
}

######################################################################################################
#### function to collect EMEP sector data, per country, for diffuse emissions
EMEPsectorEmissions <- function(fname, i, y, r_uk_EU_ext){
   
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
  l_r <- lapply(unique(dt_emis[,ISO2]), function(x) ISOsectorRaster(x, dt_emis = dt_emis, r_temp = r_dom_EU, 
                                                                    i = i, y = y, UKmask = r_uk_EU_ext))
  names(l_r) <- unique(dt_emis[,ISO2])
  
  return(l_r)
  
} # end of function

######################################################################################################
#### function to create raster from ISO information in EMEP emissions csv

 ISOsectorRaster <- function(iso, dt_emis, r_temp, i, y, UKmask){
	 	 
	 # subset the data to the ISO
	 dt <- dt_emis[ISO2 == iso, c("Lon","Lat","emis_t")]
	 
	 # rasterise using a point matrix and value field
	 r <- rasterize(as.matrix(dt[,c("Lon","Lat")]), y = r_temp, values = dt[["emis_t"]], fun = sum, na.rm=T)
	 	 
	 
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
	 names(r) <- paste0(iso,"_",i,"_emis_t")

	 return(r)
	  
}

######################################################################################################
#### function to split annual emissions out into months (or keep as annual)
splitEUannual <- function(species, time_dim = c("year","month"), eu_tp_scheme, l_annual, i){
    
  time_dim <- match.arg(time_dim)
  
  # set time splits
  if(time_dim == "year"){ i_time <- 1 }else{ i_time <- 1:12 }
  
  # if time dim is a year, no split
  if(time_dim == "year"){
    
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
	if(species == "nmvoc"){
	    dt_timing <- fread(paste0("/gws/nopw/j04/ceh_generic/samtom/TEMREG/output/model_inputs/EMEP4UK/",tp_scheme,"/MonthlyFacs.voc"))
	}else{
	    dt_timing <- fread(paste0("/gws/nopw/j04/ceh_generic/samtom/TEMREG/output/model_inputs/EMEP4UK/",tp_scheme,"/MonthlyFacs.",species))
	}
	   
    names(dt_timing) <- c("ISO","SNAP",month.abb[1:12])
    dt_timing_m <- melt(dt_timing, id.vars = c("ISO","SNAP"), variable.name = "MON", value.name = "FAC")
    
	# as there are up to 60 ISO codes in the list/stack, best to use lapply
	l_s <- lapply(names(l_annual), function(x) emepISOprofile(x, species, dt_profs = dt_timing_m, i_time, l_annual, i))
	
	names(l_s) <- names(l_annual)
		
	}else{
	
	  # THIS IS FOR POTENTIAL EDGAR USAGE
	
	}
	
  } # end of time_dim = month 
  
  return(l_s) # return the monthly/annual emissions
  
} # end function

######################################################################################################
#### function to apply temporal profile splits by ISO ID
emepISOprofile <- function(iso, species, dt_profs, i_time, l_annual, i){
    

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
summariseEUemissions <- function(y, species, i, l_s){
  
  # there are some instances where there are no emissions from the sector and it will error
  if(length(l_s) != 0){
  
    l <- lapply(l_s, function(x) global(x, sum, na.rm=T))
    l_dt <- lapply(l, function(x) as.data.table(x))
    l_dt <- lapply(1:length(l_dt), function(x) l_dt[[x]][, ISO := names(l_dt)[x]])
    l_dt <- lapply(l_dt, function(x) x[, month := paste0("emis_M",str_pad(1:12, 2, "0", side = "left"))])
    dt_emis <- rbindlist(l_dt, use.names=T)
    setnames(dt_emis, "sum", "emis_t")
    dt_w <- dcast(dt_emis, ISO~month, value.var = "emis_t")
  
  }else{
   
    dt_emis <- data.table(ISO = NA, month = paste0("emis_M",str_pad(1:12, 2, "0", side = "left")), emis_t = 0)
	dt_w <- dcast(dt_emis, ISO~month, value.var = "emis_t")
  
  }
  
  
  
  # summarise the emissions totals
  dt <- data.table(Year = y,
                   Pollutant = species,
                   Region = "eu",
                   GNFR = dt_sec[sec == i, GNFRlong],
                   SNAP = dt_sec[sec == i, SNAP],
                   EMEP_Sector = dt_sec[sec == i, str_pad(EMEP_sec, 2, "0", side = "left")],
                   sec_long = i,
                   long_name = dt_sec[sec == i, name],
				   Data_source = "EMEP_mapped_time_series")
  
  dt_i <- cbind(dt, dt_w)
  dt_i[, ann_emis_kt := rowSums(.SD, na.rm=T)/1000, .SDcols = paste0("emis_M",str_pad(1:12, 2, "0", side = "left"))]  
   
  return(dt_i)  
  
} # end of function

######################################################################################################
#### function to aggregate the emissions in a nominated way; e.g. by sector, by ISO grouping etc
aggregateEU <- function(y, species, eu_agg_schema = c(NA, "oneEU"), time_dim, l_eu_allSec){
	
	eu_agg_schema <- match.arg(eu_agg_schema)
	
	if(time_dim == "year"){ v_time <- 1 }else{ v_time <- v_yday }
    n_time <- length(v_time)
	
	if(is.na(eu_agg_schema)){
	
	  # there is no schema to aggregate and so the EU file remains as it is
	
	  return(l_eu_allSec)
	
	}else if(eu_agg_schema == "oneEU"){
		
	  # this schema aggregates all the data into one EU surface, per sector per month.
	  # this heavily reduces the variables to read (from ISO level)
	  # but retains the separation by sector
	  	  	  	  
	  # convert all list elements in rast stacks (12 x n.ISO) - skip if empty list element
	  l_s <- lapply(l_eu_allSec, function(x) aggToEU(x) )
		
	  # the new rasts are months 1 to 12, by ISO code. 
	  # the layer names suggest it is one month of ~60 ISOs, repeated, but this is WRONG
	  	  
	  l_s_sum <- lapply(l_s, function(x) sumMonths(x, n_time) )
	
	  return(l_s_sum)
	
	}
		
}

#### sub-functions for aggregating EU data, specifically oneEU

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
#### function to create a netCDF and input the data
createNETCDFeu <- function(y, species, emep_inv, output_dir, time_dim, 
                           eu_tp_scheme, eu_agg_schema, l_eu){
  
    
  # create output directory
  folname <- paste0(output_dir,"/emis",y,"/EU/TP",tp_scheme,"_AGG",eu_agg_schema)
  dir.create(file.path(folname), showWarnings = FALSE, recursive = T)   
    
  # create netcdf name
  nc_filename <- paste0(folname,"/", dt_poll[ceh_poll == species, emep_model],"_EU_",
                        y,"emis_",y,"map_",emep_inv,"inv_0.1.nc")
 
  
  # if the file already exists, just delete and rewrite
  if(file.exists(nc_filename)){
    
    print(paste0("NetCDF already exists for ",species, " in ", y,"; DELETING & REPLACING..."))
    file.remove(nc_filename)
    
  }else{}
  
  
  # netCDF variables are done on sector name and attributes determine sector, country etc
  # in this file, all ISO codes across all sectors remain as separate variables. 
		# file coming in is a list of lists (sectors) : each list is one sector comprising all ISOs
  # there are 12 month layers, or just one annual layer (dependent on time_dim) - as a variable's dimension
  # the names, e.g. sec01, are taken from the lookup table 'dt_sec' and are currently what EMEP4UK requires
  
  # Set up the dimensions: latlong, time, sectors
  
  v_lon <- as.array(seq(xmin(r_dom_EU) + 0.1/2, xmax(r_dom_EU) - 0.1/2, 0.1))
  n_lon <- length(v_lon)
  v_lat <- as.array(seq(ymin(r_dom_EU) + 0.1/2, ymax(r_dom_EU) - 0.1/2, 0.1))
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
  
  # Create names and variables for all ISOs/sectors SEPARATELY
  # all unique ISOs & sector names (EMEP sector names)
  if(eu_agg_schema == "oneEU"){
  
    v_all_iso <- "EUR"
	
  }else{
  
    v_all_iso <- sort(unique(unname(unlist(lapply(l_eu, function(x) names(x))))))
  
  }
    
  v_emep_secnames <- dt_sec[sec != "" & GNFRlong != "", name]
    
  # all unique sectors
  v_sectors_eu <- as.vector(sapply(v_all_iso, function(x) paste0("Emis_",x,"_", v_emep_secnames) ))
  
  # matching country ISO codes
  #v_country <- as.numeric(plyr::mapvalues(unname(unlist(sapply(v_sectors_eu, function(x) str_split(x, "_")[[1]][2]))),
  #                                        dt_iso[, EMEP_iso], dt_iso[, EMEP_code], warn_missing = F))
   
  #if(length(v_sectors_eu) != length(v_country)) stop("sector names and country IDs not same length")
   
  # for each sector in every ISO, create a new netcdf var
  l_variables <- lapply(X = 1:length(v_sectors_eu), function(s){
    ncvar_def(name = v_sectors_eu[s],
              missval = EMEP_fillval, # _FillValue ?
              longname = str_split(v_sectors_eu[s],"_")[[1]][3], # long_name?
              units = ifelse(time_dim == "year", "tonnes yr-1" , "tonnes month-1"), 
              dim = list(dimlon, dimlat, dimtime), 
              compression = 4,
              prec = "float")})
  
  ## Create the new netcdf
  ncnew <- nc_create(nc_filename, l_variables, force_v4=T)
  
  # now extract the data from the raster Stack and insert
  print(paste0(Sys.time(),":               Inserting data..."))
  
  
  
  # l <- lapply(v_sectors_eu, function(x) insertVariable(species, x, l_eu_allSec, ncnew, y, time_dim, n_lon, n_lat, n_time) )
  
  l <- list()  
  
  for(v in 1:length(v_sectors_eu)){
  
  if(v %in% seq(1,801, 50)) print(v)
	
    sec_name  <- v_sectors_eu[v]
    sec_desc  <- substr(sec_name, str_locate_all(sec_name,"_")[[1]][2,2] + 1, nchar(sec_name))
	sec_i     <- dt_sec[name == sec_desc, sec]
    sec_EMEP  <- dt_sec[name == sec_desc, str_pad(EMEP_sec, 2, "0", side = "left")]
    sec_GNFR  <- dt_sec[name == sec_desc, GNFRlong]
    
    ISO <- str_split(sec_name, "_")[[1]][2]
    
	if(eu_agg_schema == "oneEU"){ISO_num  <- 64} else {ISO_num  <- dt_iso[EMEP_iso == ISO, EMEP_code]}
    	
    l_insert <- l_eu[[paste0("eu_",species,"_",sec_i,"_month")]]
    
	if(eu_agg_schema == "oneEU"){s_insert <- l_insert} else {s_insert <- l_insert[[ISO]]}
    
	# as we're using ALL unique ISOs, there might not actually be an emissions surface, so make a blank one
	if(is.null(s_insert)){
	  s_insert <- rep(r_dom_EU,12)
	  s_insert[] <- 0
	  names(s_insert) <- paste0(ISO,"_",sec_i,"_emis_t_",str_pad(1:12, 2, "0", side = "left"))
	}else if(length(s_insert)==0){
	  s_insert <- rep(r_dom_EU,12)
	  s_insert[] <- 0
	  names(s_insert) <- paste0(ISO,"_",sec_i,"_emis_t_",str_pad(1:12, 2, "0", side = "left"))
	}
	
    # extract the monthly data and put into ncdf
    a <- array(s_insert, dim = c(n_lon, n_lat, n_time))
    a <- a[,520:1,] # need to reverse the rows, for a reason i have not worked out. 
    
    ncvar_put(ncnew, sec_name, a)
    
	
	
    # few extra variable attributes
    ncatt_put(ncnew, varid = sec_name, attname = "long_name"  , attval = sec_desc, prec="char")
    ncatt_put(ncnew, varid = sec_name, attname = "description", attval = sec_desc, prec="char")
    ncatt_put(ncnew, varid = sec_name, attname = "sector"     , attval = as.integer(sec_EMEP),   prec="short")
    ncatt_put(ncnew, varid = sec_name, attname = "species"    , attval = ifelse(species=="nmvoc","voc",species) , prec="char")
    ncatt_put(ncnew, varid = sec_name, attname = "country"    , attval = ISO_num , prec="int")
    
	# summary of data going into netcdf
    dt <- data.table(Pollutant = species, Year = y, Region = "eu", ISO = ISO, long_name = sec_desc, Data_source = "NetCDF",
	                 EMEP_Sector = sec_EMEP, GNFR = sec_GNFR, i_sec = sec_i, time_res = time_dim, time_unit = 1:n_time, 
					 emis_kt = round(global(s_insert, sum, na.rm=T)[,1]/1000, 6))
    dtw <- dcast(dt, Pollutant+Year+Region+ISO+long_name+Data_source+EMEP_Sector+GNFR+i_sec ~ time_res+time_unit, value.var = "emis_kt")
    if(time_dim == "year"){ v_in <- 1 }else if(time_dim == "month"){ v_in <- 1:12 }
	if(time_dim == "month"){
	  setnames(dtw, paste0("month_",1:12), paste0("ncdf_M",str_pad(v_in, 2, "0", side = "left")))
	  dtw[, emis_ncdf_kt := rowSums(.SD, na.rm=T), .SDcols = paste0("ncdf_M",str_pad(v_in, 2, "0", side = "left"))]
	}
	
	l[[v_sectors_eu[v]]] <- dtw
    
	remove(l_insert)
	remove(s_insert)
    remove(a)
    gc()
	 
  
  }
  
  ## Finally the global attributes
  ncatt_put(ncnew, 0, "description","EU EMEP", prec="char")
  ncatt_put(ncnew, 0, "Conventions","CF-1.0", prec="char")
  ncatt_put(ncnew, 0, "projection","lon lat", prec="char")
  ncatt_put(ncnew, 0, "Grid_resolution", "0.1", prec="char")
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
  
  ncatt_put(ncnew, 0, "periodicity", ifelse(time_dim == "month", "monthly", "annual"), prec="char")
  ncatt_put(ncnew, 0, "NCO","netCDF Operators version 4.9.8 (Homepage = http://nco.sf.net, Code = http://github.com/nco/nco)", prec = "char")
  
  #close connection  
  nc_close(ncnew)
  
  # combine summaries and write
  dt_ncdf_summary <- rbindlist(l, use.names = T)
  
  return(dt_ncdf_summary)
  
} # end of function

######################################################################################################
#### function to summarise what's in the written netcdf











