####################################################
#### FUNCTIONS FOR CREATION OF EMEP INPUT FILES ####
####################################################

######################################################################################################
#### function to take NAEI emissions, make ready to EMEP format and create netCDFs

creatEMEPinput <- function(v_years, v_pollutants, time_scale = c("year","month"), map_yr, output_dir){
  
  time_scale <- match.arg(time_scale)
  
  # v_years  = vector, *numeric*, year to process.
  if(!is.numeric(v_years)) stop ("Year vector is not numeric")
  
  # map_yr = *numeric*, year of spatial dist. from NAEI. 
  if(map_yr < 2018) stop ("Spatial distribution must be 2018 or later")
  
  # For the years & pollutants, take the regional emissions in Lat Long and;
  #   i) convert point emissions (.csv) into a raster
  #  ii) combine the points with the diffuse (.tif) data as required for the model
  # iii) MASK THE DATA to fit inside the EU emissions data
  #  iv) For SNAP 1: ONLY point sources go into A_PublicPower as this is treated with 200m injection. All other into B_Industry.
  #   v) split into monthly emissions, if needed
  #  vi) create netCDF
  
  emis_loc <- "//nercbuctdb.ad.nerc.ac.uk/projects1/NEC03642_Mapping_Ag_Emissions_AC0112/NAEI_data_and_SNAPS"
  res_crs <- "0.01_LL"
 
  ## loop through years and pollutants listed and make a netCDF input file for each.
  ## This is to be made with a monthly time attribute
      # for the month, incorporate the new DUKEMs temporal data 
  
  for(y in v_years){
    
    for(species in v_pollutants){
      
      ######################################################################################
      if(!(species %in% c("ch4","co2","n2o","bap","bz","hcl","nox","so2","nh3", "co", "voc","cd","cu","pb","hg","ni","zn", "pm0_1","pm1","pm2_5","pm10")) ) stop ("Species must be in: 
                                            AP:    bap, bz, co, hcl, nh3, nox, so2, voc
                                            PM:    pm0_1, pm1, pm2_5, pm10
                                            GHG:   ch4, co2, n2o
                                            Metal: cd, cu, hg, ni, pb, zn")
      ######################################################################################
      
      print(paste0(Sys.time(),": Creating EMEP input netCDF for ",species," in ",y,"..."))
      
      # set up blank stacks ready for data of different regions
      l_uk_temp  <- list()
      l_ie_temp  <- list()
      l_sea_temp <- list()
      
      # sector totals list
      l_sec_tots <- list()
      
      # sector names to loop through
      v_sectors <- dt_sec[,name]
      
      print(paste0(Sys.time(),":               Collecting and creating all emissions input data..."))
      
      for(i in v_sectors){
        
        sc <- dt_sec[name == i, EMEP_sec]
        sc_pad <- str_pad(sc, 2, "0", side = "left")
        
        ####################################
        #### LISTS OF EMISSION SURFACES ####
        
        l_uk <- sectorEmissions(emis_loc, species, y, i, res_crs, map_yr, country = "uk")
        l_ie <- sectorEmissions(emis_loc, species, y, i, res_crs, map_yr, country = "eire")
        
        # Create a combined buffer strip zone - "SEA"
        r_SEA <- app(c(l_uk[["sea"]], l_ie[["sea"]]), sum, na.rm = T)
        
        ########################
        #### TEMPORAL SPLIT ####
        # if the time_scale is year, the data stays as 1 annual total.
        # if the time_scale is month, the data needs to be split into 12 layers using profiles from DUKEMs
        
        s_uk_temp   <- splitAnnual(species = species, time_scale = "month", r_annual = l_uk[["terrestrial"]],                                          i = i, sc = sc, sc_pad = sc_pad, country = "uk")
        s_ie_temp   <- splitAnnual(species = species, time_scale = "month", r_annual = l_ie[["terrestrial"]],                                          i = i, sc = sc, sc_pad = sc_pad, country = "ie")
        s_SEA_temp  <- splitAnnual(species = species, time_scale = "month", r_annual = r_SEA,                                                          i = i, sc = sc, sc_pad = sc_pad, country = "sea")
        
        ###################
        #### COLLATING ####
        # add temporal raster stacks to lists
        
        l_uk_temp[[paste0("uk_", sc_pad, "_", i,"_",time_scale)]]   <- s_uk_temp
        l_ie_temp[[paste0("ie_", sc_pad, "_", i,"_",time_scale)]]   <- s_ie_temp
        l_sea_temp[[paste0("sea_", sc_pad, "_", i,"_",time_scale)]] <- s_SEA_temp
        
        ####################
        #### STATISTICS ####
        
        dt_totals <- summariseEmissions(y, species, i, sc_pad, l_uk = l_uk, s_uk = s_uk_temp,
                                        l_ie = l_ie, s_ie = s_ie_temp, sea = s_SEA_temp)
        
        l_sec_tots[[paste0("totals_", sc_pad, "_", i,"_",time_scale)]] <- dt_totals
        
         
      } # sector loop
      
      ############################################################
      #### CREATE AND POPULATE NETCDF ON POLLUTANT/YEAR BASIS ####
      
      dt_emis_summary <- rbindlist(l_sec_tots, use.names=T)[order(Region, GNFR)]
      
      print(paste0(Sys.time(),":               Creating and populating netcdf..."))
      
      createNETCDF(l_uk_temp, l_ie_temp, l_sea_temp, y, species, map_yr, output_dir, time_scale, dt_emis_summary)
      
    } # pollutant loop
    
    print(paste0(Sys.time(),":               DONE..."))
    
  } # year loop
  
  
} # end of function
        

######################################################################################################
#### function to collect sector data for diffuse and points, based on country and sector
sectorEmissions <- function(emis_loc, species, y, i, res_crs, map_yr, country = c("uk", "eire")){
  
  country <- match.arg(country)
  
  # v_years  = vector, *numeric*, year to process.
  if(!is.numeric(y)) stop ("Year vector is not numeric")
  
  # map_yr = *numeric*, year of spatial dist. from NAEI. 
  if(map_yr < 2018) stop ("Spatial distribution must be 2018 or later")
  
  # set the diffuse filename
  GNFRfile <- paste0(emis_loc,"/Emissions_grids_plain/LL/",species,"/diffuse/",y,"/rasters_GNFR/",species,"_diff_",y,"_",country,"_GNFR_",dt_sec[name == i, GNFRlong],"_t_",res_crs,"_",map_yr,"NAEImap.tif")
  
  # some GNFR sectors blank. Not mapped to EMEP. Check for existence and skip
  if(file.exists(GNFRfile)){
    
    ## diffuse data ##
    r_diff <- crop(extend(rast(paste0(emis_loc,"/Emissions_grids_plain/LL/",species,"/diffuse/",y,"/rasters_GNFR/",species,"_diff_",y,"_",country,"_GNFR_",dt_sec[name == i, GNFRlong],"_t_",res_crs,"_",map_yr,"NAEImap.tif")), r_dom_0.1), r_dom_0.1)
    
    ## point data ## (should be fine if GNFR name is blank)
    dt_pts <- fread(paste0(emis_loc,"/Emissions_grids_plain/LL/",species,"/point/",y,"/",species,"_pt_",y,"_",country,"_GNFR_t_LL.csv"))[GNFR == dt_sec[name == i, GNFRlong] & AREA == toupper(country)]
    
    # if there are no points, use a blank raster, otherwise rasterize
    if(nrow(dt_pts) == 0){
      
      r_pt <- r_dom_0.1
      
    }else{
      
      v_pt <- vect(dt_pts, geom=c("Lon", "Lat"), crs = "EPSG:4326")
      r_pt <- terra::rasterize(v_pt, r_dom_0.1, field = "Emission", fun = sum)
      
    } # end of ifelse for points
    
    # stack and sum to one surface
    s <- c(r_diff, r_pt) ; names(s) <- c("diffuse","point")
    r <- app(s, sum, na.rm = T) ; names(r) <- "total"
    
  }else{
    
    r <- r_dom_0.1 ; names(r) <- "total"
  
  } # sector exclusion
  
  # EMEP needs zero value, not NA, in emissions
  r[is.na(r)] <- 0
  
  # Mask to the EMEP input restrictions
  r_t   <- mask(r,     r_dom_terr)
  r_t10 <- mask(r,     r_dom_terr_10km)
  r_ow  <- mask(r,     r_dom_terr_10km, inverse=T)
  r_sea <- mask(r_t10, r_dom_terr, inverse=T)
  
  l <- list(r, r_t, r_t10, r_ow, r_sea)
  names(l) <- c("total", "terrestrial","terrestrial_10km","outwith_10km","sea")
  
  return(l)
  
} # end of function

######################################################################################################
#### function to split annual emissions out into months (or keep as annual)
splitAnnual <- function(species, time_scale = c("year","month"), r_annual, i, sc, sc_pad, country = c("uk","ie","sea")){
  
  country    <- match.arg(country)
  time_scale <- match.arg(time_scale)
  if(time_scale == "year"){ i_time <- 1 }else{ i_time <- 1:12 }
  
  if(time_scale == "year"){
    
    s <- r_annual
    names(s) < paste0(species, "_", country, "_", sc_pad)
    
  }else{
    
    # read in the monthly splits and format from DUKEMs
    dt_gam <- suppressMessages(fread(paste0("https://github.com/oSamTo/DUKEMs_TP/raw/main/output/coeffs_sector/GNFR/",species,"/GAM_month_GNFR_",species,"_allYr_LIST.csv"), verbose = F, showProgress = F))
    # set name of C_ to match the EMEP code
    dt_gam[sector == "C_OtherStationaryComb", sector := "C_OtherStatComb"]
    
    # subset the GAM data to the right GNFR sector needed
    dt_profs <- dt_gam[sector == dt_sec[name == i, GNFRlong ]]
    
    if(nrow(dt_profs) > 0){
      v_coeffs <- dt_profs[, coeff] 
    }else{
      v_coeffs <- rep(1,12)
    }
    
    # make a standard 1 month raster. Annual/12.
    r_month <- r_annual/12
    s_month <- rep(r_month, 12)
    
    # adjust with temporal profile
    s <- s_month * v_coeffs
    names(s) <- paste0(species, "_", country, "_", sc_pad,"_",i,"_",i_time)
  }
  
  return(s) # return the split (or annual) emissions
  
} # end function

######################################################################################################
#### function to summarise input emissions
summariseEmissions <- function(y, species, i, sc_pad, l_uk, s_uk, l_ie, s_ie, sea){
  
  # summarise the emissions totals
  dt <- data.table(Year = y,
                   Pollutant = species,
                   Region = c("uk","ie","sea"),
                   GNFR = dt_sec[name == i, GNFRlong],
                   EMEP_Sector = sc_pad,
                   long_name = i,
                   Incoming_annual_kt      = unlist(c(global(l_uk[["total"]], sum, na.rm=T)/1000, global(l_ie[["total"]], sum, na.rm=T)/1000, 0)), 
                   Terrestrial_annual_kt   = unlist(c(global(l_uk[["terrestrial"]], sum, na.rm=T)/1000, global(l_ie[["terrestrial"]], sum, na.rm=T)/1000, 0)), 
                   #Terrestrial_split_kt    = unlist(c(sum(global(s_uk, sum, na.rm=T))/1000, sum(global(s_ie, sum, na.rm=T))/1000)),
                   Terrestrial10_annual_kt = unlist(c(global(l_uk[["terrestrial_10km"]], sum, na.rm=T)/1000, global(l_ie[["terrestrial_10km"]], sum, na.rm=T)/1000, sum(global(sea, sum, na.rm=T))/1000)),  
                   SEA_annual_kt           = unlist(c(global(l_uk[["sea"]], sum, na.rm=T)/1000, global(l_ie[["sea"]], sum, na.rm=T)/1000, sum(global(sea, sum, na.rm=T))/1000)), 
                   Out_of_Mask_annual_kt   = unlist(c(global(l_uk[["outwith_10km"]], sum, na.rm=T)/1000, global(l_ie[["outwith_10km"]], sum, na.rm=T)/1000, 0)) )
  
  return(dt)
  
  
} # end of function

######################################################################################################
#### function to create a netCDF and input the data
createNETCDF <- function(l_uk_temp, l_ie_temp, l_sea_temp, y, species, map_yr, output_dir, time_scale, dt_emis_summary){
  
  if(species == "pm2.5"){
    nc_filename <- paste0(output_dir,"/pm25_UKEIRE_",y,"emis_",map_yr,"map_0.01.nc")
  }else{
    nc_filename <- paste0(output_dir,"/", species,"_UKEIRE_",y,"emis_",map_yr,"map_0.01.nc")
  }
  
  # if the file already exists, just delete and rewrite
  
  if(file.exists(nc_filename)){
    
    print(paste0("NetCDF already exists for ",species, " in ", y,"; DELETING & REPLACING..."))
    file.remove(nc_filename)
    
  }else{}
  
  # netCDF variables are done on sector name and attributes determine sector, country etc
  # in this file, UK (27), EIRE (14) and SEA (171) remain separate
  # there are 12 month layers, or just one annual layer (dependent on time_scale)
  
  # Set up the dimensions: latlong, time, sectors
  
  v_lon <- as.array(seq(xmin(r_dom_0.1) + 0.01/2, xmax(r_dom_0.1) - 0.01/2, 0.01))
  n_lon <- length(v_lon)
  v_lat <- as.array(seq(ymin(r_dom_0.1) + 0.01/2, ymax(r_dom_0.1) - 0.01/2, 0.01))
  n_lat <- length(v_lat)
  #timevals <- as.numeric(difftime(paste0(y,"-01-01"), dmy("01-01-1850")))
  v_time <- c(14, 45, 73, 104, 134, 165, 195, 226, 257, 287, 318, 348)
  n_time <- length(v_time)
  
  # create dimensions
  dimlon <- ncdim_def(name = "lon", longname= "longitude", units = "degrees_east", vals = v_lon)
  dimlat <- ncdim_def(name = "lat", longname= "latitude", units = "degrees_north", vals = v_lat)
  #dimtime <- ncdim_def(name = "time", units = "days since 1850-01-01 00:00", longname = "days", vals = timevals)
  dimtime <- ncdim_def(name = "time", units = "tonnes month-1", vals = v_time)
  
  
  # Create names and variables for UK, Eire and SEA SEPARATELY
  ## Ireland: ISO:  IE: country 14
  ## UK:      ISO:  GB: country 27
  ## buffer:  ISO: SEA: country 171
  v_sectors_uk   <- paste0("Emis_UK_", dt_sec[,name]) 
  v_sectors_ie   <- paste0("Emis_IE_", dt_sec[,name])
  v_sectors_sea  <- paste0("Emis_SEA_",dt_sec[,name]) 
  
  v_sectors <- c(v_sectors_uk, v_sectors_ie, v_sectors_sea)
  v_country <- c(rep(27, length(v_sectors_uk)), rep(14, length(v_sectors_ie)), rep(171, length(v_sectors_sea)))
  
  # for each sector in the given year, create a new netcdf var
  l_variables <- lapply(X = 1:length(v_sectors), function(s){
    ncvar_def(name = v_sectors[s],
              missval = EMEP_fillval, # _FillValue ?
              longname = str_split(v_sectors[s],"_")[[1]][3], # long_name?
              units = ifelse(time_scale == "year", "tonnes yr-1" , "tonnes month-1"), 
              dim = list(dimlon,dimlat,dimtime), 
              compression = 4,
              prec = "float")})
  
  ## Create the new netcdf
  ncnew <- nc_create(nc_filename, l_variables, force_v4=T)
  
  # now extract the data from the raster Stack and insert
  print(paste0(Sys.time(),":               Inserting data..."))
  
  l <- list()
  
  for(v in 1:length(v_sectors)){
    
    sec_name  <- v_sectors[v]
    sec_desc  <- substr(sec_name, str_locate_all(sec_name,"_")[[1]][2,2] + 1, nchar(sec_name))
    sec_EMEP  <- dt_sec[name == sec_desc, str_pad(EMEP_sec, 2, "0", side = "left")]
    sec_GNFR  <- dt_sec[name == sec_desc, GNFRlong]
    
    ISO <- tolower(str_split(sec_name, "_")[[1]][2])
    ISO_num  <- v_country[v]
    
    l_insert <- get(paste0("l_",ISO,"_temp"))
    s_insert <- l_insert[[paste0(ISO,"_",sec_EMEP, "_", sec_desc,"_",time_scale)]]
    
    # extract the year and pollutant and put in
    a <- array(s_insert, dim = c(n_lon, n_lat, n_time))
    a <- a[,1250:1,] # need to reverse the rows, for a reason i have not worked out. 
    
    ncvar_put(ncnew, sec_name, a)
    
    # few extra variable attributes
    ncatt_put(ncnew, varid = sec_name, attname = "long_name"  , attval = sec_desc, prec="char")
    ncatt_put(ncnew, varid = sec_name, attname = "description", attval = sec_desc, prec="char")
    ncatt_put(ncnew, varid = sec_name, attname = "sector"     , attval = sec_EMEP, prec="char")
    ncatt_put(ncnew, varid = sec_name, attname = "species"    , attval = ifelse(species=="pm2.5","pm25",species) , prec="char")
    ncatt_put(ncnew, varid = sec_name, attname = "country"    , attval = ISO_num , prec="int")
    
    # summary of data going into netcdf
    dt <- data.table(Pollutant = species, Year = y, Region = ISO, long_name = sec_desc, EMEP_Sector = sec_EMEP, GNFR = sec_GNFR, Month = 1:12, emis_kt = round(global(s_insert, sum, na.rm=T)[,1]/1000, 3))
    dtw <- dcast(dt, Pollutant+Year+Region+long_name+EMEP_Sector+GNFR ~ Month, value.var = "emis_kt")
    dtw[, tot_kt := rowSums(.SD, na.rm=T), .SDcols = as.character(1:12)]
    
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
  
  ncatt_put(ncnew, 0, "periodicity", ifelse(time_scale=="month","monthly","annual"), prec="char")
  ncatt_put(ncnew, 0, "NCO","netCDF Operators version 4.9.8 (Homepage = http://nco.sf.net, Code = http://github.com/nco/nco)", prec = "char")
  
  
  nc_close(ncnew)
  
  dt_ncdf_summary <- rbindlist(l, use.names = T)
  
  dt_ncdf_summary <- dt_ncdf_summary[dt_emis_summary, on = c("Pollutant","Year","Region","long_name","EMEP_Sector","GNFR")]
  
  fwrite(dt_ncdf_summary, paste0(output_dir,"/",species,"_UKEIRE_",y,"emis_",map_yr,"map_SUMMARY.csv"))
  
  
} # end of function

######################################################################################################



dump <- function(){
  
  
  
  
  nc_filename
  nc <- nc_open(nc_filename)
  ncatt_get(nc, 0)
  nc$dim$time
  nc$dim$lat
  
  names(nc$var)
  var.name <- "Emis_UK_OtherStationaryComb"
  ncatt_get(nc,var.name)
  
  nc_close(nc)
  
  s <- rast(nc_filename, subds="Emis_UK_OtherStationaryComb")
  
  global(s, sum, na.rm=T)
  plot(s[[6]])
  
  
  nc_file <- "C:/FastProcessingSam/EMEP_new_grid/grid_sep22/edgar_HTAPv3_2018_NOx.nc"
  
  nc <- nc_open(nc_file)
  ncatt_get(nc, 0)
  nc$dim$time
  nc$dim$lat
  
  v_yday <- nc$dim$time$vals
  
  var.name <- "HTAPv3_5_1_Road_Transport"
  
  names(nc$var)
  
  # see the attributes of the variable, such as units, number of records etc
  ncatt_get(nc,attributes(nc$var)$names[which(names(nc$var)==var.name)])
  ncatt_get(nc,var.name)
  
  nc_close(nc)
  
  #### Read the actual values into R as gridded raster surfaces
  
  # by using the 'raster' function, we can immediately read in data
  s <- rast(nc_file, lyrs = 1)
  names(s)
  
  s <- rast(nc_file, subds="HTAPv3_5_1_Road_Transport")
  
  
  
  
  
  
  
  for(sec.code in sectors.to.input){
    
    ### 17/11/20 : EPRTR NOT USED in EIRE ANYMORE, ONLY EMEP
    # UK: FOR SECTOR: A_PublicPower/SNAP01, ONLY use power station point data
    # UK: FOR SECTOR: B_Industry/SNAP03, TAKE A_PublicPower/SNAP01 diffuse surface plus non-power station point data
    # FOR all else, use specific surface and point data (eire is only EMEP)
    
    if(sec.code %in% c("A_PublicPower","S1")){
      
      # FOR POWER; UK pts only, EIRE EMEP surface
      
      # Diffuse
      diff.sec.uk <- uk.latlon.grid
      
      if(class == "GNFR"){
        diff.sec.ie <- raster(paste0("./Emissions_grids_plain/LL/",species,"/diffuse/",year,"/rasters_",class,"/",species,"_diff_",year,"_eire_",class,"_A_PublicPower_t_",res.crs,"_",mapping.yr,"NAEImap.tif"))
        
      }else{
        diff.sec.ie <- raster(paste0("./Emissions_grids_plain/LL/",species,"/diffuse/",year,"/rasters_",class,"/",species,"_diff_",year,"_eire_",class,"_S1_t_",res.crs,"_",mapping.yr,"NAEImap.tif"))
      }
      
      
      # Points
      # Point data - needs subsetting to SNAP (UK) and GNFR (Eire) and rasterizing
      pts <- fread(paste0("./Emissions_grids_plain/LL/",species,"/point/",year,"/",species,"_pt_",year,"_",region,"_",class,"_t_LL.csv"))
      
      
      # subset points
      if(class == "GNFR"){
        pts.sub.uk <- pts[get(class) == "A_PublicPower" & pow.flag == 1 & AREA == "UK"]
        #pts.sub.ie <- pts[get(class) == "A_PublicPower" & pow.flag == 1 & AREA == "EIRE"]
        
      }else{
        pts.sub.uk <- pts[get(class) == str_replace(sec.code,"S","") & pow.flag == 1 & AREA == "UK"]
        #pts.sub.ie <- pts[get(class) == str_replace(sec.code,"S","") & pow.flag == 1 & AREA == "EIRE"]
      }
      
      # if there are no points, use a blank raster, otherwise rasterize
      if(nrow(pts.sub.uk) == 0){
        
        pt.sec.uk <- uk.latlon.grid
        
      }else{
        
        pt.sec.uk <- rasterize(x = pts.sub.uk[,1:2], y = uk.latlon.grid, field = pts.sub.uk[,Emission], fun = 'sum', background=NA)
      } # end of ifelse for UK points
      
      pt.sec.ie <- uk.latlon.grid
      
      #if(nrow(pts.sub.ie) == 0){
      #  
      #  pt.sec.ie <- uk.latlon.grid
      #  
      #}else{
      #  
      #  pt.sec.ie <- rasterize(x = pts.sub.ie[,1:2], y = uk.latlon.grid, field = pts.sub.ie[,Emission], fun = 'sum', background=NA)
      #} # end of ifelse for ie points
      
      ## END OF POWER IF CLAUSE ##
      
      
    } else if (sec.code %in% c("B_Industry","S3")){
      
      # UK: Diffuse data for Industry AND power
      # EIRE:  Diffuse data for Industry
      if(class == "GNFR"){
        
        diff.power.uk <- raster(paste0("./Emissions_grids_plain/LL/",species,"/diffuse/",year,"/rasters_",class,"/",species,"_diff_",year,"_uk_",class,"_A_PublicPower_t_",res.crs,"_",mapping.yr,"NAEImap.tif"))
        
      }else{
        
        diff.power.uk <- raster(paste0("./Emissions_grids_plain/LL/",species,"/diffuse/",year,"/rasters_",class,"/",species,"_diff_",year,"_uk_",class,"_S1_t_",res.crs,"_",mapping.yr,"NAEImap.tif"))
        
      }
      
      if(class == "GNFR"){
        
        diff.industry.uk <- raster(paste0("./Emissions_grids_plain/LL/",species,"/diffuse/",year,"/rasters_",class,"/",species,"_diff_",year,"_uk_",class,"_B_Industry_t_",res.crs,"_",mapping.yr,"NAEImap.tif"))
        diff.industry.ie <- raster(paste0("./Emissions_grids_plain/LL/",species,"/diffuse/",year,"/rasters_",class,"/",species,"_diff_",year,"_eire_",class,"_B_Industry_t_",res.crs,"_",mapping.yr,"NAEImap.tif"))
        
      }else{
        
        diff.industry.uk <- raster(paste0("./Emissions_grids_plain/LL/",species,"/diffuse/",year,"/rasters_",class,"/",species,"_diff_",year,"_uk_",class,"_S3_t_",res.crs,"_",mapping.yr,"NAEImap.tif"))
        diff.industry.ie <- raster(paste0("./Emissions_grids_plain/LL/",species,"/diffuse/",year,"/rasters_",class,"/",species,"_diff_",year,"_eire_",class,"_S3_t_",res.crs,"_",mapping.yr,"NAEImap.tif"))
        
      }
      
      diff.sec.uk <- calc(stack(diff.power.uk, diff.industry.uk), sum, na.rm=T)
      diff.sec.ie <- diff.industry.ie
      
      # Point data - needs subsetting to SNAP (UK) and GNFR (Eire) and rasterizing
      pts <- fread(paste0("./Emissions_grids_plain/LL/",species,"/point/",year,"/",species,"_pt_",year,"_",region,"_",class,"_t_LL.csv"))
      
      # subset points
      if(class == "GNFR"){
        
        pts.sub.uk <- pts[get(class) == "B_Industry" | get(class) == "A_PublicPower"]
        pts.sub.uk <- pts.sub.uk[AREA == "UK"]
        pts.sub.uk <- pts.sub.uk[is.na(pow.flag)]
        #pts.sub.ie <- pts[(get(class) == "B_Industry" | get(class) == "A_PublicPower")]
        #pts.sub.ie <- pts.sub.ie[AREA == "EIRE"]
        #pts.sub.ie <- pts.sub.ie[is.na(pow.flag)]
        
      }else{
        
        pts.sub.uk <- pts[get(class) == "1" | get(class) == "3"]
        pts.sub.uk <- pts.sub.uk[AREA == "UK"]
        pts.sub.uk <- pts.sub.uk[is.na(pow.flag)]
        #pts.sub.ie <- pts[get(class) == "1" | get(class) == "3"]
        #pts.sub.ie <- pts.sub.ie[AREA == "EIRE"]
        #pts.sub.ie <- pts.sub.ie[is.na(pow.flag)]
        
      }
      
      
      # if there are no points, use a blank raster, otherwise rasterize
      if(nrow(pts.sub.uk) == 0){
        
        pt.sec.uk <- uk.latlon.grid
        
      }else{
        
        pt.sec.uk <- rasterize(x = pts.sub.uk[,1:2], y = uk.latlon.grid, field = pts.sub.uk[,Emission], fun = 'sum', background=NA)
        
      } # end of ifelse for points
      
      pt.sec.ie <- uk.latlon.grid
      
      #if(nrow(pts.sub.ie) == 0){
      #  
      #  pt.sec.ie <- uk.latlon.grid
      #  
      #}else{
      #  
      #  pt.sec.ie <- rasterize(x = pts.sub.ie[,1:2], y = uk.latlon.grid, field = pts.sub.ie[,Emission], fun = 'sum', background=NA)
      #  
      #} # end of ifelse for points
      
      
      ## END OF INDUSTRY IF CLAUSE ##
      
      
    }else{
      
      ## ANY OTHER SECTOR
      
      # Diffuse data
      diff.sec.uk <- raster(paste0("./Emissions_grids_plain/LL/",species,"/diffuse/",year,"/rasters_",class,"/",species,"_diff_",year,"_uk_",class,"_",sec.code,"_t_",res.crs,"_",mapping.yr,"NAEImap.tif"))
      diff.sec.ie <- raster(paste0("./Emissions_grids_plain/LL/",species,"/diffuse/",year,"/rasters_",class,"/",species,"_diff_",year,"_eire_",class,"_",sec.code,"_t_",res.crs,"_",mapping.yr,"NAEImap.tif"))
      
      # Point data - needs subsetting to SNAP (UK) and GNFR (Eire) and rasterizing
      pts <- fread(paste0("./Emissions_grids_plain/LL/",species,"/point/",year,"/",species,"_pt_",year,"_",region,"_",class,"_t_LL.csv"))
      
      # subset points
      if(class == "GNFR"){
        pts.sub.uk <- pts[get(class) == sec.code & AREA == "UK"]
        #pts.sub.ie <- pts[get(class) == sec.code & AREA == "EIRE"]
        
      }else{
        pts.sub.uk <- pts[get(class) == str_replace(sec.code,"S","") & AREA == "UK"]
        #pts.sub.ie <- pts[get(class) == str_replace(sec.code,"S","") & AREA == "EIRE"]
      }
      
      
      # if there are no points, use a blank raster, otherwise rasterize
      if(nrow(pts.sub.uk) == 0){
        
        pt.sec.uk <- uk.latlon.grid
        
      }else{
        
        pt.sec.uk <- rasterize(x = pts.sub.uk[,1:2], y = uk.latlon.grid, field = pts.sub.uk[,Emission], fun = 'sum', background=NA)
        
      } # end of ifelse for points
      
      pt.sec.ie <- uk.latlon.grid
      
      #if(nrow(pts.sub.ie) == 0){
      #  
      #  pt.sec.ie <- uk.latlon.grid
      #  
      #}else{
      #  
      #  pt.sec.ie <- rasterize(x = pts.sub.ie[,1:2], y = uk.latlon.grid, field = pts.sub.ie[,Emission], fun = 'sum', background=NA)
      #  
      #} # end of ifelse for points
      
      ## END OF GENERAL/OTHER SECTOR IF CLAUSE ##
      
      
    }
    
    
  
  }
}

