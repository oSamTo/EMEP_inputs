
#########################################################
#### FUNCTIONS FOR CREATION OF EMEP - EU INPUT FILES ####
#########################################################








######################################################################################################
#### function to fetch gridded emissions for a given year from EMEP website. 

griddedEMEPemissions <- function(pollutant = c("cd","co","hg","nh3","nmvoc","nox","pb","pm25","pm10","pmco","sox"), report_year, emis_year){
  
  pollutant <- match.arg(pollutant)
  
  if(!is.numeric(report_year)) stop ("Year vector is not numeric")
  if(!is.numeric(emis_year))   stop ("Year vector is not numeric")
  
  suppressWarnings(dir.create(paste0("./data/gridded/EMEP/",pollutant), recursive = T))
  
  #####
  
  emep_poll <- dt_poll[ceh_poll == pollutant, emep_poll]
    
  print(paste0(Sys.time(),": GRIDDED ",emep_poll," emissions from EMEP for ",emis_year," (reported in year ",report_year,")"))
  
  # define the URL
  emep_url <- paste0("https://webdab01.umweltbundesamt.at/download/gridding",report_year,"/",emis_year,"/",emep_poll,"_",report_year,"_GRID_",emis_year,".zip")
  
  # ZIP file name
  grid_file_name <- paste0("emep_",pollutant,"_gridded_",report_year,"_",emis_year,".zip")
    
 # download to folder
  download.file(url = emep_url,
                  destfile = paste0("./data/gridded/EMEP/",pollutant,"/",grid_file_name),
                  quiet = T)
  
  # Unzip all the files
  unzip(paste0("./data/gridded/EMEP/",pollutant,"/",grid_file_name), overwrite = T,  exdir = paste0("./data/gridded/EMEP/",pollutant,"/",emep_poll,"_",report_year,"_GRID_",emis_year))
 
  # delete the zip file
  file.remove(paste0("./data/gridded/EMEP/",pollutant,"/",grid_file_name))
  
} # function


######################################################################################################
#### function to take EMEP emissions, make ready to EMEP format and create netCDFs for EU

EMEPinputEU <- function(v_years, v_pollutants, time_scale = "month", emep_inv = 2020, map_yr = 2020, output_dir){
  
  # v_years  = vector, *numeric*, year to process.
  if(!is.numeric(v_years)) stop ("Year vector is not numeric")
  
  # emissions year = *numeric*
  if(!is.numeric(emep_inv)) stop ("Reporting year is not numeric")
  
  # For the years & pollutants, take the EU emissions in csv format and;
  #   i) convert every country/sector into a raster
  #  ii) MASK THE DATA to fit *around* the UK emissions data
  # iii) split into monthly emissions, if needed
  #  iv) create netCDF
  
  emis_loc <- "C:/FastProcessingSam/EMEP_new_grid/EMEP_gridded"
  res_crs <- "0.1_LL"
  
  ## loop through years and pollutants listed and make a netCDF input file for each.
  ## This is to be made with a monthly time attribute
      # for the month, incorporate the EDGAR temporal data 
  
  for(y in v_years){
    
    for(species in v_pollutants){
      
      ######################################################################################
      if(!(species %in% c("nox","so2","nh3", "co", "nmvoc","pm25","pm10")) ) stop ("Species must be in: 
                                            AP:    co, nh3, nox, so2, nmvoc
                                            PM:    pm25, pm10")
      ######################################################################################
      
      print(paste0(Sys.time(),": Creating blank EMEP-UK/EIRE input netCDF for ",species," in ",y,"..."))
      
      # we are using the EMEP csv emissions NOT the netcdf emissions
          # https://www.ceip.at/the-emep-grid/gridded-emissions/nox
      # this is because we need a variable *per country* per sector - the netcdf does not have country IDs
      
      # set up blank stack ready for data of different countries
      l_eu_allSec  <- list()
      
      # sector totals list
      l_sum_allSec <- list()
      
      # sector names to loop through - this is netcdf sector names (sec01 etc.)
      v_sectors <- dt_sec[,unique(sec)]
      
      print(paste0(Sys.time(),":      Collecting and creating all emissions input data..."))
      
      for(i in v_sectors){
        
        if(i == "") next # not interested in currently blank EMEP-named sectors
        if(dt_sec[sec == i, GNFRlong == ""]) next # not interested in blank named GNFR sectors either
        # this isnt the same as UK, which uses blank placeholders, but there are many countries so ignore
        
        print(paste0(Sys.time(),":         ",i))
        
        #######################################################
        #### OBTAIN 12 MONTHS OF DATA FOR EACH GNFR SECTOR ####
        
        ## There is one GNFR per EMEP sector name
        # read in the GNFR csv for EMEP
        # create a raster for every country and every sector
        # fill in missing countries/sectors with blanks
        # there are no EMEP csv files for Intl Ships, Avi Cruise or LULUCF
        
        ####################################
        #### LISTS OF EMISSION SURFACES ####
        
        l_eu <- EMEPsectorEmissions(emis_loc, species, y, i, res_crs, map_yr, emep_inv)
        
        
        ########################
        #### TEMPORAL SPLIT ####
        # if the time_scale is year, the data stays as 1 annual total.
        # if the time_scale is month, the data needs to be split into 12 layers
        # this is either the default (up to 2019) femis files
        # or the newly generated stuff that has come out of DUKEMs
        # the sea layer needs to be split based on the country it came from (i.e. UK or Eire)
        
        l_uk_prof   <- splitAnnual(species = species, time_scale, l_annual = l_uk, i = i, country = "uk")
        l_ie_prof   <- splitAnnual(species = species, time_scale, l_annual = l_ie, i = i, country = "ie")
        
        # create three stacks for UK, Eire and the SEA buffer (annual or monthly)
        s_uk  <- l_uk_prof[["terrestrial"]]
        s_ie  <- l_ie_prof[["terrestrial"]]
        s_sea <- tapp(c(l_uk_prof[["sea"]], l_ie_prof[["sea"]]), index = 1:12, sum, na.rm=T)
        # need an if clause for s_sea in-case it's annual
        
        ###################
        #### COLLATING ####
        # add temporal raster stacks to lists
        
        l_uk_allSec[[paste0("uk_", species, "_", i,"_",time_scale)]]   <- s_uk
        l_ie_allSec[[paste0("ie_", species, "_", i,"_",time_scale)]]   <- s_ie
        l_sea_allSec[[paste0("sea_", species, "_", i,"_",time_scale)]] <- s_sea
        
        ####################
        #### STATISTICS ####
        
        dt_totals <- summariseEmissions(y, species, i,
                                        l_uk = l_uk, s_uk = s_uk,
                                        l_ie = l_ie, s_ie = s_ie,
                                        sea = s_sea)
        
        l_sum_allSec[[paste0("totals_", species, "_", i,"_", time_scale)]] <- dt_totals
        
        
      } # sector loop
      
      ############################################################
      #### CREATE AND POPULATE NETCDF ON POLLUTANT/YEAR BASIS ####
      
      dt_emis_summary <- rbindlist(l_sum_allSec, use.names=T)[order(Region, GNFR)]
      
      print(paste0(Sys.time(),":               Creating and populating netcdf..."))
      
      createNETCDFuk(y, naei_inv, species, map_yr_uk, map_yr_ie, output_dir, time_scale,
                     l_uk_allSec, l_ie_allSec, l_sea_allSec, dt_emis_summary)
      
      
      
      
      
      
      
      
      
      # units = Tg yr-1 = Mt yr-1 = 1,000 kt yr-1
      nc_file <- "C:/FastProcessingSam/EMEP_new_grid/EMEP_gridded/NOx_2022_GRID_1990_to_2020.nc"
      nc <- nc_open(nc_file)
      nc
      nc_close(nc)
      
      s <- rast(nc_file, subds = "roadtransport")
      
      r <- s[[31]]
      r <- r * 1000000
      
      
      global(r, sum, na.rm=T)
      
      
      dt_road <- fread("C:/FastProcessingSam/EMEP_new_grid/EMEP_gridded/NOx_2022_GRID_2020/NOx_F_RoadTransport_2022_GRID_2020.txt")
      
      sum(dt_road$EMISSION, na.rm=T)
      
      
      plot(s[[31]])
      
      dt_iso
      
      
      
      
      
      
      
    } # species
    
    
  } # year
  
  
} # end of function
  

######################################################################################################
#### function to collect EMEP sector data, per country, for diffuse emissions
EMEPsectorEmissions <- function(emis_loc, species, y, i, res_crs, map_yr, emep_inv){
  
  # v_years  = vector, *numeric*, year to process.
  if(!is.numeric(y)) stop ("Year vector is not numeric")
  
  # set the diffuse filename
  if(i == "sec03"){
    f_diff <- paste0(emis_loc,"/",dt_poll[ceh_poll == species, emep_poll],"_2022_GRID_",y,"/",dt_poll[ceh_poll == species, emep_poll],"_C_OtherStationaryComb_2022_GRID_",y,".txt")
  }else{
    f_diff <- paste0(emis_loc,"/",dt_poll[ceh_poll == species, emep_poll],"_2022_GRID_",y,"/",dt_poll[ceh_poll == species, emep_poll],"_",dt_sec[sec == i, GNFRlong],"_2022_GRID_",y,".txt")
  }
  
  ### for every country present in the data; 
    # read in it's subset
    # rasterise and add to list
  
  
  sf_emep <- st_read("C:/FastProcessingSam/EMEP_new_grid/EMEP_gridded/EMEP_grid_01deg_shp/EMEP_GRID_FULL.shp")
  
  unique(sf_emep$ISO)[!(unique(sf_emep$ISO) %in% unique(dt_iso$EMEP_iso))]
  unique(dt_iso$EMEP_iso)[!(unique(dt_iso$EMEP_iso) %in% unique(sf_emep$ISO))]
  
  dt_emis <- fread(f_diff)
  setnames(dt_emis, c("# Format: ISO2"),c("ISO"))
  unique(dt_emis$ISO) %in% unique(dt_iso$EMEP_iso)
  unique(dt_emis$ISO) %in% unique(sf_emep$ISO)
  
  
  for(i in v_sectors){
    
    if(i == "") next # not interested in currently blank EMEP-named sectors
    if(dt_sec[sec == i, GNFRlong == ""]) next # not interested in blank named GNFR sectors either
    
    # set the diffuse filename
    if(i == "sec03"){
      f_diff <- paste0(emis_loc,"/",dt_poll[ceh_poll == species, emep_poll],"_2022_GRID_",y,"/",dt_poll[ceh_poll == species, emep_poll],"_C_OtherStationaryComb_2022_GRID_",y,".txt")
    }else{
      f_diff <- paste0(emis_loc,"/",dt_poll[ceh_poll == species, emep_poll],"_2022_GRID_",y,"/",dt_poll[ceh_poll == species, emep_poll],"_",dt_sec[sec == i, GNFRlong],"_2022_GRID_",y,".txt")
    }
    
    dt_emis <- fread(f_diff)
    setnames(dt_emis, c("# Format: ISO2"),c("ISO"))
    
    print(paste0(length(unique(dt_emis$ISO)), " unique ISOs in ",dt_sec[sec == i, GNFRlong]))
    
    
  }
  
  ## ALL ISOs in csv are in the iso lookup table
  ## but not all ISOs in csv are in the shapefile
  
  ### in csv, not in shapefile
  # AST - Asian areas in the extended EMEP domain (ASM+ASE+ARO+ARE+CAS) [these 5 at the end all exist]
  # KZT - Kazakhstan [however there is KZ & KZE in shapefile]
  # RUE - Russian Federation in the extended EMEP domain (RU+RFE+RUX) [these 3 at the end all exist]
  # UZ - Uzbekistan [however there is UZO & UZE in shapefile]
  
  dt_emis[ISO == "RUE"]
  
  if(file.exists(f_diff)){
    
    r_diff <- crop(extend(rast(f_diff), r_dom_0.1), r_dom_0.1)
    
  }else{
    
    r_diff <- r_dom_0.1
    
  } # end of read diffuse
  
  ### read point data ###
  if(file.exists(f_pt)){
    
    # get the table of points first and assess if any entries at all
    dt_pts <- fread(f_pt)[GNFR == dt_sec[sec == i, GNFRlong] & AREA == toupper(country)]
    
    # if there are no points, use a blank raster, otherwise rasterize
    if(nrow(dt_pts) == 0){
      
      r_pt <- r_dom_0.1
      
    }else{
      
      v_pt <- vect(dt_pts, geom=c("Easting", "Northing"), crs = "EPSG:4326")
      r_pt <- terra::rasterize(v_pt, r_dom_0.1, field = "pt_emis_t", fun = sum)
      
    } # end of read points
    
  }else{
    
    r_pt <- r_dom_0.1
    
  }
  
  ## combine the surfaces, whether they have data in or not. 
  s <- c(r_diff, r_pt) ; names(s) <- c("diffuse","point")
  r <- app(s, sum, na.rm = T) ; names(r) <- "total"
  
  # EMEP needs zero value, not NA, in emissions
  r[is.na(r)] <- 0
  
  # Mask to the EMEP input restrictions
  r_t   <- mask(r,     r_dom_terr)                 # emissions on UK land territory
  r_t10 <- mask(r,     r_dom_terr_10km)            # emissions on UK land territory + 10km sea buffer
  r_ow  <- mask(r,     r_dom_terr_10km, inverse=T) # emissions outwith UK land + 10km buffer
  r_sea <- mask(r_t10, r_dom_terr, inverse=T)      # emissions only in the 10km sea buffer
  
  l <- list(r, r_t, r_t10, r_ow, r_sea)
  names(l) <- c("total", "terrestrial","terrestrial_10km","outwith_10km","sea")
  
  return(l)
  
} # end of function

######################################################################################################







  
  
  pollutant <- "nox"
  emep_poll <- dt_poll[ceh_poll == pollutant, emep_poll]
  report_year <- 2022
  emis_year <- 2020
  
  print(paste0(Sys.time(),": Creating EMEP-EU input netCDF for ",species," in ",y,"..."))
  
  
  l <- list()
  
  for(j in dt_sec[,GNFRlong]){
    
    print(j)
    
    fname <- paste0("data/gridded/EMEP/",pollutant,"/",emep_poll,"_",report_year,"_GRID_",emis_year,"/",emep_poll,"_",j,"_",report_year,"_GRID_",emis_year,".txt")
    
    
    if(file.exists(fname)){
      dt <- fread(fname)
      v_iso <- unique(dt[,`# Format: ISO2`])
      l[[j]] <- data.table(iso = v_iso)
    }
    
    #dt[, .("EMISSION" = sum(EMISSION)), by = .(LONGITUDE, LATITUDE)]
    
    
  }
  
  dt_iso <- rbindlist(l)
  v_iso <- data.table(EMEP_iso = unique(dt_iso$iso))
  
  fwrite(v_iso, "C:/FastProcessingSam/Git_repos/EMEP_inputs/data/lookups/EMEP_territories.csv")
  
  
  
  fname <- paste0("data/gridded/EMEP/",pollutant,"/",emep_poll,"_",report_year,"_GRID_",emis_year,"/",emep_poll,"_NT_",report_year,"_GRID_",emis_year,".txt")
  
  dt <- fread(fname)
  dt <- dt[, sum(EMISSION, na.rm=T), by = .(LONGITUDE, LATITUDE)]
  r <- rast(dt, type = "xyz")
  r <- crop(extend(r, r_dom_EU), r_dom_EU)
  
  length(unique(dt$`# Format: ISO2`))
  
  writeRaster(r, "C:/FastProcessingSam/dump/EMEP_EU_rasters/nox_2020_EU_emis_tot.tif", overwrite=T)
  
  
  # netcdf is for one year, one pollutant
  # create netcdf first? then populate?
  # need a layer for every country & sector = ~700 variables
  # as files are sectors, loop/apply over sector files
  
  
  
  
  
  for(j in dt_sec[,GNFRlong]){
    
    fname <- paste0("data/gridded/EMEP/",pollutant,"/",emep_poll,"_",report_year,"_GRID_",emis_year,"/",emep_poll,"_",j,"_",report_year,"_GRID_",emis_year,".txt")
    
    dt <- fread(fname)
    
    #dt[, .("EMISSION" = sum(EMISSION)), by = .(LONGITUDE, LATITUDE)]
    
    
  }
  
  require(raster)
  
  nc_file <- "P:/SPEED_Metal/Atmos_inputs/EMEP_inputs/EU_files/NetCDF_2022/co_2010_EU_emis.nc"
  nc <- nc_open(nc_file)
  length(names(nc$var))
  n_names <- names(nc$var)
  nc_close(nc)
  
  r <- rast(nc_file)
  
  r <- brick(nc_file, varname = n_names[146])
  
  r[r==0] <- EMEP_fillval
  
  writeRaster(r, "C:/FastProcessingSam/dump/EMEP_EU_rasters/co_2010_EU_emis_DE_SN03.tif", overwrite=T)
  
  plot(r)
  
  n_names[146]
  
}


######################################################################################################
#### function to create a netCDF and input the data
createNETCDFeu <- function(y, species, report_year, output_dir){
  
  nc_filename <- paste0(output_dir,"/pm25_EUROPE_",y,"emis_0.1.nc")
  
  # if the file already exists, just delete and rewrite
  
  if(file.exists(nc_filename)){
    
    print(paste0("NetCDF already exists for ",species, " in ", y,"; DELETING & REPLACING..."))
    file.remove(nc_filename)
    
  }else{}
  
  # netCDF variables are done on sector name and attributes determine sector, country etc
  # in this file, UK (27), EIRE (14) and SEA (171) remain separate
  # there are 12 month layers, or just one annual layer (dependent on time_scale)
  
  # Set up the dimensions: latlong, time, sectors
  
  v_lon <- as.array(seq(xmin(r_dom_EU) + 0.1/2, xmax(r_dom_EU) - 0.1/2, 0.1))
  n_lon <- length(v_lon)
  v_lat <- as.array(seq(ymin(r_dom_EU) + 0.1/2, ymax(r_dom_EU) - 0.1/2, 0.1))
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






  ################################################################################################
  ################################################################################################
  CREATE.NETCDF<-function(nc_ref_srce,fn,nc_var_names,yr,iso){
    #load netcdf,
    nc<-nc_open(nc_srce)
    #extract dimensions
    lon<-as.vector(ncvar_get(nc,"lon"))
    lat<-as.vector(ncvar_get(nc,"lat"))
    time<-as.vector(ncvar_get(nc,"time"))
    
    #extract the names of the variables in the NetCDF
    #nc_var_names<-unlist(lapply(X=pols,function(x){paste0(x,"_",nc_var_names)}))
    lon_def<-ncdim_def("lon","degrees_east",lon)
    lat_def<-ncdim_def("lat","degrees_north",lat)
    time_def<-ncdim_def("time","days since 1850-01-01 00:00",t_lkup[year == i, time])
    
    iso_var_names <- unlist(lapply(X=iso,function(x){paste0(x,"_",nc_var_names)}))
    
    #Create a blank list (to be populated with the NetCDF variable definitions)
    
    #Loop through old variables and define them (needed to create a new NetCDF)
    vars_ls<-lapply(X=1:length(iso_var_names),function(i){
      var<-iso_var_names[i]
      ncvar_def(name = var,
                units = "tonnes/year",
                dim = list(lon_def,lat_def,time_def),
                compression = 4,
                chunksizes = c(150,130,1),
                longname = var,
                missval = 0,
                prec = "float")})
    
    nc_new<-nc_create(fn,vars=vars_ls,force_v4=T)
    nc_close(nc_new)}
  #########################################
  #########################################
  
  nc_fls<-list.files(nc_dir,pattern=".nc$")
  
  #nc_srce <- paste0(nc_dir,nc_fls[1])
  
  r <- raster(ext = extent(-30,90,30,82), res = c(0.1,0.1),
              crs = crs("+proj=longlat +datum=WGS84 +no_defs"))
  
  
  r_dt <- as.data.table(as.data.frame(r),xy = T,)
  
  sf <- st_read("N:/Useful_datasets/UK_DAs_&_eire/Simplified_Brit_isles_with_loch_neagh.shp")
  
  sf_buff <- sf %>% st_combine %>% st_buffer(.,20000)
  
  sf_buff <- st_transform(x = sf_buff, crs = 4236)
  
  msk <- fasterize(sf = st_as_sf(sf_buff), r)
  
  msk_dt <- as.data.table(as.data.frame(msk, xy = T))
  
  msk_v <- array(flip(msk,2)[],dim=c(ncol(msk),nrow(msk)))
  
  writeRaster(msk,"N:/EMEP/CORINE_2018/Emissions_mask.tif",overwrite=T)
  
  x <- fread("N:/EMEP/EU_inputs/NOx_A_PublicPower_2019_GRID_2017.txt")
  
  iso <- unique(x$`# Format: ISO2`)
  
  iso <- iso[iso != "GB"]
  
  #iso <- iso[1:30]
  
  nc_ref_srce <- paste0(nc_dir,nc_fls[1])
  
  nc <- nc_open(filename = nc_ref_srce)
  
  # time lookup
  t <- ncvar_get(nc, "time")
  
  orgn <- nc$dim$time$units
  
  orgn <- regmatches(orgn,regexpr("\\d{2,4}-\\d{2,4}-\\d{2,4}",orgn))
  
  yrs <- year(as.Date(t,origin=orgn))
  
  t_lkup <- data.table(num = 1:length(t), time = as.vector(t), year = yrs)
  
  # snap lookup
  
  vars <- names(nc$var)
  
  vars <- vars[2:14]
  
  v_lkup <- data.table(Sect = paste0("sec",str_pad(1:length(vars), width = 2, pad = 0)),
                                     Desc = vars)
  
  #nc_var_names<-paste0(toupper(letters[1:13]),"_",vars)
  
  pols <- tolower(regmatches(nc_fls, regexpr("^[^_]+",nc_fls)))
  
  pols <- pols[pols != "pm10"]
  
  pols_rnm <- ifelse(pols == "nmvoc","voc",
                     ifelse(pols=="pm2","pm25",
                            ifelse(pols=="pmcoarse","pmco",pols)))
  
  
  nc_var_names<-unlist(lapply(X=pols_rnm,function(x){paste0(tolower(x),"_",v_lkup$Sect)}))
  
  #for(i in 2001:2017){
  i <- 2017
  
  fn<-paste0("N:/EMEP/EU_inputs/NetCDF_output/",i,"_EU_emission_vAug2020.nc")
  
  CREATE.NETCDF(nc_ref_srce,fn,nc_var_names,yr = i,iso)
  
  ncnew <- nc_open(fn, write=T)

  fls <- list.files("N:/EMEP/EU_inputs/csv/",
                    pattern=paste0(i,".txt$"),
                    recursive = T)
  
  fls<- fls[!grepl("_NT_\\d{4}",fls)]
  
  
  pol_fl <- unique(regmatches(fls, regexpr("^[^_]+", fls)))
  
  
  for(j in pol_fl){
  
  pol <- tolower(j)
 
  pol_rnm <- ifelse(pol == "nmvoc","voc",
                     ifelse(pol =="pm2","pm25",
                            ifelse(pol =="pmcoarse","pmco", pol)))


 sub_fls <- fls[grep(paste0(j,"_(.)+",i),fls)]
 
for(k in v_lkup$Sect){
  
v_nm <- paste0(tolower(pol_rnm),"_",k)

t_dim_pos <- grep(t_lkup[year== i, time],nc$dim$time)

fl_txt <- sub_fls[grep(paste0("_",v_lkup[Sect == k, Desc],"_"),
               sub_fls,
               ignore.case =T)]

csv <- fread(paste0("N:/EMEP/EU_inputs/csv/",fl_txt))

for(l in iso){

tmp <- csv[`# Format: ISO2` == l,.(x=LONGITUDE,y=LATITUDE,z=EMISSION)]

if(nrow(tmp) == 0){
  print(paste0("No values for ",l," ",i))
  v_mskd <- rep(0,624000)}

if(nrow(tmp) != 0){
  #setkey(tmp, "x", "y")

#setkey(msk_dt, "x", "y")
#setkey(tmp, "x", "y")

#tmp <- tmp[msk_dt,]

v_sub <- rasterFromXYZ(tmp[,.(x,y,z)],res = 0.1)

v_sub <- extend(v_sub, msk)

v_sub <- overlay(x = v_sub, y = msk, fun = function(x,y){ ifelse(!is.na(x)& is.na(y),x * 1000000, 0)})

v_mskd <- as.vector(flip(v_sub,2)[])

print(paste0("Updating ",l," ",i," values: ",v_nm))}

  ncvar_put(ncnew, varid = paste0(l,"_",v_nm), vals = v_mskd)
  
  sect <- gsub("_","",regmatches(v_nm, regexpr("\\d+$",v_nm)))
  
  ncatt_put(nc = ncnew, varid = paste0(l,"_",v_nm), 
             attname = "country_ISO", 
            attval = l,
            prec="char")
  
  ncatt_put(nc =  ncnew,
            varid = paste0(l,"_",v_nm),
            attname = "species", 
            attval =  pol_rnm,
            prec="char")
  
  ncatt_put( ncnew, 
             varid = paste0(l,"_",v_nm), 
             attname = "sector", 
             attval =  as.numeric(sect), 
             prec="int")
  }




}
 }
ncatt_put(ncnew,0,"Conventions","CF-1.0", prec="char")
ncatt_put(ncnew,0,"projection","lon lat", prec="char")
grid<-"0.1"
ncatt_put(ncnew,0,"Grid_resolution",grid, prec="char")
r_version<-R.Version()$version.string
ncatt_put(ncnew,0,"Created_with",r_version, prec="char")
ncdf_ver<-packageDescription("ncdf4")$Version
ncatt_put(ncnew,0,"ncdf4_version",ncdf_ver,prec="char")
ncatt_put(ncnew,0,"Created_by","Ed Carnell edcarn@ceh.ac.uk", prec="char")
cr_time<-as.character(Sys.time())
ncatt_put(ncnew,0,"Created_date",cr_time, prec="char")
ncatt_put(ncnew,0,"Sector_names","GNFR",prec="char")
zr<-lapply(X = v_lkup$Sect,function(x){
  ncatt_put(ncnew,0,x,v_lkup[Sect == x, Desc],prec="char")
})
ncatt_put(ncnew,0,"periodicity","yearly",prec="char")
ncatt_put(ncnew,0,"NCO","netCDF Operators version 4.9.3-alpha03 (Homepage = http://nco.sf.net, Code = http://github.com/nco/nco)",prec="char")

nc_close(ncnew)
}

x<-raster(fn,varname="IT_pm25_sec11")




