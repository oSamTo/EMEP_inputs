
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
#### function to 


EMEPinputEU <- function(v_emis_year, v_pollutants, report_year, output_dir){
  
  # create a blank netcdf
  # process the data
  # input the data into the ncdf
  
  # v_years  = vector, *numeric*, year to process.
  if(!is.numeric(v_years)) stop ("Year vector is not numeric")
  
  # emissions year = *numeric*
  if(!is.numeric(report_year)) stop ("Reporting year is not numeric")
  if(!is.numeric(emis_year)) stop ("Emissions year is not numeric")
  
  ## method outline here
  
  res_crs <- "0.1_LL"
  
  ## loop through years and pollutants listed and make a netCDF input file for each.
  
  for(y in v_emis_year){
    
    for(species in v_pollutants){
      
      ######################################################################################
      if(!(species %in% c("nox","so2","nh3", "co", "nmvoc","pm25","pm10")) ) stop ("Species must be in: 
                                            AP:    co, nh3, nox, so2, nmvoc
                                            PM:    pm25, pm10")
      ######################################################################################
      
      print(paste0(Sys.time(),": Creating blank EMEP-UK/EIRE input netCDF for ",species," in ",y,"..."))
      
      
      dt_iso
      
      
      
    } # species
    
    
  } # year
  
  
  
  
  
  
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




