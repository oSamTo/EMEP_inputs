
  
  
  require(ncdf4)
  require(sf)
  require(raster)
  require(fasterize)
  require(data.table)
  require(stringr)
  require(lubridate)
  # Create mask
  nc_dir<-"N:/EMEP/EU_inputs/NetCDF_input/"
  
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




