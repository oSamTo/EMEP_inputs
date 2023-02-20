


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
CREATE.NETCDF<-function(nc_ref_srce,fn,nc_var_names,yr){
  #load netcdf,
  nc<-nc_open(nc_srce)
  #extract dimensions
  lon<-as.vector(ncvar_get(nc,"lon"))
  lat<-as.vector(ncvar_get(nc,"lat"))
  time<-as.vector(ncvar_get(nc,"time"))
  
  #extract the names of the variables in the NetCDF
  #nc_var_names<-unlist(lapply(X=pols,function(x){paste0(x,"_",nc_var_names)}))
  lon_def<-ncdim_def("lon","degrees",lon)
  lat_def<-ncdim_def("lat","degrees",lat)
  time_def<-ncdim_def("time","days since 1850-01-01 00:00",t_lkup[year == i, time])
  
  #Create a blank list (to be populated with the NetCDF variable definitions)
  
  #Loop through old variables and define them (needed to create a new NetCDF)
  vars_ls<-lapply(X=1:length(nc_var_names),function(i){
    var<-nc_var_names[i]
    ncvar_def(name=var,units="Mg/year",dim=list(lon_def,lat_def,time_def),compression=4,chunksizes=c(150,130,1),longname=var,missval =9.96920996838687,prec="float")})
  
  nc_new<-nc_create(fn,vars=vars_ls,force_v4=T)
  nc_close(nc_new)}
#########################################
#########################################

nc_fls<-list.files(nc_dir,pattern=".nc$")

nc_srce <- paste0(nc_dir,nc_fls[1])

r <- raster(x = nc_srce, varname = "publicpower", band = 1)

sf <- st_read("N:/Useful_datasets/UK_DAs_&_eire/Simplified_Brit_isles_with_loch_neagh.shp")

sf_buff <- sf %>% st_combine %>% st_buffer(.,20000)

sf_buff <- st_transform(x = sf_buff, crs = 4236)

msk <- fasterize(sf = st_as_sf(sf_buff), r)

msk_v <- array(flip(msk,2)[],dim=c(ncol(msk),nrow(msk)))

writeRaster(msk,"N:/EMEP/CORINE_2018/Emissions_mask.tif",overwrite=T)




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

pols <- regmatches(nc_fls, regexpr("^[^_]+",nc_fls))

nc_var_names<-unlist(lapply(X=pols,function(x){paste0(tolower(x),"_",v_lkup$Sect)}))

for(i in 2001:2017){

fn<-paste0("N:/EMEP/EU_inputs/NetCDF_output/",i,"_EU_emission.nc")

CREATE.NETCDF(nc_ref_srce,fn,nc_var_names,i)

ncnew <- nc_open(fn, write=T)

for(j in nc_fls){

pol <- regmatches(j, regexpr("^[^_]+",j))

nc <- nc_open(filename = paste0(nc_dir,j))

nc_nms<-names(nc$var)[ ! names(nc$var) %in% c("sumallsectors","Latitude-Longitude")]

for(k in nc_nms){
  
v_nm <- paste0(tolower(pol),"_",v_lkup[Desc == k, Sect])

t_dim_pos <- grep(t_lkup[year== i, time],nc$dim$time)

v <- ncvar_get(nc, k, start=c(1,1,t_dim_pos), count = c(-1,-1,1))

v_mskd <- as.vector(ifelse(is.na(msk_v),v,0))*1000000

print(paste0("Updating ",i," values: ",v_nm))

ncvar_put(ncnew, varid = v_nm, vals = v_mskd)

}}
ncatt_put(ncnew,0,"Conventions","CF-1.0", prec="char")
ncatt_put(ncnew,0,"projection","lon lat", prec="char")
grid<-"0.1"
ncatt_put(ncnew,0,"Grid_resolution",grid, prec="char")
r_version<-R.Version()$version.string
ncatt_put(ncnew,0,"Created_with",r_version, prec="char")
ncdf_ver<-packageDescription("ncdf4")$Version
ncatt_put(ncnew,0,"ncdf4_version",ncdf_ver,prec="char")
ncatt_put(ncnew,0,"Created_by","Ed Carnell edcarn@ceh.ac.uk", prec="char")
time<-as.character(Sys.time())
ncatt_put(ncnew,0,"Created_date",time, prec="char")
ncatt_put(ncnew,0,"Sector_names","GNFR",prec="char")
zr<-lapply(X = v_lkup$Sect,function(x){
  ncatt_put(ncnew,0,x,v_lkup[Sect == x, Desc],prec="char")
})
     
nc_close(ncnew)}

x<-raster(fn,varname="NH3_L_agriother")




