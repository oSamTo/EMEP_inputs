
r1 <- raster(xmn=0,xmx=100,ymn=0,ymx=100,res=1)
r1[] <- sample(1:5,100^2,replace = T)
r2 <- raster(xmn=0,xmx=100,ymn=0,ymx=100,res=1)
r2[] <- sample(1:5,100^2,replace = T)
r3 <- raster(xmn=0,xmx=100,ymn=0,ymx=100,res=1)
r3[] <- sample(1:5,100^2,replace = T)
r4 <- raster(xmn=0,xmx=100,ymn=0,ymx=100,res=1)
r4[] <- sample(1:5,100^2,replace = T)

st1 <- stack(r1,r2,r3,r4,r4,r3,r2,r3,r4,r1,r1,r4)
names(st1) <- paste0("S",1:12)
st2 <- stack(r2,r4,r2,r1,r4,r1,r4,r2,r3,r4,r1,r2)
names(st2) <- paste0("S",1:12)

stlist <- list(st1,st2)
names(stlist) <- c("uk.d.GNFR","eire.d.SNAP")

year <- 2016
otheryear <- 2017

setwd("C:/FastProcessingSam/dump/parallel_ras")

require(parallel)

cl <- makeCluster(2)
z <- clusterEvalQ(cl, c(library("raster"), library("stringr")))
clusterExport(cl, c("stlist","year","otheryear"))

p <- parLapply(cl = cl, X = names(stlist), fun = function(i) sapply(names(stlist[[i]]), FUN = function(x) writeRaster(stlist[[i]][[x]], paste0("nox_",year,"_",str_split(i,"\\.")[[1]][3],"_",x,"_",otheryear,".tif"), overwrite=T)) )

stopCluster(cl)



##########

reproj.cats <- names(stlist)
LL.st <- stack()
totals.l <- list()

rel.rasters <- list()
for(n in reproj.cats){
  ras.names <- list.files(paste0("."), pattern = paste0(species,"_",year,"_",str_split(n,"\\.")[[1]][3],"_.*",otheryear,".tif$"), full.names = T) 
  
  rel.rasters[[n]] <- ras.names
  
}


cl <- makeCluster(2)
z <- clusterEvalQ(cl, c(library("raster"), library("stringr")))
clusterExport(cl, c("rel.rasters","BNG"))

bng.rasters <- parLapply(cl = cl, X = names(rel.rasters), fun = function(i){
  result <- list()
  result[["r"]] <- stack(rel.rasters[[i]])
  crs(result[["r"]]) <- BNG
  result[["BNG.tot"]] <- cellStats(result[["r"]], sum)
  result[["r.m2"]] <- result[["r"]] / 10
  return(result)
})

stopCluster(cl)

names(bng.rasters) <- reproj.cats


cl <- makeCluster(2)
z <- clusterEvalQ(cl, c(library("raster"), library("stringr")))
clusterExport(cl, c("reproj.cats","year","otheryear", "species","LL.st","totals.l","rel.rasters", "bng.rasters"))

parLapply(cl = cl, X = names(bng.rasters), fun = function(i) sapply(names(bng.rasters[[i]]$r.m2), FUN = function(x) writeRaster(bng.rasters[[i]]$r.m2[[x]], paste0("./reproj/BNG/",species,"_",year,"_",toupper(str_split(i,"\\.")[[1]][3]),"_",str_split(x,"_")[[1]][4],"_",otheryear,"_TEMPm2.tif"), overwrite=T)) )

stopCluster(cl)

# new list of names of temp rasters
temp.bng.m2 <- list()
for(n in reproj.cats){
  ras.names <- list.files(paste0("./reproj/BNG/"), pattern = paste0(species,"_",year,"_",str_split(n,"\\.")[[1]][3],"_.*",otheryear,"_TEMPm2.tif$"), full.names = T) 
  
  temp.bng.m2[[n]] <- ras.names
  
}

# reproject and write in LL using filenames

cl <- makeCluster(2)
z <- clusterEvalQ(cl, c(library("raster"), library("stringr"), library("gdalUtils")))
clusterExport(cl, c("reproj.cats","year","otheryear", "species","temp.bng.m2","BNG","LL"))

parLapply(cl = cl, X = names(temp.bng.m2), fun = function(i) sapply(1:length(temp.bng.m2[[i]]), FUN = function(x) suppressMessages(gdalwarp(srcfile = temp.bng.m2[[i]][x], dstfile = paste0("./reproj/LL/",species,"_",year,"_",str_split(temp.bng.m2[[i]][x],"_")[[1]][3],"_",str_split(temp.bng.m2[[i]][x],"_")[[1]][4],"_",otheryear,"_TEMPm2LL.tif"), s_srs = BNG, t_srs = LL, r = "cubic", tr = c(0.0002,0.0002), te=c(-7.557, 49.766, -7.555, 49.768), output_Raster=TRUE, overwrite=TRUE,verbose=TRUE)) ))

stopCluster(cl)














#ext <- as(extent(bng.rasters[[1]][[1]][[1]]), "SpatialPolygons")
#crs(ext) <- BNG
#spTransform(ext, LL)

??gwarp


reproject.in.parallel <- function(reproj.cats, species){
  
  rel.rasters <- list.files(paste0("."), pattern = paste0(species,"_",year,"_GNFR_.*",otheryear,".tif$"), full.names = T)
  
  
  
  
}
















