######################################################################################################
#### master function to generate QAQC
create_qaqc <- function(y, species, uk_folname, map_yr_uk, 
                        naei_inv, emep_inv, time_dim){

  print(paste0(Sys.time(),": Creating QAQC for ",dt_poll[ceh_poll == species, emep_model]," in ",y,"..."))

  # folders needed
  dir.create(file.path(uk_folname, "plots"), showWarnings = FALSE, recursive = T)  
  dir.create(file.path(uk_folname, "qaqc"), showWarnings = FALSE, recursive = T)  
  
  eu_folname <- copy(uk_folname)
  eu_folname <- gsub("UKEIRE","EU",eu_folname)
  eu_folname <- gsub("TPukem_genYr_AGGNA","TPEMEP4UKv5.0_AGGoneEU",eu_folname)
  
  ##################################
  #### Pre extract data to plot ####
  ##################################
  # do not do the pre-generated summary tables from the main inputs run. 
  
  
  
  
  # uk total
  # uk sectoral annual
  # uk month total
  
  # same for EU
  
  
  ############
  #### UK ####
  ############
  
  # 1. Map; UK emissions, total cell-1 annum-1.
  l_gg_p1 <- plot_uk_map_ann(y, species, uk_folname, eu_folname, map_yr_uk,
                       naei_inv, emep_inv, time_dim)
  gg_p1 <- l_gg_p1[["uk_plot"]]
    
  # 2. Bar plot; UK/IE emissions processing check-plot
  l_gg_p2 <- plot_uk_emis(y, species, uk_folname, map_yr_uk, naei_inv)
  gg_p2 <- l_gg_p2[["plot"]]
  dt_p2 <- l_gg_p2[["table"]]
  
  # 3. Bar plot; UK/IE emissions going into netCDF and those inside the netCDF
  gg_p3 <- plot_uk_nc(y, species, uk_folname, map_yr_uk, naei_inv, dt_emis = dt_p2)
  
  # 4. Line plot; UK/IE emissions of total by month in netcdf
  gg_p4 <- plot_uk_tot_mon(y, species, uk_folname, map_yr_uk, naei_inv, time_dim)
  
  # 5. Line plot; UK/IEemissions of total by sector by month in netcdf
  gg_p5 <- plot_uk_mon_sec(y, species, uk_folname, map_yr_uk, naei_inv, time_dim)

  # 6. Maps; UK/IE (& EU) emissions, sector total cell-1 annum-1.
  gg_p6 <- plot_uk_sec_ann(y, species, uk_folname, map_yr_uk, naei_inv, emep_inv, 
                           time_dim, sUK = l_gg_p1[["uk_sec_anntot"]], sEU = l_gg_p1[["eu_sec_anntot"]])
  
  # 7. Maps; UK/IE (& EU) emissions, monthly total cell-1 annum-1.
  gg_p7 <- plot_uk_map_mon(y, species, uk_folname, map_yr_uk, naei_inv, emep_inv, 
						   time_dim, sUK = l_gg_p1[["uk_montot"]], sEU = l_gg_p1[["eu_montot"]])
    
  ############
  #### EU ####
  ############
	
  # 8. Map; EU emissions, total cell-1 annum-1.
  l_gg_p8 <- plot_eu_map_ann(y, species, uk_folname, eu_folname, map_yr_uk, 
							 naei_inv, emep_inv, time_dim)
  gg_p8 <- l_gg_p8[["EU_plot"]]
   
  # 9. Bar plot; EU emissions from EMEP processed into nc file
  
  
  
  # 10. Line plot; EU emissions of total by month in netcdf
  
  
  # 11. Line plot; EU (country) monthly pollutant by sector (large plot)
  
     
  # 12. Map;  EU emissions, sector total cell-1 annum-1.
  gg_p12 <- plot_eu_sec_ann(y, species, uk_folname, map_yr_uk, naei_inv, 
						   emep_inv, time_dim, sEU = l_gg_p8[["eu_sec_anntot"]])
    
  # 13. Maps; EU emissions, monthly total cell-1 annum-1.
  
  
  
  
  #metadata
  
  
  
  # require(cowplot)
 # cp <- plot_grid(gg_p2, gg_p3, ncol = 1, align = "hv", axis = 'tblr', rel_heights = 1)
  #cp <- plot_grid(p2, p1, rel_widths = c(0.33, 1))
 # fname <- paste0(uk_folname,"/plots/",dt_poll[ceh_poll == species, emep_model],"_UKEIRE_",y,"emis_",map_yr_uk,"map_",naei_inv,"inv_COPW1.png")
 # save_plot(fname, cp, base_height = 15, base_width = 12)
  
}

#######################
#### DATA EXTRACTS ####
#######################

######################################################################################################
#### function to extract UK gridded data
#### Annual total (all), annual total (sectors), monthly totals (all)
extract_uk_map <- function(y, species, uk_folname, map_yr_uk, 
							naei_inv, time_dim){
  
  print(paste0(Sys.time(),":      Summarising emissions maps - UK..."))  
  
  if(time_dim == "annual"){ i_time <- 1 }else if(time_dim == "month"){ i_time <- 1:12 }else{ i_time <- 1:365 }
  
  # set the required ncdf filename for the UK
  nc_fname <- paste0(dt_poll[ceh_poll == species, emep_model],"_UKEIRE_",
                  y,"emis_",map_yr_uk,"map_",naei_inv,"inv_0.01.nc")
  nc_fname <- paste0(uk_folname,"/", nc_fname)
  
  # read all data. Create an annual map of total emissions. 
  # gather sector info direct from input file. Yet another safety check. 
  v_sectors <-  dt_sec[,unique(sec)]
    
  l_sec_mon <- list()
  l_sec_ann <- list()
    
  # loop through every sector and collect data. Summarise where needed. 
  for(i in v_sectors){
    
	if(i == "") next # not interested in currently blank EMEP-named sectors
    	
	# cycle through uk, ie and sea, summarising emissions
	l_temp_mon <- list()
	l_temp_ann <- list()
	
	for(area in c("uk", "ie", "sea")){
	  
	  # sector name in the EMEP file
 	  nc_secname <- paste0("Emis_",toupper(area),"_", dt_sec[sec == i, name] ) 
	  
	  # stack the monthly data, add to monthly temp list
      s <- rast(nc_fname, subds = nc_secname)
	  s <- extend(s, r_dom_ukplot)
	  if(nlyr(s) != max(i_time)) stop("not enough temporal splits in EMEP data")
	  l_temp_mon[[paste0(area,"_",i)]] <- s
	  
      # annual total raster for sector/Area, add to list
      r <- app(s,  sum, na.rm=T)
	  l_temp_ann[[paste0(area,"_",i)]] <- r  
	
	} # area
	  
	# stack to month totals
	s_sec_mon <- rast(l_temp_mon)
	s_sec_mon <- tapp(s_sec_mon, i_time, sum, na.rm=T)
	l_sec_mon[[dt_sec[sec == i, name]]] <- s_sec_mon  
	  
	# stack to annual sector totals  
	s_sec_ann <- rast(l_temp_ann)
	r_sec_ann <- app(s_sec_ann, sum, na.rm=T)	
	l_sec_ann[[dt_sec[sec == i, name]]] <- r_sec_ann  
	  
  } # sector
    
  # stack of monthly pollutant totals
  sUK_sec_montot <- rast(l_sec_mon)
  sUK_all_montot <- tapp(sUK_sec_montot, i_time, sum, na.rm=T)
  names(sUK_all_montot) <- i_time
  
  # stack of all sector annual totals
  sUK_sec_anntot <- rast(l_sec_ann)
  
  # sum to annual total of everything. 
  rUK <- app(sUK_sec_anntot, sum, na.rm=T)
  rUK[rUK==0] <- NA
  names(rUK) <- paste0(species, "_UKEIRE_", y,"_emis_",naei_inv,"_inv")
  

  return(list("uk_ann_tot_all" = rUK, "uk_ann_tot_sec" = sUK_sec_anntot, "uk_mon_tot_all" = sUK_all_montot ))
}

######################################################################################################
#### function to



######################################################################################################
#### function to




##################
#### UK PLOTS ####
##################

######################################################################################################
#### 1. function to plot the UK mapped data, total for the species/year (map).
plot_uk_map_ann <- function(y, species, uk_folname, eu_folname, map_yr_uk, 
							naei_inv, emep_inv, time_dim){
  
  print(paste0(Sys.time(),":      Annual total emissions map - UK..."))  
  
  ## map of total pollutant ##
    
  if(time_dim == "annual"){ i_time <- 1 }else if(time_dim == "month"){ i_time <- 1:12 }else{ i_time <- 1:365 }

  ## UK ##
  # set the required ncdf filename for the UK
  nc_fname <- paste0(dt_poll[ceh_poll == species, emep_model],"_UKEIRE_",
                  y,"emis_",map_yr_uk,"map_",naei_inv,"inv_0.01.nc")
  nc_fname <- paste0(uk_folname,"/", nc_fname)
  
  # read all data. Create an annual map of total emissions. 
  # gather sector info direct from input file. Yet another safety check. 
  v_sectors <-  dt_sec[,unique(sec)]
    
  l_sec_mon <- list()
  l_sec_ann <- list()
    
  # loop through every sector and collect data. Summarise where needed. 
  for(i in v_sectors){
    
	if(i == "") next # not interested in currently blank EMEP-named sectors
    	
	# cycle through uk, ie and sea, summarising emissions
	l_temp_mon <- list()
	l_temp_ann <- list()
	
	for(area in c("uk", "ie", "sea")){
	  
	  # sector name in the EMEP file
 	  nc_secname <- paste0("Emis_",toupper(area),"_", dt_sec[sec == i, name] ) 
	  
	  # stack the monthly data, add to monthly temp list
      s <- rast(nc_fname, subds = nc_secname)
	  s <- extend(s, r_dom_ukplot)
	  if(nlyr(s) != max(i_time)) stop("not enough temporal splits in EMEP data")
	  l_temp_mon[[paste0(area,"_",i)]] <- s
	  
      # annual total raster for sector/Area, add to list
      r <- app(s,  sum, na.rm=T)
	  l_temp_ann[[paste0(area,"_",i)]] <- r  
	
	} # area
	  
	# stack to month totals
	s_sec_mon <- rast(l_temp_mon)
	s_sec_mon <- tapp(s_sec_mon, i_time, sum, na.rm=T)
	l_sec_mon[[dt_sec[sec == i, name]]] <- s_sec_mon  
	  
	# stack to annual sector totals  
	s_sec_ann <- rast(l_temp_ann)
	r_sec_ann <- app(s_sec_ann, sum, na.rm=T)	
	l_sec_ann[[dt_sec[sec == i, name]]] <- r_sec_ann  
	  
  } # sector
    
  # stack of monthly pollutant totals
  sUK_sec_montot <- rast(l_sec_mon)
  sUK_sec_montot <- tapp(sUK_sec_montot, i_time, sum, na.rm=T)
  names(sUK_sec_montot) <- i_time
  
  # stack of all sector annual totals
  sUK_sec_anntot <- rast(l_sec_ann)
  
  # sum to annual total of everything. 
  rUK <- app(sUK_sec_anntot, sum, na.rm=T)
  rUK[rUK==0] <- NA
  names(rUK) <- paste0(species, "_UKEIRE_", y,"_emis_",naei_inv,"_inv")
  
  ## EU ##
  # set the required ncdf filename for the EU.
  # we need to change the EU tp_scheme to "EMEP4UKv5.0
  # it doesn't matter which we pick: 
	# it's for background plotting
    # this annual total is the same as other TPs. 
    # this is a UK QAQC file, not EU
  
  nc_fname <- paste0(dt_poll[ceh_poll == species, emep_model],"_EU_",
                  y,"emis_",map_yr_uk,"map_",naei_inv,"inv_0.1.nc")
  nc_fname <- paste0(eu_folname,"/", nc_fname)
  
  # read all data. Create an annual map of total emissions. 
  # this is for context and background to the UK map.   
  v_sectors <-  dt_sec[,unique(sec)]
    
  l_sec_mon <- list()
  l_sec_ann <- list()
    
  # loop through every sector and collect data. Summarise where needed. 
  for(i in v_sectors){
    
	if(i == "") next # not interested in currently blank EMEP-named sectors
    if(dt_sec[sec == i, GNFRlong]  == "" ) next # none of these in EU file	
	
	# sector name in the EMEP file
 	nc_secname <- paste0("Emis_EUR_", dt_sec[sec == i, name] ) 
	  
	# stack the monthly data, add to monthly temp list
    s_sec_mon <- rast(nc_fname, subds = nc_secname)
	s_sec_mon <- crop(s_sec_mon, ext(rUK))
	if(nlyr(s_sec_mon) != max(i_time)) stop("not enough temporal splits in EMEP data")
	
	# month totals
	l_sec_mon[[dt_sec[sec == i, name]]] <- s_sec_mon  
	  
	# stack to annual sector totals  
	r_sec_ann <- app(s_sec_mon, sum, na.rm=T)	
	l_sec_ann[[dt_sec[sec == i, name]]] <- r_sec_ann  
	  
  } # sector
      
  # stack of monthly pollutant totals
  sEU_sec_montot <- rast(l_sec_mon)
  sEU_sec_montot <- tapp(sEU_sec_montot, i_time, sum, na.rm=T)
  names(sEU_sec_montot) <- i_time
  
  # stack of all sector annual totals
  sEU_sec_anntot <- rast(l_sec_ann)
  
  # sum to annual total of everything. 
  rEU <- app(sEU_sec_anntot, sum, na.rm=T)
  rEU[rEU==0] <- NA
    
  # disaggregate the EU map to 0.01 degree
  rEU <- disagg(rEU, 10)
  rEU <- rEU/100
  names(rEU) <- paste0(species, "_EU_", y,"_emis_", emep_inv,"_inv")

  ## reclassify and plot ##  
  # exract all the values present, into a vector. 
  v_v <- values(c(rUK, rEU))
  v_v <- as.vector(v_v)
    
  # create break points based on quantiles
  v_q <- as.vector(quantile(v_v, probs = c(0.20,0.40,0.55,0.7,0.82,0.92,0.97,1), na.rm=T))  
  
  # create a reclassification matrix and labels for the plot.
  leg.pos <- "bottom"
  m <- matrix(c(0, v_q[1:7], v_q[1:7], ceiling(max(global(c(rUK, rEU), max, na.rm=T)$max, na.rm=T)), 1:8), ncol = 3) 
  brew_d = -1
  v_labs <- unlist(lapply(1:nrow(m), function(x) paste0(round(m[x,1],2), " - ", round(m[x,2], 2))))
  v_labs[length(v_labs)] <- paste0("> ",round(m[nrow(m),1],2))
  
  # reclassify the data
  rUK_rc <- terra::classify(rUK, m)
  rUK_rc <- as.factor(rUK_rc)
  rEU_rc <- terra::classify(rEU, m)
  rEU_rc <- as.factor(rEU_rc)
    
  # plot and save
  p <- ggplot()+
    geom_spatraster(data = rUK_rc, na.rm = T)+
	geom_spatraster(data = rEU_rc, na.rm = T, alpha = 0.6)+
	scale_fill_brewer(labels = v_labs, palette = "Spectral", breaks = 1:length(v_q), direction = brew_d)+
	scale_y_continuous(expand = c(0, 0)) +
	scale_x_continuous(expand = c(0, 0)) +
	labs(fill = bquote(tonnes~a^-1), y = "", x = "")+
	# ggtitle(bquote("Emissions of"~.(p)~a^-1))+
	geom_sf(data = sf_uk, fill = NA, colour = "black")+
	#facet_wrap(~lyr, nrow = 1)+
	#scale_fill_hypso_d(labels = v_labs, na.value = "transparent", palette = "etopo1")+
	theme_bw()+
	  {if(leg.pos == "bottom")guides(fill = guide_legend(nrow = 2))}+
	  theme(#plot.title = element_text(size = 30, face = "bold"),
	  axis.text = element_text(size=12),
	  legend.text = element_text(size=18),
	  legend.title = element_text(size=24),
	  legend.position = leg.pos,
	  #plot.margin = grid::unit(c(2,2,2,2), "mm"),
	  margin(t = 2, r = 2, b = 2, l = 2, unit = "mm"))
  
  fname <- paste0(uk_folname,"/plots/",dt_poll[ceh_poll == species, emep_model],"_UKEIRE_",y,"emis_",map_yr_uk,"map_",naei_inv,"inv_1_UKTOTMAP.png")
  
  ggsave(fname, p, width = 10.5, height = 14)
   
  
  return(list("uk_plot" = p, "uk_montot" = sUK_sec_montot, "uk_sec_anntot" = sUK_sec_anntot,
              "eu_montot" = sEU_sec_montot, "eu_sec_anntot" = sEU_sec_anntot ))

}

######################################################################################################
#### 2. function to plot annual emissions summary from inventory (bar).
plot_uk_emis <- function(y, species, uk_folname, map_yr_uk, naei_inv){
  
  print(paste0(Sys.time(),":      Annual processed emissions..."))  
  
  fname_route <- paste0(dt_poll[ceh_poll == species, emep_model],"_UKEIRE_",y,"emis_",map_yr_uk,"map_",naei_inv,"inv")
  
  # read written summaries    
  dt_inv   <- fread(paste0(uk_folname, "/tables/", fname_route, "_INVENTORY.csv"))
  dt_mask  <- fread(paste0(uk_folname, "/tables/", fname_route, "_MASKED.csv"))
  
    
  # plot 1: plot mapped, inventory and scaled totals.  
  dt_inv_t <- dt_inv[, lapply(.SD, sum, na.rm=T), by = .(Area, mask, Pollutant, data_source, emis_y, inv_y), .SDcols = c("emis_t_inv_spatial","emis_t_inv_table","emis_t_spatial_scaled")]
  dt_inv_m <- melt(dt_inv_t, id.vars = c("Area","mask","Pollutant","data_source","emis_y","inv_y"),
				   variable.name = "stage", value.name = "emis_t")
  
  
  dt_mask_t <- dt_mask[, lapply(.SD, sum, na.rm=T), by = .(Area, mask, Pollutant, data_source, emis_y, inv_y), .SDcols = c("emis_t_tot_masked","tsum")]
  setnames(dt_mask_t, "tsum", "mon_masked")
  dt_mask_m <- melt(dt_mask_t, id.vars = c("Area","mask","Pollutant","data_source","emis_y","inv_y"),
				   variable.name = "stage", value.name = "emis_t")
    		
  dt <- rbindlist(list(dt_inv_m, dt_mask_m), use.names = T)  
  dt[, stage := gsub("emis_t_", "", stage) ]
  dt[, stage := factor(stage, levels = c("inv_spatial","inv_table","spatial_scaled","tot_masked","mon_masked"))]
  dt[,mask := factor(mask, levels = c("outwith", "sea", "terrestrial", "all"))]
  
  p <- ggplot(dt, aes(x = stage, y = emis_t/1000, group = mask, fill= mask))+
    geom_bar(stat = "identity")+
	scale_fill_manual(values = c("#eecea6","#59aed3","#6fbe6d","#ea93ea"))+
    labs(y = bquote(kt~a^-1))+
    facet_wrap(~Area, nrow=1, scales = "free_y")+
    theme_bw()+
    theme(strip.text = element_text(size = 20),
          legend.title = element_blank(), 
          legend.position = "bottom",
          legend.text = element_text(size = 16),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size = 14),
		  axis.title.y = element_text(size = 16),
		  margin(t = 2, r = 2, b = 2, l = 2, unit = "mm"))
  
  fname <- paste0(uk_folname,"/plots/",dt_poll[ceh_poll == species, emep_model],"_UKEIRE_",y,"emis_",map_yr_uk,"map_",naei_inv,"inv_2_UKINVBAR.png")
  
  ggsave(fname, p, width = 12, height = 6)
  
  return(list("table" = dt, "plot" = p))
 
}

######################################################################################################
#### 3. function to plot the emissions inside the netCDF file (bar).
plot_uk_nc <- function(y, species, uk_folname, map_yr_uk, naei_inv, dt_emis){
  
  print(paste0(Sys.time(),":      Annual emissions inside NetCDF file..."))  
  
  fname_route <- paste0(dt_poll[ceh_poll == species, emep_model],"_UKEIRE_",y,"emis_",map_yr_uk,"map_",naei_inv,"inv")
  
  # read written summaries  
  dt_group <- fread(paste0(uk_folname, "/tables/", fname_route, "_PROCESSED.csv"))
  dt_ncinp <- fread(paste0(uk_folname, "/tables/", fname_route, "_NETCDFINP.csv"))
  dt_ncout <- fread(paste0(uk_folname, "/tables/", fname_route, "_NETCDFOUT.csv"))
    
  # plot 2 - area masks, bring over the total masked, from plot 1, grouped by mask area
  dt_p1_tot <- dt_emis[stage == "tot_masked", list(emis_t = sum(emis_t, na.rm=T)), by = .(Area, Pollutant, data_source, emis_y, inv_y, stage)]
    
  dt_group_t <- dt_group[, lapply(.SD, sum, na.rm=T), by = .(Area, Pollutant, data_source, emis_y, inv_y), .SDcols = c("emis_t_tot_grouped","tsum")]
  setnames(dt_group_t, "tsum", "mon_grouped")
  dt_group_m <- melt(dt_group_t, id.vars = c("Area","Pollutant","data_source","emis_y","inv_y"),
				     variable.name = "stage", value.name = "emis_t")
    
  dt_ncinp_t <- dt_ncinp[, lapply(.SD, sum, na.rm=T), by = .(Area, Pollutant, data_source, emis_y, inv_y), .SDcols = c("emis_t_tot_ncinput","emis_t_tot_array","emis_t_tot_ncfile","tsum")]
  setnames(dt_ncinp_t, "tsum", "mon_ncfile")
  dt_ncinp_m <- melt(dt_ncinp_t, id.vars = c("Area","Pollutant","data_source","emis_y","inv_y"),
				     variable.name = "stage", value.name = "emis_t")
    
  dt_ncout_t <- dt_ncout[time_res == "annual", lapply(.SD, sum, na.rm=T), by = .(Area, Pollutant, data_source, emis_y, inv_y), .SDcols = c("emis_t_ncfile")]
  dt_ncout_m <- melt(dt_ncout_t, id.vars = c("Area","Pollutant","data_source","emis_y","inv_y"),
				     variable.name = "stage", value.name = "emis_t")

  dt <- rbindlist(list(dt_p1_tot, dt_group_m, dt_ncinp_m, dt_ncout_m), use.names = T)  
  dt[, stage := gsub("emis_t_", "", stage) ]
  dt[, stage := factor(stage, levels = c("tot_masked","tot_grouped","mon_grouped","tot_ncinput","tot_array","tot_ncfile","mon_ncfile","ncfile"))]
  dt[, Area := factor(Area, levels = c("ow","sea","ie","uk"))]
  
  p <- ggplot(dt, aes(x = stage, y = emis_t/1000, group = Area, fill = Area))+
    geom_bar(stat = "identity")+
	scale_fill_manual(values = c("#eecea6","#59aed3","#8aee87","#6cb96a"),
	                  labels = c("outwith","sea","IE (Terres)","UK (Terres)"))+
	labs(y = bquote(kt~a^-1))+
    #facet_wrap(~Area, nrow=1, scales = "free_y")+
    theme_bw()+
    theme(strip.text = element_text(size = 20),
          legend.title = element_blank(), 
          legend.position = "bottom",
          legend.text = element_text(size = 16),
          axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size = 14),
		  axis.title.y = element_text(size = 16),
		  margin(t = 2, r = 2, b = 2, l = 2, unit = "mm"))
  
  fname <- paste0(uk_folname,"/plots/",dt_poll[ceh_poll == species, emep_model],"_UKEIRE_",y,"emis_",map_yr_uk,"map_",naei_inv,"inv_3_UKNCBAR.png")
  
  ggsave(fname, p, width = 12, height = 6)
  
  return(p)
   
}

######################################################################################################
#### 4. function to plot monthly total pollutant in UK, IE and SEA (line).
plot_uk_tot_mon <- function(y, species, uk_folname, map_yr_uk, naei_inv, time_dim){

  print(paste0(Sys.time(),":      Total monthly emissions inside NetCDF file..."))  
  
  if(time_dim == "annual"){ i_time <- 1 }else if(time_dim == "month"){ i_time <- 1:12 }else{ i_time <- 1:365 }

  
  fname_route <- paste0(dt_poll[ceh_poll == species, emep_model],"_UKEIRE_",y,"emis_",map_yr_uk,"map_",naei_inv,"inv")
  
  # read written summaries  
  dt_ncinp <- fread(paste0(uk_folname, "/tables/", fname_route, "_NETCDFINP.csv"))
  dt_ncout <- fread(paste0(uk_folname, "/tables/", fname_route, "_NETCDFOUT.csv"))[time_res == "month"]
  
  # re-shape
  dt_ncinp[, c("tsum","tot_tres_ratio") := NULL]
  v_col_melt <- setdiff(names(dt_ncinp), c(paste0("t",i_time)))
  dt_ncinp_m <- melt(dt_ncinp, id.vars = v_col_melt, variable.name = time_dim, value.name = "emis_t")
  dt_ncinp_m <- dt_ncinp_m[, list(emis_kt = sum(emis_t, na.rm=T)/1000), by = .(Area, get(time_dim), data_source)]
  dt_ncinp_m[, get := as.numeric(gsub("t","",get))]
  setnames(dt_ncinp_m, "get",time_dim)
  dt_ncinp_m[, line_type := 1]
  
  dt_ncout_m <- dt_ncout[, list(emis_kt = sum(emis_t_ncfile, na.rm=T)/1000), by = .(Area, t, data_source)]
  setnames(dt_ncout_m, "t",time_dim)
  dt_ncout_m[, line_type := 2]
  
  # combine for plots
  dt_plot <- rbindlist(list(dt_ncinp_m, dt_ncout_m), use.names = T)
  dt_plot[, Area := factor(Area, levels = c("uk","ie","sea"))]
  
  # plots
  p <- ggplot(data = dt_plot, aes(x = month, y = emis_kt, group = data_source, 
							 colour = data_source, linetype = data_source))+
    geom_line()+
    geom_point()+
    scale_x_continuous(breaks = 1:12)+
    facet_wrap(~Area, scales = "free_y", ncol = 1)+
	labs(y = bquote(kt~a^-1))+
    theme_bw()+
    theme(strip.text = element_text(size = 20),
          legend.title = element_blank(), 
          legend.position = "bottom",
          legend.text = element_text(size = 16),
          axis.text.x = element_text(size = 16),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size = 14),
		  axis.title.y = element_text(size = 16),
		  margin(t = 2, r = 2, b = 2, l = 2, unit = "mm"))
	
  fname <- paste0(uk_folname,"/plots/",dt_poll[ceh_poll == species, emep_model],"_UKEIRE_",y,"emis_",map_yr_uk,"map_",naei_inv,"inv_4_UKMONTOTLINE.png")
  
  ggsave(fname, p, width = 10, height = 7)
  
  return(p)

}

######################################################################################################
#### 5. function to plot monthly pollutant in UK by sector and by Area (line).
plot_uk_mon_sec <- function(y, species, uk_folname, map_yr_uk, naei_inv, time_dim){

  print(paste0(Sys.time(),":      Sectoral monthly emissions inside NetCDF file..."))  
  
  if(time_dim == "annual"){ i_time <- 1 }else if(time_dim == "month"){ i_time <- 1:12 }else{ i_time <- 1:365 }

  
  fname_route <- paste0(dt_poll[ceh_poll == species, emep_model],"_UKEIRE_",y,"emis_",map_yr_uk,"map_",naei_inv,"inv")
  
  # read written summaries  
  dt_ncinp <- fread(paste0(uk_folname, "/tables/", fname_route, "_NETCDFINP.csv"))
  dt_ncout <- fread(paste0(uk_folname, "/tables/", fname_route, "_NETCDFOUT.csv"))[time_res == "month"]
  
  # re-shape
  dt_ncinp[, c("tsum","tot_tres_ratio") := NULL]
  v_col_melt <- setdiff(names(dt_ncinp), c(paste0("t",i_time)))
  dt_ncinp_m <- melt(dt_ncinp, id.vars = v_col_melt, variable.name = time_dim, value.name = "emis_t")
  dt_ncinp_m <- dt_ncinp_m[, list(emis_kt = sum(emis_t, na.rm=T)/1000), by = .(Area, get(time_dim), sec_GNFR, sec_EMEP, data_source)]
  dt_ncinp_m[, get := as.numeric(gsub("t","",get))]
  setnames(dt_ncinp_m, "get",time_dim)
  dt_ncinp_m[, line_type := 1]
  
  dt_ncout_m <- dt_ncout[, list(emis_kt = sum(emis_t_ncfile, na.rm=T)/1000), by = .(Area, t, sec_GNFR, sec_EMEP, data_source)]
  setnames(dt_ncout_m, "t", time_dim)
  dt_ncout_m[, line_type := 2]
  
  # combine for plots
  dt_plot <- rbindlist(list(dt_ncinp_m, dt_ncout_m), use.names = T)
  dt_plot[, Area := factor(Area, levels = c("uk","ie","sea"))]
  
  # for now exclude the sectors above sec_13
  dt_plot <- dt_plot[sec_EMEP %in% paste0("sec",str_pad(1:13, side = "left", width = 2, 0))]
  
  # plots
  p <- ggplot(data = dt_plot, aes(x = month, y = emis_kt, group = data_source, 
							 colour = data_source, linetype = data_source))+
    geom_line()+
    scale_x_continuous(breaks = 1:12)+
    facet_grid(Area~sec_GNFR)+
	labs(y = bquote(kt~a^-1))+
    theme_bw()+
    theme(strip.text = element_text(size = 12),
          legend.title = element_blank(), 
          legend.position = "bottom",
          legend.text = element_text(size = 16),
          axis.text.x = element_text(size = 8),
          axis.title.x = element_blank(),
          axis.text.y = element_text(size = 14),
		  axis.title.y = element_text(size = 16),
		  margin(t = 2, r = 2, b = 2, l = 2, unit = "mm"))
	
  fname <- paste0(uk_folname,"/plots/",dt_poll[ceh_poll == species, emep_model],"_UKEIRE_",y,"emis_",map_yr_uk,"map_",naei_inv,"inv_5_UKMONSECLINE.png")
  
  ggsave(fname, p, width = 20, height = 8)
  
  return(p)

}

######################################################################################################
#### 6. function to plot annual total sector pollutant across UK domain (maps).
plot_uk_sec_ann <- function(y, species, uk_folname, map_yr_uk, naei_inv, 
							emep_inv, time_dim, sUK, sEU){
  
  print(paste0(Sys.time(),":      Annual sectoral total emissions maps - UK..."))    

  # plot the UK sector annual totals (with EU in the background)
  sUK[sUK==0] <- NA
  sEU[sEU==0] <- NA
 
  # disaggregate the EU map to 0.01 degree
  sEU <- disagg(sEU, 10)
  sEU <- sEU/100
  #names(rEU) <- paste0(species, "_EU_", y,"_emis_", emep_inv,"_inv")							

  ## reclassify and plot ##  
  # exract all the values present, into a vector. 
  v_v <- values(c(sUK, sEU))
  v_v <- as.vector(v_v)
    
  # create break points based on quantiles
  v_q <- as.vector(quantile(v_v, probs = c(0.20,0.40,0.55,0.7,0.82,0.92,0.97,1), na.rm=T))  
  
  # create a reclassification matrix and labels for the plot.
  leg.pos <- "bottom"
  m <- matrix(c(0, v_q[1:7], v_q[1:7], ceiling(max(global(c(sUK, sEU), max, na.rm=T)$max, na.rm=T)), 1:8), ncol = 3) 
  brew_d = -1
  v_labs <- unlist(lapply(1:nrow(m), function(x) paste0(round(m[x,1],2), " - ", round(m[x,2], 2))))
  v_labs[length(v_labs)] <- paste0("> ",round(m[nrow(m),1],2))
  
  # reclassify the data
  sUK_rc <- terra::classify(sUK, m)
  sUK_rc <- as.factor(sUK_rc)
  sEU_rc <- terra::classify(sEU, m)
  sEU_rc <- as.factor(sEU_rc)
    
  # plot and save
  p <- ggplot()+
    geom_spatraster(data = sUK_rc, na.rm = T)+
	geom_spatraster(data = sEU_rc, na.rm = T, alpha = 0.6)+
	scale_fill_brewer(labels = v_labs, palette = "Spectral", breaks = 1:length(v_q), direction = brew_d)+
	scale_y_continuous(expand = c(0, 0)) +
	scale_x_continuous(expand = c(0, 0)) +
	labs(fill = bquote(tonnes~a^-1), y = "", x = "")+
	# ggtitle(bquote("Emissions of"~.(p)~a^-1))+
	geom_sf(data = sf_uk, fill = NA, colour = "black")+
	facet_wrap(~lyr, nrow = 3)+
	#scale_fill_hypso_d(labels = v_labs, na.value = "transparent", palette = "etopo1")+
	theme_bw()+
	  {if(leg.pos == "bottom")guides(fill = guide_legend(nrow = 2))}+
	  theme(#plot.title = element_text(size = 30, face = "bold"),
	  strip.text = element_text(size = 18),
	  axis.text = element_text(size=11),
	  legend.text = element_text(size=18),
	  legend.title = element_text(size=24),
	  legend.position = leg.pos,
	  #plot.margin = grid::unit(c(2,2,2,2), "mm"),
	  margin(t = 2, r = 2, b = 2, l = 2, unit = "mm"))
  
  fname <- paste0(uk_folname,"/plots/",dt_poll[ceh_poll == species, emep_model],"_UKEIRE_",y,"emis_",map_yr_uk,"map_",naei_inv,"inv_6_UKANNSECMAP.png")
  
  ggsave(fname, p, width = 14, height = 14)
   
  return(p)							
}

######################################################################################################
#### 7. function to plot monthly total pollutant across UK domain (maps).
plot_uk_map_mon <- function(y, species, uk_folname, map_yr_uk, naei_inv, 
							emep_inv, time_dim, sUK, sEU){

  print(paste0(Sys.time(),":      Monthly total emissions maps - UK..."))  
  
  # plot the UK monthly totals (with EU in the background)
  sUK[sUK==0] <- NA
  sEU[sEU==0] <- NA
 
  # disaggregate the EU map to 0.01 degree
  sEU <- disagg(sEU, 10)
  sEU <- sEU/100
  #names(rEU) <- paste0(species, "_EU_", y,"_emis_", emep_inv,"_inv")

  ## reclassify and plot ##  
  # exract all the values present, into a vector. 
  v_v <- values(c(sUK, sEU))
  v_v <- as.vector(v_v)
    
  # create break points based on quantiles
  v_q <- as.vector(quantile(v_v, probs = c(0.20,0.40,0.55,0.7,0.82,0.92,0.97,1), na.rm=T))  
  
  # create a reclassification matrix and labels for the plot.
  leg.pos <- "bottom"
  m <- matrix(c(0, v_q[1:7], v_q[1:7], ceiling(max(global(c(sUK, sEU), max, na.rm=T)$max, na.rm=T)), 1:8), ncol = 3) 
  brew_d = -1
  v_labs <- unlist(lapply(1:nrow(m), function(x) paste0(round(m[x,1],2), " - ", round(m[x,2], 2))))
  v_labs[length(v_labs)] <- paste0("> ",round(m[nrow(m),1],2))
  
  # reclassify the UK data and ensure every factor is present in every raster
  sUK_rc <- terra::classify(sUK, m)
  sUK_rc <- as.factor(sUK_rc)
  
  i_facMax <- which.max(unlist(sapply(levels(sUK_rc), function(x) nrow(x))))
  
  sUK_fac <- copy(sUK_rc)
  for(j in 1:nlyr(sUK_fac)){
    sUK_fac <- categories(sUK_fac, j, levels(sUK_rc)[[i_facMax]]) 
	names(levels(sUK_fac)[[j]]) <- c("ID",names(sUK_rc)[j])
  }
  
  # reclassify the EU data and ensure every factor is present in every raster
  sEU_rc <- terra::classify(sEU, m)
  sEU_rc <- as.factor(sEU_rc)
  
  i_facMax <- which.max(unlist(sapply(levels(sEU_rc), function(x) nrow(x))))
  
  sEU_fac <- copy(sEU_rc)
  for(j in 1:nlyr(sEU_fac)){
    sEU_fac <- categories(sEU_fac, j, levels(sEU_rc)[[i_facMax]]) 
	names(levels(sEU_fac)[[j]]) <- c("ID",names(sEU_rc)[j])
  }
  
    
  # plot and save
  p <- ggplot()+
    geom_spatraster(data = sUK_fac, na.rm = T)+
	geom_spatraster(data = sEU_fac, na.rm = T, alpha = 0.6)+
	scale_fill_brewer(labels = v_labs, palette = "Spectral", breaks = 1:length(v_q), direction = brew_d)+
	scale_y_continuous(expand = c(0, 0)) +
	scale_x_continuous(expand = c(0, 0)) +
	labs(fill = bquote(tonnes~a^-1), y = "", x = "")+
	# ggtitle(bquote("Emissions of"~.(p)~a^-1))+
	geom_sf(data = sf_uk, fill = NA, colour = "black")+
	facet_wrap(~lyr, nrow = 3)+
	#scale_fill_hypso_d(labels = v_labs, na.value = "transparent", palette = "etopo1")+
	theme_bw()+
	  {if(leg.pos == "bottom")guides(fill = guide_legend(nrow = 2))}+
	  theme(#plot.title = element_text(size = 30, face = "bold"),
	  strip.text = element_text(size = 14),
	  axis.text = element_text(size=11),
	  legend.text = element_text(size=18),
	  legend.title = element_text(size=24),
	  legend.position = leg.pos,
	  #plot.margin = grid::unit(c(2,2,2,2), "mm"),
	  margin(t = 2, r = 2, b = 2, l = 2, unit = "mm"))
  
  fname <- paste0(uk_folname,"/plots/",dt_poll[ceh_poll == species, emep_model],"_UKEIRE_",y,"emis_",map_yr_uk,"map_",naei_inv,"inv_7_UKMONTOTMAP.png")
  
  ggsave(fname, p, width = 12, height = 11)
   
  return(p)
}

##################
#### EU PLOTS ####
##################

######################################################################################################
#### 8. function to plot the EU mapped data, total for the species/year (map).
plot_eu_map_ann <- function(y, species, uk_folname, eu_folname, map_yr_uk, 
							naei_inv, emep_inv, time_dim){
  
  print(paste0(Sys.time(),":      Annual total emissions map - EU..."))  
  
  if(time_dim == "annual"){ i_time <- 1 }else if(time_dim == "month"){ i_time <- 1:12 }else{ i_time <- 1:365 }
  
  nc_fname <- paste0(dt_poll[ceh_poll == species, emep_model],"_EU_",
                  y,"emis_",map_yr_uk,"map_",naei_inv,"inv_0.1.nc")
  nc_fname <- paste0(eu_folname,"/", nc_fname)
  
  # read all data. Create an annual map of total emissions. 
  # this is for context and background to the UK map.   
  v_sectors <-  dt_sec[,unique(sec)]
    
  l_sec_ann <- list()
    
  # loop through every sector and collect data. Summarise where needed. 
  for(i in v_sectors){
    
	if(i == "") next # not interested in currently blank EMEP-named sectors
    if(dt_sec[sec == i, GNFRlong]  == "" ) next # none of these in EU file	
	
	# sector name in the EMEP file
 	nc_secname <- paste0("Emis_EUR_", dt_sec[sec == i, name] ) 
	  
	# stack the monthly data, add to monthly temp list
    s_sec_mon <- rast(nc_fname, subds = nc_secname)
	#s_sec_mon <- crop(s_sec_mon, ext(rUK))
	if(nlyr(s_sec_mon) != max(i_time)) stop("not enough temporal splits in EMEP data")
		  
	# stack to annual sector totals  
	r_sec_ann <- app(s_sec_mon, sum, na.rm=T)	
	l_sec_ann[[dt_sec[sec == i, name]]] <- r_sec_ann  
	  
  } # sector
    
  # sum to annual total of everything. 
  sEU_sec_tot <- rast(l_sec_ann)
  rEU <- app(sEU_sec_tot, sum, na.rm=T)
  rEU[rEU==0] <- NA
    
  # disaggregate the EU map to 0.01 degree
  #rEU <- disagg(rEU, 10)
  #rEU <- rEU/100
  #names(rEU) <- paste0(species, "_EU_", y,"_emis_", emep_inv,"_inv")

  ## reclassify and plot ##  
  # exract all the values present, into a vector. 
  v_v <- values(rEU)
  v_v <- as.vector(v_v)
    
  # create break points based on quantiles
  v_q <- as.vector(quantile(v_v, probs = c(0.20,0.40,0.55,0.7,0.82,0.92,0.97,1), na.rm=T))  
  
  # create a reclassification matrix and labels for the plot.
  leg.pos <- "bottom"
  m <- matrix(c(0, v_q[1:7], v_q[1:7], ceiling(max(global(c(rEU), max, na.rm=T)$max, na.rm=T)), 1:8), ncol = 3) 
  brew_d = -1
  v_labs <- unlist(lapply(1:nrow(m), function(x) paste0(round(m[x,1],2), " - ", round(m[x,2], 2))))
  v_labs[length(v_labs)] <- paste0("> ",round(m[nrow(m),1],2))
  
  # reclassify the EU data and ensure every factor is present in every raster
  rEU_rc <- terra::classify(rEU, m)
  rEU_rc <- as.factor(rEU_rc)
    
  # plot and save
  p <- ggplot()+
    #geom_spatraster(data = rUK_rc, na.rm = T)+
	geom_spatraster(data = rEU_rc, na.rm = T)+
	scale_fill_brewer(labels = v_labs, palette = "Spectral", breaks = 1:length(v_q), direction = brew_d)+
	scale_y_continuous(expand = c(0, 0)) +
	scale_x_continuous(expand = c(0, 0)) +
	labs(fill = bquote(tonnes~a^-1), y = "", x = "")+
	# ggtitle(bquote("Emissions of"~.(p)~a^-1))+
	geom_sf(data = sf_eu, fill = NA, colour = "black")+
	#facet_wrap(~lyr, nrow = 1)+
	#scale_fill_hypso_d(labels = v_labs, na.value = "transparent", palette = "etopo1")+
	theme_bw()+
	  {if(leg.pos == "bottom")guides(fill = guide_legend(nrow = 2))}+
	  theme(#plot.title = element_text(size = 30, face = "bold"),
	  axis.text = element_text(size=12),
	  legend.text = element_text(size=18),
	  legend.title = element_text(size=24),
	  legend.position = leg.pos,
	  #plot.margin = grid::unit(c(2,2,2,2), "mm"),
	  margin(t = 2, r = 2, b = 2, l = 2, unit = "mm"))
  
  fname <- paste0(uk_folname,"/plots/",dt_poll[ceh_poll == species, emep_model],"_UKEIRE_",y,"emis_",map_yr_uk,"map_",naei_inv,"inv_8_EUTOTMAP.png")
  
  ggsave(fname, p, width = 13, height = 10)
   
  return(list("EU_plot" = p, "eu_sec_anntot" = sEU_sec_tot))
						
}

######################################################################################################
#### 9. function to plot the EU emissions coming from EMEP and in the ncfile
plot_eu_nc_emis <- function(){

# 9_
}

######################################################################################################
#### 10. function to plot monthly total pollutant in EU (line).
plot_eu_tot_mon <- function(){

# 10_
}

######################################################################################################
#### 11. function to plot monthly pollutant in EU by sector and by Area (line).
plot_eu_mon_sec <- function(){

# 11_
}

######################################################################################################
#### 12. function to plot annual total sector pollutant across EU domain (maps).
plot_eu_sec_ann <- function(y, species, uk_folname, map_yr_uk, naei_inv, 
							emep_inv, time_dim, sEU){
  
  print(paste0(Sys.time(),":      Annual sectoral total emissions maps - EU..."))    
							
  
  # plot the UK sector annual totals (with EU in the background)
  sEU[sEU==0] <- NA
 
  ## reclassify and plot ##  
  # exract all the values present, into a vector. 
  v_v <- values(sEU)
  v_v <- as.vector(v_v)
    
  # create break points based on quantiles
  v_q <- as.vector(quantile(v_v, probs = c(0.20,0.40,0.55,0.7,0.82,0.92,0.97,1), na.rm=T))  
  
  # create a reclassification matrix and labels for the plot.
  leg.pos <- "bottom"
  m <- matrix(c(0, v_q[1:7], v_q[1:7], ceiling(max(global(c(sEU), max, na.rm=T)$max, na.rm=T)), 1:8), ncol = 3) 
  brew_d = -1
  v_labs <- unlist(lapply(1:nrow(m), function(x) paste0(round(m[x,1],2), " - ", round(m[x,2], 2))))
  v_labs[length(v_labs)] <- paste0("> ",round(m[nrow(m),1],2))
  
  # reclassify the data
  sEU_rc <- terra::classify(sEU, m)
  sEU_rc <- as.factor(sEU_rc)
    
  # plot and save
  p <- ggplot()+
    geom_spatraster(data = sEU_rc, na.rm = T)+
	scale_fill_brewer(labels = v_labs, palette = "Spectral", breaks = 1:length(v_q), direction = brew_d)+
	scale_y_continuous(expand = c(0, 0)) +
	scale_x_continuous(expand = c(0, 0)) +
	labs(fill = bquote(tonnes~a^-1), y = "", x = "")+
	# ggtitle(bquote("Emissions of"~.(p)~a^-1))+
	geom_sf(data = sf_eu, fill = NA, colour = "black")+
	facet_wrap(~lyr, nrow = 3)+
	#scale_fill_hypso_d(labels = v_labs, na.value = "transparent", palette = "etopo1")+
	theme_bw()+
	  {if(leg.pos == "bottom")guides(fill = guide_legend(nrow = 2))}+
	  theme(#plot.title = element_text(size = 30, face = "bold"),
	  strip.text = element_text(size = 14),
	  axis.text = element_text(size=11),
	  legend.text = element_text(size=18),
	  legend.title = element_text(size=24),
	  legend.position = leg.pos,
	  #plot.margin = grid::unit(c(2,2,2,2), "mm"),
	  margin(t = 2, r = 2, b = 2, l = 2, unit = "mm"))
  
  fname <- paste0(uk_folname,"/plots/",dt_poll[ceh_poll == species, emep_model],"_UKEIRE_",y,"emis_",map_yr_uk,"map_",naei_inv,"inv_12_EUANNSECMAP.png")
  
  ggsave(fname, p, width = 13, height = 10)
     
  return(p)							
}

######################################################################################################
#### 13. function to plot monthly total pollutant across EU domain (maps).
plot_eu_map_mon <- function(){

# 13_
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




