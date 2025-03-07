################################################################################
#### master function to generate QAQC
create_qaqc <- function(y, species, uk_folname, eu_folname, map_yr_uk, naei_inv, 
                        emep_inv, time_dim, emep_version, v_EMEP_sec,
						uk_agg_schema, eu_agg_schema, tp_scheme){

  # lapply the QAQC over v_pollutants
    
  print(paste0(format(Sys.time(), "%Y-%m-%d %X"),": Creating QAQC for ",
               dt_poll[ceh_poll == species, emep_model],
			   " in ",y,"..."))

  # folders needed
  dir.create(file.path(uk_folname, "plots", paste0("e",y)), 
			 showWarnings = FALSE, recursive = T)  
  dir.create(file.path(uk_folname, "qaqc"), 
			 showWarnings = FALSE, recursive = T)  
    
  #######################
  #### DATA EXTRACTS ####
  #######################
    
  species <- "nox" #
  
  print(paste0(format(Sys.time(), "%Y-%m-%d %X"),
               ":            collecting data - UK..."))  
  
  # summary tables - filenames to stop overwriting when using. 
  l_fname_uk_sums <- collect_uk_summaries(y, species, uk_folname, 
								          map_yr_uk, naei_inv)
  
  # annual total (sectors) / annual total (all) / monthly totals (all)
  l_uk_maps <- extract_uk_maps(y, species, uk_folname,	naei_inv, 
                               time_dim, v_EMEP_sec, uk_agg_schema)
    
  print(paste0(format(Sys.time(), "%Y-%m-%d %X"),
               ":            collecting data - EU..."))  
    
  # summary tables - filenames to stop overwriting when using. 
  l_fname_eu_sums <- collect_eu_summaries(y, species, eu_folname, emep_inv)
  
  # annual total (sectors) / annual total (all) / monthly totals (all)
  l_eu_maps <- extract_eu_maps(y, species, eu_folname,	emep_inv, 
                               time_dim, v_EMEP_sec, eu_agg_schema)
  
  
  ##################
  #### UK PLOTS ####
  ##################
  
  ## plot names follow: region_plottype_aggregation_temporal
  
  print(paste0(format(Sys.time(), "%Y-%m-%d %X"),
               ":            maps and plots - UK..."))  
  
  # 1. Map; UK emissions, total cell-1 annum-1.
  gg_p1 <- uk_map_tot_ann(l_data_uk = l_uk_maps[["ann_tot_all"]], 
				  	      l_data_eu = l_eu_maps[["ann_tot_all"]],
					      y, species, uk_folname, 
                          map_yr_uk, naei_inv, emep_inv)
      
  # 2. Bar plot; UK/IE emissions processing check-plot
  l_gg_p2 <- uk_bar_inv_ann(fname_inv = l_fname_uk_sums[["inventory"]], 
                            fname_mask = l_fname_uk_sums[["masked"]],
					        y, species, uk_folname, 
						    map_yr_uk, naei_inv)
						  
  gg_p2 <- l_gg_p2[["plot"]]  
    
  # 3. Bar plot; UK/IE emissions going into netCDF and those inside the netCDF
  gg_p3 <- uk_bar_nc_ann(fname_group = l_fname_uk_sums[["processed"]], 
					     fname_ncinp = l_fname_uk_sums[["ncinput"]], 
					     fname_ncout = l_fname_uk_sums[["ncoutput"]], 
					     y, species, uk_folname, map_yr_uk, 
					     naei_inv, dt_emis = l_gg_p2[["table"]])
       
  # 4. Line plot; UK/IE emissions of total by month in netcdf
    # if(time_dim == "annual") ; produce a plot using the EMEP version profiles
  l_gg_p4 <- uk_lin_tot_mon(fname_ncout = l_fname_uk_sums[["ncoutput"]], 
                            y, species, uk_folname, map_yr_uk, 
				  	        naei_inv, time_dim, emep_version)
  
  gg_p4 <- l_gg_p4[["plot"]] 
    
  # 5. Line plot; UK/IE emissions of total by sector by month in netcdf
    # if(time_dim == "annual") ; produce a plot using the EMEP version profiles.
  gg_p5 <- uk_lin_sec_mon(dt_month = l_gg_p4[["table"]], y,
                          species, uk_folname, map_yr_uk, 
						  naei_inv, time_dim, emep_version)

  # 6. Maps; UK/IE (& EU) emissions, sector total cell-1 annum-1.
  gg_p6 <- uk_map_sec_ann(l_data_uk = l_uk_maps[["ann_tot_sec"]], 
                          l_data_eu = l_eu_maps[["ann_tot_sec"]], 
					 	  y, species, uk_folname, 
						  map_yr_uk, naei_inv, emep_inv)
  
  # 7. Maps; UK/IE (& EU) emissions, monthly total cell-1 annum-1.
	# if(time_dim == "annual") ; produce maps using the EMEP version profiles.
	# ignore EU for this plot, gets too complicated re timing files. 
  gg_p7 <- uk_map_tot_mon(dt_month = l_gg_p4[["table"]], l_uk_maps, y, 
                          species, uk_folname, map_yr_uk, 
						  naei_inv, time_dim, emep_version)
    
  ############
  #### EU ####
  ############
	
  ## plot names follow: region_plottype_aggregation_temporal
  
  print(paste0(format(Sys.time(), "%Y-%m-%d %X"),
               ":            maps and plots - EU..."))  
			   
  # 8. Map; EU emissions, total cell-1 annum-1.
 # l_gg_p8 <- plot_eu_map_ann(y, species, uk_folname, eu_folname, map_yr_uk, 
 #							 naei_inv, emep_inv, time_dim)
 # gg_p8 <- l_gg_p8[["EU_plot"]]
   
  # 9. Bar plot; EU emissions from EMEP processed into nc file
  
  
  
  # 10. Line plot; EU emissions of total by month in netcdf
  
  
  # 11. Line plot; EU (country) monthly pollutant by sector (large plot)
  
     
  # 12. Map;  EU emissions, sector total cell-1 annum-1.
 # gg_p12 <- plot_eu_sec_ann(y, species, uk_folname, map_yr_uk, naei_inv, 
 #						   emep_inv, time_dim, sEU = l_gg_p8[["eu_sec_anntot"]])
 #   
  # 13. Maps; EU emissions, monthly total cell-1 annum-1.
  
  
  
  
  #metadata
  
  
  #############
  #### PDF ####
  #############
  
  print(paste0(format(Sys.time(), "%Y-%m-%d %X"),
               ":            rendering pdf..."))  
  
  l_pdf_params <- list(y = y, dt_poll = dt_poll, species = species, 
                       uk_folname = uk_folname, eu_folname = eu_folname,
					   map_yr_uk = map_yr_uk, naei_inv = naei_inv, 
					   emep_inv = emep_inv, v_EMEP_sec = v_EMEP_sec, 
					   dt_sec = dt_sec, time_dim = time_dim, 
					   emep_version = emep_version, 
					   uk_agg_schema = uk_agg_schema, 
					   eu_agg_schema = eu_agg_schema,
					   tp_scheme = tp_scheme,
					   l_fname_uk_sums = l_fname_uk_sums, 
					   l_uk_maps = l_uk_maps,
					   dt_month = l_gg_p4[["table"]])
  
  # render the source of the document to the default output format:
  rmarkdown::render(input = "R/QAQC.Rmd", 
                  output_file = paste0(dt_poll[ceh_poll == species, emep_model],
				                       "_",y,"emis_",naei_inv,"inv_QAQC.pdf"),
                  output_dir = paste0(uk_folname,"/qaqc"),
				  params = l_pdf_params)
  
  #tinytex::parse_install(
  #text = "! LaTeX Error: File `threeparttablex.sty' not found."
#)
 #%>%
#	  row_spec(3, hline_after = T)

  # require(cowplot)
 # cp <- plot_grid(gg_p2, gg_p3, ncol = 1, align = "hv", axis = 'tblr', rel_heights = 1)
  #cp <- plot_grid(p2, p1, rel_widths = c(0.33, 1))
 # fname <- paste0(uk_folname,"/plots/",dt_poll[ceh_poll == species, emep_model],"_UKEIRE_",y,"emis_",map_yr_uk,"map_",naei_inv,"inv_COPW1.png")
 # save_plot(fname, cp, base_height = 15, base_width = 12)
  
  print(paste0(format(Sys.time(), "%Y-%m-%d %X"), ": DONE."))  
}

#######################
#### DATA EXTRACTS ####
#######################

################################################################################
#### function to extract UK gridded data
#### Annual total (sectors) / annual total (all) / monthly totals (all)
extract_uk_maps <- function(y, species, uk_folname,	naei_inv, 
                            time_dim, v_EMEP_sec, uk_agg_schema){
    
  if(time_dim == "annual"){
	i_time <- 1
  }else if(time_dim == "month"){
	i_time <- 1:12
  }else{
	i_time <- 1:365
  }
  
  # set the required ncdf filename for the UK
  nc_fname <- paste0("/UKEIRE_", naei_inv,"inv_", y,"emis_0.01.nc")
  nc_fname <- paste0(uk_folname, nc_fname)
  
  # read all data.
  # gather sector info direct from input file. Yet another safety check. 
  nc <- nc_open(nc_fname)
  v_var <- names(nc$var)
  v_var <- v_var[grep(paste0("^",species), v_var)] # restrict to species. 
  nc_close(nc)
    
  # l_all <- list()
  l_ann_sec <- list()
  l_ann_tot <- list()
  l_mon_tot <- list()
  
      
  # loop through every sector and collect data. 
  # Summarise where needed, make one surface, not ISO split. 
  for(v in v_var){
    
	## extract and make stack
    # simply using rast() will read in all layers across sector & time dims, 
	# into 1 big stack (can't handle 4D). 
    # We can certainly work with this, but it might need to go via an array etc.
    # in the future.
	s <- suppressWarnings(rast(nc_fname, subds = v))
	
	# from the extract, split out the sectors into their own list elements, 
	# in whatever form they come. 
	l_out <- lapply(v_EMEP_sec, function(x){ s[[grep(paste0("sector=",x,"$"), 
															names(s))]]})
	names(l_out) <- v_EMEP_sec
	# l_all[[v]] <- l_out # this isn't particularly needed. 
	
	# sum each list element stack into one (annual per sector, if not already)
	l_outv2 <- suppressWarnings(lapply(l_out, function(x) app(x, sum, na.rm=T)))
	s_outv2 <- rast(l_outv2) # stack of annual sector maps
	l_ann_sec[[v]] <- l_outv2 # return the list, as we can transpose this later
	
	# sum those annual sector totals into one annual total
	s_outv3 <- suppressWarnings(app(s_outv2, sum, na.rm=T))
	names(s_outv3) <- "annual"
	l_ann_tot[[v]] <- s_outv3
	
	# also create a month by month total, across all sectors. 
	if(time_dim == "month"){
	  
	  ## CODE NEEDED ##
	  
	  l_mon_tot[[v]] <- l_outv4
	
	}
  }

  # now summarise to UK 
  # l_ann_sec = list of 3 vars (ISO), each a stack of 13 secs annual total
  # l_ann_tot = list of 3 vars (ISO), each a sum of all sectors over all time
  # l_mon_tot = list of 3 vars (ISO), each a stack-sum all sectors over 12 month

  return(list("ann_tot_sec" = l_ann_sec, "ann_tot_all" = l_ann_tot, 
			  "mon_tot_all" = l_mon_tot ))
  
}

################################################################################
#### function to collect the UK written summaries

collect_uk_summaries <- function(y, species, uk_folname, map_yr_uk,	naei_inv){
							
  fname_root <- paste0(species,"_UKEIRE_",y,"emis_",
					   map_yr_uk,"map_",naei_inv,"inv")
  
  # filenames for written summaries    
  fname_inv   <- paste0(uk_folname, "/tables/e", y, "/", 
                     fname_root, "_INVENTORY.csv")
  fname_mask  <- paste0(uk_folname, "/tables/e", y, "/",
                     fname_root,"_MASKED.csv")
  fname_group <- paste0(uk_folname, "/tables/e", y, "/",
                     fname_root,"_PROCESSED.csv")
  fname_ncinp <- paste0(uk_folname, "/tables/e", y, "/",
                     fname_root,"_NETCDFINP.csv")
  fname_ncout <- paste0(uk_folname, "/tables/e", y, "/",
                     fname_root,"_NETCDFOUT.csv")
    
  return(list("inventory" = fname_inv, "masked" = fname_mask, 
              "processed" = fname_group, "ncinput" = fname_ncinp, 
			  "ncoutput" = fname_ncout ))
    
}

################################################################################
#### function to extract EU gridded data
#### Annual total (sectors) / annual total (all) / monthly totals (all)
extract_eu_maps <- function(y, species, eu_folname,	emep_inv, 
                            time_dim, v_EMEP_sec, eu_agg_schema){
      
  # set the required ncdf filename for the UK
  nc_fname <- paste0("/EU_", emep_inv,"inv_", y,"emis_0.1.nc")
  nc_fname <- paste0(eu_folname, nc_fname)
  
  # read all data. Create an annual map of total emissions. 
  # gather sector info direct from input file. Yet another safety check. 
  nc <- nc_open(nc_fname)
  v_var <- names(nc$var)
  v_var <- v_var[grep(paste0("^",species), v_var)] # restrict to species.
  nc_close(nc)
    
  # l_all <- list()
  l_ann_sec <- list()
  l_ann_tot <- list()
  l_mon_tot <- list()
        
  # loop through every sector and collect data. Summarise where needed. 
  for(v in v_var){
    
	## extract and make stack
    # simply using rast() will read in all layers across sector & time dims, 
	# into 1 big stack (can't handle 4D). 
    # We can certainly work with this, but it might need to go via an array etc.
    # in the future. 
	s <- suppressWarnings(rast(nc_fname, subds = v))
	
	# from the extract, split out the sectors into their own list elements, 
	# in whatever form they come. 
	l_out <- lapply(v_EMEP_sec, function(x){ s[[grep(paste0("sector=",x,"$"), 
	                                                        names(s))]]})
	names(l_out) <- v_EMEP_sec
	# l_all[[v]] <- l_out # this isn't particularly needed. 
	
	# sum each list element stack into one (annual per sector, if not already)
	l_outv2 <- suppressWarnings(lapply(l_out, function(x) app(x, sum, na.rm=T)))
	s_outv2 <- rast(l_outv2) # stack of annual sector maps
	l_ann_sec[[v]] <- l_outv2 # return the list as we can transpose this later
	
	# sum those annual sector totals into one annual total
	s_outv3 <- suppressWarnings(app(s_outv2, sum, na.rm=T))
	names(s_outv3) <- "annual"
	l_ann_tot[[v]] <- s_outv3
	
	# also create a month by month total, across all sectors. 
	if(time_dim == "month"){
	  
	  ## CODE NEEDED ##
	  
	  l_mon_tot[[v]] <- l_outv4
	
	}
  }

  # now summarise to UK 
  # l_ann_sec = list of 60 vars (ISO),each a stack of 13 secs annual total
  # l_ann_tot = list of 60 vars (ISO),each a sum of all sectors over all time
  # l_mon_tot = list of 60 vars (ISO),each a stack-sum all sectors over 12 month

  return(list("ann_tot_sec" = l_ann_sec, "ann_tot_all" = l_ann_tot,
              "mon_tot_all" = l_mon_tot ))
  
}

################################################################################
#### function to collect the EU written summaries

collect_eu_summaries <- function(y, species, eu_folname, emep_inv){
							
  fname_root <- paste0(species,"_EU_",y,"emis_",naei_inv,"inv")
  
  # read written summaries    
  fname_inv   <- paste0(eu_folname, "/tables/e",y,
				        "/",fname_root,"_INVENTORY.csv")
  fname_proc  <- paste0(eu_folname, "/tables/e",y,
                        "/",fname_root,"_PROCESSED.csv")
  fname_ncinp <- paste0(eu_folname, "/tables/e",y,
                        "/",fname_root,"_NETCDFINP.csv")
  fname_ncout <- paste0(eu_folname, "/tables/e",y,
                        "/",fname_root,"_NETCDFOUT.csv")
  
  return(list("inventory" = fname_inv, "processed" = fname_proc, 
              "ncinput" = fname_ncinp, "ncoutput" = fname_ncout ))
    
}

################################################################################
#### function to set theme of ggplot dependent on GNFR - for full 13 GNFR plot
#### specifically for line plots of totals per sector per month. 
month_sector_theme <- function(sector) {
  
  if(sector %in% c("A_PublicPower", "F_RoadTransport")){
   x <- theme(plot.title = element_text(size = 20, hjust = 0.5),
          strip.text.x = element_text(size = 12),
          axis.title.x = element_blank(),
		  margin(t = 2, r = 2, b = 2, l = 2, unit = "mm"))
  }else if(sector %in% c("B_Industry","C_OtherStationaryComb","D_Fugitive",
						 "E_Solvents", "G_Shipping", "H_Aviation",
						 "I_Offroad", "J_Waste")){
   x <- theme(plot.title = element_text(size = 20, hjust = 0.5),
          strip.text.x = element_text(size = 12),
          axis.title.y = element_blank(),
          axis.title.x = element_blank(),
		  margin(t = 2, r = 2, b = 2, l = 2, unit = "mm"))
  }else if(sector %in% c("L_AgriOther", "M_Other")){
    x <- theme(plot.title = element_text(size = 20, hjust = 0.5),
               strip.text.x = element_text(size = 12),
               axis.title.y = element_blank(),
		       margin(t = 2, r = 2, b = 2, l = 2, unit = "mm"))
  }else if(sector == "K_AgriLivestock"){
    x <- theme(plot.title = element_text(size = 20, hjust = 0.5),
               strip.text.x = element_text(size = 12),
		       margin(t = 2, r = 2, b = 2, l = 2, unit = "mm"))
  }
  
  return(x)
}


##################
#### UK PLOTS ####
##################

################################################################################
#### 1. function to plot the UK mapped data, total for the species/year (map).
uk_map_tot_ann <- function(l_data_uk, l_data_eu, y, species, uk_folname, 
                          map_yr_uk, naei_inv, emep_inv){
    
  # stack the annual UK sum data and sum to one UKERIE surface
  s_uk <- rast(l_data_uk)
  r_uk <- app(s_uk, sum, na.rm=T)
  r_uk[r_uk == 0] <- NA
  r_uk <- extend(r_uk, ext(r_dom_ukplot)) # extend to larger UK plot domain
  # names(r_uk) <- paste0(species, "_UKEIRE_", y,"_emis_",naei_inv,"_inv")
  
  # stack the annual EU ISO sum data and sum to one EU surface
  s_eu <- rast(l_data_eu)
  r_eu <- app(s_eu, sum, na.rm=T)
  r_eu[r_eu == 0] <- NA
  r_eu <- crop(r_eu, ext(r_dom_ukplot)) # crop to slightly larger UK plot domain
  # names(r_eu) <- paste0(species, "_EU_", y,"_emis_", emep_inv,"_inv")
      
  # disaggregate the EU map to 0.01 degree
  r_eu <- disagg(r_eu, 10)
  r_eu <- r_eu/100
    
  ## reclassify and plot ##  
  # exract all the values present, into a vector. 
  v_v <- values(c(r_uk, r_eu))
  v_v <- as.vector(v_v)
    
  # create break points based on quantiles
  v_q <- as.vector(quantile(v_v, probs = c(0.20,0.40,0.55,0.7,0.82,0.92,0.97,1),
                            na.rm=T))  
  
  # create a reclassification matrix and labels for the plot.
  leg.pos <- "bottom"
  m <- matrix(c(0, v_q[1:7], v_q[1:7], 
                ceiling(max(global(c(r_uk, r_eu), max, na.rm=T)$max, na.rm=T)),
				1:8), ncol = 3) 
  brew_d = -1
  v_labs <- unlist(lapply(1:nrow(m), function(x) paste0(round(m[x,1],2), " - ",
                                                        round(m[x,2], 2))))
  v_labs[length(v_labs)] <- paste0("> ",round(m[nrow(m),1],2))
  
  # reclassify the data
  r_uk_rc <- terra::classify(r_uk, m)
  r_uk_rc <- as.factor(r_uk_rc)
  r_eu_rc <- terra::classify(r_eu, m)
  r_eu_rc <- as.factor(r_eu_rc)
    
  # plot and save
  p <- ggplot()+
    geom_spatraster(data = r_uk_rc, na.rm = T)+
	geom_spatraster(data = r_eu_rc, na.rm = T, alpha = 0.6)+
	scale_fill_brewer(labels = v_labs, palette = "Spectral", 
	                  breaks = 1:length(v_q), direction = brew_d)+
	scale_y_continuous(expand = c(0, 0)) +
	scale_x_continuous(expand = c(0, 0)) +
	labs(fill = bquote(tonnes~a^-1), y = "", x = "")+
	# ggtitle(bquote("Emissions of"~.(p)~a^-1))+
	geom_sf(data = sf_uk, fill = NA, colour = "black")+
	#facet_wrap(~lyr, nrow = 1)+
	theme_bw()+
	  {if(leg.pos == "bottom")guides(fill = guide_legend(nrow = 2))}+
	  theme(#plot.title = element_text(size = 30, face = "bold"),
	  axis.text = element_text(size=12),
	  legend.text = element_text(size=18),
	  legend.title = element_text(size=24),
	  legend.position = leg.pos,
	  #plot.margin = grid::unit(c(2,2,2,2), "mm"),
	  margin(t = 2, r = 2, b = 2, l = 2, unit = "mm"))
  
  fname <- paste0(uk_folname,"/plots/e",y,"/",
                  dt_poll[ceh_poll == species, emep_model],"_UKEIRE_",
				  y,"emis_",map_yr_uk,"map_",naei_inv,"inv_1_UKTOTMAP.png")
  
  ggsave(fname, p, width = 10.5, height = 14)
     
  return(p)

}

################################################################################
#### 2. function to plot annual emissions summary from inventory (bar).
uk_bar_inv_ann <- function(fname_inv, fname_mask, y, species, 
                         uk_folname, map_yr_uk, naei_inv){
      
  # summarise the inventory information.
  dt_inv <- fread(fname_inv)
  
  dt_inv_t <- dt_inv[, lapply(.SD, sum, na.rm=T), by = .(Area, mask, Pollutant, 
                              data_source, emis_y, inv_y), 
                              .SDcols = c("emis_t_inv_spatial",
							        "emis_t_inv_table","emis_t_spatial_scaled")]
							  
  dt_inv_m <- melt(dt_inv_t, id.vars = c("Area","mask","Pollutant",
                                         "data_source","emis_y","inv_y"),
				   variable.name = "stage", value.name = "emis_t")
  
  # summarise the read and masked uk/eire data
  dt_mask <- fread(fname_mask)
  
  dt_mask_t <- dt_mask[, lapply(.SD, sum, na.rm=T), by = .(Area, mask, 
                                Pollutant, data_source, emis_y, inv_y), 
                                .SDcols = c("emis_t_tot_masked","tsum")]
								
  setnames(dt_mask_t, "tsum", "mon_masked")
  
  dt_mask_m <- melt(dt_mask_t, id.vars = c("Area","mask","Pollutant",
                                           "data_source","emis_y","inv_y"),
				    variable.name = "stage", value.name = "emis_t")
  
  # put into one table   
  dt <- rbindlist(list(dt_inv_m, dt_mask_m), use.names = T)  
  dt[, stage := gsub("emis_t_", "", stage) ]
  dt[, stage := factor(stage, levels = c("inv_spatial","inv_table",
                                   "spatial_scaled","tot_masked","mon_masked"))]
  dt[,mask := factor(mask, levels = c("outwith", "sea", "terrestrial", "all"))]
  
  # labels text
  dt_text <- data.table(Area = rep(c("uk", "ie"), 5),
	                    stage = rep(c("inv_spatial","inv_table",
                                   "spatial_scaled","tot_masked","mon_masked"),
								   each = 2),
						emis_t = c(dt[, sum(emis_t), 
						             by = .(Area, stage)][,V1]),	
						label = as.character(round(dt[, sum(emis_t)/1000, 
						             by = .(Area, stage)][,V1], 1)))
  
  # plot
  p <- ggplot()+
    geom_bar(data = dt, aes(x = stage, y = emis_t/1000, group = mask, fill= mask), stat = "identity")+
	scale_fill_manual(values = c("#eecea6","#59aed3","#6fbe6d","#ea93ea"))+
    labs(y = bquote(kt~a^-1))+	
    facet_wrap(~Area, nrow=1, scales = "free_y")+
	geom_text(data = dt_text, aes(x = stage, y = emis_t/1000, label = label), 
	          size = 5)+
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
  
  fname <- paste0(uk_folname,"/plots/e",y,"/",
                  dt_poll[ceh_poll == species, emep_model],"_UKEIRE_",
				  y,"emis_",map_yr_uk,"map_",naei_inv,"inv_2_UKINVBAR.png")
  
  ggsave(fname, p, width = 12, height = 6)
  
  return(list("plot" = p, "table" = dt))
 
}

################################################################################
#### 3. function to plot the emissions inside the netCDF file (bar).
uk_bar_nc_ann <- function(fname_group, fname_ncinp, fname_ncout, 
                          y, species, uk_folname, map_yr_uk, 
						  naei_inv, dt_emis){
      
  # area masks, bring over the total masked, from plot 1, grouped by mask area
  dt_p1_tot <- dt_emis[stage == "tot_masked", 
                       list(emis_t = sum(emis_t, na.rm=T)), 
					 by = .(Area, Pollutant, data_source, emis_y, inv_y, stage)]
  
  # summarise processed data, grouped by mask areas
  dt_group <- fread(fname_group)
  dt_group_t <- dt_group[, lapply(.SD, sum, na.rm=T), 
                           by = .(Area, Pollutant, data_source, emis_y, inv_y), 
						   .SDcols = c("emis_t_tot_grouped","tsum")]
						   
  setnames(dt_group_t, "tsum", "mon_grouped")
  
  dt_group_m <- melt(dt_group_t, id.vars = c("Area","Pollutant",
                                             "data_source","emis_y","inv_y"),
				     variable.name = "stage", value.name = "emis_t")
  
  # summarise data input to nc file  
  dt_ncinp <- fread(fname_ncinp)
  
  # minor changes to summary file
  dt_ncinp[, Area := tolower(Area)] # lower case the areas
  dt_ncinp[Area == "gb", Area := "uk"] # change gb back to uk (from nc file)
  
  dt_ncinp_t <- dt_ncinp[, lapply(.SD, sum, na.rm=T), by = .(Area, Pollutant, 
                                  data_source, emis_y, inv_y), 
						          .SDcols = c("emis_t_tot_ncinput",
								              "emis_t_tot_array","tsum")]
						   
  setnames(dt_ncinp_t, "tsum", "time_layers")  
  
  dt_ncinp_m <- melt(dt_ncinp_t, id.vars = c("Area","Pollutant",
                                             "data_source","emis_y","inv_y"),
				     variable.name = "stage", value.name = "emis_t")
  
  # summary data that has been read from output nc file (i.e. separate process)
  dt_ncout <- fread(fname_ncout)  
  
  # minor changes to summary file
  dt_ncout[, Area := tolower(Area)] # lower case the areas
  dt_ncout[Area == "gb", Area := "uk"] # change gb back to uk (from nc file)  
  
  dt_ncout_t <- dt_ncout[time_res == "annual", lapply(.SD, sum, na.rm=T), 
                                             by = .(Area, Pollutant, 
											        data_source, emis_y, inv_y), 
											 .SDcols = c("emis_t_tot_ncoutput")]
    
  dt_ncout_m <- melt(dt_ncout_t, id.vars = c("Area","Pollutant",
                                             "data_source","emis_y","inv_y"),
				     variable.name = "stage", value.name = "emis_t")
					 
  # put into one table
  dt <- rbindlist(list(dt_p1_tot, dt_group_m, 
                       dt_ncinp_m, dt_ncout_m), use.names = T)  
					   
  dt[, stage := gsub("emis_t_", "", stage) ]
  
  dt[, stage := factor(stage, levels = c("tot_masked", "tot_grouped",
                                         "mon_grouped", "tot_ncinput",
										 "tot_array", "time_layers", 
										 "tot_ncoutput"))]
										 
  dt[, Area := factor(Area, levels = c("ow","sea","ie","uk"))]
  
  # labels text
  dt_text <- data.table(stage = c("tot_masked", "tot_grouped",
                                         "mon_grouped", "tot_ncinput",
										 "tot_array", "time_layers", 
										 "tot_ncoutput"),
						emis_t = c(dt[, sum(emis_t), 
						             by = .(stage)][,V1]),	
						label = as.character(round(dt[, sum(emis_t)/1000, 
						             by = .(stage)][,V1], 1)))
  
  # plot
  p <- ggplot()+
    geom_bar(data = dt, aes(x = stage, y = emis_t/1000, group = Area, 
	                        fill = Area), 
	         stat = "identity")+
	scale_fill_manual(values = c("#eecea6","#59aed3","#8aee87","#6cb96a"),
	                  labels = c("outwith","sea","IE (Terres)","UK (Terres)"))+
	labs(y = bquote(kt~a^-1))+
	geom_text(data = dt_text, aes(x = stage, y = emis_t/1000, label = label), 
	          size = 5)+
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
  
  fname <- paste0(uk_folname,"/plots/e",y,"/",
                  dt_poll[ceh_poll == species, emep_model],"_UKEIRE_",
				  y,"emis_",map_yr_uk,"map_",naei_inv,"inv_3_UKNCBAR.png")
  
  ggsave(fname, p, width = 12, height = 6)
  
  return(p)
   
}

################################################################################
#### 4. function to plot monthly total pollutant in UK, IE and SEA (line).
uk_lin_tot_mon <- function(fname_ncout, y, species, uk_folname, 
                       map_yr_uk, naei_inv, time_dim, emep_version){
  
  # only using the NC output file summary data
  
  if(time_dim == "annual"){
	i_time <- 1
  }else if(time_dim == "month"){
	i_time <- 1:12
  }else{
	i_time <- 1:365
  }
  
  # read
  dt_ncout <- fread(fname_ncout)
  
  # minor changes to summary file
  dt_ncout[, Area := tolower(Area)] # lower case the areas
  dt_ncout[Area == "gb", Area := "uk"] # change gb back to uk (from nc file) 
 
  # re-shape
  dt_ncout[, c("tsum","tot_tres_ratio") := NULL]
  
  # keep cols and melt cols
  v_col_melt <- setdiff(names(dt_ncout), c(paste0("t",1:12))) # month regardless
  v_col_keep <- c(v_col_melt, paste0("t",1:12))
    
  # if time_dim is 'annual', files have been made with annual total emissions
  # this means we should use the EMEP model version temporal data for plotting
  if(time_dim == "annual"){
   
   # copy the NC input summary to manipulate it without altering the original
   dt_ncout_modpro <- copy(dt_ncout)
   
   # set the SEA area to be ISO GB/27 for this plot.
   # annual inputs don't carry through parent monthly from processing (UK or IE)
   dt_ncout_modpro[Area == "sea", iso_code := 27]
   
   # remove the current t1 to replace with t1:t12
   dt_ncout_modpro[, t1 := NULL] 
   
   dt_ncout_modpro[, snap := dt_sec$SNAP[match(sec_num, dt_sec[,sec])] ]
   
   # name the EMEP model timings file
   folname_prof <- paste0("data/temporal/EMEP4UK",emep_version,"/MonthlyFacs.",
						  dt_poll[ceh_poll == species, emep_model])
   
   # read in the EMEP model timings
   dt_prof <- fread(folname_prof)   
   names(dt_prof) <- c("iso_code", "snap", paste0("t", 1:12))
   
   # join data - only relevant isos should carry
   dt_nc_month <- dt_prof[dt_ncout_modpro, on = c("iso_code", "snap")]
   
   # for emissions, divide 'emis_t_tot_ncoutput' by 12 and multiply by factors
   time_cols <- paste0("t",1:12)
   dt_nc_month[ , (time_cols) := lapply(.SD, function(x) 
                                             (emis_t_tot_ncoutput / 12) * x), 
											 .SDcols = time_cols]
   
   # sum check
   dt_nc_month[, tsum := rowSums(.SD, na.rm=T), .SDcols = time_cols]
   dt_nc_month[, inp_prof_ratio := emis_t_tot_ncoutput / tsum]
   
   # check profiled monthly vs total
   if(any(dt_nc_month$inp_prof_ratio < 0.99 | dt_nc_month$inp_prof_ratio > 1.01,
                                                                      na.rm=T)){
     stop("profiling total with model timings has gone wrong")
   }
   
   # subset and melt
   dt_nc_month <- dt_nc_month[,..v_col_keep]
   dt_ncout_m <- melt(dt_nc_month, id.vars = v_col_melt, 
                                             variable.name = "time", 
											 value.name = "emis_t")
  
  }else{
  
    # subset and melt
    dt_nc_month <- dt_ncout[,..v_col_keep]
    dt_ncout_m <- melt(dt_nc_month, id.vars = v_col_melt, 
                                             variable.name = "time", 
											 value.name = "emis_t")
  
  }
  
  # sum by snap & iso  
  dt_out_sum <- dt_ncout_m[, list(emis_kt = sum(emis_t, na.rm=T)/1000), 
                           by = .(Area, data_source, time)]
  dt_out_sum[, time := as.numeric(gsub("t","",time))]
  dt_out_sum[, line_type := 1]
  
  # plot info
  dt_out_sum[, Area := factor(Area, levels = c("uk","ie","sea"))]
  
  # plot annotations
  if(time_dim == "annual"){
  
    dt_text <- data.table(Area = dt_out_sum[,max(emis_kt), by = Area][["Area"]],
	                      time = 6, 
						  fac = dt_out_sum[,max(emis_kt), by = Area][["V1"]], 
						  source = dt_out_sum[, unique(data_source)], 
						  label = paste0("Annual input file: using EMEP",
						                 emep_version," profiles"))
	
  }else{
  
  }
  
  
  # plot
  p <- ggplot(data = dt_out_sum, aes(x = time, y = emis_kt))+
    geom_line()+
    geom_point()+
    scale_x_continuous(breaks = 1:12)+
    facet_wrap(~Area, scales = "free_y", ncol = 1)+
	labs(y = bquote(kt~a^-1))+
	geom_text(data = dt_text, aes(x = time, y = fac, label = label), size = 5)+
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
	
  fname <- paste0(uk_folname,"/plots/e",y,"/",
				  dt_poll[ceh_poll == species, emep_model],"_UKEIRE_",
				  y,"emis_",map_yr_uk,"map_",naei_inv,"inv_4_UKMONTOTLINE.png")
  
  ggsave(fname, p, width = 10, height = 7)
  
  # return the plot and non-aggregated table for other plots
  return(list("plot" = p, "table" = dt_ncout_m))

}

################################################################################
#### 5. function to plot monthly pollutant in UK by sector and by Area (line).
uk_lin_sec_mon <- function(dt_month, y, species, uk_folname, 
                           map_yr_uk, naei_inv, time_dim, emep_version){

  # The data being used is that summarised in function 4. 

  if(time_dim == "annual"){
	i_time <- 1
  }else if(time_dim == "month"){
	i_time <- 1:12
  }else{
	i_time <- 1:365
  }
  
  # No summary step
  
  # plot info
  dt_month[, emis_kt := emis_t/1000]
  dt_month[, time := as.numeric(gsub("t","",time))]
  dt_month[, line_type := 1]
  dt_month[, Area := factor(Area, levels = c("uk","ie","sea"))]
  

  # list for plots
  l_p <- list()
  
  for(i in dt_month[,unique(sec_name)]){
  
  g1 <- ggplot(data = dt_month[sec_name == i & Area != "sea"], 
               aes(x = time, y = emis_kt))+
    geom_line()+
    geom_point()+
    ggtitle(i)+
	labs(y = bquote(kt~a^-1))+	
    scale_x_continuous(breaks = 1:12)+
    facet_wrap(~Area, scales = "free_y", ncol = 1)+
	theme_bw()+
    month_sector_theme(sector = i)
  
   l_p[[i]] <- g1
  
  }
  
  # plot
  label_plot <- ggdraw() + draw_label(paste0("Annual input file:\n using EMEP",
                                             emep_version," profiles"),
									  x = 0.1, y = 0.65, size = 24)

  p <- l_p[[1]] + l_p[[2]] + l_p[[3]] + l_p[[4]] + l_p[[5]] + l_p[[6]] + 
       l_p[[7]] + l_p[[8]] + l_p[[9]] + l_p[[10]] + l_p[[11]] + l_p[[12]] + 
	   l_p[[13]] + plot_spacer() + label_plot + plot_layout(ncol = 5)
	
  fname <- paste0(uk_folname,"/plots/e",y,"/",
				  dt_poll[ceh_poll == species, emep_model],"_UKEIRE_",
				  y,"emis_",map_yr_uk,"map_",naei_inv,"inv_5_UKMONSECLINE.png")
  
  ggsave(fname, p, width = 18, height = 12)
  
  return(p)

}

###############################################################################
#### 6. function to plot annual total sector pollutant across UK domain (maps)
uk_map_sec_ann <- function(l_data_uk, l_data_eu, y, species, uk_folname, 
                          map_yr_uk, naei_inv, emep_inv){
  
  # transpose the data so it is l[[sector]][[ISOs]] (not l[[ISO]][[sector]])
  l_data_uk_t <- list_transpose(l_data_uk)
  l_data_eu_t <- list_transpose(l_data_eu)
  
  # collapse to UKEIRE totals, per sector (and same for EU)
  s_data_uk_t <- rast(lapply(l_data_uk_t, function(x) app(rast(x), 
                                                          sum, na.rm=T)))
  s_data_eu_t <- rast(lapply(l_data_eu_t, function(x) app(rast(x), 
                                                          sum, na.rm=T)))

  # stack the annual UK sector data
  # extend to larger UK plot domain
  s_data_uk_t[s_data_uk_t == 0] <- NA
  s_uk <- extend(s_data_uk_t, ext(r_dom_ukplot))
  
  # stack the annual EU ISO sum data and sum to one EU surface
  # crop to larger UK plot domain
  s_data_eu_t[s_data_eu_t == 0] <- NA
  s_eu <- crop(s_data_eu_t, ext(r_dom_ukplot)) 
 
  # disaggregate the EU map to 0.01 degree
  s_eu <- disagg(s_eu, 10)
  s_eu <- s_eu/100
  
  ## reclassify and plot ##  
  # exract all the values present, into a vector. 
  v_v <- values(c(s_uk, s_eu))
  v_v <- as.vector(v_v)

  # create break points based on quantiles
  v_q <- as.vector(quantile(v_v, probs = c(0.20,0.40,0.55,0.7,0.82,0.92,0.97,1),
                            na.rm=T))  

  # create a reclassification matrix and labels for the plot.
  leg.pos <- "bottom"
  m <- matrix(c(0, v_q[1:7], v_q[1:7], 
              ceiling(max(global(c(s_uk, s_eu), max, na.rm=T)$max, na.rm=T)),
              1:8), ncol = 3) 
  brew_d = -1
  v_labs <- unlist(lapply(1:nrow(m), function(x) paste0(round(m[x,1],2), " - ",
                                                      round(m[x,2], 2))))
  v_labs[length(v_labs)] <- paste0("> ",round(m[nrow(m),1],2))
  
  # reclassify the data
  s_uk_rc <- terra::classify(s_uk, m)
  s_uk_rc <- as.factor(s_uk_rc)
  s_eu_rc <- terra::classify(s_eu, m)
  s_eu_rc <- as.factor(s_eu_rc)
  
  # names
  if(nlyr(s_uk_rc) != 13) stop("There should be 13 layers in the UK map data")
  if(nlyr(s_eu_rc) != 13) stop("There should be 13 layers in the EU map data")
    
  # An issue with reclassification is that if the first layer does not have 
  # all the classes in the re-classification, the subsequent image/legend 
  # will go haywire. 
  
  # get all the levels. Change NULL to NA to preserve full layer vector. 
  l_levels <- (lapply(levels(s_uk_rc), function(x) nrow(x)))
  l_levels[sapply(l_levels, is.null)] <- NA
  
  # get the layer with biggest set of factors (just frist instance will do)
  i_max <- which.max(unlist(l_levels))
    
  levels(s_uk_rc) <- lapply(1:length(levels(s_uk_rc)),
                            function(x) levels(s_uk_rc)[[i_max]])
  levels(s_uk_rc) <- lapply(1:length(levels(s_uk_rc)), 
                            function(x) setNames(levels(s_uk_rc)[[x]], 
							                     c("ID",x)))
  
  levels(s_eu_rc) <- lapply(1:length(levels(s_eu_rc)),  
                            function(x) levels(s_eu_rc)[[i_max]])
  levels(s_eu_rc) <- lapply(1:length(levels(s_eu_rc)), 
                            function(x) setNames(levels(s_eu_rc)[[x]], 
							                     c("ID",x)))
  
  # rename to sectors
  names(s_uk_rc) <- dt_sec[, GNFRlong][1:13]
  names(s_eu_rc) <- dt_sec[, GNFRlong][1:13]
  
  
  # plot and save
  p <- ggplot()+
    geom_spatraster(data = s_uk_rc, na.rm = T)+
	geom_spatraster(data = s_eu_rc, na.rm = T, alpha = 0.6)+
	scale_fill_brewer(labels = v_labs, palette = "Spectral", 
	                  breaks = 1:length(v_q), direction = brew_d)+
	scale_y_continuous(expand = c(0, 0)) +
	scale_x_continuous(expand = c(0, 0)) +
	ggtitle("For UKEIRE: G_, H_ & I_ are all in I_, while agri all in K_")+
	labs(fill = bquote(tonnes~a^-1), y = "", x = "")+
	# ggtitle(bquote("Emissions of"~.(p)~a^-1))+
	geom_sf(data = sf_uk, fill = NA, colour = "black")+
	facet_wrap(~lyr, nrow = 3)+
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
  
  fname <- paste0(uk_folname,"/plots/e",y,"/",
                  dt_poll[ceh_poll == species, emep_model],"_UKEIRE_",
				  y,"emis_",map_yr_uk,"map_",naei_inv,"inv_6_UKANNSECMAP.png")
  
  ggsave(fname, p, width = 14, height = 14)
   
  return(p)							
}

###############################################################################
#### 7. function to plot monthly total pollutant across UK domain (maps).
uk_map_tot_mon <- function(dt_month, l_uk_maps, y, 
                          species, uk_folname, map_yr_uk, 
						  naei_inv, time_dim, emep_version){
						  
  # if time_dim == "annual", we have to use the EMEP profiles to
  # split out the annual map data (l_uk_maps[[ann_tot_all]]).
  # But if time_dim == "month", we can use the month maps directly
  # from the list (l_uk_maps[[mon_tot_all]]). 
  
  # process data by time_dim choice
  if(time_dim == "annual"){
  
    # We can use monthly emissions as calculated in function 4. 
	# We have to use the sector splits against sector totals, to get
	# the correct annual pattern of total emissions. 
	
	dt_iso_mon <- dt_month[, list(emis_kt = sum(emis_kt, na.rm=T)), 
	                         by = .(Area, time)]
    # index the emissions
	dt_iso_mon[, fac := emis_kt/mean(emis_kt, na.rm=T), by = Area]
	dt_iso_mon <- dt_iso_mon[order(time)]
	
	# apply the factors to the country total maps. 
	# do this in a loop to explicitly match name, don't rely on list position. 
	l_montot <- list()
	
	for(j in c("GB", "IE", "SEA")){ # these are names in ncfile
	
	  r <- l_uk_maps[["ann_tot_all"]][[paste0(species,"_",j)]]
	  # extend to larger UK plot domain
      r[r == 0] <- NA
      r <- extend(r, ext(r_dom_ukplot))
	  
	  if(j == "GB") v_fac <- dt_iso_mon[Area == "uk"]
	  if(j != "GB") v_fac <- dt_iso_mon[Area == tolower(j)]
	  
	  l <- lapply(v_fac$fac, function(x) (r/12) * x)
	  
	  l_montot[[j]] <- l
	  # dont collapse as we need to transpose. 
	}
	
	# transpose to months and stack sum those month lists. 
	l_montot <- list_transpose(l_montot)
	l_montot <- lapply(l_montot, function(x) app(rast(x), sum, na.rm=T))
	s_uk <- rast(l_montot)
    
  }else if(time_dim == "month"){
  
   # this will need writing
   # make s_uk
  
  }
  
  
  ## reclassify and plot ##  
  # exract all the values present, into a vector. 
  v_v <- values(c(s_uk))
  v_v <- as.vector(v_v)
    
  # create break points based on quantiles
  v_q <- as.vector(quantile(v_v, probs = c(0.20,0.40,0.55,0.7,0.82,0.92,0.97,1),
                            na.rm=T))  

  # create a reclassification matrix and labels for the plot.
  leg.pos <- "bottom"
  m <- matrix(c(0, v_q[1:7], v_q[1:7], 
                ceiling(max(global(c(s_uk), max, na.rm=T)$max, na.rm=T)),
                1:8), ncol = 3) 
  brew_d = -1
  v_labs <- unlist(lapply(1:nrow(m), function(x) paste0(round(m[x,1],2), " - ",
                                                        round(m[x,2], 2))))
  v_labs[length(v_labs)] <- paste0("> ",round(m[nrow(m),1],2))

  # reclassify the data
  s_uk_rc <- terra::classify(s_uk, m)
  s_uk_rc <- as.factor(s_uk_rc)
  
  if(nlyr(s_uk_rc) != 12) stop("There should be 12 layers in the UK map data")

  # An issue with reclassification is that if the first layer does not have 
  # all the classes in the re-classification, the subsequent image/legend 
  # will go haywire. 

  # get all the levels. Change NULL to NA to preserve full layer vector. 
  l_levels <- (lapply(levels(s_uk_rc), function(x) nrow(x)))
  l_levels[sapply(l_levels, is.null)] <- NA

  # get the layer with biggest set of factors (just first instance will do)
  i_max <- which.max(unlist(l_levels))

  levels(s_uk_rc) <- lapply(1:length(levels(s_uk_rc)),
                            function(x) levels(s_uk_rc)[[i_max]])
  levels(s_uk_rc) <- lapply(1:length(levels(s_uk_rc)), 
                            function(x) setNames(levels(s_uk_rc)[[x]], 
                                                 c("ID",x)))

  # rename to months
  names(s_uk_rc) <- 1:12
    
  # plot and save
  p <- ggplot()+
    geom_spatraster(data = s_uk_rc, na.rm = T)+
	#geom_spatraster(data = sEU_fac, na.rm = T, alpha = 0.6)+
	scale_fill_brewer(labels = v_labs, palette = "Spectral", 
	                  breaks = 1:length(v_q), direction = brew_d)+
	scale_y_continuous(expand = c(0, 0)) +
	scale_x_continuous(expand = c(0, 0)) +
	labs(fill = bquote(tonnes~m^-1), y = "", x = "")+
	ggtitle(paste0("Annual input file: using EMEP",
				   emep_version," profiles"))+
	geom_sf(data = sf_uk, fill = NA, colour = "black")+
	facet_wrap(~lyr, nrow = 3)+	
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
  
  fname <- paste0(uk_folname,"/plots/e",y,"/",
                  dt_poll[ceh_poll == species, emep_model],"_UKEIRE_",
				  y,"emis_",map_yr_uk,"map_",naei_inv,"inv_7_UKMONTOTMAP.png")
  
  ggsave(fname, p, width = 13, height = 12)
   
  return(p)
}

##################
#### EU PLOTS ####
##################

###############################################################################
#### 8. function to plot the EU mapped data, total for the species/year (map).
plot_eu_map_ann <- function(y, species, uk_folname, eu_folname, map_yr_uk, 
							naei_inv, emep_inv, time_dim){
  
  print(paste0(format(Sys.time(), "%Y-%m-%d %X"),":      Annual total emissions map - EU..."))  
  
  if(time_dim == "annual"){
	i_time <- 1
  }else if(time_dim == "month"){
	i_time <- 1:12
  }else{
	i_time <- 1:365
  }
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

###############################################################################
#### 9. function to plot the EU emissions coming from EMEP and in the ncfile
plot_eu_nc_emis <- function(){

# 9_
}

###############################################################################
#### 10. function to plot monthly total pollutant in EU (line).
plot_eu_tot_mon <- function(){

# 10_
}

###############################################################################
#### 11. function to plot monthly pollutant in EU by sector and by Area (line).
plot_eu_mon_sec <- function(){

# 11_
}

###############################################################################
#### 12. function to plot annual total sector pollutant across EU domain (maps).
plot_eu_sec_ann <- function(y, species, uk_folname, map_yr_uk, naei_inv, 
							emep_inv, time_dim, sEU){
  
  print(paste0(format(Sys.time(), "%Y-%m-%d %X"),":      Annual sectoral total emissions maps - EU..."))    
							
  
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

###############################################################################
#### 13. function to plot monthly total pollutant across EU domain (maps).
plot_eu_map_mon <- function(){

# 13_
}

###############################################################################
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



  writeLines(c(paste0("File creation timestamp: ", format(Sys.time(), "%Y-%m-%d %X")),
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




