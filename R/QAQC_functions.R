################################################################################
#### master function to generate QAQC
create_qaqc <- function(
  project,
  scenario,
  y,
  species,
  uk_folname,
  eu_folname,
  map_yr_uk,
  naei_inv,
  emep_inv,
  time_dim,
  emep_version,
  v_EMEP_sec,
  uk_agg_schema,
  eu_agg_schema,
  tp_scheme
) {
  # lapply the QAQC over v_pollutants

  print(paste0(
    format(Sys.time(), "%Y-%m-%d %X"),
    ": Creating QAQC for ",
    dt_poll[ceh_poll == species, emep_model],
    " in ",
    y,
    "..."
  ))

  # folders needed
  dir.create(
    file.path(uk_folname, "plots", paste0("e", y)),
    showWarnings = FALSE,
    recursive = T
  )
  dir.create(
    file.path(uk_folname, "qaqc", paste0("e", y)),
    showWarnings = FALSE,
    recursive = T
  )
  dir.create(
    file.path(eu_folname, "plots", paste0("e", y)),
    showWarnings = FALSE,
    recursive = T
  )
  dir.create(
    file.path(eu_folname, "qaqc", paste0("e", y)),
    showWarnings = FALSE,
    recursive = T
  )

  #######################
  #### DATA EXTRACTS ####
  #######################

  # species <- "nox" #

  print(paste0(
    format(Sys.time(), "%Y-%m-%d %X"),
    ":            collecting data - UK..."
  ))

  # summary tables - filenames to stop overwriting when using.
  l_fname_uk_sums <- collect_uk_summaries(
    y,
    species,
    uk_folname,
    map_yr_uk,
    naei_inv
  )

  # annual total (sectors) / annual total (all) / monthly totals (all)
  l_uk_maps <- extract_uk_maps(
    y,
    species,
    uk_folname,
    naei_inv,
    emep_version,
    time_dim,
    v_EMEP_sec,
    uk_agg_schema
  )

  print(paste0(
    format(Sys.time(), "%Y-%m-%d %X"),
    ":            collecting data - EU..."
  ))

  # summary tables - filenames to stop overwriting when using.
  l_fname_eu_sums <- collect_eu_summaries(y, species, eu_folname, emep_inv)

  # annual total (sectors) / annual total (all) / monthly totals (all)
  l_eu_maps <- extract_eu_maps(
    y,
    species,
    eu_folname,
    emep_inv,
    time_dim,
    v_EMEP_sec,
    eu_agg_schema
  )

  ##################
  #### UK PLOTS ####
  ##################

  ## plot names follow: region_plottype_aggregation_temporal

  print(paste0(
    format(Sys.time(), "%Y-%m-%d %X"),
    ":            maps and plots - UK..."
  ))

  # 1. Map; UK emissions, total cell-1 annum-1.
  gg_p1 <- uk_map_tot_ann(
    l_data_uk = l_uk_maps[["ann_tot_all"]],
    l_data_eu = l_eu_maps[["ann_tot_all"]],
    y,
    species,
    uk_folname,
    map_yr_uk,
    naei_inv,
    emep_inv
  )

  # 2. Bar plot; UK/IE emissions processing check-plot
  l_gg_p2 <- uk_bar_inv_ann(
    fname_inv = l_fname_uk_sums[["inventory"]],
    fname_mask = l_fname_uk_sums[["masked"]],
    y,
    species,
    uk_folname,
    map_yr_uk,
    naei_inv
  )

  gg_p2 <- l_gg_p2[["plot"]]

  # 3. Bar plot; UK/IE emissions going into netCDF and those inside the netCDF
  gg_p3 <- uk_bar_nc_ann(
    fname_group = l_fname_uk_sums[["processed"]],
    fname_ncinp = l_fname_uk_sums[["ncinput"]],
    fname_ncout = l_fname_uk_sums[["ncoutput"]],
    y,
    species,
    uk_folname,
    map_yr_uk,
    naei_inv,
    dt_emis = l_gg_p2[["table"]]
  )

  # 3b. Bar graph; changes to TOTAL inventory totals with alternate emissions (added after)
  gg_p3b <- uk_bar_tot_ann_alt(
    fname_in = l_fname_uk_sums[["inventory"]],
    fname_out = l_fname_uk_sums[["ncoutput"]]
  )

  # 3c. Bar graph; changes to SECTORAL inventory totals with alternate emissions (added after)
  gg_p3c <- uk_bar_sec_ann_alt(fname = l_fname_uk_sums[["inventory"]])

  # 4. Line plot; UK/IE emissions of total by month in netcdf
  # if(time_dim == "annual") ; produce a plot using the EMEP version profiles
  l_gg_p4 <- uk_lin_tot_mon(
    fname_ncout = l_fname_uk_sums[["ncoutput"]],
    y,
    species,
    uk_folname,
    map_yr_uk,
    naei_inv,
    time_dim,
    emep_version
  )

  gg_p4 <- l_gg_p4[["plot"]]

  # 5. Line plot; UK/IE emissions of total by sector by month in netcdf
  # if(time_dim == "annual") ; produce a plot using the EMEP version profiles.
  gg_p5 <- uk_lin_sec_mon(
    dt_month = l_gg_p4[["table"]],
    y,
    species,
    uk_folname,
    map_yr_uk,
    naei_inv,
    time_dim,
    emep_version
  )

  # 6. Maps; UK/IE (& EU) emissions, sector total cell-1 annum-1.
  gg_p6 <- uk_map_sec_ann(
    l_data_uk = l_uk_maps[["ann_tot_sec"]],
    l_data_eu = l_eu_maps[["ann_tot_sec"]],
    y,
    species,
    uk_folname,
    map_yr_uk,
    naei_inv,
    emep_inv
  )

  # 7. Maps; UK/IE (& EU) emissions, monthly total cell-1 annum-1.
  # if(time_dim == "annual") ; produce maps using the EMEP version profiles.
  # ignore EU for this plot, gets too complicated re timing files.
  gg_p7 <- uk_map_tot_mon(
    dt_month = l_gg_p4[["table"]],
    l_uk_maps,
    y,
    species,
    uk_folname,
    map_yr_uk,
    naei_inv,
    time_dim,
    emep_version
  )

  ############
  #### EU ####
  ############

  ## plot names follow: region_plottype_aggregation_temporal

  print(paste0(
    format(Sys.time(), "%Y-%m-%d %X"),
    ":            maps and plots - EU..."
  ))

  # 8. Map; EU emissions, total cell-1 annum-1.
  gg_p8 <- eu_map_tot_ann(
    l_data_uk = l_uk_maps[["ann_tot_all"]],
    l_data_eu = l_eu_maps[["ann_tot_all"]],
    y,
    species,
    eu_folname,
    emep_inv
  )

  # 9. Bar plot; EU emissions processing check-plot
  l_gg_p9 <- eu_bar_inv_ann(
    fname_inv = l_fname_eu_sums[["inventory"]],
    fname_proc = l_fname_eu_sums[["processed"]],
    y,
    species,
    eu_folname,
    emep_inv
  )

  gg_p9 <- l_gg_p9[["plot"]]

  # 10. Bar plot; EU emissions going into netCDF and those inside the netCDF
  gg_p10 <- eu_bar_nc_ann(
    fname_ncinp = l_fname_eu_sums[["ncinput"]],
    fname_ncout = l_fname_eu_sums[["ncoutput"]],
    y,
    species,
    eu_folname,
    emep_inv,
    dt_emis = l_gg_p9[["table"]]
  )

  # 11. Line plot; EU emissions of total by month in netcdf
  # if(time_dim == "annual") ; produce a plot using the EMEP version profiles
  l_gg_p11 <- eu_lin_tot_mon(
    fname_ncout = l_fname_eu_sums[["ncoutput"]],
    y,
    species,
    eu_folname,
    emep_inv,
    time_dim,
    emep_version
  )

  gg_p11 <- l_gg_p11[["plot"]]

  # 12. Line plot; EU emissions of total by sector by month in netcdf
  # only do for EU, not ISOs
  gg_p12 <- eu_lin_sec_mon(
    dt_month = l_gg_p11[["table"]],
    y,
    species,
    eu_folname,
    emep_inv,
    time_dim,
    emep_version
  )

  # 13. Maps; EU emissions, sector total cell-1 annum-1.
  gg_p13 <- eu_map_sec_ann(
    l_data_uk = l_uk_maps[["ann_tot_sec"]],
    l_data_eu = l_eu_maps[["ann_tot_sec"]],
    y,
    species,
    eu_folname,
    map_yr_uk,
    naei_inv,
    emep_inv
  )

  # 14. Maps; EU emissions, monthly total cell-1 annum-1.
  # if(time_dim == "annual") ; produce maps using the EMEP version profiles.
  #gg_p14 <- eu_map_tot_mon(dt_month = l_gg_p11[["table"]], l_eu_maps, y,
  #                          species, eu_folname,
  #					   emep_inv, time_dim, emep_version)

  # 15. line plots of total emissions and emissions per sector, over time.
  l_gg_p15 <- time_series_tot(
    y,
    species,
    naei_inv,
    emep_inv,
    emep_version,
    uk_folname,
    map_yr_uk
  )

  #############
  #### PDF ####
  #############

  print(paste0(
    format(Sys.time(), "%Y-%m-%d %X"),
    ":            rendering pdf..."
  ))

  l_pdf_params <- list(
    project = project,
    scenario = scenario,
    y = y,
    dt_poll = dt_poll,
    species = species,
    uk_folname = uk_folname,
    eu_folname = eu_folname,
    map_yr_uk = map_yr_uk,
    naei_inv = naei_inv,
    emep_inv = emep_inv,
    v_EMEP_sec = v_EMEP_sec,
    dt_sec = dt_sec,
    time_dim = time_dim,
    emep_version = emep_version,
    uk_agg_schema = uk_agg_schema,
    eu_agg_schema = eu_agg_schema,
    tp_scheme = tp_scheme,
    l_fname_uk_sums = l_fname_uk_sums,
    l_fname_eu_sums = l_fname_eu_sums,
    l_uk_maps = l_uk_maps,
    l_eu_maps = l_eu_maps,
    dt_month_uk = l_gg_p4[["table"]],
    dt_month_eu = l_gg_p11[["table"]],
    dt_ts_tots = l_gg_p15[["totals_table"]]
  )

  # render the source of the document to the default output format:
  rmarkdown::render(
    input = "R/QAQC.Rmd",
    output_file = paste0(
      dt_poll[ceh_poll == species, emep_model],
      "_",
      y,
      "emis_",
      naei_inv,
      "inv_QAQC.pdf"
    ),
    output_dir = paste0(uk_folname, "/qaqc/e", y),
    params = l_pdf_params
  )

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
extract_uk_maps <- function(
  y,
  species,
  uk_folname,
  naei_inv,
  emep_version,
  time_dim,
  v_EMEP_sec,
  uk_agg_schema
) {
  if (time_dim == "annual") {
    i_time <- 1
  } else if (time_dim == "month") {
    i_time <- 1:12
  } else {
    i_time <- 1:365
  }

  # set the required ncdf filename for the UK - different for v4.36
  if (emep_version == "v4.36") {
    nc_fname <- paste0(
      "/UKEIRE_",
      species,
      "_",
      naei_inv,
      "inv_",
      y,
      "emis_0.01.nc"
    )
    nc_fname <- paste0(uk_folname, nc_fname)
  } else {
    nc_fname <- paste0("/UKEIRE_", naei_inv, "inv_", y, "emis_0.01.nc")
    nc_fname <- paste0(uk_folname, nc_fname)
  }

  # read all data.
  # gather sector info direct from input file. Yet another safety check.
  nc <- nc_open(nc_fname)
  v_var <- names(nc$var)
  if (emep_version != "v4.36") {
    v_var <- v_var[grep(paste0("^", species), v_var)]
  } # restrict to species.
  nc_close(nc)

  # l_all <- list()
  l_ann_sec <- list()
  l_ann_tot <- list()
  l_mon_tot <- list()

  if (emep_version == "v4.36") {
    # collect sectors differently for v4.36
    require(raster)

    # annual country totals, plus annual sector totals.
    for (j in c("GB", "IE", "SEA")) {
      v_sub <- v_var[grep(paste0("Emis:", j), v_var)]

      s <- suppressWarnings(lapply(v_sub, function(x) {
        xx <- raster(nc_fname, varname = x)
        xxNA <- copy(xx)
        xxNA[] <- NA
        xxr <- calc(stack(xx, xxNA), sum, na.rm = TRUE)
        xxr <- as(xxr, "SpatRaster")
      }))

      names(s) <- v_EMEP_sec

      # annual sector totals, per country
      l_ann_sec[[paste0(species, "_", j)]] <- s

      s <- rast(s)
      r <- app(s, sum, na.rm = TRUE)
      names(r) <- "annual"

      # annual total for the country
      l_ann_tot[[paste0(species, "_", j)]] <- r
    }
  } else {
    # code for post v4.36

    # loop through every sector and collect data.
    # Summarise where needed, make one surface, not ISO split.
    for (v in v_var) {
      ## extract and make stack
      # simply using rast() will read in all layers across sector & time dims,
      # into 1 big stack (can't handle 4D).
      # We can certainly work with this, but it might need to go via an array etc.
      # in the future.

      s <- suppressWarnings(rast(nc_fname, subds = v))

      # from the extract, split out the sectors into their own list elements,
      # in whatever form they come.
      l_out <- lapply(v_EMEP_sec, function(x) {
        s[[grep(paste0("sector=", x, "$"), names(s))]]
      })
      names(l_out) <- v_EMEP_sec
      # l_all[[v]] <- l_out # this isn't particularly needed.

      # sum each list element stack into one (annual per sector, if not already)
      l_outv2 <- suppressWarnings(lapply(l_out, function(x) {
        app(x, sum, na.rm = T)
      }))
      s_outv2 <- rast(l_outv2) # stack of annual sector maps
      l_ann_sec[[v]] <- l_outv2 # return the list, as we can transpose this later

      # sum those annual sector totals into one annual total
      s_outv3 <- suppressWarnings(app(s_outv2, sum, na.rm = T))
      names(s_outv3) <- "annual"
      l_ann_tot[[v]] <- s_outv3

      # also create a month by month total, across all sectors.
      if (time_dim == "month") {
        ## CODE NEEDED ##

        l_mon_tot[[v]] <- l_outv4
      }
    }
  }

  # now summarise to UK
  # l_ann_sec = list of 3 vars (ISO), each a stack of 13 secs annual total
  # l_ann_tot = list of 3 vars (ISO), each a sum of all sectors over all time
  # l_mon_tot = list of 3 vars (ISO), each a stack-sum all sectors over 12 month

  return(list(
    "ann_tot_sec" = l_ann_sec,
    "ann_tot_all" = l_ann_tot,
    "mon_tot_all" = l_mon_tot
  ))
}

################################################################################
#### function to collect the UK written summaries

collect_uk_summaries <- function(y, species, uk_folname, map_yr_uk, naei_inv) {
  fname_root <- paste0(
    species,
    "_UKEIRE_",
    y,
    "emis_",
    map_yr_uk,
    "map_",
    naei_inv,
    "inv"
  )

  # filenames for written summaries
  fname_inv <- paste0(
    uk_folname,
    "/tables/e",
    y,
    "/",
    fname_root,
    "_INVENTORY.csv"
  )
  fname_mask <- paste0(
    uk_folname,
    "/tables/e",
    y,
    "/",
    fname_root,
    "_MASKED.csv"
  )
  fname_group <- paste0(
    uk_folname,
    "/tables/e",
    y,
    "/",
    fname_root,
    "_PROCESSED.csv"
  )
  fname_ncinp <- paste0(
    uk_folname,
    "/tables/e",
    y,
    "/",
    fname_root,
    "_NETCDFINP.csv"
  )
  fname_ncout <- paste0(
    uk_folname,
    "/tables/e",
    y,
    "/",
    fname_root,
    "_NETCDFOUT.csv"
  )

  return(list(
    "inventory" = fname_inv,
    "masked" = fname_mask,
    "processed" = fname_group,
    "ncinput" = fname_ncinp,
    "ncoutput" = fname_ncout
  ))
}

################################################################################
#### function to extract EU gridded data
#### Annual total (sectors) / annual total (all) / monthly totals (all)
extract_eu_maps <- function(
  y,
  species,
  eu_folname,
  emep_inv,
  time_dim,
  v_EMEP_sec,
  eu_agg_schema
) {
  # set the required ncdf filename for the UK
  nc_fname <- paste0("/EU_", emep_inv, "inv_", y, "emis_0.1.nc")
  nc_fname <- paste0(eu_folname, nc_fname)

  # read all data. Create an annual map of total emissions.
  # gather sector info direct from input file. Yet another safety check.
  nc <- nc_open(nc_fname)
  v_var <- names(nc$var)
  v_var <- v_var[grep(paste0("^", species), v_var)] # restrict to species.
  nc_close(nc)

  # l_all <- list()
  l_ann_sec <- list()
  l_ann_tot <- list()
  l_mon_tot <- list()

  # loop through every sector and collect data. Summarise where needed.
  for (v in v_var) {
    ## extract and make stack
    # simply using rast() will read in all layers across sector & time dims,
    # into 1 big stack (can't handle 4D).
    # We can certainly work with this, but it might need to go via an array etc.
    # in the future.
    s <- suppressWarnings(rast(nc_fname, subds = v))

    # from the extract, split out the sectors into their own list elements,
    # in whatever form they come.
    l_out <- lapply(v_EMEP_sec, function(x) {
      s[[grep(paste0("sector=", x, "$"), names(s))]]
    })
    names(l_out) <- v_EMEP_sec
    # l_all[[v]] <- l_out # this isn't particularly needed.

    # sum each list element stack into one (annual per sector, if not already)
    l_outv2 <- suppressWarnings(lapply(l_out, function(x) {
      app(x, sum, na.rm = T)
    }))
    s_outv2 <- rast(l_outv2) # stack of annual sector maps
    l_ann_sec[[v]] <- l_outv2 # return the list as we can transpose this later

    # sum those annual sector totals into one annual total
    s_outv3 <- suppressWarnings(app(s_outv2, sum, na.rm = T))
    names(s_outv3) <- "annual"
    l_ann_tot[[v]] <- s_outv3

    # also create a month by month total, across all sectors.
    if (time_dim == "month") {
      ## CODE NEEDED ##

      l_mon_tot[[v]] <- l_outv4
    }
  }

  # now summarise to UK
  # l_ann_sec = list of 60 vars (ISO),each a stack of 13 secs annual total
  # l_ann_tot = list of 60 vars (ISO),each a sum of all sectors over all time
  # l_mon_tot = list of 60 vars (ISO),each a stack-sum all sectors over 12 month

  return(list(
    "ann_tot_sec" = l_ann_sec,
    "ann_tot_all" = l_ann_tot,
    "mon_tot_all" = l_mon_tot
  ))
}

################################################################################
#### function to collect the EU written summaries

collect_eu_summaries <- function(y, species, eu_folname, emep_inv) {
  fname_root <- paste0(species, "_EU_", y, "emis_", naei_inv, "inv")

  # read written summaries
  fname_inv <- paste0(
    eu_folname,
    "/tables/e",
    y,
    "/",
    fname_root,
    "_INVENTORY.csv"
  )
  fname_proc <- paste0(
    eu_folname,
    "/tables/e",
    y,
    "/",
    fname_root,
    "_PROCESSED.csv"
  )
  fname_ncinp <- paste0(
    eu_folname,
    "/tables/e",
    y,
    "/",
    fname_root,
    "_NETCDFINP.csv"
  )
  fname_ncout <- paste0(
    eu_folname,
    "/tables/e",
    y,
    "/",
    fname_root,
    "_NETCDFOUT.csv"
  )

  return(list(
    "inventory" = fname_inv,
    "processed" = fname_proc,
    "ncinput" = fname_ncinp,
    "ncoutput" = fname_ncout
  ))
}

################################################################################
#### function to set theme of ggplot dependent on GNFR - for full 13 GNFR plot
#### specifically for line plots of totals per sector per month.
month_sector_theme <- function(sector) {
  if (sector %in% c("A_PublicPower", "F_RoadTransport")) {
    x <- theme(
      legend.position = "none",
      plot.title = element_text(size = 20, hjust = 0.5),
      strip.text.x = element_text(size = 12),
      axis.title.x = element_blank(),
      margin(t = 2, r = 2, b = 2, l = 2, unit = "mm")
    )
  } else if (
    sector %in%
      c(
        "B_Industry",
        "C_OtherStationaryComb",
        "D_Fugitive",
        "E_Solvents",
        "G_Shipping",
        "H_Aviation",
        "I_Offroad",
        "J_Waste"
      )
  ) {
    x <- theme(
      legend.position = "none",
      plot.title = element_text(size = 20, hjust = 0.5),
      strip.text.x = element_text(size = 12),
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      margin(t = 2, r = 2, b = 2, l = 2, unit = "mm")
    )
  } else if (sector %in% c("L_AgriOther")) {
    x <- theme(
      legend.position = "none",
      plot.title = element_text(size = 20, hjust = 0.5),
      strip.text.x = element_text(size = 12),
      axis.title.y = element_blank(),
      margin(t = 2, r = 2, b = 2, l = 2, unit = "mm")
    )
  } else if (sector %in% c("M_Other")) {
    x <- theme(
      legend.position = "none",
      plot.title = element_text(size = 20, hjust = 0.5),
      strip.text.x = element_text(size = 12),
      axis.title.y = element_blank(),
      margin(t = 2, r = 2, b = 2, l = 2, unit = "mm")
    )
  } else if (sector == "K_AgriLivestock") {
    x <- theme(
      legend.position = "none",
      plot.title = element_text(size = 20, hjust = 0.5),
      strip.text.x = element_text(size = 12),
      margin(t = 2, r = 2, b = 2, l = 2, unit = "mm")
    )
  }

  return(x)
}


##################
#### UK PLOTS ####
##################

################################################################################
#### 1. function to plot the UK mapped data, total for the species/year (map).
uk_map_tot_ann <- function(
  l_data_uk,
  l_data_eu,
  y,
  species,
  uk_folname,
  map_yr_uk,
  naei_inv,
  emep_inv
) {
  # stack the annual UK sum data and sum to one UKERIE surface
  s_uk <- rast(l_data_uk)
  r_uk <- app(s_uk, sum, na.rm = T)
  r_uk[r_uk == 0] <- NA
  r_uk <- extend(r_uk, ext(r_dom_ukplot)) # extend to larger UK plot domain
  # names(r_uk) <- paste0(species, "_UKEIRE_", y,"_emis_",naei_inv,"_inv")

  # stack the annual EU ISO sum data and sum to one EU surface
  s_eu <- rast(l_data_eu)
  r_eu <- app(s_eu, sum, na.rm = T)
  r_eu[r_eu == 0] <- NA
  r_eu <- crop(r_eu, ext(r_dom_ukplot)) # crop to slightly larger UK plot domain
  # names(r_eu) <- paste0(species, "_EU_", y,"_emis_", emep_inv,"_inv")

  # disaggregate the EU map to 0.01 degree
  r_eu <- disagg(r_eu, 10)
  r_eu <- r_eu / 100

  ## reclassify and plot ##
  # exract all the values present, into a vector.
  v_v <- values(c(r_uk, r_eu))
  v_v <- as.vector(v_v)

  # create break points based on quantiles
  v_q <- as.vector(quantile(
    v_v,
    probs = c(0.20, 0.40, 0.55, 0.7, 0.82, 0.92, 0.97, 1),
    na.rm = T
  ))

  # create a reclassification matrix and labels for the plot.
  leg.pos <- "bottom"
  m <- matrix(
    c(
      0,
      v_q[1:7],
      v_q[1:7],
      ceiling(max(global(c(r_uk, r_eu), max, na.rm = T)$max, na.rm = T)),
      1:8
    ),
    ncol = 3
  )
  brew_d = -1
  v_labs <- unlist(lapply(1:nrow(m), function(x) {
    paste0(round(m[x, 1], 2), " - ", round(m[x, 2], 2))
  }))
  v_labs[length(v_labs)] <- paste0("> ", round(m[nrow(m), 1], 2))

  # reclassify the data
  r_uk_rc <- terra::classify(r_uk, m)
  r_uk_rc <- as.factor(r_uk_rc)
  r_eu_rc <- terra::classify(r_eu, m)
  r_eu_rc <- as.factor(r_eu_rc)

  # plot and save
  p <- ggplot() +
    geom_spatraster(data = r_uk_rc, na.rm = T) +
    geom_spatraster(data = r_eu_rc, na.rm = T, alpha = 0.6) +
    scale_fill_brewer(
      labels = v_labs,
      palette = "Spectral",
      breaks = 1:length(v_q),
      direction = brew_d
    ) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0)) +
    labs(fill = bquote(tonnes ~ a^-1), y = "", x = "") +
    # ggtitle(bquote("Emissions of"~.(p)~a^-1))+
    geom_sf(data = sf_uk, fill = NA, colour = "black") +
    #facet_wrap(~lyr, nrow = 1)+
    theme_bw() +
    {
      if (leg.pos == "bottom") guides(fill = guide_legend(nrow = 2))
    } +
    theme(
      #plot.title = element_text(size = 30, face = "bold"),
      axis.text = element_text(size = 12),
      legend.text = element_text(size = 18),
      legend.title = element_text(size = 24),
      legend.position = leg.pos,
      #plot.margin = grid::unit(c(2,2,2,2), "mm"),
      margin(t = 2, r = 2, b = 2, l = 2, unit = "mm")
    )

  fname <- paste0(
    uk_folname,
    "/plots/e",
    y,
    "/",
    dt_poll[ceh_poll == species, emep_model],
    "_UKEIRE_",
    y,
    "emis_",
    map_yr_uk,
    "map_",
    naei_inv,
    "inv_1_UKTOTMAP.png"
  )

  ggsave(fname, p, width = 10.5, height = 14)

  return(p)
}

################################################################################
#### 2. function to plot annual emissions summary from inventory (bar).
uk_bar_inv_ann <- function(
  fname_inv,
  fname_mask,
  y,
  species,
  uk_folname,
  map_yr_uk,
  naei_inv
) {
  # summarise the inventory information.
  dt_inv <- fread(fname_inv)

  dt_inv_t <- dt_inv[,
    lapply(.SD, sum, na.rm = T),
    by = .(Area, mask, Pollutant, emis_y, inv_y),
    .SDcols = c("emis_t_spatial", "emis_t_scalar", "emis_t_spatial_scaled")
  ]

  dt_inv_m <- melt(
    dt_inv_t,
    id.vars = c("Area", "mask", "Pollutant", "emis_y", "inv_y"),
    variable.name = "stage",
    value.name = "emis_t"
  )

  # summarise the read and masked uk/eire data
  dt_mask <- fread(fname_mask)

  dt_mask_t <- dt_mask[,
    lapply(.SD, sum, na.rm = T),
    by = .(Area, mask, Pollutant, emis_y, inv_y),
    .SDcols = c("emis_t_tot_masked", "tsum")
  ]

  setnames(dt_mask_t, "tsum", "mon_masked")

  dt_mask_m <- melt(
    dt_mask_t,
    id.vars = c("Area", "mask", "Pollutant", "emis_y", "inv_y"),
    variable.name = "stage",
    value.name = "emis_t"
  )

  # put into one table
  dt <- rbindlist(list(dt_inv_m, dt_mask_m), use.names = T)
  dt[, stage := gsub("emis_t_", "", stage)]
  dt[,
    stage := factor(
      stage,
      levels = c(
        "spatial",
        "scalar",
        "spatial_scaled",
        "tot_masked",
        "mon_masked"
      )
    )
  ]
  dt[, mask := factor(mask, levels = c("outwith", "sea", "terrestrial", "all"))]

  # labels text
  dt_text <- data.table(
    Area = rep(c("uk", "ie"), 5),
    stage = rep(
      c("spatial", "scalar", "spatial_scaled", "tot_masked", "mon_masked"),
      each = 2
    ),
    emis_t = c(dt[, sum(emis_t), by = .(Area, stage)][, V1]),
    label = as.character(round(
      dt[, sum(emis_t) / 1000, by = .(Area, stage)][, V1],
      1
    ))
  )

  # plot
  p <- ggplot() +
    geom_bar(
      data = dt,
      aes(x = stage, y = emis_t / 1000, group = mask, fill = mask),
      stat = "identity"
    ) +
    scale_fill_manual(values = c("#eecea6", "#59aed3", "#6fbe6d", "#ea93ea")) +
    labs(y = bquote(kt ~ a^-1)) +
    facet_wrap(~Area, nrow = 1, scales = "free_y") +
    geom_text(
      data = dt_text,
      aes(x = stage, y = emis_t / 1000, label = label),
      size = 5
    ) +
    theme_bw() +
    theme(
      strip.text = element_text(size = 20),
      legend.title = element_blank(),
      legend.position = "bottom",
      legend.text = element_text(size = 16),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
      axis.title.x = element_blank(),
      axis.text.y = element_text(size = 14),
      axis.title.y = element_text(size = 16),
      margin(t = 2, r = 2, b = 2, l = 2, unit = "mm")
    )

  fname <- paste0(
    uk_folname,
    "/plots/e",
    y,
    "/",
    dt_poll[ceh_poll == species, emep_model],
    "_UKEIRE_",
    y,
    "emis_",
    map_yr_uk,
    "map_",
    naei_inv,
    "inv_2_UKINVBAR.png"
  )

  ggsave(fname, p, width = 12, height = 6)

  return(list("plot" = p, "table" = dt))
}

################################################################################
#### 3. function to plot the emissions inside the netCDF file (bar).
uk_bar_nc_ann <- function(
  fname_group,
  fname_ncinp,
  fname_ncout,
  y,
  species,
  uk_folname,
  map_yr_uk,
  naei_inv,
  dt_emis
) {
  # area masks, bring over the total masked, from plot 1, grouped by mask area
  dt_p1_tot <- dt_emis[
    stage == "tot_masked",
    list(emis_t = sum(emis_t, na.rm = T)),
    by = .(Area, Pollutant, emis_y, inv_y, stage)
  ]

  # summarise processed data, grouped by mask areas
  dt_group <- fread(fname_group)
  dt_group_t <- dt_group[,
    lapply(.SD, sum, na.rm = T),
    by = .(Area, Pollutant, emis_y, inv_y),
    .SDcols = c("emis_t_tot_grouped", "tsum")
  ]

  setnames(dt_group_t, "tsum", "mon_grouped")

  dt_group_m <- melt(
    dt_group_t,
    id.vars = c("Area", "Pollutant", "emis_y", "inv_y"),
    variable.name = "stage",
    value.name = "emis_t"
  )

  # summarise data input to nc file
  dt_ncinp <- fread(fname_ncinp)

  # minor changes to summary file
  dt_ncinp[, Area := tolower(Area)] # lower case the areas
  dt_ncinp[Area == "gb", Area := "uk"] # change gb back to uk (from nc file)

  dt_ncinp_t <- dt_ncinp[,
    lapply(.SD, sum, na.rm = T),
    by = .(Area, Pollutant, emis_y, inv_y),
    .SDcols = c("emis_t_tot_ncinput", "emis_t_tot_array", "tsum")
  ]

  setnames(dt_ncinp_t, "tsum", "time_layers")

  dt_ncinp_m <- melt(
    dt_ncinp_t,
    id.vars = c("Area", "Pollutant", "emis_y", "inv_y"),
    variable.name = "stage",
    value.name = "emis_t"
  )

  # summary data that has been read from output nc file (i.e. separate process)
  dt_ncout <- fread(fname_ncout)

  # minor changes to summary file
  dt_ncout[, Area := tolower(Area)] # lower case the areas
  dt_ncout[Area == "gb", Area := "uk"] # change gb back to uk (from nc file)

  dt_ncout_t <- dt_ncout[
    time_res == "annual",
    lapply(.SD, sum, na.rm = T),
    by = .(Area, Pollutant, emis_y, inv_y),
    .SDcols = c("emis_t_tot_ncoutput")
  ]

  dt_ncout_m <- melt(
    dt_ncout_t,
    id.vars = c("Area", "Pollutant", "emis_y", "inv_y"),
    variable.name = "stage",
    value.name = "emis_t"
  )

  # put into one table
  dt <- rbindlist(
    list(dt_p1_tot, dt_group_m, dt_ncinp_m, dt_ncout_m),
    use.names = T
  )

  dt[, stage := gsub("emis_t_", "", stage)]

  dt[,
    stage := factor(
      stage,
      levels = c(
        "tot_masked",
        "tot_grouped",
        "mon_grouped",
        "tot_ncinput",
        "tot_array",
        "time_layers",
        "tot_ncoutput"
      )
    )
  ]

  dt[, Area := factor(Area, levels = c("ow", "sea", "ie", "uk"))]

  # labels text
  dt_text <- data.table(
    stage = c(
      "tot_masked",
      "tot_grouped",
      "mon_grouped",
      "tot_ncinput",
      "tot_array",
      "time_layers",
      "tot_ncoutput"
    ),
    emis_t = c(dt[, sum(emis_t), by = .(stage)][, V1]),
    label = as.character(round(
      dt[, sum(emis_t) / 1000, by = .(stage)][, V1],
      1
    ))
  )

  # plot
  p <- ggplot() +
    geom_bar(
      data = dt,
      aes(x = stage, y = emis_t / 1000, group = Area, fill = Area),
      stat = "identity"
    ) +
    scale_fill_manual(
      values = c("#eecea6", "#59aed3", "#8aee87", "#6cb96a"),
      labels = c("outwith", "sea", "IE (Terres)", "UK (Terres)")
    ) +
    labs(y = bquote(kt ~ a^-1)) +
    geom_text(
      data = dt_text,
      aes(x = stage, y = emis_t / 1000, label = label),
      size = 5
    ) +
    #facet_wrap(~Area, nrow=1, scales = "free_y")+
    theme_bw() +
    theme(
      strip.text = element_text(size = 20),
      legend.title = element_blank(),
      legend.position = "bottom",
      legend.text = element_text(size = 16),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
      axis.title.x = element_blank(),
      axis.text.y = element_text(size = 14),
      axis.title.y = element_text(size = 16),
      margin(t = 2, r = 2, b = 2, l = 2, unit = "mm")
    )

  fname <- paste0(
    uk_folname,
    "/plots/e",
    y,
    "/",
    dt_poll[ceh_poll == species, emep_model],
    "_UKEIRE_",
    y,
    "emis_",
    map_yr_uk,
    "map_",
    naei_inv,
    "inv_3_UKNCBAR.png"
  )

  ggsave(fname, p, width = 12, height = 6)

  return(p)
}

################################################################################
#### 3b. function to plot bar chart of differences when using alternate emissions - TOTAL
uk_bar_tot_ann_alt <- function(fname_in, fname_out) {
  # get data of what should be in inventory
  dt_in <- fread(fname_in)
  dt_out <- fread(fname_out)

  ## INPUT DATA ##
  # assign flags for plotting
  dt_in[, change_flag := "No Change"]
  dt_in[
    data_source_diff == "alt_file" | data_source_pt == "alt_file",
    change_flag := sec_GNFR
  ]

  col_names <- c(
    "Area",
    "sec_GNFR",
    "emis_t_spatial_inv",
    "emis_t_spatial_scaled",
    "change_flag"
  )
  dt_in <- dt_in[, ..col_names]
  setnames(
    dt_in,
    c("emis_t_spatial_inv", "emis_t_spatial_scaled"),
    c("Inventory", "thisProject")
  )

  # melt
  dtm_in <- melt(
    dt_in,
    id.vars = c("Area", "sec_GNFR", "change_flag"),
    variable.name = "source",
    variable.factor = FALSE,
    value.name = "emis_t"
  )
  dtm_in[, Area := toupper(Area)]

  # get sectors and subset (keep ie for plot)
  v_graph_secs <- dtm_in[change_flag != "No Change", unique(sec_GNFR)]

  ## PROCESSED DATA ##
  setnames(
    dt_out,
    c("sec_name", "emis_t_tot_ncoutput"),
    c("sec_GNFR", "ncFile")
  )
  dt_out[, change_flag := "No Change"]
  dt_out[sec_GNFR %in% v_graph_secs, change_flag := sec_GNFR]

  col_names <- c("Area", "sec_GNFR", "ncFile", "change_flag")
  dt_out <- dt_out[, ..col_names]

  # melt
  dtm_out <- melt(
    dt_out,
    id.vars = c("Area", "sec_GNFR", "change_flag"),
    variable.name = "source",
    variable.factor = FALSE,
    value.name = "emis_t"
  )
  dtm_out[Area == "GB", Area := "UK"]

  ## COMBINE ##
  #dtm <- rbindlist(list(dtm_in, dtm_out), use.names = T)
  dtm <- copy(dtm_in)

  ## LABELS ##
  dt_temp <- dtm[, .(emis_t = sum(emis_t, na.rm = T)), by = .(Area, source)]

  #dt_text <- data.table(Area = rep(c("UK", "IE", "SEA"), each = 3),
  #                      source = rep(c("Inventory", "thisProject", "ncFile"), 3))
  dt_text <- data.table(
    Area = rep(c("UK", "IE"), each = 2),
    source = rep(c("Inventory", "thisProject"), 2)
  )

  dt_text <- dt_temp[dt_text, on = c("Area", "source")]
  dt_text[, label := as.character(round(emis_t / 1000, 2))]

  ## FACTORS ##
  dtm[, Area := factor(Area, levels = c("IE", "UK"))]
  dtm[, source := factor(source, levels = c("Inventory", "thisProject"))]
  dtm[, sec_GNFR := factor(sec_GNFR, levels = dtm[, unique(sec_GNFR)])]

  dt_text[, Area := factor(Area, levels = c("IE", "UK"))]
  dt_text[, source := factor(source, levels = c("Inventory", "thisProject"))]

  # plot
  p <- ggplot() +
    geom_bar(
      data = dtm,
      aes(x = source, y = emis_t / 1000, group = sec_GNFR, fill = change_flag),
      stat = "identity"
    ) +
    labs(y = bquote(kt ~ a^-1)) +
    geom_text(
      data = dt_text,
      aes(x = source, y = emis_t / 1000, label = label),
      size = 5
    ) +
    facet_wrap(~Area, nrow = 1, scales = "free_y") +
    theme_bw() +
    theme(
      strip.text = element_text(size = 20),
      legend.title = element_blank(),
      legend.position = "right",
      legend.text = element_text(size = 16),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
      axis.title.x = element_blank(),
      axis.text.y = element_text(size = 14),
      axis.title.y = element_text(size = 16),
      margin(t = 2, r = 2, b = 2, l = 2, unit = "mm")
    )

  fname <- paste0(
    uk_folname,
    "/plots/e",
    y,
    "/",
    dt_poll[ceh_poll == species, emep_model],
    "_UKEIRE_",
    y,
    "emis_",
    map_yr_uk,
    "map_",
    naei_inv,
    "inv_3b_UKALTTOTBAR.png"
  )

  ggsave(fname, p, width = 12, height = 5)

  return(p)
}

################################################################################
#### 3c. function to plot bar chart of differences when using alternate emissions - SECTORS
uk_bar_sec_ann_alt <- function(fname) {
  # get data of what should be in inventory
  dt <- fread(fname)

  # get sectors and subset (keep ie for plot)
  v_graph_secs <- dt[
    data_source_diff == "alt_file" | data_source_pt == "alt_file",
    unique(sec_GNFR)
  ]
  dt <- dt[sec_GNFR %in% v_graph_secs]

  if (nrow(dt) == 0) {
    p <- NA

    return(p)
  }

  # assign flags for plotting
  dt[, change_flag := "No Change"]
  dt[
    data_source_diff == "alt_file" | data_source_pt == "alt_file",
    change_flag := "Change"
  ]

  # subsets
  col_names <- c(
    "Area",
    "sec_GNFR",
    "emis_t_spatial_inv",
    "emis_t_spatial_scaled",
    "change_flag"
  )
  dt <- dt[, ..col_names]
  setnames(
    dt,
    c("emis_t_spatial_inv", "emis_t_spatial_scaled"),
    c("Inventory", "thisProject")
  )

  # get sectors have change for individual plots
  #

  dtm <- melt(
    dt,
    id.vars = c("Area", "sec_GNFR", "change_flag"),
    variable.name = "source",
    value.name = "emis_t"
  )
  dtm[, source := factor(source, levels = c("Inventory", "thisProject"))]
  #dtm[, sec_GNFR := factor(sec_GNFR, levels = dtm[,unique(sec_GNFR)])]

  # plot
  p <- ggplot() +
    geom_bar(
      data = dtm,
      aes(
        x = interaction(source, Area),
        y = emis_t / 1000,
        group = change_flag,
        fill = change_flag
      ),
      stat = "identity"
    ) +
    labs(y = bquote(kt ~ a^-1)) +
    facet_wrap(~sec_GNFR, scales = "free_y") +
    theme_bw() +
    theme(
      strip.text = element_text(size = 20),
      legend.title = element_blank(),
      legend.position = "right",
      legend.text = element_text(size = 16),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
      axis.title.x = element_blank(),
      axis.text.y = element_text(size = 14),
      axis.title.y = element_text(size = 16),
      margin(t = 2, r = 2, b = 2, l = 2, unit = "mm")
    )

  fname <- paste0(
    uk_folname,
    "/plots/e",
    y,
    "/",
    dt_poll[ceh_poll == species, emep_model],
    "_UKEIRE_",
    y,
    "emis_",
    map_yr_uk,
    "map_",
    naei_inv,
    "inv_3c_UKALTSECBAR.png"
  )

  ggsave(fname, p, width = 10, height = 7)

  return(p)
}

################################################################################
#### 4. function to plot monthly total pollutant in UK, IE and SEA (line).
uk_lin_tot_mon <- function(
  fname_ncout,
  y,
  species,
  uk_folname,
  map_yr_uk,
  naei_inv,
  time_dim,
  emep_version
) {
  # only using the NC output file summary data

  if (time_dim == "annual") {
    i_time <- 1
  } else if (time_dim == "month") {
    i_time <- 1:12
  } else {
    i_time <- 1:365
  }

  # read
  dt_ncout <- fread(fname_ncout)

  # minor changes to summary file
  dt_ncout[, Area := tolower(Area)] # lower case the areas
  dt_ncout[Area == "gb", Area := "uk"] # change gb back to uk (from nc file)

  # re-shape
  dt_ncout[, c("tsum", "tot_tres_ratio") := NULL]

  # keep cols and melt cols
  v_col_melt <- setdiff(names(dt_ncout), c(paste0("t", 1:12))) # month regardless
  v_col_keep <- c(v_col_melt, paste0("t", 1:12))

  # if time_dim is 'annual', files have been made with annual total emissions
  # this means we should use the EMEP model version temporal data for plotting
  if (time_dim == "annual") {
    # copy the NC input summary to manipulate it without altering the original
    dt_ncout_modpro <- copy(dt_ncout)

    # set the SEA area to be ISO GB/27 for this plot.
    # annual inputs don't carry through parent monthly from processing (UK or IE)
    dt_ncout_modpro[Area == "sea", iso_code := 27]

    # remove the current t1 to replace with t1:t12
    dt_ncout_modpro[, t1 := NULL]

    if (emep_version == "v4.36") {
      dt_ncout_modpro[, snap := as.numeric(sec_num)]
    } else {
      dt_ncout_modpro[, snap := dt_sec$SNAP[match(sec_num, dt_sec[, sec])]]
    }

    # name the EMEP model timings file
    folname_prof <- paste0(
      "data/temporal/EMEP4UK",
      emep_version,
      "/MonthlyFacs.",
      dt_poll[ceh_poll == species, emep_model]
    )

    if (emep_version == "v4.36") {
      folname_prof <- gsub("MonthlyFacs.", "MonthlyFac.", folname_prof)
    }

    # read in the EMEP model timings
    dt_prof <- fread(folname_prof)
    names(dt_prof) <- c("iso_code", "snap", paste0("t", 1:12))

    # join data - only relevant isos should carry
    dt_nc_month <- dt_prof[dt_ncout_modpro, on = c("iso_code", "snap")]

    # for emissions, divide 'emis_t_tot_ncoutput' by 12 and multiply by factors
    time_cols <- paste0("t", 1:12)
    dt_nc_month[,
      (time_cols) := lapply(.SD, function(x) {
        (emis_t_tot_ncoutput / 12) * x
      }),
      .SDcols = time_cols
    ]

    # sum check
    dt_nc_month[, tsum := rowSums(.SD, na.rm = T), .SDcols = time_cols]
    dt_nc_month[, inp_prof_ratio := emis_t_tot_ncoutput / tsum]

    # check profiled monthly vs total
    if (
      any(
        dt_nc_month$inp_prof_ratio < 0.99 | dt_nc_month$inp_prof_ratio > 1.01,
        na.rm = T
      )
    ) {
      stop("profiling total with model timings has gone wrong")
    }

    # subset and melt
    dt_nc_month <- dt_nc_month[, ..v_col_keep]
    dt_ncout_m <- melt(
      dt_nc_month,
      id.vars = v_col_melt,
      variable.name = "time",
      value.name = "emis_t"
    )
  } else {
    # subset and melt
    dt_nc_month <- dt_ncout[, ..v_col_keep]
    dt_ncout_m <- melt(
      dt_nc_month,
      id.vars = v_col_melt,
      variable.name = "time",
      value.name = "emis_t"
    )
  }

  # sum by snap & iso
  dt_out_sum <- dt_ncout_m[,
    list(emis_kt = sum(emis_t, na.rm = T) / 1000),
    by = .(Area, data_source, time)
  ]
  dt_out_sum[, time := as.numeric(gsub("t", "", time))]
  dt_out_sum[, line_type := 1]

  # plot info
  dt_out_sum[, Area := factor(Area, levels = c("uk", "ie", "sea"))]

  # plot annotations
  if (time_dim == "annual") {
    dt_text <- data.table(
      Area = dt_out_sum[, max(emis_kt), by = Area][["Area"]],
      time = 6,
      fac = dt_out_sum[, max(emis_kt), by = Area][["V1"]],
      source = dt_out_sum[, unique(data_source)],
      label = paste0("Annual input file: using EMEP", emep_version, " profiles")
    )
  } else {}

  # plot
  p <- ggplot(data = dt_out_sum, aes(x = time, y = emis_kt)) +
    geom_line() +
    geom_point() +
    scale_x_continuous(breaks = 1:12) +
    facet_wrap(~Area, scales = "free_y", ncol = 1) +
    labs(y = bquote(kt ~ a^-1)) +
    geom_text(data = dt_text, aes(x = time, y = fac, label = label), size = 5) +
    theme_bw() +
    theme(
      strip.text = element_text(size = 20),
      legend.title = element_blank(),
      legend.position = "bottom",
      legend.text = element_text(size = 16),
      axis.text.x = element_text(size = 16),
      axis.title.x = element_blank(),
      axis.text.y = element_text(size = 14),
      axis.title.y = element_text(size = 16),
      margin(t = 2, r = 2, b = 2, l = 2, unit = "mm")
    )

  fname <- paste0(
    uk_folname,
    "/plots/e",
    y,
    "/",
    dt_poll[ceh_poll == species, emep_model],
    "_UKEIRE_",
    y,
    "emis_",
    map_yr_uk,
    "map_",
    naei_inv,
    "inv_4_UKMONTOTLINE.png"
  )

  ggsave(fname, p, width = 10, height = 7)

  # return the plot and non-aggregated table for other plots
  return(list("plot" = p, "table" = dt_ncout_m))
}

################################################################################
#### 5. function to plot monthly pollutant in UK by sector and by Area (line).
uk_lin_sec_mon <- function(
  dt_month,
  y,
  species,
  uk_folname,
  map_yr_uk,
  naei_inv,
  time_dim,
  emep_version
) {
  # The data being used is that summarised in function 4.

  if (time_dim == "annual") {
    i_time <- 1
  } else if (time_dim == "month") {
    i_time <- 1:12
  } else {
    i_time <- 1:365
  }

  # No summary step

  # plot info
  dt_month[, emis_kt := emis_t / 1000]
  dt_month[, time := as.numeric(gsub("t", "", time))]
  dt_month[, line_type := 1]
  dt_month[, Area := factor(Area, levels = c("uk", "ie", "sea"))]

  # list for plots
  l_p <- list()

  for (i in dt_month[, unique(sec_name)]) {
    g1 <- ggplot(
      data = dt_month[sec_name == i & Area != "sea"],
      aes(x = time, y = emis_kt)
    ) +
      geom_line() +
      geom_point() +
      ggtitle(i) +
      labs(y = bquote(kt ~ month^-1)) +
      scale_x_continuous(breaks = 1:12) +
      facet_wrap(~Area, scales = "free_y", ncol = 1) +
      theme_bw()

    if (emep_version != "v4.36") {
      g1 <- g1 + month_sector_theme(sector = i)
    }

    l_p[[i]] <- g1
  }

  # plot
  if (emep_version == "v4.36") {
    label_plot <- ggdraw() +
      draw_label(
        paste0("Annual input file: using EMEP", emep_version, " profiles"),
        x = 0.8,
        y = 0.8,
        size = 22
      )

    layout_cols <- 4
  } else {
    label_plot <- ggdraw() +
      draw_label(
        paste0("Annual input file:\n using EMEP", emep_version, " profiles"),
        x = 0.1,
        y = 0.65,
        size = 24
      )

    layout_cols <- 5
  }

  p <- l_p[[1]] +
    l_p[[2]] +
    l_p[[3]] +
    l_p[[4]] +
    l_p[[5]] +
    l_p[[6]] +
    l_p[[7]] +
    l_p[[8]] +
    l_p[[9]] +
    l_p[[10]] +
    l_p[[11]]

  if (emep_version != "v4.36") {
    p <- p +
      l_p[[12]] +
      l_p[[13]]
  }

  p <- p +
    plot_spacer() +
    label_plot +
    plot_layout(ncol = layout_cols)

  fname <- paste0(
    uk_folname,
    "/plots/e",
    y,
    "/",
    dt_poll[ceh_poll == species, emep_model],
    "_UKEIRE_",
    y,
    "emis_",
    map_yr_uk,
    "map_",
    naei_inv,
    "inv_5_UKMONSECLINE.png"
  )

  ggsave(fname, p, width = 18, height = 12)

  return(p)
}

###############################################################################
#### 6. function to plot annual total sector pollutant across UK domain (maps)
uk_map_sec_ann <- function(
  l_data_uk,
  l_data_eu,
  y,
  species,
  uk_folname,
  map_yr_uk,
  naei_inv,
  emep_inv
) {
  # transpose the data so it is l[[sector]][[ISOs]] (not l[[ISO]][[sector]])
  l_data_uk_t <- list_transpose(l_data_uk)
  l_data_eu_t <- list_transpose(l_data_eu)

  # collapse to UKEIRE totals, per sector (and same for EU)
  s_data_uk_t <- rast(lapply(l_data_uk_t, function(x) {
    app(rast(x), sum, na.rm = T)
  }))
  s_data_eu_t <- rast(lapply(l_data_eu_t, function(x) {
    app(rast(x), sum, na.rm = T)
  }))

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
  s_eu <- s_eu / 100

  ## reclassify and plot ##
  # exract all the values present, into a vector.
  v_v <- values(c(s_uk, s_eu))
  v_v <- as.vector(v_v)

  # create break points based on quantiles
  v_q <- as.vector(quantile(
    v_v,
    probs = c(0.20, 0.40, 0.55, 0.7, 0.82, 0.92, 0.97, 1),
    na.rm = T
  ))

  # create a reclassification matrix and labels for the plot.
  leg.pos <- "bottom"
  m <- matrix(
    c(
      0,
      v_q[1:7],
      v_q[1:7],
      ceiling(max(global(c(s_uk, s_eu), max, na.rm = T)$max, na.rm = T)),
      1:8
    ),
    ncol = 3
  )
  brew_d = -1
  v_labs <- unlist(lapply(1:nrow(m), function(x) {
    paste0(round(m[x, 1], 2), " - ", round(m[x, 2], 2))
  }))
  v_labs[length(v_labs)] <- paste0("> ", round(m[nrow(m), 1], 2))

  # reclassify the data
  s_uk_rc <- terra::classify(s_uk, m)
  s_uk_rc <- as.factor(s_uk_rc)
  s_eu_rc <- terra::classify(s_eu, m)
  s_eu_rc <- as.factor(s_eu_rc)

  # names
  #if (nlyr(s_uk_rc) != 13) {
  #  stop("There should be 13 layers in the UK map data")
  #}
  #if (nlyr(s_eu_rc) != 13) {
  #  stop("There should be 13 layers in the EU map data")
  #}

  # An issue with reclassification is that if the first layer does not have
  # all the classes in the re-classification, the subsequent image/legend
  # will go haywire.

  # get all the levels. Change NULL to NA to preserve full layer vector.
  l_levels <- (lapply(levels(s_uk_rc), function(x) nrow(x)))
  l_levels[sapply(l_levels, is.null)] <- NA

  # get the layer with biggest set of factors (just frist instance will do)
  i_max <- which.max(unlist(l_levels))

  levels(s_uk_rc) <- lapply(1:length(levels(s_uk_rc)), function(x) {
    levels(s_uk_rc)[[i_max]]
  })
  levels(s_uk_rc) <- lapply(1:length(levels(s_uk_rc)), function(x) {
    setNames(levels(s_uk_rc)[[x]], c("ID", x))
  })

  levels(s_eu_rc) <- lapply(1:length(levels(s_eu_rc)), function(x) {
    levels(s_eu_rc)[[i_max]]
  })
  levels(s_eu_rc) <- lapply(1:length(levels(s_eu_rc)), function(x) {
    setNames(levels(s_eu_rc)[[x]], c("ID", x))
  })

  # rename to sectors
  if (emep_version == "v4.36") {
    names(s_uk_rc) <- paste0("SNAP", str_pad(1:11, 2, pad = "0"))
    names(s_eu_rc) <- paste0("SNAP", str_pad(1:11, 2, pad = "0"))
  } else {
    names(s_uk_rc) <- dt_sec[, GNFRlong][1:13]
    names(s_eu_rc) <- dt_sec[, GNFRlong][1:13]
  }

  # plot and save
  p <- ggplot() +
    geom_spatraster(data = s_uk_rc, na.rm = T) +
    geom_spatraster(data = s_eu_rc, na.rm = T, alpha = 0.6) +
    scale_fill_brewer(
      labels = v_labs,
      palette = "Spectral",
      breaks = 1:length(v_q),
      direction = brew_d
    ) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0)) +
    ggtitle("For UKEIRE: G_, H_ & I_ are all in I_, while agri all in K_") +
    labs(fill = bquote(tonnes ~ a^-1), y = "", x = "") +
    # ggtitle(bquote("Emissions of"~.(p)~a^-1))+
    geom_sf(data = sf_uk, fill = NA, colour = "black") +
    facet_wrap(~lyr, nrow = 3) +
    theme_bw() +
    {
      if (leg.pos == "bottom") guides(fill = guide_legend(nrow = 2))
    } +
    theme(
      #plot.title = element_text(size = 30, face = "bold"),
      strip.text = element_text(size = 18),
      axis.text = element_text(size = 11),
      legend.text = element_text(size = 18),
      legend.title = element_text(size = 24),
      legend.position = leg.pos,
      #plot.margin = grid::unit(c(2,2,2,2), "mm"),
      margin(t = 2, r = 2, b = 2, l = 2, unit = "mm")
    )

  fname <- paste0(
    uk_folname,
    "/plots/e",
    y,
    "/",
    dt_poll[ceh_poll == species, emep_model],
    "_UKEIRE_",
    y,
    "emis_",
    map_yr_uk,
    "map_",
    naei_inv,
    "inv_6_UKANNSECMAP.png"
  )

  ggsave(fname, p, width = 14, height = 13)

  return(p)
}

###############################################################################
#### 7. function to plot monthly total pollutant across UK domain (maps).
uk_map_tot_mon <- function(
  dt_month,
  l_uk_maps,
  y,
  species,
  uk_folname,
  map_yr_uk,
  naei_inv,
  time_dim,
  emep_version
) {
  # if time_dim == "annual", we have to use the EMEP profiles to
  # split out the annual map data (l_uk_maps[[ann_tot_all]]).
  # But if time_dim == "month", we can use the month maps directly
  # from the list (l_uk_maps[[mon_tot_all]]).

  # process data by time_dim choice
  if (time_dim == "annual") {
    # We can use monthly emissions as calculated in function 4.
    # We have to use the sector splits against sector totals, to get
    # the correct annual pattern of total emissions.

    dt_iso_mon <- dt_month[,
      list(emis_kt = sum(emis_kt, na.rm = T)),
      by = .(Area, time)
    ]
    # index the emissions
    dt_iso_mon[, fac := emis_kt / mean(emis_kt, na.rm = T), by = Area]
    dt_iso_mon <- dt_iso_mon[order(time)]

    # apply the factors to the country total maps.
    # do this in a loop to explicitly match name, don't rely on list position.
    l_montot <- list()

    for (j in c("GB", "IE", "SEA")) {
      # these are names in ncfile

      r <- l_uk_maps[["ann_tot_all"]][[paste0(species, "_", j)]]
      # extend to larger UK plot domain
      r[r == 0] <- NA
      r <- extend(r, ext(r_dom_ukplot))

      if (j == "GB") {
        v_fac <- dt_iso_mon[Area == "uk"]
      }
      if (j != "GB") {
        v_fac <- dt_iso_mon[Area == tolower(j)]
      }

      l <- lapply(v_fac$fac, function(x) (r / 12) * x)

      l_montot[[j]] <- l
      # dont collapse as we need to transpose.
    }

    # transpose to months and stack sum those month lists.
    l_montot <- list_transpose(l_montot)
    l_montot <- lapply(l_montot, function(x) app(rast(x), sum, na.rm = T))
    s_uk <- rast(l_montot)
  } else if (time_dim == "month") {
    # this will need writing
    # make s_uk
  }

  ## reclassify and plot ##
  # exract all the values present, into a vector.
  v_v <- values(c(s_uk))
  v_v <- as.vector(v_v)

  # create break points based on quantiles
  v_q <- as.vector(quantile(
    v_v,
    probs = c(0.20, 0.40, 0.55, 0.7, 0.82, 0.92, 0.97, 1),
    na.rm = T
  ))

  # create a reclassification matrix and labels for the plot.
  leg.pos <- "bottom"
  m <- matrix(
    c(
      0,
      v_q[1:7],
      v_q[1:7],
      ceiling(max(global(c(s_uk), max, na.rm = T)$max, na.rm = T)),
      1:8
    ),
    ncol = 3
  )
  brew_d = -1
  v_labs <- unlist(lapply(1:nrow(m), function(x) {
    paste0(round(m[x, 1], 2), " - ", round(m[x, 2], 2))
  }))
  v_labs[length(v_labs)] <- paste0("> ", round(m[nrow(m), 1], 2))

  # reclassify the data
  s_uk_rc <- terra::classify(s_uk, m)
  s_uk_rc <- as.factor(s_uk_rc)

  if (nlyr(s_uk_rc) != 12) {
    stop("There should be 12 layers in the UK map data")
  }

  # An issue with reclassification is that if the first layer does not have
  # all the classes in the re-classification, the subsequent image/legend
  # will go haywire.

  # get all the levels. Change NULL to NA to preserve full layer vector.
  l_levels <- (lapply(levels(s_uk_rc), function(x) nrow(x)))
  l_levels[sapply(l_levels, is.null)] <- NA

  # get the layer with biggest set of factors (just first instance will do)
  i_max <- which.max(unlist(l_levels))

  levels(s_uk_rc) <- lapply(1:length(levels(s_uk_rc)), function(x) {
    levels(s_uk_rc)[[i_max]]
  })
  levels(s_uk_rc) <- lapply(1:length(levels(s_uk_rc)), function(x) {
    setNames(levels(s_uk_rc)[[x]], c("ID", x))
  })

  # rename to months
  names(s_uk_rc) <- 1:12

  # plot and save
  p <- ggplot() +
    geom_spatraster(data = s_uk_rc, na.rm = T) +
    #geom_spatraster(data = sEU_fac, na.rm = T, alpha = 0.6)+
    scale_fill_brewer(
      labels = v_labs,
      palette = "Spectral",
      breaks = 1:length(v_q),
      direction = brew_d
    ) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0)) +
    labs(fill = bquote(tonnes ~ m^-1), y = "", x = "") +
    ggtitle(paste0(
      "Annual input file: using EMEP",
      emep_version,
      " profiles"
    )) +
    geom_sf(data = sf_uk, fill = NA, colour = "black") +
    facet_wrap(~lyr, nrow = 3) +
    theme_bw() +
    {
      if (leg.pos == "bottom") guides(fill = guide_legend(nrow = 2))
    } +
    theme(
      #plot.title = element_text(size = 30, face = "bold"),
      strip.text = element_text(size = 14),
      axis.text = element_text(size = 11),
      legend.text = element_text(size = 18),
      legend.title = element_text(size = 24),
      legend.position = leg.pos,
      #plot.margin = grid::unit(c(2,2,2,2), "mm"),
      margin(t = 2, r = 2, b = 2, l = 2, unit = "mm")
    )

  fname <- paste0(
    uk_folname,
    "/plots/e",
    y,
    "/",
    dt_poll[ceh_poll == species, emep_model],
    "_UKEIRE_",
    y,
    "emis_",
    map_yr_uk,
    "map_",
    naei_inv,
    "inv_7_UKMONTOTMAP.png"
  )

  ggsave(fname, p, width = 13, height = 12)

  return(p)
}

##################
#### EU PLOTS ####
##################

###############################################################################
#### 8. function to plot the EU mapped data, total for the species/year (map).
eu_map_tot_ann <- function(
  l_data_uk,
  l_data_eu,
  y,
  species,
  eu_folname,
  emep_inv
) {
  # stack the annual UK sum data and sum to one UKEIRE surface
  s_uk <- rast(l_data_uk)
  r_uk <- app(s_uk, sum, na.rm = T)
  r_uk[r_uk == 0] <- NA

  # aggregate the UK map to 0.1 degree
  r_uk <- aggregate(r_uk, 10, fun = sum)
  r_uk <- extend(r_uk, ext(l_data_eu[[1]])) # extend to EU plot domain

  # stack the annual EU ISO sum data and sum to one EU surface
  s_eu <- rast(l_data_eu)
  r_eu <- app(s_eu, sum, na.rm = T)
  r_eu[r_eu == 0] <- NA

  ## reclassify and plot ##
  # exract all the values present, into a vector.
  v_v <- values(c(r_uk, r_eu))
  v_v <- as.vector(v_v)

  # create break points based on quantiles
  v_q <- as.vector(quantile(
    v_v,
    probs = c(0.20, 0.40, 0.55, 0.7, 0.82, 0.92, 0.97, 1),
    na.rm = T
  ))

  # create a reclassification matrix and labels for the plot.
  leg.pos <- "bottom"
  m <- matrix(
    c(
      0,
      v_q[1:7],
      v_q[1:7],
      ceiling(max(global(c(r_uk, r_eu), max, na.rm = T)$max, na.rm = T)),
      1:8
    ),
    ncol = 3
  )
  brew_d = -1
  v_labs <- unlist(lapply(1:nrow(m), function(x) {
    paste0(round(m[x, 1], 2), " - ", round(m[x, 2], 2))
  }))
  v_labs[length(v_labs)] <- paste0("> ", round(m[nrow(m), 1], 2))

  # reclassify the data
  r_uk_rc <- terra::classify(r_uk, m)
  r_uk_rc <- as.factor(r_uk_rc)
  r_eu_rc <- terra::classify(r_eu, m)
  r_eu_rc <- as.factor(r_eu_rc)

  # plot and save
  p <- ggplot() +
    geom_spatraster(data = r_eu_rc, na.rm = T) +
    geom_spatraster(data = r_uk_rc, na.rm = T, alpha = 0.6) +
    scale_fill_brewer(
      labels = v_labs,
      palette = "Spectral",
      breaks = 1:length(v_q),
      direction = brew_d
    ) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0)) +
    labs(fill = bquote(tonnes ~ a^-1), y = "", x = "") +
    # ggtitle(bquote("Emissions of"~.(p)~a^-1))+
    geom_sf(data = sf_eu, fill = NA, colour = "black") +
    #facet_wrap(~lyr, nrow = 1)+
    theme_bw() +
    {
      if (leg.pos == "bottom") guides(fill = guide_legend(nrow = 2))
    } +
    theme(
      #plot.title = element_text(size = 30, face = "bold"),
      axis.text = element_text(size = 12),
      legend.text = element_text(size = 18),
      legend.title = element_text(size = 24),
      legend.position = leg.pos,
      #plot.margin = grid::unit(c(2,2,2,2), "mm"),
      margin(t = 2, r = 2, b = 2, l = 2, unit = "mm")
    )

  fname <- paste0(
    eu_folname,
    "/plots/e",
    y,
    "/",
    dt_poll[ceh_poll == species, emep_model],
    "_EU_",
    y,
    "emis_",
    y,
    "map_",
    emep_inv,
    "inv_8_EUTOTMAP.png"
  )

  ggsave(fname, p, width = 13.5, height = 10)

  return(p)
}

###############################################################################
#### 9. function to plot the EU emissions coming from EMEP and in the ncfile
eu_bar_inv_ann <- function(
  fname_inv,
  fname_proc,
  y,
  species,
  eu_folname,
  emep_inv
) {
  # summarise the inventory information.
  dt_inv <- fread(fname_inv)
  setnames(
    dt_inv,
    c("Data_source", "Year", "EMEP_data"),
    c("data_source", "emis_y", "inv_y")
  )

  dt_inv_t <- dt_inv[,
    lapply(.SD, sum, na.rm = T),
    by = .(ISO, Pollutant, data_source, emis_y, inv_y),
    .SDcols = c("emis_t")
  ]

  dt_inv_t[, stage := "inventory_data"]

  # summarise the read and masked uk/eire data
  dt_proc <- fread(fname_proc)
  setnames(dt_proc, c("Data_source", "Year"), c("data_source", "emis_y"))
  dt_proc[, inv_y := emep_inv]
  dt_proc[, emis_t := ann_emis_kt * 1000]

  dt_proc_t <- dt_proc[,
    lapply(.SD, sum, na.rm = T),
    by = .(ISO, Pollutant, data_source, emis_y, inv_y),
    .SDcols = c("emis_t")
  ]

  dt_proc_t[, stage := "processed_data"]

  # put into one table
  dt <- rbindlist(list(dt_inv_t, dt_proc_t), use.names = T)
  dt[, stage := factor(stage, levels = c("inventory_data", "processed_data"))]
  #dtw <- dcast(dt, ISO+Pollutant~stage, value.var = "emis_t")
  #dtw[, kept := processed_data / inventory_data]

  # labels text
  dt_text <- data.table(
    stage = rep(c("inventory_data", "processed_data")),
    emis_t = c(dt[, sum(emis_t), by = .(stage)][, V1]),
    label = as.character(round(
      dt[, sum(emis_t) / 1000, by = .(stage)][, V1],
      1
    ))
  )

  # plot
  p <- ggplot() +
    geom_bar(
      data = dt,
      aes(x = stage, y = emis_t / 1000, group = ISO, fill = ISO),
      stat = "identity"
    ) +
    # scale_fill_manual(values = c("#eecea6","#59aed3","#6fbe6d","#ea93ea"))+
    labs(y = bquote(kt ~ a^-1)) +
    # facet_wrap(~Area, nrow=1, scales = "free_y")+
    geom_text(
      data = dt_text,
      aes(x = stage, y = emis_t / 1000, label = label),
      size = 5
    ) +
    theme_bw() +
    theme(
      strip.text = element_text(size = 20),
      legend.title = element_blank(),
      legend.position = "none",
      legend.text = element_text(size = 16),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
      axis.title.x = element_blank(),
      axis.text.y = element_text(size = 14),
      axis.title.y = element_text(size = 16),
      margin(t = 2, r = 2, b = 2, l = 2, unit = "mm")
    )

  fname <- paste0(
    eu_folname,
    "/plots/e",
    y,
    "/",
    dt_poll[ceh_poll == species, emep_model],
    "_EU_",
    y,
    "emis_",
    y,
    "map_",
    emep_inv,
    "inv_9_EUINVBAR.png"
  )

  ggsave(fname, p, width = 7, height = 7)

  return(list("plot" = p, "table" = dt))
}

###############################################################################
#### 10. function to plot the emissions inside the netCDF file (bar).
eu_bar_nc_ann <- function(
  fname_ncinp,
  fname_ncout,
  y,
  species,
  eu_folname,
  emep_inv,
  dt_emis
) {
  # area masks, bring over the total masked, from plot 1, grouped by mask area
  dt_p1_tot <- dt_emis[stage == "processed_data"]

  # summarise data input to nc file
  dt_ncinp <- fread(fname_ncinp)
  setnames(dt_ncinp, c("iso_char", "Data_source"), c("ISO", "data_source"))

  dt_ncinp_t <- dt_ncinp[,
    lapply(.SD, sum, na.rm = T),
    by = .(ISO, Pollutant, data_source, emis_y, inv_y),
    .SDcols = c("emis_t_tot_ncinput", "emis_t_tot_array", "tsum")
  ]

  setnames(dt_ncinp_t, "tsum", "time_layers")

  dt_ncinp_m <- melt(
    dt_ncinp_t,
    id.vars = c("ISO", "Pollutant", "data_source", "emis_y", "inv_y"),
    variable.name = "stage",
    value.name = "emis_t"
  )

  # summary data that has been read from output nc file (i.e. separate process)
  dt_ncout <- fread(fname_ncout)
  setnames(dt_ncout, c("iso_char", "Data_source"), c("ISO", "data_source"))

  dt_ncout_t <- dt_ncout[
    time_res == "annual",
    lapply(.SD, sum, na.rm = T),
    by = .(ISO, Pollutant, data_source, emis_y, inv_y),
    .SDcols = c("emis_t_tot_ncoutput")
  ]

  dt_ncout_m <- melt(
    dt_ncout_t,
    id.vars = c("ISO", "Pollutant", "data_source", "emis_y", "inv_y"),
    variable.name = "stage",
    value.name = "emis_t"
  )

  # put into one table
  dt <- rbindlist(list(dt_p1_tot, dt_ncinp_m, dt_ncout_m), use.names = T)

  dt[, stage := gsub("emis_t_", "", stage)]

  dt[,
    stage := factor(
      stage,
      levels = c(
        "processed_data",
        "tot_ncinput",
        "tot_array",
        "time_layers",
        "tot_ncoutput"
      )
    )
  ]

  # labels text
  dt_text <- data.table(
    stage = c(
      "processed_data",
      "tot_ncinput",
      "tot_array",
      "time_layers",
      "tot_ncoutput"
    ),
    emis_t = c(dt[, sum(emis_t), by = .(stage)][, V1]),
    label = as.character(round(
      dt[, sum(emis_t) / 1000, by = .(stage)][, V1],
      1
    ))
  )

  # plot
  p <- ggplot() +
    geom_bar(
      data = dt,
      aes(x = stage, y = emis_t / 1000, fill = Pollutant),
      stat = "identity"
    ) +
    scale_fill_manual(values = c("#e292ed")) +
    labs(y = bquote(kt ~ a^-1)) +
    geom_text(
      data = dt_text,
      aes(x = stage, y = emis_t / 1000, label = label),
      size = 5
    ) +
    #facet_wrap(~Area, nrow=1, scales = "free_y")+
    theme_bw() +
    theme(
      strip.text = element_text(size = 20),
      legend.title = element_blank(),
      legend.position = "none",
      legend.text = element_text(size = 16),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
      axis.title.x = element_blank(),
      axis.text.y = element_text(size = 14),
      axis.title.y = element_text(size = 16),
      margin(t = 2, r = 2, b = 2, l = 2, unit = "mm")
    )

  fname <- paste0(
    eu_folname,
    "/plots/e",
    y,
    "/",
    dt_poll[ceh_poll == species, emep_model],
    "_EU_",
    y,
    "emis_",
    y,
    "map_",
    emep_inv,
    "inv_10_EUNCBAR.png"
  )

  ggsave(fname, p, width = 10, height = 6)

  return(p)
}

###############################################################################
#### 11. function to plot monthly pollutant in EU by sector and by Area (line).
eu_lin_tot_mon <- function(
  fname_ncout,
  y,
  species,
  eu_folname,
  emep_inv,
  time_dim,
  emep_version
) {
  # only using the NC output file summary data

  if (time_dim == "annual") {
    i_time <- 1
  } else if (time_dim == "month") {
    i_time <- 1:12
  } else {
    i_time <- 1:365
  }

  # read
  dt_ncout <- fread(fname_ncout)
  setnames(dt_ncout, c("iso_char", "Data_source"), c("ISO", "data_source"))

  # re-shape
  dt_ncout[, c("tsum", "tot_tres_ratio") := NULL]

  # keep cols and melt cols
  v_col_melt <- setdiff(names(dt_ncout), c(paste0("t", 1:12))) # month regardless
  v_col_keep <- c(v_col_melt, paste0("t", 1:12))

  # if time_dim is 'annual', files have been made with annual total emissions
  # this means we should use the EMEP model version temporal data for plotting
  if (time_dim == "annual") {
    # copy the NC input summary to manipulate it without altering the original
    dt_ncout_modpro <- copy(dt_ncout)

    # remove the current t1 to replace with t1:t12
    dt_ncout_modpro[, t1 := NULL]

    dt_ncout_modpro[, snap := dt_sec$SNAP[match(sec_num, dt_sec[, sec])]]

    # name the EMEP model timings file
    folname_prof <- paste0(
      "data/temporal/EMEP4UK",
      emep_version,
      "/MonthlyFacs.",
      dt_poll[ceh_poll == species, emep_model]
    )

    if (emep_version == "v4.36") {
      folname_prof <- gsub("MonthlyFacs.", "MonthlyFac.", folname_prof)
    }

    # read in the EMEP model timings
    dt_prof <- fread(folname_prof)
    names(dt_prof) <- c("iso_code", "snap", paste0("t", 1:12))

    # join data - only relevant isos should carry
    dt_nc_month <- dt_prof[dt_ncout_modpro, on = c("iso_code", "snap")]

    # quite a few snaps in quite a few iso's will have no profile info
    # set all of these to be 1.
    time_cols <- paste0("t", 1:12)

    dt_nc_month[, 3:14][is.na(dt_nc_month[, 3:14])] <- 1

    # for emissions, divide 'emis_t_tot_ncoutput' by 12 and multiply by factors
    dt_nc_month[,
      (time_cols) := lapply(.SD, function(x) {
        (emis_t_tot_ncoutput / 12) * x
      }),
      .SDcols = time_cols
    ]

    # sum check
    dt_nc_month[, tsum := rowSums(.SD, na.rm = T), .SDcols = time_cols]
    dt_nc_month[, inp_prof_ratio := emis_t_tot_ncoutput / tsum]

    # check profiled monthly vs total
    if (
      any(
        dt_nc_month$inp_prof_ratio < 0.99 | dt_nc_month$inp_prof_ratio > 1.01,
        na.rm = T
      )
    ) {
      stop("profiling total with model timings has gone wrong")
    }

    # subset and melt
    dt_nc_month <- dt_nc_month[, ..v_col_keep]
    dt_ncout_m <- melt(
      dt_nc_month,
      id.vars = v_col_melt,
      variable.name = "time",
      value.name = "emis_t"
    )
  } else {
    # subset and melt
    dt_nc_month <- dt_ncout[, ..v_col_keep]
    dt_ncout_m <- melt(
      dt_nc_month,
      id.vars = v_col_melt,
      variable.name = "time",
      value.name = "emis_t"
    )
  }

  # sum by snap & iso
  dt_out_sum <- dt_ncout_m[,
    list(emis_kt = sum(emis_t, na.rm = T) / 1000),
    by = .(ISO, data_source, time)
  ]
  dt_out_sum[, time := as.numeric(gsub("t", "", time))]
  dt_out_sum[, line_type := 1]

  # plot info - sum EU + keep ISO in and around UK/EIRE
  dt_out_EU <- dt_out_sum[,
    list(emis_kt = sum(emis_kt, na.rm = T)),
    by = .(data_source, time, line_type)
  ]
  dt_out_EU[, ISO := "EU"]
  dt_out_iso <- dt_out_sum[
    ISO %in% c("NOS", "ATL", "BE", "FR", "NL", "ES", "DE")
  ]

  # overwrite dt_sum_out;
  dt_out_sum <- rbindlist(list(dt_out_EU, dt_out_iso), use.names = T)
  dt_out_sum[,
    ISO := factor(
      ISO,
      levels = c("EU", "FR", "NL", "BE", "DE", "ES", "NOS", "ATL")
    )
  ]

  # plot
  p <- ggplot(data = dt_out_sum, aes(x = time, y = emis_kt)) +
    geom_line() +
    geom_point() +
    scale_x_continuous(breaks = 1:12) +
    facet_wrap(~ISO, scales = "free_y", ncol = 1) +
    labs(y = bquote(kt ~ a^-1)) +
    #geom_text(data = dt_text, aes(x = time, y = fac, label = label), size = 5)+
    theme_bw() +
    theme(
      strip.text = element_text(size = 20),
      legend.title = element_blank(),
      legend.position = "bottom",
      legend.text = element_text(size = 16),
      axis.text.x = element_text(size = 16),
      axis.title.x = element_blank(),
      axis.text.y = element_text(size = 14),
      axis.title.y = element_text(size = 16),
      margin(t = 2, r = 2, b = 2, l = 2, unit = "mm")
    )

  fname <- paste0(
    eu_folname,
    "/plots/e",
    y,
    "/",
    dt_poll[ceh_poll == species, emep_model],
    "_EU_",
    y,
    "emis_",
    y,
    "map_",
    emep_inv,
    "inv_11_EUMONTOTLINE.png"
  )

  ggsave(fname, p, width = 9, height = 12)

  # return the plot and non-aggregated table for other plots
  return(list("plot" = p, "table" = dt_ncout_m))
}

###############################################################################
#### 12. function to plot annual total sector pollutant across EU domain (maps).
eu_lin_sec_mon <- function(
  dt_month,
  y,
  species,
  eu_folname,
  emep_inv,
  time_dim,
  emep_version
) {
  # The data being used is that summarised in function 11.

  if (time_dim == "annual") {
    i_time <- 1
  } else if (time_dim == "month") {
    i_time <- 1:12
  } else {
    i_time <- 1:365
  }

  # No summary step

  # plot info
  dt_month <- dt_month[,
    list(emis_kt = sum(emis_t, na.rm = T) / 1000),
    by = .(emis_y, inv_y, time, sec_name)
  ]

  dt_month[, time := as.numeric(gsub("t", "", time))]
  dt_month[, line_type := 1]

  p <- ggplot(data = dt_month, aes(x = time, y = emis_kt)) +
    geom_line() +
    geom_point() +
    # ggtitle(i)+
    labs(y = bquote(kt ~ month^-1)) +
    scale_x_continuous(breaks = 1:12) +
    facet_wrap(~sec_name, scales = "free_y") +
    theme_bw()
  #month_sector_theme(sector = i)

  fname <- paste0(
    eu_folname,
    "/plots/e",
    y,
    "/",
    dt_poll[ceh_poll == species, emep_model],
    "_EU_",
    y,
    "emis_",
    y,
    "map_",
    emep_inv,
    "inv_12_EUMONSECLINE.png"
  )

  ggsave(fname, p, width = 13, height = 8)

  return(p)
}

###############################################################################
#### 13. function to plot annual total sector pollutant across UK domain (maps)
eu_map_sec_ann <- function(
  l_data_uk,
  l_data_eu,
  y,
  species,
  eu_folname,
  map_yr_uk,
  naei_inv,
  emep_inv
) {
  # transpose the data so it is l[[sector]][[ISOs]] (not l[[ISO]][[sector]])
  l_data_uk_t <- list_transpose(l_data_uk)
  l_data_eu_t <- list_transpose(l_data_eu)

  # collapse to UKEIRE totals, per sector (and same for EU)
  s_data_uk_t <- rast(lapply(l_data_uk_t, function(x) {
    app(rast(x), sum, na.rm = T)
  }))
  s_data_eu_t <- rast(lapply(l_data_eu_t, function(x) {
    app(rast(x), sum, na.rm = T)
  }))

  # stack the annual EU ISO sum data and sum to one EU surface
  # crop to larger UK plot domain
  s_data_eu_t[s_data_eu_t == 0] <- NA
  s_eu <- copy(s_data_eu_t)

  # stack the annual UK sector data
  # aggregate to 0.1 degree
  # extend to EU domain
  s_data_uk_t <- aggregate(s_data_uk_t, fac = 10, fun = sum)
  s_data_uk_t[s_data_uk_t == 0] <- NA
  s_uk <- extend(s_data_uk_t, ext(s_eu[[1]]))

  ## reclassify and plot ##
  # exract all the values present, into a vector.
  v_v <- values(c(s_uk, s_eu))
  v_v <- as.vector(v_v)

  # create break points based on quantiles
  v_q <- as.vector(quantile(
    v_v,
    probs = c(0.20, 0.40, 0.55, 0.7, 0.82, 0.92, 0.97, 1),
    na.rm = T
  ))

  # create a reclassification matrix and labels for the plot.
  leg.pos <- "bottom"
  m <- matrix(
    c(
      0,
      v_q[1:7],
      v_q[1:7],
      ceiling(max(global(c(s_uk, s_eu), max, na.rm = T)$max, na.rm = T)),
      1:8
    ),
    ncol = 3
  )
  brew_d = -1
  v_labs <- unlist(lapply(1:nrow(m), function(x) {
    paste0(round(m[x, 1], 3), " - ", round(m[x, 2], 3))
  }))
  v_labs[length(v_labs)] <- paste0("> ", round(m[nrow(m), 1], 3))

  # reclassify the data
  s_uk_rc <- terra::classify(s_uk, m)
  s_uk_rc <- as.factor(s_uk_rc)
  s_eu_rc <- terra::classify(s_eu, m)
  s_eu_rc <- as.factor(s_eu_rc)

  # names
  #if (nlyr(s_uk_rc) != 13) {
  #  stop("There should be 13 layers in the UK map data")
  #}
  #if (nlyr(s_eu_rc) != 13) {
  #  stop("There should be 13 layers in the EU map data")
  #}

  # An issue with reclassification is that if the first layer does not have
  # all the classes in the re-classification, the subsequent image/legend
  # will go haywire.

  # get all the levels. Change NULL to NA to preserve full layer vector.
  l_levels <- (lapply(levels(s_uk_rc), function(x) nrow(x)))
  l_levels[sapply(l_levels, is.null)] <- NA

  # get the layer with biggest set of factors (just frist instance will do)
  i_max <- which.max(unlist(l_levels))

  levels(s_uk_rc) <- lapply(1:length(levels(s_uk_rc)), function(x) {
    levels(s_uk_rc)[[i_max]]
  })
  levels(s_uk_rc) <- lapply(1:length(levels(s_uk_rc)), function(x) {
    setNames(levels(s_uk_rc)[[x]], c("ID", x))
  })

  levels(s_eu_rc) <- lapply(1:length(levels(s_eu_rc)), function(x) {
    levels(s_eu_rc)[[i_max]]
  })
  levels(s_eu_rc) <- lapply(1:length(levels(s_eu_rc)), function(x) {
    setNames(levels(s_eu_rc)[[x]], c("ID", x))
  })

  # rename to sectors
  if (emep_version == "v4.36") {
    names(s_uk_rc) <- paste0("SNAP", str_pad(1:11, 2, pad = "0"))
    names(s_eu_rc) <- paste0("SNAP", str_pad(1:11, 2, pad = "0"))
  } else {
    names(s_uk_rc) <- dt_sec[, GNFRlong][1:13]
    names(s_eu_rc) <- dt_sec[, GNFRlong][1:13]
  }

  # plot and save
  p <- ggplot() +
    geom_spatraster(data = s_eu_rc, na.rm = T) +
    geom_spatraster(data = s_uk_rc, na.rm = T, alpha = 0.6) +
    scale_fill_brewer(
      labels = v_labs,
      palette = "Spectral",
      breaks = 1:length(v_q),
      direction = brew_d
    ) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0)) +
    #ggtitle("For UKEIRE: G_, H_ & I_ are all in I_, while agri all in K_")+
    labs(fill = bquote(tonnes ~ a^-1), y = "", x = "") +
    # ggtitle(bquote("Emissions of"~.(p)~a^-1))+
    geom_sf(data = sf_eu, fill = NA, colour = "black") +
    facet_wrap(~lyr, nrow = 3) +
    theme_bw() +
    {
      if (leg.pos == "bottom") guides(fill = guide_legend(nrow = 2))
    } +
    theme(
      #plot.title = element_text(size = 30, face = "bold"),
      strip.text = element_text(size = 18),
      axis.text = element_text(size = 11),
      legend.text = element_text(size = 18),
      legend.title = element_text(size = 24),
      legend.position = leg.pos,
      #plot.margin = grid::unit(c(2,2,2,2), "mm"),
      margin(t = 2, r = 2, b = 2, l = 2, unit = "mm")
    )

  fname <- paste0(
    eu_folname,
    "/plots/e",
    y,
    "/",
    dt_poll[ceh_poll == species, emep_model],
    "_EU_",
    y,
    "emis_",
    y,
    "map_",
    emep_inv,
    "inv_13_EUANNSECMAP.png"
  )

  ggsave(fname, p, width = 14, height = 10)

  return(p)
}

###############################################################################
#### 14. function to plot monthly total pollutant across UK domain (maps).
eu_map_tot_mon <- function(
  dt_month,
  l_eu_maps,
  y,
  species,
  eu_folname,
  emep_inv,
  time_dim,
  emep_version
) {
  # if time_dim == "annual", we have to use the EMEP profiles to
  # split out the annual map data (l_uk_maps[[ann_tot_all]]).
  # But if time_dim == "month", we can use the month maps directly
  # from the list (l_uk_maps[[mon_tot_all]]).

  # process data by time_dim choice
  if (time_dim == "annual") {
    # We can use monthly emissions as calculated in function 4.
    # We have to use the sector splits against sector totals, to get
    # the correct annual pattern of total emissions.

    dt_iso_mon <- dt_month[,
      list(emis_kt = sum(emis_t, na.rm = T) / 1000),
      by = .(time)
    ]
    # index the emissions
    dt_iso_mon[, fac := emis_kt / mean(emis_kt, na.rm = T)]
    dt_iso_mon <- dt_iso_mon[order(time)]

    # apply the factors to the country total maps.
    # do this in a loop to explicitly match name, don't rely on list position.
    s <- rast(l_eu_maps[["ann_tot_all"]])
    r <- app(s, sum, na.rm = T)
    r[r == 0] <- NA

    l <- lapply(dt_iso_mon$fac, function(x) (r / 12) * x)

    ##!!## WAIT - this isnt right, this is simply splitting the total
    ##     pollutant by total monthly share, meaning same distribution per
    ##     month. Each sector should be split by month and then all added
    ##     back up, by month. (That way any sector that 'dies' off in a month
    ##     comes across in the map).

    # transpose to months and stack sum those month lists.
    l_montot <- list_transpose(l_montot)
    l_montot <- lapply(l_montot, function(x) app(rast(x), sum, na.rm = T))
    s_uk <- rast(l_montot)
  } else if (time_dim == "month") {
    # this will need writing
    # make s_uk
  }

  ## reclassify and plot ##
  # exract all the values present, into a vector.
  v_v <- values(c(s_uk))
  v_v <- as.vector(v_v)

  # create break points based on quantiles
  v_q <- as.vector(quantile(
    v_v,
    probs = c(0.20, 0.40, 0.55, 0.7, 0.82, 0.92, 0.97, 1),
    na.rm = T
  ))

  # create a reclassification matrix and labels for the plot.
  leg.pos <- "bottom"
  m <- matrix(
    c(
      0,
      v_q[1:7],
      v_q[1:7],
      ceiling(max(global(c(s_uk), max, na.rm = T)$max, na.rm = T)),
      1:8
    ),
    ncol = 3
  )
  brew_d = -1
  v_labs <- unlist(lapply(1:nrow(m), function(x) {
    paste0(round(m[x, 1], 2), " - ", round(m[x, 2], 2))
  }))
  v_labs[length(v_labs)] <- paste0("> ", round(m[nrow(m), 1], 2))

  # reclassify the data
  s_uk_rc <- terra::classify(s_uk, m)
  s_uk_rc <- as.factor(s_uk_rc)

  if (nlyr(s_uk_rc) != 12) {
    stop("There should be 12 layers in the UK map data")
  }

  # An issue with reclassification is that if the first layer does not have
  # all the classes in the re-classification, the subsequent image/legend
  # will go haywire.

  # get all the levels. Change NULL to NA to preserve full layer vector.
  l_levels <- (lapply(levels(s_uk_rc), function(x) nrow(x)))
  l_levels[sapply(l_levels, is.null)] <- NA

  # get the layer with biggest set of factors (just first instance will do)
  i_max <- which.max(unlist(l_levels))

  levels(s_uk_rc) <- lapply(1:length(levels(s_uk_rc)), function(x) {
    levels(s_uk_rc)[[i_max]]
  })
  levels(s_uk_rc) <- lapply(1:length(levels(s_uk_rc)), function(x) {
    setNames(levels(s_uk_rc)[[x]], c("ID", x))
  })

  # rename to months
  names(s_uk_rc) <- 1:12

  # plot and save
  p <- ggplot() +
    geom_spatraster(data = s_uk_rc, na.rm = T) +
    #geom_spatraster(data = sEU_fac, na.rm = T, alpha = 0.6)+
    scale_fill_brewer(
      labels = v_labs,
      palette = "Spectral",
      breaks = 1:length(v_q),
      direction = brew_d
    ) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0)) +
    labs(fill = bquote(tonnes ~ m^-1), y = "", x = "") +
    ggtitle(paste0(
      "Annual input file: using EMEP",
      emep_version,
      " profiles"
    )) +
    geom_sf(data = sf_uk, fill = NA, colour = "black") +
    facet_wrap(~lyr, nrow = 3) +
    theme_bw() +
    {
      if (leg.pos == "bottom") guides(fill = guide_legend(nrow = 2))
    } +
    theme(
      #plot.title = element_text(size = 30, face = "bold"),
      strip.text = element_text(size = 14),
      axis.text = element_text(size = 11),
      legend.text = element_text(size = 18),
      legend.title = element_text(size = 24),
      legend.position = leg.pos,
      #plot.margin = grid::unit(c(2,2,2,2), "mm"),
      margin(t = 2, r = 2, b = 2, l = 2, unit = "mm")
    )

  fname <- paste0(
    uk_folname,
    "/plots/e",
    y,
    "/",
    dt_poll[ceh_poll == species, emep_model],
    "_UKEIRE_",
    y,
    "emis_",
    map_yr_uk,
    "map_",
    naei_inv,
    "inv_7_UKMONTOTMAP.png"
  )

  ggsave(fname, p, width = 13, height = 12)

  return(p)
}

###############################################################################
#### 15. function to plot time series of emissions data (line). NAEI tables.
time_series_tot <- function(
  y,
  species,
  naei_inv,
  emep_inv,
  emep_version,
  uk_folname,
  map_yr_uk
) {
  # inventory processor folder
  folname_ip <- "/gws/ssde/j25b/ceh_generic/inventory_processor/data"

  # earliest inventory year processed for NAEI
  uk_earliest_inv <- 2022
  eu_earliest_inv <- 2023

  # create a list ready for all data
  l_SEC <- list()
  l_TOTAL <- list()

  ## UK ##
  # cycle through nominated invenotry years and plot 10 years;
  for (y_inv in uk_earliest_inv:naei_inv) {
    # read in the NAEI data
    fname <- paste0(
      folname_ip,
      "/NAEI/inv",
      y_inv,
      "/totals/",
      "NAEI_AllPoll_TOTALS_inv",
      y_inv,
      "_emis_1970-",
      y_inv - 2,
      "_GNFR_t.csv"
    )

    dt <- fread(fname)
    # remove international shipping and aviation cruise
    dt <- dt[!(GNFR %in% c("O_AviCruise", "P_IntShipping"))]
    dt <- dt[GNFR != ""]

    dt <- dt[Pollutant == dt_poll[ceh_poll == species, invProc]]
    dt <- dt[Year >= (y - 9) & Year <= (y + 4)] # context for plot
    dt[, dataYear := y_inv]

    l_SEC[[paste0(y_inv, "_uk")]] <- dt

    # summarise to total
    dt_TOTAL <- dt[,
      .(emis_t = sum(emis_t, na.rm = T)),
      by = .(Pollutant, Year, AREA, dataYear)
    ]
    l_TOTAL[[paste0(y_inv, "_uk")]] <- dt_TOTAL
  }

  ## IE ##
  # cycle through nominated invenotry years and plot 10 years;
  for (y_inv in eu_earliest_inv:emep_inv) {
    # read in the NAEI data
    fname <- paste0(
      folname_ip,
      "/EMEP/inv",
      y_inv,
      "/totals/",
      "EMEP_AllPoll_TOTALS_inv",
      y_inv,
      "_emis_1970-",
      y_inv - 2,
      "_GNFR_t.csv"
    )

    dt <- fread(fname)
    # remove international shipping and aviation cruise
    dt <- dt[!(GNFR %in% c("O_AviCruise", "P_IntShipping"))]
    dt <- dt[GNFR != ""]

    dt <- dt[Pollutant == dt_poll[ceh_poll == species, invProc] & ISO2 == "IE"]
    setnames(dt, "ISO2", "AREA")
    dt <- dt[Year >= (y - 9) & Year <= (y + 4)] # context for plot
    dt[, dataYear := y_inv]

    l_SEC[[paste0(y_inv, "_ie")]] <- dt

    # summarise to total
    dt_TOTAL <- dt[,
      .(emis_t = sum(emis_t, na.rm = T)),
      by = .(Pollutant, Year, AREA, dataYear)
    ]
    l_TOTAL[[paste0(y_inv, "_ie")]] <- dt_TOTAL
  }

  ## plot of totals ##
  dt_plotTot <- rbindlist(l_TOTAL, use.names = T)

  p_tots <- ggplot(
    data = dt_plotTot,
    aes(
      x = Year,
      y = emis_t / 1000,
      colour = factor(dataYear),
      group = dataYear
    )
  ) +
    geom_line() +
    geom_point() +
    geom_vline(xintercept = y, linetype = "dashed", colour = "grey50") +
    facet_wrap(~AREA, ncol = 1, scales = "free_y") +
    theme_bw() +
    labs(y = bquote(kt ~ a^-1), colour = "InvYear") +
    theme(
      strip.text = element_text(size = 20),
      legend.title = element_text(size = 18),
      legend.position = "right",
      legend.text = element_text(size = 16),
      axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
      axis.title.x = element_text(size = 18),
      axis.text.y = element_text(size = 16),
      axis.title.y = element_text(size = 18),
      margin(t = 2, r = 2, b = 2, l = 2, unit = "mm")
    )
  # save
  fname_tot <- paste0(
    uk_folname,
    "/plots/e",
    y,
    "/",
    dt_poll[ceh_poll == species, emep_model],
    "_UKEIRE_",
    y,
    "emis_",
    map_yr_uk,
    "map_",
    naei_inv,
    "inv_8a_UKTSTOTLINE.png"
  )

  ggsave(fname_tot, p_tots, width = 10, height = 7)

  ## plot of sectors ##
  # needs to be a special grid, like monthly sectoral emissions
  # pad out missing ones.
  dt_plot <- rbindlist(l_SEC, use.names = T)
  dt_plot[, dataYear := factor(dataYear)]
  setnames(dt_plot, "dataYear", "invYear")

  v_sec_miss_uk <- dt_plot[
    AREA == "UK",
    setdiff(dt_sec$GNFRlong[1:13], unique(GNFR))
  ]
  v_sec_miss_ie <- dt_plot[
    AREA == "IE",
    setdiff(dt_sec$GNFRlong[1:13], unique(GNFR))
  ]

  # add missing secotrs with zero emissions (if they are missing)
  if (length(v_sec_miss_uk) > 0) {
    dt_temp <- data.table(
      Pollutant = species,
      Year = rep(seq(y - 9, y), length(v_sec_miss_uk)),
      AREA = "UK",
      GNFR = rep(v_sec_miss_uk, each = 10),
      emis_t = 0,
      invYear = naei_inv
    )
    dt_plot <- rbindlist(list(dt_plot, dt_temp), use.names = T)
  }

  if (length(v_sec_miss_ie) > 0) {
    dt_temp <- data.table(
      Pollutant = species,
      Year = rep(seq(y - 9, y), length(v_sec_miss_ie)),
      AREA = "IE",
      GNFR = rep(v_sec_miss_ie, each = 10),
      emis_t = 0,
      invYear = emep_inv
    )
    dt_plot <- rbindlist(list(dt_plot, dt_temp), use.names = T)
  }

  l_p <- list()

  for (i in dt_plot[, unique(GNFR)]) {
    if (i == "N_Natural") {
      next
    }
    g1 <- ggplot(
      data = dt_plot[GNFR == i],
      aes(
        x = Year,
        y = emis_t / 1000,
        colour = invYear,
        group = invYear
      )
    ) +
      geom_line() +
      geom_point() +
      geom_vline(xintercept = y, linetype = "dashed", colour = "grey50") +
      ggtitle(i) +
      labs(y = bquote(kt ~ a^-1)) +
      # scale_x_continuous(breaks = 1:12) +
      facet_wrap(~AREA, scales = "free_y", ncol = 1) +
      theme_bw()

    if (emep_version != "v4.36") {
      g1 <- g1 + month_sector_theme(sector = i)
    }

    l_p[[i]] <- g1
  }

  p <- l_p[[1]] +
    l_p[[2]] +
    l_p[[3]] +
    l_p[[4]] +
    l_p[[5]] +
    l_p[[6]] +
    l_p[[7]] +
    l_p[[8]] +
    l_p[[9]] +
    l_p[[10]] +
    l_p[[11]]

  if (emep_version != "v4.36") {
    p <- p +
      l_p[[12]] +
      l_p[[13]]
  }

  # take out the legend
  if (emep_version != "v4.36") {
    p_leg <- ggplotGrob(l_p[[2]] + theme(legend.position = "right"))$grobs
    p_leg <- p_leg[[which(sapply(p_leg, function(x) x$name) == "guide-box")]]

    p <- p + p_leg
  }

  p_secs <- p +
    plot_spacer() +
    plot_layout(ncol = 5)

  # save
  fname_sec <- paste0(
    uk_folname,
    "/plots/e",
    y,
    "/",
    dt_poll[ceh_poll == species, emep_model],
    "_UKEIRE_",
    y,
    "emis_",
    map_yr_uk,
    "map_",
    naei_inv,
    "inv_8b_UKTSSECLINE.png"
  )

  ggsave(fname_sec, p_secs, width = 15, height = 11)

  return(list(
    "totals" = p_tots,
    "sectors" = p_secs,
    "totals_table" = dt_plotTot
  ))
}

###############################################################################
