##############################################################################################################
packs <- c(
  "sf",
  "terra",
  "stringr",
  "dplyr",
  "ggplot2",
  "data.table",
  "stats",
  "readxl",
  "ncdf4",
  "lubridate",
  "ggrepel",
  "patchwork"
)

lapply(packs, require, character.only = TRUE)
##############################################################################################################

#### MASTER function to call needed functions based on choices
evaluateNC <- function(
  species,
  v_years,
  naei_inv,
  map_yr_uk,
  map_yr_ie,
  emep_inv,
  tp_scheme = "genYr",
  dt_sec,
  summarise_UK = FALSE,
  plot_UK = FALSE,
  summarise_EIRE = FALSE,
  plot_EIRE = FALSE,
  summarise_EU = FALSE,
  plot_EU = FALSE
) {
  if (summarise_UK == TRUE) {
    evaluateUKnc(species, v_years, naei_inv, map_yr_uk, tp_scheme, dt_sec)
  }
  if (plot_UK == TRUE) {
    plotUKnc(species, naei_inv, map_yr_uk, tp_scheme)
  }

  #if(summarise_EIRE == TRUE) evaluateEIREnc(species, v_years, naei_inv, map_yr_uk, tp_scheme, dt_sec)
  if (plot_EIRE == TRUE) {
    plotEIREnc(species, emep_inv, tp_scheme)
  }

  if (summarise_EU == TRUE) {
    evaluateEUnc(species, v_years, emep_inv, tp_scheme, dt_sec)
  }
  if (plot_EU == TRUE) {
    plotEUnc(species, emep_inv, tp_scheme)
  }

  print("Processing Complete.")
}


#### function to evaluate the UK EMEP4UK input files for a vector of years and pollutants
#### a vector of species can be put into lapply while the vector of years is looped over

evaluateUKnc <- function(
  species,
  v_years,
  naei_inv,
  map_yr_uk,
  tp_scheme,
  dt_sec
) {
  folname <- paste0("outputs/EMEP4UK/inv", naei_inv, "/plots/UK/", tp_scheme)
  suppressWarnings(dir.create(folname, recursive = T))

  print(paste0(Sys.time(), ": Summarising ", species, " data in the UK..."))

  # master list for annual data
  l <- list()

  for (y in v_years) {
    print(paste0(Sys.time(), ":                               ", y))

    ## choose output directory for the EMEP input files
    output_dir <- paste0(
      "/gws/ssde/j25b/ceh_generic/samtom/EMEP_inputs/outputs/EMEP4UK/inv",
      naei_inv,
      "/emis",
      y,
      "/UKEIRE/TP",
      tp_scheme,
      "_AGGNA"
    )

    nc_file <- paste0(
      output_dir,
      "/",
      species,
      "_UKEIRE_",
      y,
      "emis_",
      map_yr_uk,
      "map_",
      naei_inv,
      "inv_0.01.nc"
    )

    nc <- nc_open(nc_file)
    nc_secs <- names(nc$var)
    nc_close(nc)

    # go through all sectors to summarise. Skip some later ones.
    for (v in nc_secs) {
      v_secs_skip <- c("PublicPower_Point", "PublicPower_Area", "Exhaust")

      if (grepl(paste(v_secs_skip, collapse = "|"), v)) {
        next
      }

      r <- rast(nc_file, subds = v)

      layer_name <- str_split(v, "_")[[1]][3]
      GNFR_name <- dt_sec[name == layer_name, GNFRlong]

      dt <- data.table(
        Pollutant = species,
        Area = str_split(v, "_")[[1]][2],
        Year = y,
        Month = 1:12,
        GNFR = GNFR_name,
        Sector = layer_name,
        layer = names(r),
        emis_kt = global(r, sum, na.rm = T)$sum / 1000
      )

      l[[paste0(species, "_", y, "_", v)]] <- dt
    }
  } # end of year loop

  dt_all <- rbindlist(l, use.names = T)

  #return(dt_all)

  fwrite(dt_all, paste0(folname, "/", species, "_UKEIRE_nc_data.csv"))
}

#####################################################################################################
#### function to plot the UK summary data

plotUKnc <- function(species, naei_inv, map_yr_uk, tp_scheme) {
  folname <- paste0("outputs/EMEP4UK/inv", naei_inv, "/plots/UK/", tp_scheme)
  suppressWarnings(dir.create(folname, recursive = T))

  print(paste0(Sys.time(), ": Plotting ", species, " data in the UK..."))

  dt_lu <- as.data.table(read_excel(
    "../../inventory_processor/data/lookups/sector_lookups.xlsx",
    sheet = "GNFRtoSNAP"
  ))

  ###########################
  #### NETCDF input data ####
  ###########################

  # format the NC summary data
  dt <- fread(paste0(folname, "/", species, "_UKEIRE_nc_data.csv"))
  dt <- dt[Area == "UK"]
  dt <- dt[Pollutant == species]
  dt <- dt_lu[dt, on = "GNFR"]

  # aggregate nc to GNFR and SNAP
  dt_ncGNFRsum <- dt[,
    .(emis_kt = sum(emis_kt, na.rm = T)),
    by = .(Pollutant, Area, Year, GNFR, Sector)
  ] %>%
    .[, source := "UK"]
  dt_ncSNAPsum <- dt[,
    .(emis_kt = sum(emis_kt, na.rm = T)),
    by = .(Pollutant, Area, Year, SNAP)
  ] %>%
    .[, source := "UK"]

  ##############################
  #### NETCDF summary stats ####
  ##############################

  # bring in the data that is outwith the UK area (SEA and also to domain edge, lost to masking)
  v_years <- dt[, unique(Year)]
  v_nc_info <- paste0(
    "outputs/EMEP4UK/inv",
    naei_inv,
    "/emis",
    v_years,
    "/UKEIRE/TP",
    tp_scheme,
    "_AGGNA/",
    species,
    "_UKEIRE_",
    v_years,
    "emis_",
    map_yr_uk,
    "map_",
    naei_inv,
    "inv_SUMMARY.csv"
  )
  l_nc_info <- lapply(v_nc_info, fread)
  dt_nc_info <- rbindlist(l_nc_info, use.names = T)
  setnames(dt_nc_info, "Region", "Area")
  if (species == "voc") {
    dt_nc_info[, Pollutant := "voc"]
  }

  dt_nc_info <- dt_nc_info[
    !(sec_long %in% c("sec14", "sec15", "sec16", "sec17", "sec18", "sec19")) &
      Area == "uk" &
      Data_source == "Emissions_files"
  ]

  dt_nc_info[,
    c(
      "Area",
      "GNFR",
      "EMEP_Sector",
      "sec_long",
      "long_name",
      "Data_source",
      "Incoming_annual_kt",
      "Terres_annual_kt",
      "Terres_10_annual_kt",
      paste0("Terres_M", 1:12),
      "Terres_ann"
    ) := NULL
  ]
  setnames(
    dt_nc_info,
    c("SEA_annual_kt", "Out_of_Mask_annual_kt"),
    c("SEA", "OOM")
  )

  dtm_nc <- melt(
    dt_nc_info,
    id.vars = c("Pollutant", "SNAP", "Year"),
    variable.name = "Area",
    variable.factor = F,
    value.name = "emis_kt"
  )
  dtm_nc <- dtm_nc[,
    .(emis_kt = sum(emis_kt, na.rm = T)),
    by = .(Pollutant, Area, Year, SNAP)
  ] %>%
    .[, source := "x"]

  dt_terres_sea <- rbindlist(
    list(dt_ncSNAPsum, dtm_nc[Area == "SEA"]),
    use.names = T
  ) %>%
    .[, source := "SEA"]
  dt_terres_sea <- dt_terres_sea[,
    .(emis_kt = sum(emis_kt, na.rm = T)),
    by = .(Pollutant, Year, SNAP, source)
  ]
  dt_terres_sea[, Area := "SEA"]

  dt_terres_sea_oom <- rbindlist(list(dt_ncSNAPsum, dtm_nc), use.names = T) %>%
    .[, source := "ALL"]
  dt_terres_sea_oom <- dt_terres_sea_oom[,
    .(emis_kt = sum(emis_kt, na.rm = T)),
    by = .(Pollutant, Year, SNAP, source)
  ]
  dt_terres_sea_oom[, Area := "ALL"]

  dt_point_data <- rbindlist(
    list(dt_ncSNAPsum, dt_terres_sea, dt_terres_sea_oom),
    use.names = T
  )
  setnames(dt_point_data, "Area", "AREA")

  # make a totals table, for the totals line graph
  dt_nc_TOT_TER <- dt[,
    .(emis_kt = sum(emis_kt, na.rm = T)),
    by = .(Pollutant, Year, Area)
  ]
  setnames(dt_nc_TOT_TER, "Area", "AREA")
  dt_nc_TOT_TER[, source := "UK"]
  dt_nc_TOT_TER[, lt := "A"]

  dt_nc_TOT_TERSEA <- dt_terres_sea[,
    .(emis_kt = sum(emis_kt, na.rm = T)),
    by = .(Pollutant, Year, Area)
  ]
  setnames(dt_nc_TOT_TERSEA, "Area", "AREA")
  dt_nc_TOT_TERSEA[, source := "SEA"]
  dt_nc_TOT_TERSEA[, lt := "B"]

  dt_nc_TOT_TERSEADOM <- dt_terres_sea_oom[,
    .(emis_kt = sum(emis_kt, na.rm = T)),
    by = .(Pollutant, Year, Area)
  ]
  setnames(dt_nc_TOT_TERSEADOM, "Area", "AREA")
  dt_nc_TOT_TERSEADOM[, source := "ALL"]
  dt_nc_TOT_TERSEADOM[, lt := "C"]

  # shipping info

  dt_ship <- rbindlist(l_nc_info, use.names = T)
  setnames(dt_ship, "Region", "AREA")
  dt_ship[, c(paste0("Terres_M", 1:12), "Terres_ann") := NULL]
  dt_ship <- dt_ship[
    !(sec_long %in% c("sec14", "sec15", "sec16", "sec17", "sec18", "sec19")) &
      AREA == "uk" &
      Data_source == "Emissions_files"
  ]

  dt_ship <- dt_ship[
    SNAP == 8,
    c("Pollutant", "Year", "SNAP", "Out_of_Mask_annual_kt", "AREA")
  ]
  dt_ship <- dt_ship[,
    .(ship_kt = sum(Out_of_Mask_annual_kt, na.rm = T)),
    by = .(Pollutant, Year, SNAP, AREA)
  ]

  dt_ship <- dt_nc_TOT_TERSEADOM[dt_ship, on = c("Pollutant", "Year")][, c(
    "Pollutant",
    "Year",
    "emis_kt",
    "ship_kt"
  )] %>%
    setnames(., "emis_kt", "all_kt")
  dt_ship <- dt_nc_TOT_TERSEA[dt_ship, on = c("Pollutant", "Year")][, c(
    "Pollutant",
    "Year",
    "emis_kt",
    "all_kt",
    "ship_kt"
  )] %>%
    setnames(., "emis_kt", "sea_kt")

  dt_ship[, ship_share := ship_kt / (all_kt - sea_kt)]
  dt_ship[, lab := paste0("SN08 (outwith) = ", round(ship_share * 100, 0), "%")]

  if (species == "nox") {
    NULL
  } else if (species == "sox") {
    dt_ship <- dt_ship[Year <= 2010]
  } else if (species == "nh3") {
    NULL
  } else if (species == "co") {
    NULL
  } else if (species == "pm25") {
    dt_ship <- dt_ship[Year <= 2010]
  } else if (species == "pmco") {
    dt_ship <- dt_ship[Year <= 1975]
  } else if (species == "voc") {
    NULL
  }

  #############################
  #### full inventory data ####
  #############################

  dt_NAEI_SNAP <- fread(paste0(
    "../../inventory_processor/data/NAEI/inv",
    naei_inv,
    "/alpha/NAEI_AllPoll_TOTALS_inv",
    naei_inv,
    "_emis_1970-",
    naei_inv - 2,
    "_SNAP_alpha.csv"
  ))[AREA == "UK"]
  if (species != "voc") {
    dt_NAEI_SNAP <- dt_NAEI_SNAP[Pollutant == species]
  } else {
    dt_NAEI_SNAP <- dt_NAEI_SNAP[Pollutant == "nmvoc"]
  }

  setnames(dt_NAEI_SNAP, "tot_emis_t", "emis_t")
  dt_NAEI_SNAP[, "tot_alpha" := NULL]
  dt_NAEI_SNAP[, source := "INVENTORY"]

  dt_NAEI_GNFR <- fread(paste0(
    "../../inventory_processor/data/NAEI/inv",
    naei_inv,
    "/alpha/NAEI_AllPoll_TOTALS_inv",
    naei_inv,
    "_emis_1970-",
    naei_inv - 2,
    "_GNFR_alpha.csv"
  ))[AREA == "UK"]
  if (species != "voc") {
    dt_NAEI_GNFR <- dt_NAEI_GNFR[Pollutant == species]
  } else {
    dt_NAEI_GNFR <- dt_NAEI_GNFR[Pollutant == "nmvoc"]
  }

  setnames(dt_NAEI_GNFR, "tot_emis_t", "emis_t")
  dt_NAEI_GNFR[, "tot_alpha" := NULL]
  dt_NAEI_GNFR[, source := "INVENTORY"]

  # time series of point data from NAEI
  dt_pts_NAEI_SNAP <- fread(paste0(
    "../../inventory_processor/data/NAEI/inv",
    naei_inv,
    "/points/NAEI_AllPoll_POINTS_inv",
    naei_inv,
    "_emis_1990_",
    naei_inv - 2,
    "_SNAP_t_LL.csv"
  ))[AREA == "UK"]
  if (species != "voc") {
    dt_pts_NAEI_SNAP <- dt_pts_NAEI_SNAP[Pollutant == species]
  } else {
    dt_pts_NAEI_SNAP <- dt_pts_NAEI_SNAP[Pollutant == "nmvoc"]
  }

  dt_pts_NAEI_SNAP <- dt_pts_NAEI_SNAP[, c(
    "Pollutant",
    "Year",
    "SNAP",
    "AREA",
    "emis_t"
  )]
  dt_pts_NAEI_SNAP[, source := "POINTS_NAEI"]

  dt_pts_NAEI_GNFR <- fread(paste0(
    "../../inventory_processor/data/NAEI/inv",
    naei_inv,
    "/points/NAEI_AllPoll_POINTS_inv",
    naei_inv,
    "_emis_1990_",
    naei_inv - 2,
    "_GNFR_t_LL.csv"
  ))[AREA == "UK"]
  if (species != "voc") {
    dt_pts_NAEI_GNFR <- dt_pts_NAEI_GNFR[Pollutant == species]
  } else {
    dt_pts_NAEI_GNFR <- dt_pts_NAEI_GNFR[Pollutant == "nmvoc"]
  }

  dt_pts_NAEI_GNFR <- dt_pts_NAEI_GNFR[, c(
    "Pollutant",
    "Year",
    "GNFR",
    "AREA",
    "emis_t"
  )]
  dt_pts_NAEI_GNFR[, source := "POINTS_NAEI"]

  # SNAP data from SPEED
  #dt_pts_SPEED <- fread(paste0("../SPEED/power_station_emissions_ALL_1950-2000_GNFR_t_LL.csv"))[AREA == "UK" & Year >= 1960 & Year < 1990]
  #dt_pts_SPEED[, SNAP := 1]
  #dt_pts_SPEED <- dt_pts_SPEED[Pollutant == species]

  dt_SPEED <- fread(paste0(
    "../SPEED/SPEED_AllPoll_TOTALS_invNA_emis_1960-1970_SNAP_alpha.csv"
  ))
  dt_SPEED <- dt_SPEED[Pollutant == species]
  setnames(dt_SPEED, "tot_emis_t", "emis_t")

  dt_pts_SPEED_SNAP <- dt_SPEED[, c(
    "Pollutant",
    "Year",
    "SNAP",
    "AREA",
    "emis_t"
  )]
  dt_pts_SPEED_SNAP[, source := "SPEED"]
  #dt_pts_SPEED_GNFR <- dt_pts_SPEED[, c("Pollutant", "Year","GNFR","AREA","emis_t")]
  #dt_pts_SPEED_GNFR[, source := "POINTS_SPEED"]

  # bind and plot
  dt_SNAP_data <- rbindlist(
    list(dt_NAEI_SNAP, dt_pts_NAEI_SNAP, dt_pts_SPEED_SNAP),
    use.names = T
  )
  dt_SNAP_data <- dt_SNAP_data[,
    .(emis_kt = sum(emis_t, na.rm = T) / 1000),
    by = .(Pollutant, Year, SNAP, AREA, source)
  ]
  if (species == "voc") {
    dt_SNAP_data[, Pollutant := "voc"]
  }

  #dt_GNFR_data <- rbindlist(list(dt_NAEI_GNFR, dt_pts_NAEI_GNFR, dt_pts_SPEED_GNFR), use.names = T)
  #dt_GNFR_data <- dt_GNFR_data[, .(emis_kt = sum(emis_t, na.rm=T)/1000), by = .(Pollutant, Year, GNFR, AREA, source)]
  #if(species == "voc") dt_GNFR_data[, Pollutant := "voc"]

  # inventory totals and bind onto NC totals
  dt_NAEI_no99 <- dt_NAEI_SNAP[
    SNAP != 99,
    .(emis_kt = sum(emis_t, na.rm = T) / 1000),
    by = .(Pollutant, Year, AREA, source)
  ] %>%
    .[, lt := "D"]
  dt_NAEI_99 <- dt_NAEI_SNAP[,
    .(emis_kt = sum(emis_t, na.rm = T) / 1000),
    by = .(Pollutant, Year, AREA, source)
  ] %>%
    .[, c("source", "lt") := list("INVENTORY+INT.AVI", "E")]

  dt_plot_TOTS <- rbindlist(
    list(
      dt_NAEI_no99,
      dt_NAEI_99,
      dt_nc_TOT_TER,
      dt_nc_TOT_TERSEA,
      dt_nc_TOT_TERSEADOM
    ),
    use.names = T
  )
  if (species == "voc") {
    dt_plot_TOTS[, Pollutant := "voc"]
  }

  # plot EMEP input totals vs NAEI totals vs point source representation

  ## SNAP PLOT ##

  # info for labelling

  # label for industry
  dt_ind_lab <- data.table(SNAP = 3, lab = "NC = SN3 + SN4")
  ind_lab_y <- max(c(
    dt_SNAP_data[SNAP == 3, max(emis_kt)],
    dt_point_data[SNAP == 3, max(emis_kt)]
  )) *
    0.6

  # green point SNAP info
  if (species == "nox") {
    v_gp_SN <- c(1, 5, 8)
  } else if (species == "sox") {
    v_gp_SN <- c(5, 8)
  } else if (species == "nh3") {
    v_gp_SN <- c(8, 11)
  } else if (species == "co") {
    v_gp_SN <- c(1, 5)
  } else if (species == "pm25") {
    v_gp_SN <- 8
  } else if (species == "pmco") {
    v_gp_SN <- 8
  } else if (species == "voc") {
    v_gp_SN <- c(5, 8)
  }

  g_SNAP <- ggplot() +
    geom_line(
      data = dt_SNAP_data,
      aes(x = Year, y = emis_kt, group = source, colour = source)
    ) +
    geom_point(
      data = dt_point_data,
      aes(x = Year, y = emis_kt, group = source),
      colour = "#23CD38",
      size = 2
    ) +
    geom_line(
      data = dt_point_data[SNAP %in% v_gp_SN],
      aes(x = Year, y = emis_kt, group = source),
      colour = "#23CD38",
      alpha = 0.4
    ) +
    geom_text_repel(
      data = dt_point_data[SNAP %in% v_gp_SN],
      min.segment.length = 0,
      aes(x = Year, y = emis_kt, label = source),
      size = 3.5
    ) +
    geom_label(
      data = dt_ind_lab,
      aes(label = lab),
      x = 2001,
      y = ind_lab_y,
      hjust = 0,
      size = 3
    ) +
    scale_colour_manual(values = c("black", "blue", "red")) +
    labs(y = "Emissions (kt)") +
    geom_vline(xintercept = 2000, linetype = "dashed", colour = "black") +
    geom_vline(xintercept = 1990, linetype = "dashed", colour = "grey40") +
    facet_wrap(~SNAP, scales = "free_y") +
    theme(
      legend.position = "top",
      legend.title = element_blank(),
      legend.text = element_text(size = 14),
      strip.text = element_text(size = 14),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 11)
    )

  ## TOTAL PLOT ##

  # years for labels
  dt_plot_TOTS_labs <- copy(dt_plot_TOTS)
  dt_plot_TOTS_labs <- dt_plot_TOTS_labs[source %in% c("UK", "SEA", "ALL")]

  if (species == "nox") {
    NULL
  } else if (species == "sox") {
    dt_plot_TOTS_labs <- dt_plot_TOTS_labs[Year <= 2010]
  } else if (species == "nh3") {
    NULL
  } else if (species == "co") {
    NULL
  } else if (species == "pm25") {
    dt_plot_TOTS_labs <- dt_plot_TOTS_labs[Year <= 2010]
  } else if (species == "pmco") {
    dt_plot_TOTS_labs <- dt_plot_TOTS_labs[Year <= 1975]
  } else if (species == "voc") {
    NULL
  }

  g_TOTS <- ggplot() +
    geom_line(
      data = dt_plot_TOTS,
      aes(x = Year, y = emis_kt, group = lt, colour = lt, linetype = lt)
    ) +
    geom_line(
      data = dt_plot_TOTS[source %in% c("UK", "SEA", "ALL")],
      aes(x = Year, y = emis_kt, group = lt, colour = lt),
      alpha = 0.4
    ) +
    geom_point(
      data = dt_plot_TOTS[source %in% c("UK", "SEA", "ALL")],
      aes(x = Year, y = emis_kt, group = lt, colour = lt),
      size = 3
    ) +
    {
      if (species %in% c("nox", "sox", "pmco", "pm25", "voc")) {
        geom_text_repel(
          data = dt_plot_TOTS_labs,
          min.segment.length = 0,
          aes(x = Year, y = emis_kt, label = source),
          size = 4
        )
      }
    } +
    scale_colour_manual(
      name = "source",
      values = c(
        "D" = "black",
        "E" = "#078CBE",
        "A" = "#23CD38",
        "B" = "#23CD38",
        "C" = "#23CD38"
      ),
      breaks = c("D", "E"),
      labels = c("INVENTORY", "INVENTORY+INT.AVI")
    ) +
    scale_linetype_manual(
      name = "source",
      values = c("A" = 0, "B" = 0, "C" = 0, "D" = 1, "E" = 2),
      breaks = c("D", "E"),
      labels = c("INVENTORY", "INVENTORY+INT.AVI")
    ) +
    labs(y = "Emissions (kt)") +
    geom_vline(xintercept = 2000, linetype = "dashed", colour = "black") +
    geom_vline(xintercept = 1990, linetype = "dashed", colour = "grey40") +
    {
      if (species %in% c("nox", "sox", "pmco", "pm25", "voc")) {
        geom_text(
          data = dt_ship,
          aes(
            x = Year,
            y = all_kt + ((all_kt - sea_kt) / 2),
            label = lab,
            fontface = "italic"
          )
        )
      }
    } +
    #guides(linetype = "none")+
    theme(
      legend.position = "top",
      legend.title = element_blank(),
      legend.text = element_text(size = 14),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 11)
    )

  p1 <- g_SNAP / g_TOTS + plot_layout(heights = c(1.2, 1))

  p1 <- p1 +
    plot_annotation(
      title = paste0(
        species,
        ' emissions in the UK-NAEI inventory and EMEP4UK input files (green dots)'
      ),
      subtitle = 'UK = terrestrial emissions ; SEA = UK + 10km sea buffer ; ALL = all emissions, including those excluded to outer domain'
    ) &
    theme(
      plot.title = element_text(size = 24),
      plot.subtitle = element_text(size = 18)
    )

  ggsave(
    paste0(folname, "/", species, "_UK_NAEI_totals_vs_nc_SNAP.png"),
    p1,
    width = 16,
    height = 19
  )
  #ggsave(paste0("outputs/EMEP4UK/inv",naei_inv,"/plots/",species, "_UK_NAEI_totals_vs_nc_SNAP.png"), gSNAP, width = 13, height = 10)

  ## GNFR PLOT ##

  #GNFR_labs <- data.table(GNFR = sort(dt_GNFR_data[, unique(GNFR)]),
  #                        x = c(rep(NA,6), 1995, 1995, rep(NA,8)),
  #						  y = c(rep(NA,6), 50*0.8, 2.5*0.8, rep(NA,8)),
  #					  lab = c(rep(NA,6), "All in I_Offroad", "All in I_Offroad", rep(NA,8)))

  #gGNFR <- ggplot()+
  # geom_line(data = dt_GNFR_data, aes(x = Year, y = emis_kt, group = source, colour = source))+
  # scale_colour_manual(values = c("black","blue","red"))+
  # geom_point(data = dt_ncGNFRsum, aes(x = Year, y = emis_kt), colour = "#0BC822")+
  # geom_vline(xintercept = 2000, linetype="dashed", colour = "black")+
  # #geom_vline(xintercept = 1995, linetype="dashed", colour = "grey40")+
  # geom_vline(xintercept = 1990, linetype="dashed", colour = "grey40")+
  # geom_label(data = GNFR_labs, aes(x = x, y = y, label = lab), color = "black")+
  # facet_wrap(~GNFR, scales = "free_y")+
  # theme(legend.position = "top")

  #ggsave(paste0("outputs/EMEP4UK/inv",naei_inv,"/plots/",species, "_UK_NAEI_totals_vs_nc_GNFR.png"), gGNFR, width = 13, height = 10)
}

#####################################################################################################
#### function to plot the EIRE EMEP4UK input files for a vector of years and pollutants
#### the EIRE data should already be summarised via the UK function - just read data

plotEIREnc <- function(species, emep_inv, tp_scheme) {
  folname <- paste0("outputs/EMEP4UK/inv", emep_inv, "/plots/EIRE/", tp_scheme)
  suppressWarnings(dir.create(folname, recursive = T))

  print(paste0(Sys.time(), ": Plotting ", species, " data in EIRE..."))

  #dt_lu <- as.data.table(read_excel("../../inventory_processor/data/lookups/sector_lookups.xlsx", sheet = "GNFRtoSNAP"))

  ###########################
  #### NETCDF input data ####
  ###########################

  # format the NC summary data

  dt <- fread(paste0(
    "outputs/EMEP4UK/inv2023/plots/UK/",
    species,
    "_UKEIRE_nc_data.csv"
  ))
  dt <- dt[Area == "IE"]
  dt <- dt[Pollutant == species]
  #dt <- dt_lu[dt, on = "GNFR"]

  # aggregate nc to GNFR
  dt_ncGNFRsum <- dt[,
    .(emis_kt = sum(emis_kt, na.rm = T)),
    by = .(Pollutant, Area, Year, GNFR, Sector)
  ] %>%
    .[, source := "EIRE"]

  ########################
  #### full EMEP data ####
  ########################

  dt_EMEP_GNFR <- fread(paste0(
    "../../inventory_processor/data/EMEP/inv",
    emep_inv,
    "/alpha/EMEP_AllPoll_TOTALS_inv",
    emep_inv,
    "_emis_1970-",
    emep_inv - 2,
    "_GNFR_alpha.csv"
  ))[ISO2 == "IE"]
  if (species != "voc") {
    dt_EMEP_GNFR <- dt_EMEP_GNFR[Pollutant == species]
  } else {
    dt_EMEP_GNFR <- dt_EMEP_GNFR[Pollutant == "nmvoc"]
  }

  dt_EMEP_GNFR[, emis_kt := tot_emis_t / 1000]
  dt_EMEP_GNFR[, c("tot_emis_t", "tot_alpha") := NULL]
  dt_EMEP_GNFR[, source := "INVENTORY"]

  #####################
  #### CEDS add on ####
  #####################
  if (species != "voc") {
    dt_CEDS_GNFR <- fread(paste0(
      "../SPEED/CEDS_for_EMEP/",
      species,
      "_CEDS_1950_1990_ISO_GNFR_kt.csv"
    ))[ISO2 == "IE"]
  } else {
    dt_CEDS_GNFR <- fread(paste0(
      "../SPEED/CEDS_for_EMEP/nmvoc_CEDS_1950_1990_ISO_GNFR_kt.csv"
    ))[ISO2 == "IE"]
  }

  dt_CEDS_GNFR[, c("ISO3", "EMEP_code") := NULL]
  dt_CEDS_GNFR[, Pollutant := species]

  # make a table of actual CEDS values
  dt_CEDS_act <- copy(dt_CEDS_GNFR) %>% .[, alpha := NULL]
  dt_CEDS_act[, source := "CEDS_act"]

  # make a table of relative scaled CEDS values - from EMEP 1990
  dt_CEDS_reltemp <- copy(dt_CEDS_GNFR)

  dt_CEDS_reltemp <- dt_EMEP_GNFR[
    dt_CEDS_reltemp,
    on = c("Pollutant", "ISO2", "Year", "GNFR")
  ]
  dt_CEDS_reltemp[, emis_scal := emis_kt[Year == 1990] * alpha, by = GNFR]

  dt_CEDS_rel <- dt_CEDS_reltemp[, c(
    "Year",
    "ISO2",
    "GNFR",
    "emis_scal",
    "Pollutant"
  )] %>%
    .[, source := "CEDS_rel"]
  setnames(dt_CEDS_rel, "emis_scal", "emis_kt")

  # bind and plot
  dt_GNFR_data <- rbindlist(
    list(dt_EMEP_GNFR, dt_CEDS_act, dt_CEDS_rel),
    use.names = T
  )
  if (species == "voc") {
    dt_GNFR_data[, Pollutant := "voc"]
  }

  ## TOTALS
  dt1 <- dt_ncGNFRsum[,
    .(emis_kt = sum(emis_kt, na.rm = T)),
    by = .(Year, source)
  ]
  dt2 <- dt_GNFR_data[,
    .(emis_kt = sum(emis_kt, na.rm = T)),
    by = .(Year, source)
  ]
  dt_TOTS <- rbindlist(list(dt1, dt2), use.names = T)

  ## GNFR PLOT ##

  # info for labelling

  # label for industry
  #dt_ind_lab <- data.table(SNAP = 3, lab = "NC = SN3 + SN4")
  #ind_lab_y <- max(c(dt_SNAP_data[SNAP==3,max(emis_kt)], dt_point_data[SNAP==3,max(emis_kt)]))*0.6

  # green point SNAP info
  #if(species == "nox"){
  #  v_gp_SN <- c(1,5,8)
  #}else if(species == "sox"){
  #  v_gp_SN <- c(5,8)
  #}else if(species == "nh3"){
  #  v_gp_SN <- c(8,11)
  #}else if(species == "co"){
  #  v_gp_SN <- c(1,5)
  #}else if(species == "pm25"){
  #  v_gp_SN <- 8
  #}else if(species == "pmco"){
  #  v_gp_SN <- 8
  #}else if(species == "voc"){
  #  v_gp_SN <- c(5,8)
  #}

  g_GNFR <- ggplot() +
    geom_line(
      data = dt_GNFR_data,
      aes(x = Year, y = emis_kt, group = source, colour = source)
    ) +
    geom_point(
      data = dt_ncGNFRsum,
      aes(x = Year, y = emis_kt, group = source),
      colour = "#23CD38",
      size = 2
    ) +
    #geom_line(data = dt_point_data[SNAP %in% v_gp_SN], aes(x = Year, y = emis_kt, group = source), colour = "#23CD38", alpha=0.4)+
    #geom_text_repel(data = dt_point_data[SNAP %in% v_gp_SN], min.segment.length = 0, aes(x = Year, y = emis_kt, label = source), size = 3.5)+
    #geom_label(data = dt_ind_lab, aes(label = lab), x = 2001, y = ind_lab_y, hjust = 0, size = 3)+
    scale_colour_manual(values = c("black", "blue", "red")) +
    labs(y = "Emissions (kt)") +
    geom_vline(xintercept = 2000, linetype = "dashed", colour = "black") +
    geom_vline(xintercept = 1990, linetype = "dashed", colour = "grey40") +
    facet_wrap(~GNFR, scales = "free_y") +
    theme(
      legend.position = "top",
      legend.title = element_blank(),
      legend.text = element_text(size = 14),
      strip.text = element_text(size = 14),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 11)
    )

  ## TOTAL PLOT ##

  # years for labels
  #dt_plot_TOTS_labs <- copy(dt_plot_TOTS)
  #dt_plot_TOTS_labs <- dt_plot_TOTS_labs[source %in% c("UK","SEA","ALL")]

  #if(species == "nox"){
  #  NULL
  #}else if(species == "sox"){
  #  dt_plot_TOTS_labs <- dt_plot_TOTS_labs[Year <= 2010]
  #}else if(species == "nh3"){
  #  NULL
  #}else if(species == "co"){
  #  NULL
  #}else if(species == "pm25"){
  #dt_plot_TOTS_labs <- dt_plot_TOTS_labs[Year <= 2010]
  #}else if(species == "pmco"){
  #  dt_plot_TOTS_labs <- dt_plot_TOTS_labs[Year <= 1975]
  #}else if(species == "voc"){
  #  NULL
  #}

  g_TOTS <- ggplot() +
    geom_line(
      data = dt_TOTS,
      aes(x = Year, y = emis_kt, group = source, colour = source)
    ) +
    #geom_line(data = dt_plot_TOTS[source %in% c("UK","SEA","ALL")], aes(x = Year, y = emis_kt, group = lt, colour = lt), alpha=0.4)+
    #geom_point(data = dt_plot_TOTS[source %in% c("UK","SEA","ALL")], aes(x = Year, y = emis_kt, group = lt, colour = lt), size = 3)+
    #{if(species %in% c("nox","sox","pmco","pm25","voc"))geom_text_repel(data = dt_plot_TOTS_labs, min.segment.length = 0, aes(x = Year, y = emis_kt, label = source), size = 4)}+
    #scale_colour_manual(name = "source", values = c("D" = "black","E" = "#078CBE","A" = "#23CD38","B" = "#23CD38","C" = "#23CD38"), breaks = c("D","E"), labels = c("INVENTORY","INVENTORY+INT.AVI"))+
    #scale_linetype_manual(name = "source", values = c("A" = 0,"B" = 0,"C" = 0,"D" = 1,"E" = 2), breaks = c("D","E"), labels = c("INVENTORY","INVENTORY+INT.AVI"))+
    labs(y = "Emissions (kt)") +
    geom_vline(xintercept = 2000, linetype = "dashed", colour = "black") +
    geom_vline(xintercept = 1990, linetype = "dashed", colour = "grey40") +
    #{if(species %in% c("nox","sox","pmco","pm25","voc"))geom_text(data = dt_ship, aes(x = Year, y = all_kt + ((all_kt-sea_kt)/2), label = lab, fontface = "italic"))}+
    #guides(linetype = "none")+
    theme(
      legend.position = "top",
      legend.title = element_blank(),
      legend.text = element_text(size = 14),
      axis.title = element_text(size = 14),
      axis.text = element_text(size = 11)
    )

  p1 <- g_GNFR / g_TOTS + plot_layout(heights = c(1.2, 1))

  p1 <- p1 +
    plot_annotation(
      title = paste0(
        species,
        ' emissions in EIRE in the EMEP & CEDS inventory and EMEP4UK input files'
      ),
      subtitle = 'UK = terrestrial emissions ; SEA = UK + 10km sea buffer ; ALL = all emissions, including those excluded to outer domain'
    ) &
    theme(
      plot.title = element_text(size = 24),
      plot.subtitle = element_text(size = 18)
    )

  ggsave(
    paste0(folname, "/", species, "_EIRE_EMEP_totals_vs_nc_GNFR.png"),
    p1,
    width = 16,
    height = 19
  )
  #ggsave(paste0("outputs/EMEP4UK/inv",naei_inv,"/plots/",species, "_UK_NAEI_totals_vs_nc_SNAP.png"), gSNAP, width = 13, height = 10)

  ## GNFR PLOT ##

  #GNFR_labs <- data.table(GNFR = sort(dt_GNFR_data[, unique(GNFR)]),
  #                        x = c(rep(NA,6), 1995, 1995, rep(NA,8)),
  #						  y = c(rep(NA,6), 50*0.8, 2.5*0.8, rep(NA,8)),
  #					  lab = c(rep(NA,6), "All in I_Offroad", "All in I_Offroad", rep(NA,8)))

  #gGNFR <- ggplot()+
  # geom_line(data = dt_GNFR_data, aes(x = Year, y = emis_kt, group = source, colour = source))+
  # scale_colour_manual(values = c("black","blue","red"))+
  # geom_point(data = dt_ncGNFRsum, aes(x = Year, y = emis_kt), colour = "#0BC822")+
  # geom_vline(xintercept = 2000, linetype="dashed", colour = "black")+
  # #geom_vline(xintercept = 1995, linetype="dashed", colour = "grey40")+
  # geom_vline(xintercept = 1990, linetype="dashed", colour = "grey40")+
  # geom_label(data = GNFR_labs, aes(x = x, y = y, label = lab), color = "black")+
  # facet_wrap(~GNFR, scales = "free_y")+
  # theme(legend.position = "top")

  #ggsave(paste0("outputs/EMEP4UK/inv",naei_inv,"/plots/",species, "_UK_NAEI_totals_vs_nc_GNFR.png"), gGNFR, width = 13, height = 10)
}


#####################################################################################################
#### function to evaluate the EU EMEP4UK input files for a vector of years and pollutants
#### this is done with the EMEP ISO map, as the input files are summed to one EU area

evaluateEUnc <- function(species, v_years, emep_inv, tp_scheme, dt_sec) {
  if (tp_scheme %in% c("genYr", "ukem_genYr")) {
    next
  }

  folname <- paste0("outputs/EMEP4UK/inv", emep_inv, "/plots/EU/", tp_scheme)
  suppressWarnings(dir.create(folname, recursive = T))

  print(paste0(Sys.time(), ": Summarising ", species, " data in the EU..."))

  ## as the emissions are in one EU variable, use an EMEP domain raster to summarise inputs.
  r_EU <- rast("data/spatial/iso_map.tif")
  dt_iso <- fread("data/lookups/EMEP_territories.csv")

  # master list for annual data
  l <- list()

  for (y in v_years) {
    print(paste0(Sys.time(), ":                               ", y))

    ## choose output directory for the EMEP input files
    output_dir <- paste0(
      "/gws/ssde/j25b/ceh_generic/samtom/EMEP_inputs/outputs/EMEP4UK/inv",
      naei_inv,
      "/emis",
      y,
      "/EU/TP",
      tp_scheme,
      "_AGGoneEU"
    )

    nc_file <- paste0(
      output_dir,
      "/",
      species,
      "_EU_",
      y,
      "emis_",
      y,
      "map_",
      emep_inv,
      "inv_0.1.nc"
    )

    nc <- nc_open(nc_file)
    nc_secs <- names(nc$var)
    nc_close(nc)

    # go through all sectors to summarise. Skip some later ones.
    for (v in nc_secs) {
      v_secs_skip <- c("PublicPower_Point", "PublicPower_Area", "Exhaust")

      if (grepl(paste(v_secs_skip, collapse = "|"), v)) {
        next
      }

      # read the EU raster and resize to the ISO raster (v v small difference)
      r <- crop(extend(rast(nc_file, subds = v), r_EU), r_EU)

      layer_name <- str_split(v, "_")[[1]][3]
      GNFR_name <- dt_sec[name == layer_name, GNFRlong]

      dt_zonal <- as.data.table(terra::zonal(r, r_EU, fun = sum, na.rm = T))
      dtm_zonal <- melt(
        dt_zonal,
        id.vars = c("EMEP_code"),
        variable.name = "layer",
        variable.factor = F,
        value.name = "emis_t"
      )
      dtm_zonal <- dt_iso[dtm_zonal, on = c("EMEP_code")]

      # extra info
      dtm_zonal[, "Month" := tstrsplit(layer, "_", keep = 4)] %>%
        .[, Month := as.numeric(Month)]
      dtm_zonal[,
        c("Pollutant", "Year", "GNFR", "Sector", "emis_kt") := list(
          species,
          y,
          GNFR_name,
          layer_name,
          emis_t / 1000
        )
      ]

      dtm_zonal[, emis_t := NULL]

      l[[paste0(species, "_", y, "_", v)]] <- dtm_zonal
    }
  } # end of year loop

  lapply(l, names)

  dt_all <- rbindlist(l, use.names = T)

  #return(dt_all)

  fwrite(
    dt_all,
    paste0(
      "outputs/EMEP4UK/inv",
      emep_inv,
      "/plots/EU/",
      tp_scheme,
      "/",
      species,
      "_EU_nc_data.csv"
    )
  )
}

#####################################################################################################
#### function to plot the EU summary data

plotEUnc <- function(species, emep_inv, tp_scheme) {
  if (tp_scheme %in% c("genYr", "ukem_genYr")) {
    next
  }

  folname <- paste0("outputs/EMEP4UK/inv", emep_inv, "/plots/EU/", tp_scheme)
  suppressWarnings(dir.create(folname, recursive = T))

  print(paste0(Sys.time(), ": Plotting ", species, " data in the EU..."))

  # format the NC summary data
  dt <- fread(paste0(
    "outputs/EMEP4UK/inv2023/plots/EU/",
    species,
    "_EU_nc_data.csv"
  ))

  dt_EU <- dt[,
    .(emis_kt = sum(emis_kt, na.rm = T)),
    by = .(layer, Month, Pollutant, Year, GNFR, Sector)
  ]
  dt_EU[, c("EMEP_iso", "EMEP_code", "Name") := list("XX", NA, "EMEP_domain")]

  # aggregate nc to GNFR and SNAP
  dt_GNFRsum <- dt[,
    .(emis_kt = sum(emis_kt, na.rm = T)),
    by = .(EMEP_iso, EMEP_code, Name, Pollutant, Year, GNFR, Sector)
  ]
  dt_EU_GNFRsum <- dt_EU[,
    .(emis_kt = sum(emis_kt, na.rm = T)),
    by = .(EMEP_iso, EMEP_code, Name, Pollutant, Year, GNFR, Sector)
  ]

  dt_plot <- rbindlist(list(dt_GNFRsum, dt_EU_GNFRsum), use.names = T)

  # bring in some CEDS data
  dt_iso <- fread("../SPEED/proxy/ISO2_list.csv")

  if (species != "voc") {
    dt_ceds_data <- fread(paste0(
      "../SPEED/CEDS_for_EMEP/",
      species,
      "_CEDS_1950_1990_ISO_GNFR_kt.csv"
    ))
  } else {
    dt_ceds_data <- fread(paste0(
      "../SPEED/CEDS_for_EMEP/nmvoc_CEDS_1950_1990_ISO_GNFR_kt.csv"
    ))
  }

  dt_ceds <- dt_ceds_data[Year < 1990 & ISO2 != ""] # this is for tricky ISOs (below)
  dt_ceds <- dt_iso[dt_ceds, on = "ISO2"] %>%
    setnames(., "EMEP_ISO", "EMEP_iso")

  g_iso <- ggplot() +
    geom_line(data = dt_plot[emis_kt > 0], aes(x = Year, y = emis_kt)) +
    geom_point(data = dt_plot[emis_kt > 0], aes(x = Year, y = emis_kt)) +
    geom_line(data = dt_ceds, aes(x = Year, y = emis_kt), colour = "red") +
    facet_grid(EMEP_iso ~ GNFR, scales = "free_y") +
    geom_vline(xintercept = 1990, linetype = "dashed")

  ggsave(
    paste0(folname, "/", species, "_EU_totals_vs_nc_GNFR.png"),
    g_iso,
    width = 16,
    height = 45
  )
}
