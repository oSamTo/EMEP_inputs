#############################################################
#### FUNCTIONS FOR CREATION OF EMEP - GLOBAL INPUT FILES ####
#############################################################

#############################################################
#### function to take global emissions data, make ready  ####
####     to EMEP format and create netCDFs for Globe     ####

EMEP_GLOBAL_v5.0 <- function(
  data_source = c("HTAP", "EDGAR"),
  y,
  v_pollutants,
  time_dim = c("annual", "month", "yday"),
  v_EMEP_sec,
  glob_inv,
  folname,
  tp_scheme,
  global_agg_schema
) {
  # v_years  = vector, *numeric*, year to process.
  if (!is.numeric(v_years)) {
    stop("Year vector is not numeric")
  }

  print(paste0(
    format(Sys.time(), "%F %T"),
    ": Creating EMEP4UK GLOBAL inputs (",
    time_dim,
    " - ",
    data_source,
    " ",
    glob_inv,
    ") for ",
    y,
    "..."
  ))

  # For the years & pollutants, take the globals emissions in GNFR form;
  #   i) convert every country/sector into one raster, using an iso mask
  #  ii) split into monthly emissions, if needed
  # iii) create netCDF

  # For the globe, the emissions are taken from EDGAR or HTAP, which produce
  # global surfaces per pollutant/sector. In the latest EMEP version, we have
  # a variable for each pollutant-country, with dimensions for the sectors and
  # time.
  #
  # Therefore when using HTAP/EDGAR, we need to split that global coverage into
  # country areas using an ISO map. (As opposed to EMEP emissions which come
  # pre-split by country in a csv, but need to be rasterised).
  #
  # Much of the matching of ISOs and ISO areas, to the data, has been done
  # elsewhere (contact samtom@ceh.ac.uk). It's not exact, but best effort, and
  # emissions have been represented as best as possible.

  ## ISO information - summaries, look-ups, EMEP territory names etc.
  # HTAP ISO table - add on experimental SEA zone
  dt_iso <- fread(paste0("data/lookups/dt_iso_", data_source, ".csv"))
  dt_iso <- rbindlist(list(
    dt_iso,
    data.table(ISO_A3_EH = "SEA", ISO_N3_EH = 0)
  ))
  names(dt_iso) <- c("ISO_char", "ISO_num")

  ## HTAP specific zonal rasters
  # - These are 0.01 degree rasters, to capture data from HTAP more accurately.
  # - They are based on international ISO territories but boundaries have been
  # adjusted before rasterisation to match HTAP data better.
  # - There is a SEA mask to capture intl shipping
  # - Furthermore, land masks have been expanded into the sea and the SEA mask
  # expanded onto land to capture as much emission as possible.
  fname_iso <- paste0("data/spatial/iso_map_", data_source, ".tif")
  r_iso <- terra::rast(fname_iso)
  names(r_iso) <- "ISO_num"

  fname_iso_ship <- paste0(
    "data/spatial/iso_map_",
    data_source,
    "_ships.tif"
  )
  r_iso_ship <- terra::rast(fname_iso_ship)
  names(r_iso_ship) <- "ISO_num"

  ## (For ncdf data creation, a raster for each ISO has already been made) ##
  ## (Each one of these single ISO rasters takes ~30 secs to make on the fly) ##

  ####################################################
  #### CREATE THE NETCDF FILE TO PUT EMISSIONS IN
  fname_ncdf <- create_NETCDF_global(
    data_source,
    y,
    v_pollutants,
    folname,
    glob_inv,
    v_EMEP_sec,
    time_dim,
    global_agg_schema,
    dt_iso
  )

  for (species in v_pollutants) {
    ########
    if (
      !(species %in%
        c("nox", "sox", "nh3", "co", "voc", "pm25", "pm10", "pmco", "hcl"))
    ) {
      stop(
        "Species must be in:
                                            AP:    co, nh3, nox, sox, voc, hcl
                                            PM:    pm25, pm10, pmco"
      )
    }
    ########

    print(paste0(format(Sys.time(), "%F %T"), ":        ", species, " data:"))

    # root emissions folder
    fol_emis <- "/gws/ssde/j25b/ceh_generic/inventory_processor/data"

    # hcl comes from Zhang inventory
    if (species == "hcl") {
      emis_loc <- paste0(
        fol_emis,
        "/Zhang/inv2022/maps/tif"
      )
    } else {
      emis_loc <- paste0(
        fol_emis,
        "/",
        data_source,
        "/",
        glob_inv,
        "/maps/tif"
      )
    }

    # set up blank stack ready for data of different countries
    l_glob_emis <- list()

    # Unprocessed data_source sector totals list
    l_ds_tots <- list()

    # Processed sector totals list
    l_glob_summary <- list()

    # sector names to loop through - this is netcdf sector names (sec01 etc.)
    v_sectors <- dt_sec[GNFRlong != ""][EMEP_sec %in% v_EMEP_sec, unique(sec)]

    print(paste0(
      format(Sys.time(), "%F %T"),
      ":            gathering emissions..."
    ))

    for (i in v_sectors) {
      if (i == "") {
        next
      } # not interested in currently blank EMEP-named sectors
      if (dt_sec[sec == i, GNFRlong] == "") {
        next
      } # not interested in blank named GNFR sectors either

      print(paste0(
        format(Sys.time(), "%F %T"),
        ":                          ",
        dt_sec[sec == i, GNFRlong]
      ))

      #######################################################
      #### OBTAIN 12 MONTHS OF DATA FOR EACH GNFR SECTOR ####

      ## There is one GNFR sector per EMEP sector name
      # Global data has been converted form native sectors to GNFR
      # read in the GNFR data from the data_source, from inventory_processor
      # create a raster for every country in that sector, using the iso map
      # fill in missing countries/sectors with blanks

      # NOTE ABOUT AVI CRUISE? AGRI BURN?

      # THERE IS NO UK MASK (unless there is?)
      # --- could a custom mask/input be an option? e.g. uk or elsewhere

      ###################################
      #### LIST OF EMISSION SURFACES ####
      # evaluate the emissions input file only.
      l_EMEP_file <- summarise_EMEP_file(
        emis_loc,
        species,
        data_source,
        y,
        i,
        glob_inv,
        dt_iso,
        fname_iso,
        fname_iso_ship
      )

      l_ds_tots[[i]] <- l_EMEP_file[[2]]

      # bring in all emissions into stacks
      l_glob <- new_EMEP_sector_Emissions(
        fname_data = l_EMEP_file[[1]],
        i,
        y,
        species,
        dt_iso,
        data_source,
        r_iso,
        r_iso_ship
      )

      #l_glob <- EMEP_sector_Emissions(
      #  fname_data = l_EMEP_file[[1]],
      #  i,
      #  y,
      #  species,
      #  dt_iso,
      #  data_source
      #)

      ########################
      #### TEMPORAL SPLIT ####
      # if the time_dim is year, the data stays as 1 annual total.
      # if the time_dim is month, the data needs to be split into 12 layers
      # this is either;
      # the default (up to 2019) timing files in EMEP. <CURRENTLY THIS>
      # or something newer like EDGAR?
      l_glob_prof <- split_GLOBAL_annual(
        species = species,
        time_dim,
        tp_scheme,
        l_annual = l_glob,
        i = i
      )

      ###################
      #### COLLATING ####
      # add temporal raster stacks to lists;
      # this is now a list (sectors) of lists (countries by temporal split)
      l_glob_emis[[i]] <- l_glob_prof

      ####################
      #### STATISTICS ####
      # summarising the processed emissions files
      dt_totals <- summarise_GLOBAL_emissions(
        y,
        species,
        i,
        l_s = l_glob_prof,
        time_dim,
        glob_inv,
        data_source
      )

      l_glob_summary[[i]] <- dt_totals

      rm(l_glob)
      rm(l_glob_prof)
    } # sector loop

    ###########################################################
    #### summary files - emissions and processed emissions ####

    dt_emep_emis <- rbindlist(l_ds_tots, use.names = T)[order(ISO, GNFR)]

    dt_proc_emis <- rbindlist(l_glob_summary, use.names = T)[order(ISO, GNFR)]

    #######################################################
    #### AGGREGATE/RESHAPE BY SECTOR/ISO (IF REQUIRED) ####

    # The netcdf needs to have every sector at annual/month, per ISO/EU
    # tp_schema already taken care of above. Incoming data is;
    # l_glob_emis ---> list of 13 x sectors pollutant-1 a-1
    #             ---> each of which is a list of ~235 ISO sector-1
    #             ---> each of which is a stack of month/annual data ISO-1
    # this structure means the large emissions files are read in once
    # However the data need to be `pollutant_iso`; the sectors as a dimension.
    # Reshape here.

    # if agg_schema is;
    # 'oneGRID' : one GLOBAL territory file for month/annual data (i.e. no ISO).
    # 'allISO'  : separate ISO inputs for month/annual data.

    print(paste0(format(Sys.time(), "%F %T"), ":            Reshaping..."))
    l_global_toInp <- reshape_GLOBAL(
      y,
      species,
      global_agg_schema,
      time_dim,
      l_glob_emis
    )

    ###################################################
    #### INPUT DATA TO NETCDF TO SPECIES VARIABLES ####
    print(paste0(
      format(Sys.time(), "%F %T"),
      ":            populating netcdf..."
    ))

    # input data and summarise what's going in.
    dt_ncinput_summary <- input_data_NETCDF_global(
      y,
      species,
      glob_inv,
      time_dim,
      v_EMEP_sec,
      global_agg_schema,
      l_glob = l_global_toInp,
      fname_ncdf,
      dt_iso
    )

    ##############################
    #### QAQC; TABLES & PLOTS ####
    print(paste0(format(Sys.time(), "%F %T"), ":            summaries..."))

    # summarise the nc file itself, post writing. Double checker.
    # (bit of a time sap but perhaps worth it)
    dt_ncoutput_summary <- summarise_nc_file_global(
      fname_ncdf,
      y,
      species,
      glob_inv,
      time_dim,
      v_EMEP_sec,
      dt_iso,
      global_agg_schema
    )

    # write out polluatnt level tables. These can be used for plots etc.
    write_summaries_global(
      y,
      species,
      glob_inv,
      folname,
      dt_emep_emis = dt_emep_emis,
      dt_proc_emis = dt_proc_emis,
      dt_ncinp = dt_ncinput_summary,
      dt_ncout = dt_ncoutput_summary
    )

    # need clever plot function for all summaries;
    # dt_in_file
    # dt_emis_summary
    # dt_ncdf_summary

    rm(l_glob_emis)
    rm(dt_emep_emis)
    rm(l_glob_summary)
    rm(l_global_toInp)

    print(paste0(
      format(Sys.time(), "%F %T"),
      ":            pollutant complete."
    ))

    gc()
    terra::tmpFiles(remove = TRUE)
    unlink("dump/GLOBAL_write_dump/iso/*.tif")
  } # pollutant loop

  print(paste0(format(Sys.time(), "%F %T"), ": DONE."))
} # end of function

###############################################################################
#### function to summarise the emissions file, before anything is done to it.

summarise_EMEP_file <- function(
  emis_loc,
  species,
  data_source,
  y,
  i,
  glob_inv,
  dt_iso,
  fname_iso,
  fname_iso_ship
) {
  # v_years  = vector, *numeric*, year to process.
  if (!is.numeric(y)) {
    stop("Year vector is not numeric")
  }

  # read iso rasters inside parallel job
  r_iso_f <- terra::rast(fname_iso)
  names(r_iso_f) <- "ISO_num"

  r_iso_ship_f <- terra::rast(fname_iso_ship)
  names(r_iso_ship_f) <- "ISO_num"

  # set the diffuse filename - inventory version determines when produced
  # e.g. choose HTAP inventory v32

  # if the year is earlier than the earliest year, reset to earliest;
  # needs to be if-else for hcl as we need to convert to a csv

  # we read in the raster and make tables. This mimics the EMEP-EU approach

  if (species == "hcl") {
    # Zhang hcl data
    file_y <- ifelse(y < 1990, 1990, ifelse(y > 2014, 2014, y))

    f_diff <- paste0(
      emis_loc,
      "/",
      file_y,
      "/Zhang_tcl_DIFFUSE_inv2022_emis_",
      file_y,
      "/GNFR/Zhang_tcl_DIFFUSE_inv2022_emis_",
      file_y,
      "_GNFR_",
      dt_sec[sec == i, GNFRlong],
      "_t_LL.tif"
    )

    data_y <- copy(file_y)
  } else {
    # NOT hcl...
    # normal global data
    if (data_source == "HTAP") {
      file_y <- ifelse(y < 2000, 2000, y)
      file_y <- ifelse(y > 2020, 2020, y)
    } else if (data_source == "EDGAR") {
      file_y <- ifelse(y < 1970, 1970, y)
      file_y <- ifelse(y > 2022, 2022, y)
    }

    f_diff <- paste0(
      emis_loc,
      "/",
      file_y,
      "/",
      dt_poll[emep_model == species, invProc],
      "_DIFFUSE_emis_",
      file_y,
      "/GNFR/",
      data_source,
      "_",
      dt_poll[emep_model == species, invProc],
      "_DIFFUSE_",
      glob_inv,
      "_emis_",
      file_y,
      "_GNFR_",
      dt_sec[sec == i, GNFRlong],
      "_t_LL.tif"
    )

    data_y <- copy(file_y)
  }

  if (file.exists(f_diff)) {
    # get raster
    r_emis <- terra::crop(
      terra::extend(terra::rast(f_diff), r_dom_glob),
      r_dom_glob
    )

    # then we disagg it to get best summaries.
    r_emis <- disagg(r_emis, fact = 10)
    r_emis <- r_emis / 100 # due to disaggregation

    # stack with iso codes
    # this is now non-standard as EU has different codes.
    # need to summarise ships and territories with different rules.

    if (dt_sec[sec == i, GNFRlong] == "P_IntShipping") {
      dt_emis <- as.data.table(terra::zonal(
        r_emis,
        r_iso_ship_f,
        fun = "sum",
        na.rm = T
      ))
      # } else if (
      #  dt_sec[sec == i, GNFRlong] == "D_Fugitive" & species == "nmvoc"
      # ) {
      #  s <- c(r_iso_ship, r_emis)
    } else {
      dt_emis <- as.data.table(terra::zonal(
        r_emis,
        r_iso_f,
        fun = "sum",
        na.rm = T
      ))
    }

    # attach iso names
    dt_emis <- dt_iso[dt_emis, on = "ISO_num"]

    # Global data uses ISO on the ISO3 system
    setnames(dt_emis, c("ISO_char"), c("ISO3"))
    setnames(dt_emis, dt_sec[sec == i, GNFRlong], c("emis_t"))

    # format and subset
    dt_emis[, c("Year", "Pollutant", "GNFR") := list(y, species, i)]

    dt_emis <- dt_emis[!is.na(ISO3)]

    dt_emis <- dt_emis[, c(
      "ISO3",
      "Year",
      "GNFR",
      "Pollutant",
      "emis_t"
    )]
  } else {
    # empty table if file doesn't exist
    dt_emis <- data.table(
      ISO3 = character(),
      Year = numeric(),
      GNFR = character(),
      Pollutant = character(),
      emis_t = numeric()
    )
  }

  # format a dt_totals table
  dt_tot <- copy(dt_emis)

  dt_tot[, year_of_data := data_y]

  dt_tot[,
    c(
      "SNAP",
      "Region",
      "EMEP_Sector",
      "sec_long",
      "long_name",
      "Data_source"
    ) := list(
      dt_sec[sec == i, SNAP],
      "global",
      dt_sec[sec == i, EMEP_sec],
      i,
      dt_sec[sec == i, name],
      if (species == "hcl") {
        "Zhang_tif"
      } else {
        paste0(data_source, "_unprocessed")
      }
    )
  ]

  setnames(dt_tot, "ISO3", "ISO")

  # Check if the ISO codes in the emissions data are present in the v_iso
  # If not, something needs looking at - is the latest model version being used?
  v_iso_emis <- unique(dt_emis[, ISO3])
  if (any(!(v_iso_emis %in% dt_iso[, ISO_char]))) {
    stop(
      "There are extra ISO codes in the emissions - check the model version!"
    )
  }

  # Finally, we have to write the raster and pass the filename.
  # We are parallelizing at the ISO level, so we need to write out the
  # high-res raster to pass to cores (can't pass a rast object in mclapply).
  # (Well, you can, but you can run into memory issues VERY quickly).
  fname <- paste0(
    "dump/GLOBAL_write_dump/array_",
    i_a,
    "_highresSector_",
    i,
    ".tif"
  )
  terra::writeRaster(
    r_emis,
    fname,
    overwrite = TRUE
  )

  # here we return the gridded emissions (not table as per EU), plus totals
  return(list(fname, dt_tot))
}

###############################
#### new test function to speed up ISO raster creation
new_EMEP_sector_Emissions <- function(
  fname_data,
  i,
  y,
  species,
  dt_iso,
  data_source,
  r_iso,
  r_iso_ship
) {
  sector_long <- dt_sec[sec == i, GNFRlong]

  # use ship mask for P_IntShipping
  is_shipping <- sector_long == "P_IntShipping"
  is_fugitive_voc <- species == "voc" && sector_long == "D_Fugitive"

  # allow the use of iso_code = 0 in the r_iso, if voc + D_Fugitive
  allow_sea <- is_shipping || is_fugitive_voc

  # 0.01° emissions
  r_emis_001 <- rast(fname_data)

  # 0.01° ISO mask - choose
  r_iso_001 <- if (is_shipping) {
    copy(r_iso_ship)
  } else {
    copy(r_iso)
  }

  # create 0.1° cell ID raster, then disaggregate to 0.01°
  r_cell_01deg <- init(r_dom_glob, "cell")
  r_cell_001deg <- terra::disagg(r_cell_01deg, fact = 10, method = "near")

  # stack emissions, ISO, and target 0.1° cell ID
  s <- c(r_emis_001, r_iso_001, r_cell_001deg)
  names(s) <- c("emis", "iso", "cell_001deg")

  # make a huge data table

  dt <- as.data.table(values(s, dataframe = TRUE, na.rm = TRUE))

  dt <- dt[!is.na(iso) & !is.na(cell_001deg)]

  # apply SEA rule before aggregation
  if (!allow_sea) {
    dt <- dt[iso != 0]
  }

  if (sector_long == "P_IntShipping") {
    dt <- dt[iso == 0]
  }

  # aggregate at 0.01° precision into final 0.1° EMEP cells
  dt_sum <- dt[,
    .(emis_t = sum(emis, na.rm = TRUE)),
    by = .(iso, cell_001deg)
  ]

  # list of iso rasters

  l_rt <- lapply(dt_iso[, ISO_num], function(x) {
    temp_iso_raster(
      species,
      i,
      dt_sum,
      iso_code = x,
      dt_iso,
      r_template = r_dom_glob
    )
  })

  names(l_rt) <- dt_iso[, ISO_char]

  rm(dt, dt_sum, s, r_emis_001, r_iso_001, r_cell_001deg)
  gc()
  terra::tmpFiles(orphan = TRUE, old = FALSE, remove = TRUE)

  return(l_rt)
}


temp_iso_raster <- function(species, i, dt_sum, iso_code, dt_iso, r_template) {
  r <- copy(r_template)

  # set values to 0 to ensure we dont have to do that later.
  terra::values(r) <- 0

  d <- dt_sum[iso == iso_code]

  if (nrow(d) > 0) {
    r[d$cell_001deg] <- d$emis_t
  }

  # name
  names(r) <- paste0(
    species,
    "_",
    dt_iso[ISO_num == iso_code, ISO_char],
    "_sector=",
    dt_sec[sec == i, EMEP_sec]
  )

  r
}


###############################################################################
#### function to collect sector data, per country, for diffuse emissions
EMEP_sector_Emissions <- function(
  fname_data,
  i,
  y,
  species,
  dt_iso,
  data_source
) {
  # fname_data is a pointer to the temp high-res emissions raster (global).

  ################
  ### STACKING ###

  # Using pre-made ISO rasters, mask the data to just that country.
  # NO UK-domain masking takes place, or for any other domain.

  # Only International Shipping uses "SEA" (all pollutants)
  # PLUS Fugitive (NMVOCs only).

  # l_r <- lapply(dt_iso[, ISO_char], function(x) {
  #  ISO_sector_raster(
  #    x,
  #    r = fname_data,
  #    i,
  #    species,
  #    y
  #  )
  # })
  # names(l_r) <- dt_iso[, ISO_char]

  # parallel lapply using a small function - which includes setting NA to 0
  # use all ISO codes, make a blank surface if needed.

  # Straightforward lapply using all 235 codes using non-subsetted ISO map
  # was > 60 mins (80 ish?).
  # Then we moved to pre-made ISO rasters for each country but still slow.
  # This is now a parallel operation which presents many challenges re temp
  # files and memory, but is much faster, ~ 16 mins.

  n_cores <- 16
  iso_chunks <- split(
    dt_iso$ISO_char,
    ceiling(seq_along(dt_iso$ISO_char) / n_cores)
  )

  l_out_all <- list()

  for (k in seq_along(iso_chunks)) {
    v_iso <- iso_chunks[[k]]

    l_out <- parallel::mclapply(
      v_iso,
      function(x) {
        suppressMessages({
          f_out <- ISO_sector_raster(
            iso = x,
            fname = fname_data,
            i = i,
            species = species,
            y = y,
            data_source = data_source
          )
        })
      },
      mc.cores = n_cores,
      mc.preschedule = FALSE
    )
    l_out_all <- c(l_out_all, l_out)

    gc()
    terra::tmpFiles(orphan = TRUE, old = FALSE, remove = TRUE)
  }

  # test
  # l_out <- lapply(
  #   dt_iso[, ISO_char],
  #   function(x) {
  #     suppressMessages({
  #       f_out <- ISO_sector_raster(
  #         iso = x,
  #         fname = fname_data,
  #         i = i,
  #         species = species,
  #         y = y,
  #         data_source = data_source
  #       )
  #     })
  #   }
  # )

  # gc()
  # terra::tmpFiles(orphan = TRUE, old = FALSE, remove = TRUE)

  # Read all the files back in as rasters, and put in a list with ISO names.
  # This avoids passing lots of data between cores (v problematic).

  l <- lapply(l_out_all, rast)

  names(l) <- dt_iso[, ISO_char]
  #names(l_out) <- dt_iso[, ISO_char]

  #return(l_out_all)
  return(l)
} # end of function

###############################################################################
#### function to create raster from ISO information in emissions grid

ISO_sector_raster <- function(
  iso,
  fname,
  i,
  species,
  y,
  data_source
) {
  # read the high-res emissions raster for this sector
  r <- rast(fname)

  # read in the correct high-res mask, pre-generated.
  r_iso_sub <- rast(paste0(
    "data/spatial/",
    data_source,
    "_iso_grids/iso_grid_",
    iso,
    ".tif"
  ))

  # Use blank if the ISO is SEA and the sector is not international shipping.
  if (
    iso == "SEA" &&
      species == "voc" &&
      dt_sec[sec == i, GNFRlong] %in% c("D_Fugitive", "P_IntShipping")
  ) {
    r_masked <- mask(r, r_iso_sub)
    # aggregate back to 0.1 degree, to match EMEP grid.
    r_agg <- aggregate(r_masked, fact = 10, fun = sum, na.rm = T)
  } else if (iso == "SEA" && dt_sec[sec == i, GNFRlong] != "P_IntShipping") {
    # use empty domain - don't aggregate (already at 0.1 degree).
    r_agg <- copy(r_dom_glob)
  } else {
    # now mask the emissions raster to this ISO area
    r_masked <- mask(r, r_iso_sub)
    # aggregate back to 0.1 degree, to match EMEP grid.
    r_agg <- aggregate(r_masked, fact = 10, fun = sum, na.rm = T)
  }

  # set NA to 0 (for EMEP), and NaN, and Infinite
  r_agg[is.na(r_agg)] <- 0
  r_agg[is.nan(r_agg)] <- 0
  r_agg[is.infinite(r_agg)] <- 0

  # name
  names(r_agg) <- paste0(
    species,
    "_",
    iso,
    "_sector=",
    dt_sec[sec == i, EMEP_sec]
  )

  # print(iso)

  # write this out as a temp dump file to work across cores
  fname_dump <- paste0(
    "dump/GLOBAL_write_dump/iso/array_",
    i_a,
    "_",
    names(r_agg),
    ".tif"
  )
  terra::writeRaster(r_agg, fname_dump, overwrite = TRUE)

  return(fname_dump)
}

###############################################################################
#### function to split annual emissions out into months (or keep as annual)
split_GLOBAL_annual <- function(
  species,
  time_dim = c("annual", "month"),
  tp_scheme,
  l_annual,
  i
) {
  time_dim <- match.arg(time_dim)

  # set time splits
  if (time_dim == "annual") {
    i_time <- 1
  } else {
    i_time <- 1:12
  }

  # if time dim is a annual, no split
  if (time_dim == "annual") {
    l_s <- copy(l_annual)
    names(l_s) <- names(l_annual)
  } else {
    stop("Develop monthly splits for global data - not ready/tested yet.")
    ## Use the nominated temporal schema to split the data to monthly layers

    ## ! HAVE TO READ IN RASTERS TO SPLIT THEM

    # NOT INCORPORATED:
    ## Option to change this to use EDGAR
    # we could use the EDGAR generated regional profiles.

    if (tp_scheme %in% c("EMEP4UKv4.45", "EMEP4UKv5.0")) {
      # If the tp_scheme = version of EMEP4UK (e.g. 'EMEP4UKv4.45');
      # use EMEP defaults of that version
      # read in timing file for legacy temporal splits
      # (subset to ISO)
      dt_timing <- fread(paste0(
        "/gws/nopw/j04/ukem/test/sam/ukem_pro/output/model_inputs/EMEP4UK/",
        tp_scheme,
        "/MonthlyFacs.",
        species
      ))

      names(dt_timing) <- c("ISO", "SNAP", month.abb[1:12])
      dt_profs <- melt(
        dt_timing,
        id.vars = c("ISO", "SNAP"),
        variable.name = "MON",
        value.name = "FAC"
      )

      # as there are up to 60 ISO codes in the list/stack, best to use lapply
      l_s <- lapply(names(l_annual), function(x) {
        emep_ISO_profile(x, species, dt_profs, i_time, l_annual, i)
      })

      names(l_s) <- names(l_annual)
    } else {
      # THIS IS FOR POTENTIAL EDGAR USAGE
    }
  } # end of time_dim = month

  return(l_s) # return the monthly/annual emissions
} # end function

###############################################################################
#### function to apply temporal profile splits by ISO ID
emep_ISO_profile <- function(iso, species, dt_profs, i_time, l_annual, i) {
  ## THIS HAS NOT BEEN DEVELOPED/TESTED FOR GLOBAL DATA.

  # subset list
  r <- l_annual[[iso]]

  # collect the country code and the SNAP ID
  country_id <- dt_iso[EMEP_iso == iso, EMEP_code]
  # set the SNAP to read from the timing.
  snap_id <- dt_sec[sec == i, as.numeric(SNAP)]

  # extract required timing data - Use 1s if the SNAP sector in dt_dec is "NA"
  if (is.na(snap_id)) {
    # If snap is NA in the sector file
    v_timing <- rep(1, 12)
  } else if (!(snap_id %in% dt_profs[ISO == country_id, SNAP])) {
    # if SNAP is not in the timing file
    v_timing <- rep(1, 12)
  } else if (!(country_id %in% dt_profs[, ISO])) {
    # if country ISO is not in timing file
    v_timing <- rep(1, 12)
  } else {
    # vector of monthly splits
    v_timing <- dt_profs[ISO == country_id & SNAP == snap_id][["FAC"]]
    # not always adding to 12 in the timing file, adjust slightly
    v_timing <- v_timing / mean(v_timing)
  }

  # make a standard 1 month raster. Annual/12.
  s_month <- rast(lapply(r, function(x) rep(x / 12, 12)))

  # adjust with temporal profile
  s <- s_month * v_timing

  names(s) <- paste0(
    iso,
    "_",
    i,
    "_emis_t_",
    str_pad(i_time, 2, "0", side = "left")
  )

  return(s)
}

###############################################################################
#### function to summarise input emissions
summarise_GLOBAL_emissions <- function(
  y,
  species,
  i,
  l_s,
  time_dim,
  glob_inv,
  data_source
) {
  # set time splits
  if (time_dim == "annual") {
    i_time <- 1
  } else {
    i_time <- 1:12
  }

  # l_s should always be full length of ISOs, even if empty emissions.
  # Inventory processor should ensure every GNFR sector is made, even if empty.

  l <- lapply(l_s, function(x) global(x, sum, na.rm = T))

  l_dt <- lapply(l, function(x) as.data.table(x))

  l_dt <- lapply(names(l_s), function(x) {
    l_dt[[x]][, layer_name := names(rast(l_s[[x]]))]
  })
  names(l_dt) <- names(l)
  l_dt <- lapply(1:length(l_dt), function(x) {
    l_dt[[x]][, ISO := names(l_dt)[x]]
  })
  l_dt <- lapply(l_dt, function(x) x[, step := paste0("t", i_time)])
  dt_emis <- rbindlist(l_dt, use.names = T)
  setnames(dt_emis, "sum", "emis_t")
  dt_w <- dcast(dt_emis, ISO + layer_name ~ step, value.var = "emis_t")

  # summarise the emissions totals
  dt <- data.table(
    Year = y,
    Pollutant = species,
    Region = "global",
    GNFR = dt_sec[sec == i, GNFRlong],
    SNAP = dt_sec[sec == i, SNAP],
    EMEP_Sector = dt_sec[sec == i, EMEP_sec],
    sec_long = i,
    long_name = dt_sec[sec == i, name],
    Data_source = "EMEP_processed",
    Data = data_source,
    Data_version = glob_inv
  )

  dt_i <- cbind(dt, dt_w)
  dt_i[,
    ann_emis_kt := rowSums(.SD, na.rm = T) / 1000,
    .SDcols = paste0("t", i_time)
  ]

  return(dt_i)
} # end of function

###############################################################################
#### function to aggregate the emissions in a nominated way;
#### e.g. by sector, by ISO grouping etc
reshape_GLOBAL <- function(
  y,
  species,
  global_agg_schema = c("allISO", "oneGRID"),
  time_dim,
  l_glob_emis
) {
  global_agg_schema <- match.arg(global_agg_schema)

  if (time_dim == "annual") {
    i_time <- 1
  } else {
    i_time <- 1:12
  }
  n_time <- length(i_time)

  if (global_agg_schema == "allISO") {
    # no need to aggregate to GLOBAL - only reshape to lists of sectors ISO-1
    l_reshaped <- purrr::list_transpose(l_glob_emis)

    # but do not collapse sectoral list (per ISO) into stacks, as;
    # a) that overwrites names.
    # b) we can have time disaggregated stacks per sector.

    return(l_reshaped)
  } else if (global_agg_schema == "oneGRID") {
    # aggregates all data into one GLOBAL surface, per sector per time step.
    # this heavily reduces the variables to read (from ISO level)
    # but retains the separation by sector

    # convert all list elements in rast stacks (12 x n.ISO) - skip if empty
    l_s <- lapply(l_glob_emis, function(x) aggToGLOBAL(x))

    # the new rasts are months 1 to 12, by ISO code.
    # the layer names suggest it is one month of ~60 ISOs, repeated
    # but this is WRONG

    l_s_sum <- lapply(l_s, function(x) sumMonths(x, n_time))

    return(l_s_sum)
  }
}

#### sub-functions for aggregating GLOBAL data

# function to allow for empty list elements (e.g. CO in AgriLivestock)
aggToGLOBAL <- function(l) {
  if (length(l) == 0) {
    return(l)
  } else {
    l2 <- rast(l)
    return(l2)
  }
}

sumMonths <- function(s, n_time) {
  if (length(s) == 0) {
    return(s)
  } else {
    s12 <- tapp(s, index = 1:n_time, sum, na.rm = T)
    return(s12)
  }
}


###############################################################################
#### function to create a netCDF and input the data - simple routine chooser
create_NETCDF_global <- function(
  data_source,
  y,
  v_pollutants,
  folname,
  glob_inv,
  v_EMEP_sec,
  time_dim,
  global_agg_schema,
  dt_iso
) {
  if (time_dim == "annual") {
    fname <- create_NETCDF_global_annual(
      data_source,
      y,
      v_pollutants,
      folname,
      glob_inv,
      v_EMEP_sec,
      time_dim,
      global_agg_schema,
      dt_iso
    )
  } else if (time_dim == "month") {
    fname <- create_NETCDF_global_month()
  }

  return(fname)
}

###############################################################################
#### function to create a netCDF and input the data;
#### this is the ANNUAL/MONTHLY input (EMEPv5.0)
create_NETCDF_global_annual <- function(
  data_source,
  y,
  v_pollutants,
  folname,
  glob_inv,
  v_EMEP_sec,
  time_dim,
  global_agg_schema,
  dt_iso
) {
  if (time_dim != "annual") {
    stop("time choice has to be annual to make an annual netcdf.")
  }

  # create output directory
  dir.create(file.path(folname), showWarnings = FALSE, recursive = T)

  # create netcdf name
  nc_filename <- paste0(folname, "/GLOBAL_", y, "emis_0.1.nc")

  # if the file already exists, just delete and rewrite
  if (file.exists(nc_filename)) {
    print(paste0(
      "NetCDF already exists in this folder location; DELETING & REPLACING..."
    ))
    file.remove(nc_filename)
  } else {}

  ## EMEPv5.0 file creation.
  # this contains every pollutant_iso as variables (235 * 7).
  # pollutant_iso variable has sector dim = length(sectors);
  # length of sectors is determined by dt_sec
  # dt_iso determines the iso codes.

  # Set up the dimensions: latlong, time, sectors
  v_lon <- as.array(seq(
    xmin(r_dom_glob) + 0.1 / 2,
    xmax(r_dom_glob) - 0.1 / 2,
    0.1
  ))
  n_lon <- length(v_lon)

  v_lat <- as.array(seq(
    ymin(r_dom_glob) + 0.1 / 2,
    ymax(r_dom_glob) - 0.1 / 2,
    0.1
  ))
  n_lat <- length(v_lat)

  v_sector <- v_EMEP_sec
  n_sector <- length(v_sector)

  v_time <- 1 # this has to be 1 for this annual function.
  n_time <- length(v_time)

  # create dimensions
  dimlon <- ncdim_def(
    name = "lon",
    longname = "longitude",
    units = "degrees_east",
    vals = v_lon,
    unlim = FALSE
  )
  dimlat <- ncdim_def(
    name = "lat",
    longname = "latitude",
    units = "degrees_north",
    vals = v_lat,
    unlim = FALSE
  )
  dimsecs <- ncdim_def(
    name = "sector",
    longname = "GNFR sector index",
    units = "",
    vals = v_sector,
    unlim = FALSE
  )
  dimtime <- ncdim_def(
    name = "time",
    longname = "time",
    units = "",
    vals = v_time,
    unlim = FALSE
  )

  # create the dims as variables.
  # ncdim_lon <- ncvar_def(name = "lon", longname = "longitude", units = "degrees_east",
  # 				  	  		   dim = list(dimlon), compression = 4, prec = "float")
  #  ncdim_lat <- ncvar_def(name = "lat", longname = "latitude", units = "degrees_north",
  # 				  	  		   dim = list(dimlat), compression = 4, prec = "float")
  #  ncdim_sec <- ncvar_def(name = "sector", longname = "GNFR sector index", units = "",
  # 				  	  		   dim = list(dimsecs), compression = 4, prec = "float")
  #  ncdim_tim <- ncvar_def(name = "time", longname = "time index", units = "",
  # 				  	  		   dim = list(dimtime), compression = 4, prec = "float")
  #
  #  l_dim_var <- list(ncdim_lon, ncdim_lat, ncdim_sec, ncdim_tim)

  # Create names and variables for all pollutants_ISOs SEPARATELY
  if (global_agg_schema == "oneGRID") {
    v_vars <- unlist(lapply(v_pollutants, function(x) paste0(x, "_GLOBAL")))
  } else if (global_agg_schema == "allISO") {
    v_iso_emep <- sort(dt_iso[, ISO_char])
    v_vars <- unlist(lapply(v_pollutants, function(x) {
      paste0(x, "_", v_iso_emep)
    }))
  }

  # for each pollutant_ISO variable, create a new netcdf var
  l_iso_var <- lapply(X = 1:length(v_vars), function(s) {
    ncvar_def(
      name = v_vars[s],
      units = "tonnes/year",
      # missval = EMEP_fillval, # _FillValue ?
      dim = list(dimlon, dimlat, dimsecs, dimtime),
      compression = 4,
      prec = "float"
    )
  })

  # combine the variable lists
  #  l_var	<- c(l_dim_var, l_iso_var)

  ## Create the new netcdf
  nc_new <- nc_create(nc_filename, l_iso_var, force_v4 = T)

  ###############
  ## NO - see Janice Scheffler email 28/01/25

  # now ADD the summary variables, that dont have a sector dim.
  # l_sum_var <- lapply(X = 1:length(v_pollutants), function(s){
  #                   ncvar_def(name = v_pollutants[s],
  #                             units = "tonnes/year",
  # 				  	  		   #missval = EMEP_fillval, # _FillValue ?
  # 				               dim = list(dimlon, dimlat, dimtime),
  #                             compression = 4,
  #                             prec = "float")})

  # can var_add list of new variables, so loop
  # for(j in 1:length(l_sum_var)){
  #  nc_new <- ncvar_add(nc_new, l_sum_var[[j]])
  # }
  ###############

  # Finally the global attributes
  ncatt_put(nc_new, 0, "description", "GLOBAL_EMEP", prec = "char")
  ncatt_put(nc_new, 0, "Conventions", "CF-1.6 for coordinates", prec = "char")
  ncatt_put(
    nc_new,
    0,
    "created_date",
    format(Sys.Date(), "%Y%m%d"),
    prec = "int"
  )
  ncatt_put(
    nc_new,
    0,
    "created_hour",
    gsub(":", "", format(Sys.time(), "%F %R")),
    prec = "double"
  )
  ncatt_put(nc_new, 0, "projection", "lon lat", prec = "char")
  ncatt_put(nc_new, 0, "periodicity", "yearly", prec = "char")

  # 3 extras by me
  ncatt_put(nc_new, 0, "Grid_resolution", "0.1", prec = "char")
  ncatt_put(
    nc_new,
    0,
    "Created_with",
    R.Version()$version.string,
    prec = "char"
  )
  ncatt_put(
    nc_new,
    0,
    "ncdf4_version",
    packageDescription("ncdf4")$Version,
    prec = "char"
  )

  # emissions data source
  ncatt_put(nc_new, 0, "EMISSIONS_SOURCE", data_source, prec = "char")
  ncatt_put(nc_new, 0, "EMISSIONS_VERSION", glob_inv, prec = "char")

  # sectors - this might have to change if the amount of sectors input changes
  ncatt_put(nc_new, 0, "SECTORS_NAME", "GNFR", prec = "char")

  for (i in v_EMEP_sec) {
    glob_att_name <- dt_sec[GNFRlong != "" & EMEP_sec == i, sec]
    glob_att_val <- dt_sec[GNFRlong != "" & EMEP_sec == i, GNFRlong]

    ncatt_put(nc_new, 0, glob_att_name, glob_att_val, prec = "char")
  }

  # ncatt_put(nc_new, 0, "NCO","netCDF Operators version 4.9.8 (Homepage = http://nco.sf.net, Code = http://github.com/nco/nco)", prec = "char")

  if ("hcl" %in% v_pollutants) {
    ncatt_put(
      nc_new,
      0,
      "non-UK HCl",
      "doi.org/10.1021/acs.est.1c05634",
      prec = "char"
    )
  }

  # close connection
  nc_close(nc_new)

  return(nc_filename)
} # end of function


###############################################################################
#### function to create a netCDF and input the data
#### this is the MONTHLY input (EMEPv4.45)
create_NETCDF_global_month <- function() {}

###############################################################################
#### function to put data into the pre-made netcdf file
input_data_NETCDF_global <- function(
  y,
  species,
  glob_inv,
  time_dim,
  v_EMEP_sec,
  global_agg_schema,
  l_glob,
  fname_ncdf,
  dt_iso
) {
  if (length(l_glob) != 235) {
    stop("There are not 235 ISO sector lists.")
  }

  if (
    unique(unlist(lapply(l_glob, function(x) length(x)))) != length(v_EMEP_sec)
  ) {
    stop("Some ISOs do not have the same sector length as nominated.")
  }

  # set time length based on choice of time dimension
  if (time_dim == "annual") {
    i_time <- 1
  } else {
    i_time <- 1:12
  }

  # open netcdf
  nc <- nc_open(fname_ncdf, write = T)

  # create the vector of variables (needs to match ncdf created above).
  # pollutant_iso only, not the total summary variable.
  if (global_agg_schema == "oneGRID") {
    v_vars <- unlist(lapply(species, function(x) paste0(x, "_GLOBAL")))
  } else if (global_agg_schema == "allISO") {
    v_iso_emep <- sort(dt_iso[, ISO_char])
    v_vars <- unlist(lapply(species, function(x) paste0(x, "_", v_iso_emep)))
  }

  # array dimensions (identical to ncdf)
  n_lon <- nc$dim$lon$len
  n_lat <- nc$dim$lat$len
  n_sector <- nc$dim$sector$len
  n_time <- nc$dim$time$len

  if (length(i_time) != n_time) {
    stop("time_dim choice and time dim inside netCDF are not the same.")
  }

  l <- list() # for summary data

  for (v in v_vars) {
    #print(v)
    # set some variables for inputting.
    var_iso <- sub("^[^_]+_", "", v)

    if (global_agg_schema == "allISO") {
      var_code <- dt_iso[ISO_char == var_iso, ISO_num]
    }
    if (global_agg_schema == "oneGRID") {
      var_code <- 64
    }
    mol_weight <- get_mol_weight(species)

    # make a few extra attributes for the variables.
    ncatt_put(
      nc,
      varid = v,
      attname = "species",
      attval = species,
      prec = "char"
    )
    if (!is.na(mol_weight)) {
      ncatt_put(
        nc,
        varid = v,
        attname = "molecular_weight",
        attval = mol_weight,
        prec = "int"
      )
    }
    if (!is.na(mol_weight)) {
      ncatt_put(
        nc,
        varid = v,
        attname = "molecular_weight_units",
        attval = "g mole-1",
        prec = "char"
      )
    }
    ncatt_put(
      nc,
      varid = v,
      attname = "country_ISO",
      attval = var_iso,
      prec = "char"
    )
    ncatt_put(
      nc,
      varid = v,
      attname = "countrycode",
      attval = var_code,
      prec = "int"
    )

    # collect the data for inserting - list of sector stacks.
    # nlyr = v_time (from time_dim)
    # also, flip the data at this stage:
    # netCDF and R terra have different start points for data.
    l_sec_v <- l_glob[[var_iso]]
    l_sec_v <- lapply(l_sec_v, function(x) flip(x, direction = "vertical"))

    # convert to array - use lapply over the sectors.
    # if it's annual, you could stack the sector list and just make the array
    # a <- array(rast(l_sec_v), dim = c(n_lon, n_lat, n_sector, n_time))
    # but we want flexibility for month layers.
    # Making a huge sector-month stack wont work, the array will 'fill-up'
    # the sector dim with the time layers from sector 1 and so on.
    l_a <- lapply(l_sec_v, function(x) {
      array(x, dim = c(n_lon, n_lat, 1, n_time))
    })
    a <- abind(l_a, along = 3) # combine on the sector dimension

    # last check for NA/NaN/Inf and set to 0
    a[is.na(a)] <- 0
    a[is.nan(a)] <- 0
    a[is.infinite(a)] <- 0

    # insert data
    ncvar_put(nc, v, a)

    ## summary of data going into netcdf ##
    # basic table
    dt <- data.table(
      iso_char = var_iso,
      iso_code = var_code,
      Pollutant = species,
      Data_source = "NetCDF_input",
      emis_y = y,
      inv_y = glob_inv,
      agg = global_agg_schema,
      time_res = time_dim,
      sec_num = names(l_sec_v)
    )

    dt[, sec_name := dt_sec$GNFRlong[match(names(l_sec_v), dt_sec[, sec])]]

    # add some summarised data from netCDF surface
    time_cols <- paste0("t", i_time)

    # add in annual totals
    # - emissions coming in, array and the newly input ncdf data
    dt[,
      emis_t_tot_ncinput := unlist(lapply(l_sec_v, function(x) {
        sum(global(x, sum, na.rm = T)[, 1])
      }))
    ]
    dt[,
      emis_t_tot_array := unlist(lapply(1:n_sector, function(x) sum(a[,, x, ])))
    ]

    # summarise the monthly emissions totals put into ncdf
    dt[,
      (time_cols) := unlist(lapply(1:n_sector, function(x) {
        global(l_sec_v[[x]], sum, na.rm = T)[, 1]
      }))
    ]

    # summarise the monthly emissions totals
    dt[, tsum := rowSums(.SD, na.rm = T), .SDcols = time_cols]
    dt[, tot_tres_ratio := emis_t_tot_ncinput / tsum]

    # list
    l[[v]] <- dt

    # tidy
    remove(l_sec_v)
    remove(a)
    gc()
  }

  ###############
  ## NO - see Janice Scheffler email 28/01/25

  # For the species, there needs to be one sum surface, all ISO all sector.
  # set some variables for inputting.
  # mol_weight <- get_mol_weight(species)

  # make a few extra attributes for the variables.
  # v <- species
  # ncatt_put(nc, varid = v, attname = "species", attval = species, prec = "char")
  # if(!is.na(mol_weight)) ncatt_put(nc, varid = v, attname = "molecular_weight"  , attval = mol_weight, prec = "int")
  # if(!is.na(mol_weight)) ncatt_put(nc, varid = v, attname = "molecular_weight_units"  , attval = "g mole-1", prec = "char")

  # collect the data for inserting - sum all ISO-sectors, over time dim
  # in other data, this will be 13 x i_time layers, per ISO
  # THIS WILL NEED A FUTURE CHECK THAT SUM OVER MONTH IS CORRECT
  # l_iso <- lapply(l_glob, function(x) rast(x))
  # l_iso <- lapply(l_iso, function(x) suppressWarnings(tapp(x, i_time, sum, na.rm=T)) )
  # s_iso <- rast(l_iso)
  # s_all_time <- suppressWarnings(tapp(s_iso, i_time, sum, na.rm=T))
  # s_all_time <- lapply(s_all_time, function(x) flip(x, direction="vertical"))
  # s_all_time <- rast(s_all_time) # comes out as list after the flip.

  # convert to array - use lapply over the sectors.
  # as it's summary, just make the array
  # a <- array(s_all_time, dim = c(n_lon, n_lat, n_time))

  # last check for NA/NaN/Inf and set to 0
  # a[is.na(a)] <- 0 ; a[is.nan(a)] <- 0 ; a[is.infinite(a)] <- 0

  # insert data
  # ncvar_put(nc, v, a)

  ## summary of data going into netcdf ##
  # basic table
  # dt <- data.table(iso_char = "SUM_ALL",
  #  			   iso_code = 0,
  # 				   Pollutant = species,
  # 				   Data_source = "NetCDF_input",
  # 	               emis_y = y,
  # 				   inv_y = glob_inv,
  # 				   agg = global_agg_schema,
  # 				   time_res = time_dim,
  # 				   sec_num = "TOTAL")

  # dt[, sec_name := "TOTAL" ]

  # add some summarised data from netCDF surface
  # time_cols <- paste0("t",i_time)

  ## THE FOLLOWING WILL NEED A REVIEW WHEN RUNNING MONTHLY

  # add in annual totals - emissions coming in, array and the newly input ncdf data
  # dt[, emis_t_tot_ncinput := sum(global(s_all_time, sum, na.rm=T)[,1]) ]
  # dt[, emis_t_tot_array := sum(a) ]

  # summarise the emissions totals put into ncdf
  # dt[, (time_cols) := unlist(global(s_all_time, sum, na.rm=T)[,1]) ]

  # summarise the monthly emissions totals
  # dt[, tsum := rowSums(.SD, na.rm=T), .SDcols = time_cols]
  # dt[, tot_tres_ratio := emis_t_tot_ncinput / tsum]

  # list
  # l[[paste0(v,"_sum")]] <- dt
  ###############

  # close connection
  nc_close(nc)

  # combine summaries and write
  dt_ncdf_summary <- rbindlist(l, use.names = T)

  return(dt_ncdf_summary)
} # end of function

######################################################################################################
#### function to read and summarise nc file fresh. Post writing.
summarise_nc_file_global <- function(
  fname_ncdf,
  y,
  species,
  glob_inv,
  time_dim,
  v_EMEP_sec,
  dt_iso,
  global_agg_schema
) {
  # time dims
  if (time_dim == "annual") {
    i_time <- 1
  } else if (time_dim == "month") {
    i_time <- 1:12
  } else {
    i_time <- 1:365
  }

  # gather sector info direct from input file. Yet another safety check.
  nc <- nc_open(fname_ncdf)
  v_var <- names(nc$var)
  # as all variables exist in .nc, restrict to species.
  v_var <- v_var[grep(paste0("^", species), v_var)]
  nc_close(nc)

  # list for summary data
  l <- list()

  for (v in v_var) {
    ## if it's the full sum layer, change the v_EMEP_sec to 1.
    if (v == species) {
      v_EMEP_sec_now <- 1
    }
    if (v != species) {
      v_EMEP_sec_now <- v_EMEP_sec
    }

    ## extract and make stack
    # simply using rast() will read in all layers across sector & time dims,
    # into 1 big stack (can't handle 4D).
    # We can certainly work with this, but it might need to go via
    # an array etc in the future.
    s_nc <- suppressWarnings(terra::rast(fname_ncdf, subds = v))

    # this ensures the sectors are split out into their own stacks
    # (important for non-annual)
    if (v == species) {
      l_out <- list(s_nc)
    } else {
      l_out <- lapply(v_EMEP_sec_now, function(x) {
        s_nc[[grep(paste0("sector=", x, "$"), names(s_nc))]]
      })
    }

    # some variables for table.
    var_spec <- strsplit(v, "_")[[1]][1]
    if (v != species) {
      var_iso <- sub("^[^_]+_", "", v)
    }
    if (v == species) {
      var_iso <- "SUM_ALL"
    }

    if (global_agg_schema == "allISO") {
      var_code <- dt_iso[ISO_char == var_iso, ISO_num]
    }
    if (v == species) {
      var_code <- 0
    }
    if (global_agg_schema == "oneGRID") {
      var_code <- 64
    }

    ## unsure how this will behave if the data is monthly - presume too long
    v_secs <- as.numeric(unlist(lapply(l_out, function(x) {
      strsplit(names(x), "=")[[1]][2]
    })))
    v_secs <- paste0(
      "sec",
      str_pad(v_secs, width = 2, side = "left", pad = "0")
    )

    # summarise all
    dt <- data.table(
      iso_char = var_iso,
      iso_code = var_code,
      Pollutant = species,
      Data_source = "NetCDF_output",
      emis_y = y,
      inv_y = glob_inv,
      agg = global_agg_schema,
      time_res = time_dim,
      sec_num = v_secs
    )

    dt[, sec_name := dt_sec$GNFRlong[match(v_secs, dt_sec[, sec])]]
    if (v == species) {
      dt[, sec_name := "TOTAL"]
    }

    # add some summarised data from netCDF surface
    time_cols <- paste0("t", i_time)

    # summarise the emissions totals put into ncdf (totals, ignore time)
    # this formula sums stacks per sector, in case it's less than annual
    dt[,
      emis_t_tot_ncoutput := suppressWarnings(unlist(lapply(l_out, function(x) {
        sum(global(x, sum, na.rm = T)[, 1])
      })))
    ]

    # summarise nc emissions data in time splits
    ## AGAIN - THE FOLOWING WILL NOT WORK FOR MONTHLY DATA, WRONG STRUCTURE
    # SUSPENDED until monthly is made and we can test this.
    #dt[,
    #  (time_cols) := suppressWarnings(unlist(lapply(
    #    v_EMEP_sec_now,
    #    function(x) global(l_out[[x]], sum, na.rm = T)[, 1]
    #  )))
    #]
    dt[, (time_cols) := emis_t_tot_ncoutput]

    # summarise the emissions totals
    dt[, tsum := rowSums(.SD, na.rm = T), .SDcols = time_cols]
    dt[, tot_tres_ratio := emis_t_tot_ncoutput / tsum]

    ## plus a totals table
    #
    #
    # HERE
    # dt <- rbindlist(list(dt_time, dt_tot), use.names = T)

    l[[v]] <- dt
  } # var name

  dt_ncfile_summary <- rbindlist(l, use.names = T)

  return(dt_ncfile_summary)
}

######################################################################################################
#### function to write out the summary tables into a new folder
write_summaries_global <- function(
  y,
  species,
  glob_inv,
  folname,
  dt_emep_emis,
  dt_proc_emis,
  dt_ncinp,
  dt_ncout
) {
  dir.create(
    file.path(folname, "tables", paste0("e", y)),
    showWarnings = FALSE,
    recursive = T
  )

  fname_route <- paste0(
    dt_poll[ceh_poll == species, emep_model],
    "_GLOBAL_",
    y,
    "emis"
  )

  fwrite(
    dt_emep_emis,
    paste0(folname, "/tables/e", y, "/", fname_route, "_INVENTORY.csv")
  )
  fwrite(
    dt_proc_emis,
    paste0(folname, "/tables/e", y, "/", fname_route, "_PROCESSED.csv")
  )
  fwrite(
    dt_ncinp,
    paste0(folname, "/tables/e", y, "/", fname_route, "_NETCDFINP.csv")
  )
  fwrite(
    dt_ncout,
    paste0(folname, "/tables/e", y, "/", fname_route, "_NETCDFOUT.csv")
  )
}

###############################################################################
#### function to
