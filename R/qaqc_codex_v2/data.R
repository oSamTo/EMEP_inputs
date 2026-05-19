## Build the expected table paths for each QAQC processing stage.
qaqc_v2_summary_paths <- function(
  domain,
  y,
  species,
  folname,
  inv,
  map_yr_uk = NA,
  data_source = NA
) {
  domain <- qaqc_v2_normalise_domain(domain)
  poll <- qaqc_v2_poll_model(species)

  ## UKEIRE filenames include both emissions year and mapping year.
  if (domain == "UKEIRE") {
    roots <- paste0(poll, "_UKEIRE_", y, "emis_", map_yr_uk, "map_", inv, "inv")
    suffixes <- c(
      inventory = "INVENTORY",
      masked = "MASKED",
      processed = "PROCESSED",
      ncinput = "NETCDFINP",
      ncoutput = "NETCDFOUT"
    )
  } else if (domain == "EU") {
    ## EU can have multiple naming conventions; all variants are checked.
    roots <- c(
      paste0(poll, "_EU_", y, "emis_", inv, "inv"),
      paste0(poll, "_EU_", y, "emis_", y, "map_", inv, "inv"),
      paste0(poll, "_EU_", y, "emis")
    )
    suffixes <- c(
      inventory = "INVENTORY",
      masked = "MASKED",
      processed = "PROCESSED",
      ncinput = "NETCDFINP",
      ncoutput = "NETCDFOUT"
    )
  } else if (domain == "GLOBAL") {
    ## GLOBAL domain supports multiple data sources (EDGAR, HTAP) with different naming patterns.
    ## Data source can be inferred or explicitly passed; check both with and without source prefix.
    source_prefix <- if (!is.na(data_source) && data_source != "") {
      paste0(tolower(data_source), "_")
    } else {
      ""
    }
    roots <- c(
      paste0(poll, "_GLOBAL_", y, "emis_", inv, "inv"),
      paste0(poll, "_GLOBAL_", y, "emis_", source_prefix, inv, "inv"),
      paste0(poll, "_GLOBAL_", y, "emis"),
      paste0(poll, "_GLOBAL_", y, "emis_", source_prefix, "emis")
    )
    suffixes <- c(
      inventory = "INVENTORY",
      processed = "PROCESSED",
      ncinput = "NETCDFINP",
      ncoutput = "NETCDFOUT"
    )
  } else {
    stop("Unknown domain: ", domain)
  }

  ## For each stage, prefer an existing file but return the first expected path if none exists.
  out <- lapply(suffixes, function(suffix) {
    candidates <- file.path(
      folname,
      "tables",
      paste0("e", y),
      paste0(roots, "_", suffix, ".csv")
    )
    existing <- candidates[file.exists(candidates)][1]
    ifelse(is.na(existing), candidates[1], existing)
  })
  names(out) <- names(suffixes)
  out
}

## Locate the total and per-sector QAQC raster outputs for one pollutant/year.
qaqc_v2_raster_paths <- function(y, species, folname) {
  rast_dir <- file.path(folname, "rast", paste0("e", y))
  sector <- list.files(
    rast_dir,
    pattern = paste0("^", species, "_sector[0-9]+_emis_qaqc[.]tif$"),
    full.names = TRUE
  )
  sector <- sector[order(as.integer(sub(
    paste0("^.*", species, "_sector([0-9]+)_emis_qaqc[.]tif$"),
    "\\1",
    sector
  )))]
  list(
    total = file.path(rast_dir, paste0(species, "_total_emis_qaqc.tif")),
    sector = sector
  )
}

## Read all available stage summary tables and put their totals into one common format.
qaqc_v2_stage_totals <- function(paths) {
  l_dt <- list()

  ## Inventory tables can use several emissions column names depending on domain/source.
  dt <- qaqc_v2_read_csv(paths$inventory)
  if (nrow(dt) > 0) {
    value_col <- intersect(
      c("emis_t", "emis_t_spatial_scaled", "emis_t_scalar"),
      names(dt)
    )[1]
    by_cols <- intersect(c("Area", "ISO", "iso_char", "Pollutant"), names(dt))
    if (!is.na(value_col)) {
      l_dt$inventory <- dt[,
        .(emis_t = sum(get(value_col), na.rm = TRUE)),
        by = by_cols
      ][,
        stage := "inventory"
      ]
    }
  }

  ## Masked tables are only present for some domains, so missing input is allowed.
  dt <- qaqc_v2_read_csv(paths$masked)
  if (nrow(dt) > 0) {
    value_col <- intersect(c("tsum", "emis_t_tot_masked"), names(dt))[1]
    by_cols <- intersect(c("Area", "ISO", "iso_char", "Pollutant"), names(dt))
    if (!is.na(value_col)) {
      l_dt$masked <- dt[,
        .(emis_t = sum(get(value_col), na.rm = TRUE)),
        by = by_cols
      ][,
        stage := "masked"
      ]
    }
  }

  ## Processed GLOBAL totals may be stored in kilotonnes and are converted back to tonnes.
  dt <- qaqc_v2_read_csv(paths$processed)
  if (nrow(dt) > 0) {
    value_col <- intersect(c("emis_t_tot_grouped", "ann_emis_kt"), names(dt))[1]
    by_cols <- intersect(c("Area", "ISO", "iso_char", "Pollutant"), names(dt))
    if (!is.na(value_col)) {
      mult <- if (value_col == "ann_emis_kt") 1000 else 1
      l_dt$processed <- dt[,
        .(emis_t = sum(get(value_col) * mult, na.rm = TRUE)),
        by = by_cols
      ][,
        stage := "processed"
      ]
    }
  }

  ## NetCDF input totals record the values just before the file is written.
  dt <- qaqc_v2_read_csv(paths$ncinput)
  if (nrow(dt) > 0) {
    value_col <- intersect(
      c("emis_t_tot_ncinput", "emis_t_tot_array", "tsum"),
      names(dt)
    )[1]
    by_cols <- intersect(c("Area", "ISO", "iso_char", "Pollutant"), names(dt))
    if (!is.na(value_col)) {
      l_dt$ncinput <- dt[,
        .(emis_t = sum(get(value_col), na.rm = TRUE)),
        by = by_cols
      ][,
        stage := "netcdf_input"
      ]
    }
  }

  ## NetCDF output totals record what was read back from the generated file.
  dt <- qaqc_v2_read_csv(paths$ncoutput)
  if (nrow(dt) > 0 && "emis_t_tot_ncoutput" %in% names(dt)) {
    by_cols <- intersect(c("Area", "ISO", "iso_char", "Pollutant"), names(dt))
    l_dt$ncoutput <- dt[,
      .(emis_t = sum(emis_t_tot_ncoutput, na.rm = TRUE)),
      by = by_cols
    ][,
      stage := "netcdf_output"
    ]
  }

  if (length(l_dt) == 0) {
    return(data.table::data.table())
  }
  out <- data.table::rbindlist(l_dt, fill = TRUE)
  ## Different tables call the area field different things; collapse them to one Area column.
  area_cols <- intersect(c("Area", "ISO", "iso_char"), names(out))
  if (length(area_cols) == 0) {
    out[, Area := "domain"]
  } else {
    out[, Area_qaqc := NA_character_]
    for (area_col in area_cols) {
      out[
        is.na(Area_qaqc) | Area_qaqc == "",
        Area_qaqc := as.character(get(area_col))
      ]
    }
    out[, (setdiff(area_cols, "Area_qaqc")) := NULL]
    data.table::setnames(out, "Area_qaqc", "Area")
  }
  out
}

## Attribute inventory-to-processed losses to GNFR sectors.
qaqc_v2_inventory_processed_sector_loss <- function(paths, dt_sec = NULL) {
  normalise_sector <- function(dt) {
    sector_col <- intersect(
      c("sec_GNFR", "GNFR", "sec_name", "Sector", "sec_long", "long_name"),
      names(dt)
    )[1]
    if (is.na(sector_col)) {
      return(NULL)
    }

    dt[, Sector := as.character(get(sector_col))]
    if (!is.null(dt_sec) && nrow(dt_sec) > 0 && "GNFRlong" %in% names(dt_sec)) {
      lu <- data.table::as.data.table(dt_sec)
      if ("sec" %in% names(lu)) {
        dt[Sector %in% lu$sec, Sector := lu$GNFRlong[match(Sector, lu$sec)]]
      }
      if ("name" %in% names(lu)) {
        dt[Sector %in% lu$name, Sector := lu$GNFRlong[match(Sector, lu$name)]]
      }
      if ("GNFR" %in% names(lu)) {
        dt[Sector %in% lu$GNFR, Sector := lu$GNFRlong[match(Sector, lu$GNFR)]]
      }
      if ("sec_long" %in% names(dt) && "name" %in% names(lu)) {
        dt[
          is.na(Sector) | Sector == "" | grepl("^sec[0-9]+$", Sector),
          Sector := lu$GNFRlong[match(as.character(sec_long), lu$name)]
        ]
      }
      if ("long_name" %in% names(dt) && "name" %in% names(lu)) {
        dt[
          is.na(Sector) | Sector == "" | grepl("^sec[0-9]+$", Sector),
          Sector := lu$GNFRlong[match(as.character(long_name), lu$name)]
        ]
      }
    }
    dt[is.na(Sector) | Sector == "", Sector := "Unknown sector"]
    dt
  }

  summarise_stage <- function(path, value_cols) {
    dt <- qaqc_v2_read_csv(path)
    if (nrow(dt) == 0) {
      return(data.table::data.table())
    }
    dt <- normalise_sector(dt)
    if (is.null(dt)) {
      return(data.table::data.table())
    }
    value_col <- intersect(value_cols, names(dt))[1]
    if (is.na(value_col)) {
      return(data.table::data.table())
    }
    mult <- if (value_col == "ann_emis_kt") 1000 else 1
    dt[,
      .(emis_t = sum(get(value_col) * mult, na.rm = TRUE)),
      by = Sector
    ]
  }

  dt_inventory <- summarise_stage(
    paths$inventory,
    c("emis_t", "emis_t_spatial_scaled", "emis_t_scalar")
  )
  dt_processed <- summarise_stage(
    paths$processed,
    c("emis_t_tot_grouped", "ann_emis_kt")
  )
  if (nrow(dt_inventory) == 0 || nrow(dt_processed) == 0) {
    return(data.table::data.table())
  }

  dt <- merge(
    dt_inventory[, .(Sector, inventory_t = emis_t)],
    dt_processed[, .(Sector, processed_t = emis_t)],
    by = "Sector",
    all = TRUE
  )
  dt[is.na(inventory_t), inventory_t := 0]
  dt[is.na(processed_t), processed_t := 0]
  dt[, missing_t := inventory_t - processed_t]
  dt <- dt[missing_t > 1e-6]
  if (nrow(dt) == 0) {
    return(data.table::data.table())
  }

  total_missing_t <- sum(dt$missing_t, na.rm = TRUE)
  dt <- dt[order(-missing_t)]
  dt[, `:=`(
    Inventory_kt = round(inventory_t / 1000, 3),
    Processed_kt = round(processed_t / 1000, 3),
    Missing_kt = round(missing_t / 1000, 3),
    Missing_pct = round(100 * missing_t / total_missing_t, 1)
  )]
  dt[, .(Sector, Inventory_kt, Processed_kt, Missing_kt, Missing_pct)]
}

## Summarise total and sector rasters with simple totals and cell statistics.
qaqc_v2_raster_summary <- function(raster_paths) {
  total_path <- raster_paths$total
  sector_paths <- raster_paths$sector
  sector_paths <- sector_paths[!is.na(sector_paths) & file.exists(sector_paths)]
  total_path <- total_path[!is.na(total_path) & file.exists(total_path)][1]
  if (length(total_path) == 0 && length(sector_paths) == 0) {
    return(data.table::data.table())
  }

  ## Small inner helper because total and sector rasters need the same statistics.
  summarise_one <- function(path, group) {
    r <- terra::rast(path)
    data.table::data.table(
      Group = group,
      Raster = basename(path),
      Total_kt = round(
        as.numeric(terra::global(r, "sum", na.rm = TRUE)[1, 1]) / 1000,
        3
      ),
      Mean_t = round(
        as.numeric(terra::global(r, "mean", na.rm = TRUE)[1, 1]),
        5
      ),
      Max_t = round(as.numeric(terra::global(r, "max", na.rm = TRUE)[1, 1]), 5)
    )
  }

  l_out <- list()
  if (length(total_path) == 1) {
    l_out$total <- summarise_one(total_path, "Total raster")
  }
  if (length(sector_paths) > 0) {
    dt_sector <- data.table::rbindlist(lapply(
      sector_paths,
      summarise_one,
      group = "Sector raster"
    ))
    ## Add an aggregate row so sector rasters can be compared with the total raster.
    dt_sector_total <- data.table::data.table(
      Group = "Sector raster sum",
      Raster = "sum of sector rasters",
      Total_kt = round(sum(dt_sector$Total_kt, na.rm = TRUE), 3),
      Mean_t = NA_real_,
      Max_t = NA_real_
    )
    blank <- data.table::data.table(
      Group = "",
      Raster = "",
      Total_kt = NA_real_,
      Mean_t = NA_real_,
      Max_t = NA_real_
    )
    l_out$blank <- blank
    l_out$sector <- dt_sector
    l_out$sector_total <- dt_sector_total
  }
  data.table::rbindlist(l_out, use.names = TRUE)
}

## Create a wide sector-by-ISO table for the highest emitting areas.
qaqc_v2_top_iso_sector_table <- function(paths, top_n = 10) {
  dt <- qaqc_v2_read_csv(paths$ncoutput)
  if (nrow(dt) == 0 || !"emis_t_tot_ncoutput" %in% names(dt)) {
    return(data.table::data.table())
  }

  area_col <- intersect(c("Area", "ISO", "iso_char"), names(dt))[1]
  sec_col <- intersect(c("sec_name", "sec_GNFR", "GNFR"), names(dt))[1]
  if (is.na(area_col) || is.na(sec_col)) {
    return(data.table::data.table())
  }
  data.table::setnames(dt, c(area_col, sec_col), c("Area", "Sector"))
  ## SUM_ALL is a domain total and would double-count if kept with individual areas.
  dt <- dt[Area != "SUM_ALL"]

  top_iso <- dt[,
    .(Total_t = sum(emis_t_tot_ncoutput, na.rm = TRUE)),
    by = Area
  ][
    order(-Total_t)
  ][seq_len(min(.N, top_n)), Area]

  dt_long <- dt[
    Area %in% top_iso,
    .(
      emis_kt = round(sum(emis_t_tot_ncoutput, na.rm = TRUE) / 1000, 3)
    ),
    by = .(Sector, Area)
  ]
  dt_wide <- data.table::dcast(
    dt_long,
    Sector ~ Area,
    value.var = "emis_kt",
    fill = 0
  )
  setcolorder(dt_wide, c("Sector", top_iso))
  total_row <- dt_wide[, lapply(.SD, sum, na.rm = TRUE), .SDcols = top_iso]
  total_row[, Sector := "Total"]
  total_row <- total_row[, names(dt_wide), with = FALSE]
  dt_wide <- data.table::rbindlist(list(dt_wide, total_row), use.names = TRUE)
  dt_wide
}

## Read historical GLOBAL inventory totals for trend plots.
## Supports multiple data sources (EDGAR, HTAP) with flexible naming patterns.
qaqc_v2_global_timeseries <- function(
  data_source,
  inv,
  species,
  y,
  years_back = 25,
  root_ip = "/gws/ssde/j25b/ceh_generic/inventory_processor/data"
) {
  if (is.na(data_source) || data_source == "") {
    return(list(
      data = data.table::data.table(),
      sector = data.table::data.table(),
      file = NA_character_
    ))
  }

  ## Try multiple path patterns to find the source directory.
  possible_dirs <- c(
    file.path(root_ip, data_source, inv, "totals"),
    file.path(root_ip, tolower(data_source), inv, "totals"),
    file.path(root_ip, toupper(data_source), inv, "totals")
  )

  source_dir <- NULL
  for (dir in possible_dirs) {
    if (dir.exists(dir)) {
      source_dir <- dir
      break
    }
  }

  if (is.null(source_dir)) {
    return(list(
      data = data.table::data.table(),
      sector = data.table::data.table(),
      file = NA_character_
    ))
  }

  ## Search for totals files with flexible naming patterns.
  candidates <- list.files(
    source_dir,
    pattern = paste0(".*AllPoll_TOTALS.*", inv, ".*GNFR.*[.]csv$"),
    full.names = TRUE,
    ignore.case = TRUE
  )

  if (length(candidates) == 0) {
    return(list(
      data = data.table::data.table(),
      sector = data.table::data.table(),
      file = NA_character_
    ))
  }

  fname <- candidates[1]
  tryCatch(
    {
      dt <- data.table::fread(fname)

      ## Handle different column name variations for pollutant and sector
      poll_col <- intersect(c("Pollutant", "pollutant", "species"), names(dt))[
        1
      ]
      sector_col <- intersect(c("GNFR", "gnfr", "Sector"), names(dt))[1]
      value_col <- intersect(c("emis_t", "emis_kt", "emissions"), names(dt))[1]
      year_col <- intersect(c("Year", "year"), names(dt))[1]

      if (is.na(poll_col) || is.na(value_col) || is.na(year_col)) {
        return(list(
          data = data.table::data.table(),
          sector = data.table::data.table(),
          file = fname
        ))
      }

      ## Filter to the requested species
      if (!is.na(poll_col)) {
        dt <- dt[get(poll_col) == species]
      }

      if (nrow(dt) == 0) {
        return(list(
          data = data.table::data.table(),
          sector = data.table::data.table(),
          file = fname
        ))
      }

      ## Keep the most recent window so the report stays readable.
      min_y <- max(min(dt[[year_col]], na.rm = TRUE), y - years_back + 1)
      dt <- dt[get(year_col) >= min_y & get(year_col) <= y]

      ## Convert emissions to kilotonnes if needed
      mult <- if (grepl("kt", tolower(value_col))) 1 else 0.001

      dt_total <- dt[,
        .(emis_kt = sum(get(value_col), na.rm = TRUE) * mult),
        by = year_col
      ][order(get(year_col))]
      data.table::setnames(dt_total, year_col, "Year")

      dt_sector <- NULL
      if (!is.na(sector_col)) {
        dt_sector <- dt[,
          .(emis_kt = sum(get(value_col), na.rm = TRUE) * mult),
          by = c(year_col, sector_col)
        ][order(get(year_col))]
        data.table::setnames(
          dt_sector,
          c(year_col, sector_col),
          c("Year", "GNFR")
        )
      }

      list(data = dt_total, sector = dt_sector, file = fname)
    },
    error = function(e) {
      list(
        data = data.table::data.table(),
        sector = data.table::data.table(),
        file = fname
      )
    }
  )
}
