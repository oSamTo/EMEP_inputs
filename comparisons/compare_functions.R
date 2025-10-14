###############################################################################
packs <- c(
  "sf",
  "terra",
  "stringr",
  "dplyr",
  "ggplot2",
  "data.table",
  "purrr",
  "cowplot",
  "abind",
  "stats",
  "readxl",
  "ncdf4",
  "lubridate",
  "patchwork",
  "tidyterra",
  "knitr",
  "kableExtra",
  "janitor"
)

lapply(packs, require, character.only = TRUE)

###############################################################################
options(datatable.showProgress = FALSE)

# shape for plotting.
# disable some spherical geometry in sf() that causes plot issues
suppressWarnings(sf::sf_use_s2(FALSE))

# new extended domain to include both UK & Eire
r_dom_1km <<- rast(
  xmin = -230000,
  xmax = 750000,
  ymin = -50000,
  ymax = 1300000,
  res = 1000,
  crs = "epsg:27700",
  vals = NA
)

# This is the lat long equivalent raster of the UK domain at 1km in BNG
r_dom_UKIE <<- rast(
  xmin = -13.8,
  xmax = 4.6,
  ymin = 49,
  ymax = 61.5,
  res = 0.01,
  crs = "epsg:4326",
  vals = NA
)

# plot domain for UK, slightly larger.
r_dom_ukplot <<- rast(
  xmin = -13.8,
  xmax = 5.4,
  ymin = 48.1,
  ymax = 62,
  res = 0.01,
  crs = "epsg:4326",
  vals = NA
)

# EU domain
r_dom_EU <<- rast(
  xmin = -30,
  xmax = 90,
  ymin = 30,
  ymax = 82,
  res = 0.1,
  crs = "epsg:4326",
  vals = NA
)

sf_world <<- st_read("data/spatial/world/TM_WORLD_BORDERS-0.3.shp")
st_crs(sf_world) <- "epsg:4326"
sf_world <- st_make_valid(sf_world)
sf_uk <<- st_crop(sf_world, ext(r_dom_ukplot))
sf_eu <<- st_crop(sf_world, ext(r_dom_EU))

# reinstate spherical geometry in sf()
suppressWarnings(sf::sf_use_s2(TRUE))

# the emissions need to be masked to terrestrial cells (plus some coastal cells)
# Massimo wants EMEP emissions data on the sea
# the mask is in 0.1 degree, disaggregate to 0.01 so masking can be done
# UK data does not have IOM, but EMEP only has shipping - use the mask with no IOM.
r_dom_terr_10km <<- crop(
  extend(
    disagg(rast("data/spatial/Emissions_mask_10km_noIOM.tif"), fact = 10),
    r_dom_UKIE
  ),
  r_dom_UKIE
)
r_dom_terr <<- rast("data/spatial/terrestrial_mask.tif")

# lookup file for sector mapping
dt_sec <<- fread("data/lookups/EMEP_sectors.csv")[!is.na(sec)]

# lookup file for pollutant names
dt_poll <<- fread("data/lookups/pollutant_names.csv")

# lookup file for EMEP country names - taken from EMEPv5.0 file
dt_iso <<- readRDS("data/lookups/dt_iso.rds")
dt_iso <<- dt_iso[!is.na(ISO_char)]

# EMEP input missing value
EMEP_fillval <<- 9.96920996838687e+36

# EMEP sector numbers
v_EMEP_sec <<- 1:13

# EMEP yday numbers, to represent month central days, as taken from an EMEP input file - fixed (?)
v_mday <<- c(14, 45, 73, 104, 134, 165, 195, 226, 257, 287, 318, 348)
v_yday <<- 1:365

###############################################################################
#### wrapper function to call other functions for document creation
output_comparison <- function(
  pollutant,
  folname_1,
  emis_yr_1,
  folname_2,
  emis_yr_2
) {
  print(paste0(
    format(Sys.time(), "%Y-%m-%d %X"),
    ": Comparing 2 sets of EMEP inputs:"
  ))
  # If the 2nd part of the folder path DOES NOT have an EMEP version, it is an
  # older file (pre-2025) and each pollutant has its own .nc file.
  # If the 2nd part of the folder path DOES have an EMEP version, it is the
  # 2025+ version and all pollutants are in 1 .nc file.

  print(paste0(
    format(Sys.time(), "%Y-%m-%d %X"),
    ":            collecting data..."
  ))
  # Get filename for pollutant, for folder name 1
  l_fname_1 <- construct_filepath(pollutant, fol = folname_1, y = emis_yr_1)

  # Get filename for pollutant, for folder name 2
  l_fname_2 <- construct_filepath(pollutant, fol = folname_2, y = emis_yr_2)

  # create some folders.
  plot_dir <- create_folders(
    pollutant,
    folname_1,
    emis_yr_1,
    folname_2,
    emis_yr_2,
    l_fname_1,
    l_fname_2
  )

  # Get the requisite data - in monthly format.
  # If the data are annual, get the EMEP version internal file and split it.
  l_m_1 <- worked_totals(
    pollutant,
    folname = folname_1,
    fname = l_fname_1[["filename"]],
    dt_metadata = l_fname_1[["metadata"]]
  )

  l_m_2 <- worked_totals(
    pollutant,
    folname = folname_2,
    fname = l_fname_2[["filename"]],
    dt_metadata = l_fname_2[["metadata"]]
  )

  print(paste0(
    format(Sys.time(), "%Y-%m-%d %X"),
    ":            making plots..."
  ))

  # Plot 1: plot differences in annual total emissions. (UK).
  gg_p1 <- comp_map_tot_ann(pollutant, l_m_1, l_m_2, plot_dir)

  # Plot 2: plot differences in annual sector total emissions. (UK).
  gg_p2 <- comp_map_sec_ann(pollutant, l_m_1, l_m_2, plot_dir)

  # Plot 3: plot differences in monthly total emissions. (UK).
  gg_p3 <- comp_map_tot_mon(pollutant, l_m_1, l_m_2, plot_dir)

  # Plot 4: monthly line graph; totals. (UK).
  gg_p4 <- comp_lin_tot_mon(pollutant, l_m_1, l_m_2, plot_dir)

  # Plot 5: monthly line graphs; sectors. (UK).
  gg_p5 <- comp_lin_sec_mon(pollutant, l_m_1, l_m_2, plot_dir)

  # Plot 6: annual bar charts; sectors. (UK).
  gg_p6 <- comp_bar_sec_ann(pollutant, l_m_1, l_m_2, plot_dir)

  # Plot 7: major SNAP contributor to change, both positive and negative. (UK).
  gg_p7 <- domSNAP_map_tot_ann(pollutant, l_m_1, l_m_2, plot_dir)

  # then we need to put together .Rmd parameters and call a 'comparison_pdf.Rmd' file

  # tables of totals, etc.

  # 4 panel map - 2 actual + 1 relative diff and 1 absolute diff.
  # Tables:
  #	- total & sectoral emissions, annual
  #	- total & sectoral emissions, time breakdown (probably month)
  # Sectoral maps??

  ############
  #### PDF ####
  #############

  print(paste0(
    format(Sys.time(), "%Y-%m-%d %X"),
    ":            rendering pdf..."
  ))

  l_pdf_params <- list(
    species = pollutant,
    plot_dir = plot_dir,
    emis_yr_1 = emis_yr_1,
    emis_yr_2 = emis_yr_2,
    folname_1 = folname_1,
    folname_2 = folname_2,
    l_fname_1 = l_fname_1,
    l_fname_2 = l_fname_2,
    l_m_1 = l_m_1,
    l_m_2 = l_m_2,
    uk_plot1 = gg_p1,
    uk_plot2 = gg_p2,
    uk_plot3 = gg_p3,
    uk_plot4 = gg_p4,
    uk_plot5 = gg_p5,
    uk_plot6 = gg_p6,
    uk_plot7 = gg_p7
  )

  # render the source of the document to the default output format:
  rmarkdown::render(
    input = "comparisons/compare_markdown.Rmd",
    output_file = paste0("input_comparison_", pollutant, ".pdf"),
    output_dir = paste0(plot_dir, "/.."),
    params = l_pdf_params
  )

  print(paste0(format(Sys.time(), "%Y-%m-%d %X"), ": DONE."))
}

###############################################################################
#### function to make the filepath based
construct_filepath <- function(pollutant, fol, y) {
  # return a filename based on the folder structure
  fol_projName <- strsplit(fol, "/")[[1]][2] # project name, e.g. NFC
  fol_basescen <- strsplit(fol, "/")[[1]][3] # BASE or SCENARIO name
  fol_EMEPver <- strsplit(fol, "/")[[1]][4] # EMEP4UKvX.XX
  fol_emisInv <- strsplit(fol, "/")[[1]][5] # invXXXX
  fol_invYear <- as.numeric(gsub("inv", "", fol_emisInv))
  fol_area <- strsplit(fol, "/")[[1]][6] # UKEIRE or EU

  #fol_part2 <- strsplit(fol, "/")[[1]][2]
  #fol_partInv <- strsplit(fol, "/")[[1]][3]
  #fol_partInv <- as.numeric(gsub("inv", "", fol_partInv))

  # set resolution part of filename to be in-line with UK or EU
  if (fol_area == "UKEIRE") {
    file_res <- "0.01"
  } else if (fol_area == "EU") {
    file_res <- "0.1"
  }

  # If the string above is just 'EMEP4UK', it is pre-2025 format. (v4.45)
  # This format is split into pre-made folders of emissions years and does not
  # require the y variable.
  if (grepl("archive/EMEP4UK/", fol)) {
    version_pre25 = TRUE
    EMEP_v <- "v4.45" # using this as default for pre25
    fname <- list.files(
      fol,
      pattern = paste0("^", pollutant, ".*", file_res, ".nc$")
    )
  }

  # If fol_part2 has a version number, extract some filename info and set
  # fname to the general all pollutant file.
  # These file types DO require a y object as many exist in one folder.
  if (grepl("EMEP4UKv", fol_EMEPver)) {
    version_pre25 = FALSE
    EMEP_v <- gsub("EMEP4UK", "", fol_EMEPver)
    EMEP_v <- gsub("_Jan25", "", EMEP_v)
    fname <- list.files(
      fol,
      pattern = paste0("^", fol_area, ".*", y, ".*", file_res, ".nc$")
    )
  }

  # extract n time steps from ncdf
  nc_file <- file.path(fol, fname)
  nc <- nc_open(nc_file)
  nc_time_len <- nc$dim$time$len
  nc_close(nc)

  # metadata
  dt <- data.table(
    projectName = fol_projName,
    emis_y = paste0("e", y),
    inv_y = paste0("i", fol_invYear),
    time = paste0("t", nc_time_len),
    area = fol_area,
    res = file_res,
    EMEP_version = EMEP_v,
    pre25 = version_pre25
  )

  return(list("filename" = fname, "metadata" = dt))
}

###############################################################################
#### function to create a folder structure for outputs, plus dir to write to
create_folders <- function(
  pollutant,
  folname_1,
  emis_yr_1,
  folname_2,
  emis_yr_2,
  l_fname_1,
  l_fname_2
) {
  # top level is UK or EU
  #  - next is emis yr vs emis yr (e.g. 2021 vs 2022)
  #    - next is invYears with EMEP version
  #      - put all pollutant docs in here. Plots/tables folder here.

  if (l_fname_1$metadata$area != l_fname_2$metadata$area) {
    stop("not comparing same area!")
  } # nolint

  # construct folders and create
  plot_dir <- paste0(
    "comparisons/",
    l_fname_1$metadata$area,
    "/",
    l_fname_1$metadata$emis_y,
    "_vs_",
    l_fname_2$metadata$emis_y,
    "/",
    l_fname_1$metadata$inv_y,
    "emep",
    l_fname_1$metadata$EMEP_version,
    "_vs_",
    l_fname_2$metadata$inv_y,
    "emep",
    l_fname_2$metadata$EMEP_version,
    "/plots"
  )

  table_dir <- paste0(
    "comparisons/",
    l_fname_1$metadata$area,
    "/",
    l_fname_1$metadata$emis_y,
    "_vs_",
    l_fname_2$metadata$emis_y,
    "/",
    l_fname_1$metadata$inv_y,
    "emep",
    l_fname_1$metadata$EMEP_version,
    "_vs_",
    l_fname_2$metadata$inv_y,
    "emep",
    l_fname_2$metadata$EMEP_version,
    "/tables"
  )

  dir.create(plot_dir, showWarnings = FALSE, recursive = TRUE)
  dir.create(table_dir, showWarnings = FALSE, recursive = TRUE)

  return(plot_dir)
}

###############################################################################
#### function to extract data and ensure it is monthly.
worked_totals <- function(pollutant, folname, fname, dt_metadata) {
  # set nc file name and get variable names
  nc_file <- file.path(folname, fname)
  nc <- nc_open(nc_file)
  v_var <- names(nc$var)
  nc_close(nc)

  ###################################################
  # get data if the file/folder is pre 2025 processing
  # we know this is monthly.
  if (dt_metadata[, pre25]) {
    # inputs are in UK_, IE_, SEA_ format, per sector.
    l_summary <- list()
    l_a_m <- list() # monthly total list, per ISO
    l_a_s <- list() # annual sector total list, per ISO

    for (a in c("UK", "IE", "SEA")) {
      l_v <- list()

      # subset variables to country
      v_a <- v_var[grep(a, v_var)]

      # cycle through and read
      for (v in v_a) {
        # month data
        s <- rast(nc_file, subds = v)
        l_v[[v]] <- s

        # annual sector summary
        r <- app(s, sum, na.rm = T)
        r_GNFR <- dt_sec[name == gsub(paste0("Emis_", a, "_"), "", v), GNFRlong]
        r_name <- paste0(a, "_", r_GNFR)
        names(r) <- r_name
        l_a_s[[r_GNFR]][[a]] <- r

        # summarise
        dt <- data.table(
          Area = a,
          pollutant,
          fname,
          emis_y = dt_metadata[, emis_y],
          inv_y = dt_metadata[, inv_y],
          emep_v = dt_metadata[, EMEP_version],
          sector_file = v,
          t = 1:12,
          emis_t = global(s, sum, na.rm = T)[, 1]
        )
        dt[, sector_name := tstrsplit(sector_file, "_", keep = 3)]
        dt[,
          GNFR := dt_sec[name == sector_name, GNFRlong],
          by = seq_len(nrow(dt))
        ]
        dt[, SNAP := dt_sec[name == sector_name, SNAP], by = seq_len(nrow(dt))]

        l_summary[[paste0(a, "_", v)]] <- dt
      }

      # extract each sector for each month and sum
      temp_summary <- list()
      l_m <- list()

      for (m in 1:12) {
        # extract
        l_v_m <- lapply(1:length(l_v), function(x) {
          l_v[[x]][[grep(paste0("_", m, "$"), names(l_v[[x]]))]]
        })
        s_m <- rast(l_v_m)
        r_m <- app(s_m, sum, na.rm = T)

        names(r_m) <- paste0(a, "_", m)

        # add to the area list
        l_m[[m]] <- r_m

        # summary for check
        dt <- data.table(t = m, emis_t = global(r_m, sum, na.rm = T)[, 1])
        temp_summary[[m]] <- dt
      }

      # quick tots check
      sumsec <- rbindlist(l_summary, use.names = T)[
        Area == a,
        sum(emis_t, na.rm = T)
      ]
      summon <- rbindlist(temp_summary, use.names = T)[, sum(emis_t, na.rm = T)]
      if (sumsec / summon < 0.995 | sumsec / summon > 1.005) {
        stop("pre-2025 emissions sums have gone wrong.")
      }

      # stack up the months and add to country list
      s <- rast(l_m)
      l_a_m[[a]] <- s
    } # ISO loop
  } # ifelse for pre 2025 data

  ###################################################
  # get data if the file/folder is 2025+ processing
  if (!(dt_metadata[, pre25])) {
    l_summary <- list()
    l_a_m <- list() # monthly total list, per ISO
    l_a_s <- list() # annual sector total list, per ISO (this is l_out, below)

    for (a in c("GB", "IE", "SEA")) {
      # 2025+ data has GB, not UK

      # set the required iso code (SEA is going to be same as GB)
      a_iso <- ifelse(a == "IE", 14, 27)
      a_adj <- ifelse(a == "GB", "UK", a)

      # set the variable name
      v <- paste0(pollutant, "_", a)

      # get the time_dim from the nc_file to determine whether to use profiles
      nc <- nc_open(nc_file)
      nc_time_len <- nc$dim$time$len
      nc_close(nc)

      # if the nc time length is just one, use profiles to split sectoral data
      if (nc_time_len == 1) {
        # read annual sectors
        s <- suppressWarnings(rast(nc_file, subds = v))

        l_out <- lapply(v_EMEP_sec, function(x) {
          s[[grep(paste0("sector=", x, "$"), names(s))]]
        })

        # get the max sector number from EMEP using names
        #l_out_names <- strsplit(unlist(lapply(l_out, function(x) names(x))), "=")
        #max_sec <- max(as.numeric(unlist(lapply(l_out_names, function(x) x[2]))))
        #max_sec <- paste0("sec",str_pad(max_sec, width = 2, side = "left", 0))

        # name the EMEP model timings file
        folname_prof <- paste0(
          "data/temporal/EMEP4UK",
          dt_metadata[, EMEP_version],
          "/MonthlyFacs.",
          pollutant
        )

        # read in the EMEP model timings and subset to iso
        dt_prof <- fread(folname_prof)
        names(dt_prof) <- c("iso_code", "snap", paste0("t", 1:12))

        dt_prof <- dt_prof[iso_code == a_iso]

        # go through nc sectors, split them out by EMEP_sec --> SNAP
        l_v <- list()

        for (i in 1:length(l_out)) {
          r <- l_out[[i]]
          model_sec <- as.numeric(strsplit(names(r), "=")[[1]][2])
          model_sec <- paste0(
            "sec",
            str_pad(model_sec, width = 2, side = "left", 0)
          )

          r_SNAP <- dt_sec[sec == model_sec, unique(SNAP)]
          r_GNFR <- dt_sec[sec == model_sec, unique(GNFRlong)]
          r_secname <- dt_sec[sec == model_sec, unique(name)]

          dt_prof_snap <- dt_prof[snap == r_SNAP]

          if (nrow(dt_prof_snap) != 1) {
            stop("n.row of profile table is not 1.")
          }

          # list of annual sector map
          r_name <- paste0(a_adj, "_", r_GNFR)
          names(r) <- r_name
          l_a_s[[r_GNFR]][[a_adj]] <- r

          # get vector of splits.
          v_fac <- unname(unlist(dt_prof_snap[, paste0("t", 1:12)]))

          # adjust slightly to get exact values
          v_fac <- (v_fac / mean(v_fac)) / 12

          # make a stack and re-list
          s_fac <- r * v_fac
          names(s_fac) <- paste0(a_adj, "_", model_sec, "_", 1:12)

          l_v[[i]] <- s_fac

          # summarise
          dt <- data.table(
            Area = a_adj,
            pollutant,
            fname,
            emis_y = dt_metadata[, emis_y],
            inv_y = dt_metadata[, inv_y],
            emep_v = dt_metadata[, EMEP_version],
            sector_file = names(r),
            t = 1:12,
            emis_t = global(s_fac, sum, na.rm = T)[, 1]
          )
          dt[, sector_name := r_secname]
          dt[, GNFR := r_GNFR]
          dt[, SNAP := r_SNAP]

          l_summary[[paste0(a_adj, "_", r_secname)]] <- dt
        }

        # extract each sector for each month and sum
        temp_summary <- list()
        l_m <- list()

        for (m in 1:12) {
          # extract
          l_v_m <- lapply(1:length(l_v), function(x) {
            l_v[[x]][[grep(paste0("_", m, "$"), names(l_v[[x]]))]]
          })
          s_m <- rast(l_v_m)
          r_m <- app(s_m, sum, na.rm = T)

          names(r_m) <- paste0(a_adj, "_", m)

          # add to the area list
          l_m[[m]] <- r_m

          # summary for check
          dt <- data.table(t = m, emis_t = global(r_m, sum, na.rm = T)[, 1])
          temp_summary[[m]] <- dt
        }

        # quick tots check
        sumsec <- rbindlist(l_summary, use.names = T)[
          Area == a_adj,
          sum(emis_t, na.rm = T)
        ]
        summon <- rbindlist(temp_summary, use.names = T)[, sum(
          emis_t,
          na.rm = T
        )]
        if (sumsec / summon < 0.995 | sumsec / summon > 1.005) {
          stop("post-2025 emissions sums have gone wrong.")
        }

        # stack up the months and add to country list
        s_m <- rast(l_m)
        l_a_m[[a_adj]] <- s_m

        if (
          sum(global(s, sum, na.rm = T)[, 1]) /
            sum(global(s_m, sum, na.rm = T)[, 1]) <
            0.995 |
            sum(global(s, sum, na.rm = T)[, 1]) /
              sum(global(s_m, sum, na.rm = T)[, 1]) >
              1.005
        ) {
          stop("splitting annual emissions has changed overall sum value")
        }
      } else {
        stop("need to write code for monthly input files")
      } # splitting annual input or using monthly input
    } # ISO loop
  } # ifelse for 2025+ data

  # return data
  dt <- rbindlist(l_summary, use.names = T)
  dt[Area == "GB", Area := "UK"]

  return(list("total_month" = l_a_m, "annual_sector" = l_a_s, "summary" = dt))
}

###############################################################################
#### function to compare annual totals of pollutants in run files.
comp_map_tot_ann <- function(pollutant, l_m_1, l_m_2, plot_dir) {
  # as this is an annual map, we need to some everything up.
  s1 <- rast(l_m_1[["total_month"]])
  r1 <- app(s1, sum, na.rm = T)

  s2 <- rast(l_m_2[["total_month"]])
  r2 <- app(s2, sum, na.rm = T)

  ### Plot national totals

  # new stack, with the rasters in order of the filenames provided
  s <- c(r1, r2)
  s[s == 0] <- NA

  ## reclassify and plot ##
  # exract all the values present, into a vector.
  v_v <- values(s)
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
      ceiling(max(global(s, max, na.rm = T)$max, na.rm = T)),
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
  s_rc <- terra::classify(s, m)
  s_rc <- as.factor(s_rc)

  # names
  if (nlyr(s_rc) != 2) {
    stop("There should be 2 layers in the comparison")
  }

  # rename to sectors
  names(s_rc) <- c(
    paste0(
      unique(l_m_1$summary$emis_y),
      "_",
      unique(l_m_1$summary$inv_y),
      "_",
      unique(l_m_1$summary$emep_v)
    ),
    paste0(
      unique(l_m_2$summary$emis_y),
      "_",
      unique(l_m_2$summary$inv_y),
      "_",
      unique(l_m_2$summary$emep_v)
    )
  )

  p_tots <- ggplot() +
    geom_spatraster(data = s_rc, na.rm = T, maxcell = 1e+06) +
    scale_fill_brewer(
      labels = v_labs,
      palette = "Spectral",
      breaks = 1:length(v_q),
      direction = brew_d
    ) +
    scale_y_continuous(expand = c(0, 0)) +
    scale_x_continuous(expand = c(0, 0)) +
    ggtitle(paste0(
      unique(l_m_1$summary$fname),
      " vs ",
      unique(l_m_2$summary$fname)
    )) +
    labs(fill = bquote(tonnes ~ a^-1), y = "", x = "") +
    # ggtitle(bquote("Emissions of"~.(p)~a^-1))+
    geom_sf(data = sf_uk, fill = NA, colour = "black") +
    facet_wrap(~lyr, nrow = 1) +
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

  #fname <- paste0(plot_dir,"/test1.png")
  #ggsave(fname, p_tots, width = 14, height = 11)

  ### Plot a relative change map - file 2 divided by file 1.
  r_a <- r2 / r1
  r_a[r_a == 0] <- NA
  r_a[is.infinite(r_a)] <- NA

  ## reclassify and plot ##
  # exract all the values present, into a vector.
  v_q <- c(0, 0.25, 0.5, 0.7, 0.8, 0.9, 0.98, 1.02, 1.1, 1.2, 1.3, 1.5, 2, 3)

  v_cols <- c(
    grDevices::colorRampPalette(colors = c("#259bdf", "#d8f1ff"))(6),
    "#ececec",
    grDevices::colorRampPalette(colors = c("#ffdad8", "#f8493f"))(7)
  )

  # create a reclassification matrix and labels for the plot.
  m <- matrix(
    c(
      v_q,
      v_q[2:length(v_q)],
      ceiling(max(global(r_a, max, na.rm = T)$max)),
      1:length(v_q)
    ),
    ncol = 3
  )
  if (m[nrow(m), 2] < m[nrow(m), 1]) {
    m[nrow(m), 2] <- m[nrow(m), 1] + 1
  }

  # create labels
  v_labs <- unlist(lapply(1:nrow(m), function(x) {
    paste0(round(m[x, 1], 2), "-", round(m[x, 2], 2))
  }))
  v_labs[length(v_labs)] <- paste0("> ", round(m[nrow(m), 1], 2))

  # reclassify and extract the levels
  r_a_rc <- terra::classify(r_a, m)
  r_a_rc <- as.factor(r_a_rc)

  names(r_a_rc) <- paste0("file_2 / file_1")

  dt_levs <- levels(r_a_rc)[[1]]

  # subset the colours and the labels based on factors present
  v_cols <- v_cols[unique(dt_levs$ID)]
  names(v_cols) <- unique(dt_levs$ID)
  v_labs <- v_labs[unique(dt_levs$ID)]
  names(v_labs) <- unique(dt_levs$ID)

  g_diff <- ggplot() +
    geom_spatraster(data = r_a_rc, na.rm = T, maxcell = 1e+06) +
    # scale_fill_gradient2(low = "#16abf5", mid = "white", high = "#ff514e", midpoint = 5, breaks = 1:length(v_labs), labels = v_labs)+
    #scale_fill_brewer(labels = v_labs, palette = "RdBu", direction = brew_d, na.value = "grey90")+
    scale_fill_manual(
      values = v_cols,
      labels = v_labs,
      na.value = "transparent",
      drop = FALSE
    ) +
    #scale_y_continuous(expand = c(0, 0)) +
    labs(fill = "rel") +
    ggtitle(names(r_a_rc)) +
    geom_sf(data = sf_uk, fill = NA, colour = "black") +
    facet_wrap(~lyr) +
    #scale_fill_hypso_d(labels = v_labs, na.value = "transparent", palette = "etopo1")+
    theme_bw() +
    theme(
      strip.text.x = element_blank(),
      plot.title = element_text(size = 18, hjust = 0.5),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 16),
      legend.position = "right",
      legend.box.spacing = unit(2, "mm"),
      legend.margin = margin(0, 0, 0, 0),
      legend.key.height = unit(0.9, "cm"),
      plot.margin = unit(c(1, 1, 1, 1), "mm")
    )

  #fname <- paste0(plot_dir,"/test2.png")
  #ggsave(fname, g_diff, width = 7, height = 5)

  ### Plot an absolute change map - file 2 minus file 1.
  r_a <- r2 - r1
  # r_a[r_a == 0] <- NA
  r_a[is.infinite(r_a)] <- NA

  ## reclassify and plot ##
  # exract all the values present, into a vector.
  v_q <- c(
    -1000,
    -10,
    -5,
    -1,
    -0.5,
    -0.1,
    -0.01,
    0.01,
    0.1,
    0.5,
    1,
    5,
    10,
    1000
  )

  if (global(r_a, min, na.rm = T) < -1000) {
    v_q[1] <- floor(global(r_a, min, na.rm = T)[, 1])
  }
  if (global(r_a, max, na.rm = T) > 1000) {
    v_q[14] <- ceiling(global(r_a, max, na.rm = T)[, 1])
  }

  v_cols <- c(
    grDevices::colorRampPalette(colors = c("#259bdf", "#d8f1ff"))(6),
    "#ececec",
    grDevices::colorRampPalette(colors = c("#ffdad8", "#f8493f"))(6)
  )

  # create a reclassification matrix and labels for the plot.
  m <- matrix(c(v_q[1:13], v_q[2:14], 1:13), ncol = 3)

  # create labels
  v_labs <- unlist(lapply(1:nrow(m), function(x) {
    paste0(round(m[x, 1], 2), "-", round(m[x, 2], 2))
  }))

  # reclassify and extract the levels
  r_a_rc <- terra::classify(r_a, m)
  r_a_rc <- as.factor(r_a_rc)

  names(r_a_rc) <- paste0("file_2 - file_1")

  dt_levs <- levels(r_a_rc)[[1]]

  # subset the colours and the labels based on factors present
  v_cols <- v_cols[unique(dt_levs$ID)]
  v_labs <- v_labs[unique(dt_levs$ID)]

  g_diffabs <- ggplot() +
    geom_spatraster(data = r_a_rc, na.rm = T, maxcell = 1e+06) +
    # scale_fill_gradient2(low = "#16abf5", mid = "white", high = "#ff514e", midpoint = 5, breaks = 1:length(v_labs), labels = v_labs)+
    #scale_fill_brewer(labels = v_labs, palette = "RdBu", direction = brew_d, na.value = "grey90")+
    scale_fill_manual(
      values = v_cols,
      labels = v_labs,
      na.value = "transparent"
    ) +
    #scale_y_continuous(expand = c(0, 0)) +
    labs(fill = "tonnes") +
    ggtitle(names(r_a_rc)) +
    geom_sf(data = sf_uk, fill = NA, colour = "black") +
    facet_wrap(~lyr) +
    #scale_fill_hypso_d(labels = v_labs, na.value = "transparent", palette = "etopo1")+
    theme_bw() +
    theme(
      strip.text.x = element_blank(),
      plot.title = element_text(size = 18, hjust = 0.5),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 16),
      legend.position = "right",
      legend.box.spacing = unit(2, "mm"),
      legend.margin = margin(0, 0, 0, 0),
      legend.key.height = unit(0.9, "cm"),
      plot.margin = unit(c(1, 1, 1, 1), "mm")
    )

  #fname <- paste0(plot_dir,"/test3.png")
  #ggsave(fname, g_diffabs, width = 7, height = 5)

  ## patchwork them
  p <- p_tots /
    (g_diff + g_diffabs) +
    plot_layout(nrow = 2, heights = c(1, 0.85))

  fname <- paste0(plot_dir, "/", pollutant, "_UKEIRE_1_UKTOTMAP.png")
  ggsave(fname, p, width = 15, height = 15)

  return(fname)
}

###############################################################################
#### function to compare annual totals of pollutant, per sector
comp_map_sec_ann <- function(pollutant, l_m_1, l_m_2, plot_dir) {
  # premise is to plot as per total emissions, then patchwork together all.
  # only do for the first 12 sectors. IGNORE M_Other.

  l <- list()

  # ignore 13, M_Other
  for (i in 1:13) {
    print(i)
    sec_GNFR <- dt_sec[
      sec == paste0("sec", str_pad(i, "width" = 2, side = "left", 0)),
      GNFRlong
    ]

    # as this is an annual map, we need to some everything up.
    s1 <- rast(l_m_1[["annual_sector"]][[sec_GNFR]])
    r1 <- app(s1, sum, na.rm = T)

    s2 <- rast(l_m_2[["annual_sector"]][[sec_GNFR]])
    r2 <- app(s2, sum, na.rm = T)

    ### Plot national totals

    # new stack, with the rasters in order of the filenames provided
    s <- c(r1, r2)

    if (sum(global(s, sum, na.rm = T)[, 1]) > 0) {
      s[s == 0] <- NA
      # s[is.na(s)] <- 0

      ## reclassify and plot ##
      # exract all the values present, into a vector.
      v_v <- values(s)
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
          ceiling(max(global(s, max, na.rm = T)$max, na.rm = T)),
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
      s_rc <- terra::classify(s, m)
      s_rc <- as.factor(s_rc)
    } else {
      s_rc <- copy(s)
    }

    # names
    if (nlyr(s_rc) != 2) {
      stop("There should be 2 layers in the comparison")
    }

    # rename to sectors
    names(s_rc) <- c(
      paste0(
        unique(l_m_1$summary$emis_y),
        "_",
        unique(l_m_1$summary$inv_y),
        "_",
        unique(l_m_1$summary$emep_v)
      ),
      paste0(
        unique(l_m_2$summary$emis_y),
        "_",
        unique(l_m_2$summary$inv_y),
        "_",
        unique(l_m_2$summary$emep_v)
      )
    )

    p_tots <- ggplot() +
      geom_spatraster(data = s_rc, na.rm = T, maxcell = 1e+06) +
      {
        if (sum(global(s, sum, na.rm = T)[, 1]) > 0) {
          scale_fill_brewer(
            labels = v_labs,
            palette = "Spectral",
            breaks = 1:length(v_q),
            direction = brew_d
          )
        }
      } +
      scale_y_continuous(expand = c(0, 0)) +
      scale_x_continuous(expand = c(0, 0)) +
      # ggtitle(paste0(unique(l_m_1$summary$fname)," vs ",unique(l_m_2$summary$fname)))+
      ggtitle(sec_GNFR) +
      labs(fill = bquote(tonnes ~ a^-1), y = "", x = "") +
      # ggtitle(bquote("Emissions of"~.(p)~a^-1))+
      geom_sf(data = sf_uk, fill = NA, colour = "black") +
      facet_wrap(~lyr, nrow = 1) +
      theme_bw() +
      {
        if (leg.pos == "bottom") guides(fill = guide_legend(nrow = 2))
      } +
      theme(
        #plot.title = element_text(size = 30, face = "bold"),
        strip.text = element_text(size = 18),
        axis.text = element_text(size = 11),
        plot.title = element_text(size = 28, hjust = 0.5),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 24),
        legend.position = leg.pos,
        #plot.margin = grid::unit(c(2,2,2,2), "mm"),
        margin(t = 2, r = 2, b = 2, l = 2, unit = "mm")
      )

    #fname <- paste0(plot_dir,"/test1.png")
    #ggsave(fname, p_tots, width = 14, height = 11)

    ### Plot a relative change map - file 2 divided by file 1.

    r_a <- r2 / r1
    r_a[is.na(r_a)] <- 0

    if (global(r_a, sum, na.rm = T)[, 1] > 0) {
      #r_a[is.na(r_a)] <- 0
      #r_a[is.infinite(r_a)] <- 0
      r_a[r_a == 0] <- NA
      r_a[is.infinite(r_a)] <- NA

      ## reclassify and plot ##
      # exract all the values present, into a vector.
      v_q <- c(
        0,
        0.25,
        0.5,
        0.7,
        0.8,
        0.9,
        0.98,
        1.02,
        1.1,
        1.2,
        1.3,
        1.5,
        2,
        3
      )

      v_cols <- c(
        grDevices::colorRampPalette(colors = c("#259bdf", "#d8f1ff"))(6),
        "#ececec",
        grDevices::colorRampPalette(colors = c("#ffdad8", "#f8493f"))(7)
      )

      # create a reclassification matrix and labels for the plot.
      m <- matrix(
        c(
          v_q,
          v_q[2:length(v_q)],
          ceiling(max(global(r_a, max, na.rm = T)$max)),
          1:length(v_q)
        ),
        ncol = 3
      )
      if (is.na(m[nrow(m), 2])) {
        m[nrow(m), 2] <- m[nrow(m), 1] + 1
      }
      if (m[nrow(m), 2] < m[nrow(m), 1]) {
        m[nrow(m), 2] <- m[nrow(m), 1] + 1
      }

      # create labels
      v_labs <- unlist(lapply(1:nrow(m), function(x) {
        paste0(round(m[x, 1], 2), "-", round(m[x, 2], 2))
      }))
      v_labs[length(v_labs)] <- paste0("> ", round(m[nrow(m), 1], 2))

      # reclassify and extract the levels
      r_a_rc <- terra::classify(r_a, m)
      r_a_rc <- as.factor(r_a_rc)

      names(r_a_rc) <- paste0("file_2 / file_1")

      dt_levs <- levels(r_a_rc)[[1]]
      #levels(r_a_rc)[[1]] <- data.table(ID = 1:11, sum = 1:11)

      # subset the colours and the labels based on factors present
      v_cols <- v_cols[unique(dt_levs$ID)]
      v_labs <- v_labs[unique(dt_levs$ID)]
    } else {
      r_a_rc <- copy(r_a)
      r_a_rc[r_a_rc == 0] <- NA
      v_cols <- "#ececec"
      v_labs <- "1"
    }

    g_diff <- ggplot() +
      geom_spatraster(data = r_a_rc, na.rm = T, maxcell = 1e+06) +
      # scale_fill_gradient2(low = "#16abf5", mid = "white", high = "#ff514e", midpoint = 5, breaks = 1:length(v_labs), labels = v_labs)+
      #scale_fill_brewer(labels = v_labs, palette = "RdBu", direction = brew_d, na.value = "grey90")+
      {
        if (!is.na(global(r_a_rc, sum, na.rm = T)[, 1])) {
          scale_fill_manual(
            values = v_cols,
            labels = v_labs,
            na.value = "transparent"
          )
        }
      } +
      #scale_y_continuous(expand = c(0, 0)) +
      labs(fill = "rel") +
      ggtitle(names(r_a_rc)) +
      geom_sf(data = sf_uk, fill = NA, colour = "black") +
      facet_wrap(~lyr) +
      #scale_fill_hypso_d(labels = v_labs, na.value = "transparent", palette = "etopo1")+
      theme_bw() +
      theme(
        strip.text.x = element_blank(),
        plot.title = element_text(size = 18, hjust = 0.5),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 16),
        legend.position = "right",
        legend.box.spacing = unit(2, "mm"),
        legend.margin = margin(0, 0, 0, 0),
        legend.key.height = unit(0.9, "cm"),
        plot.margin = unit(c(1, 1, 1, 1), "mm")
      )

    #fname <- paste0(plot_dir,"/test2.png")
    #ggsave(fname, g_diff, width = 7, height = 5)

    ### Plot an absolute change map - file 2 divided by file 1.
    r_a <- r2 - r1

    if (global(r_a, sum, na.rm = T)[, 1] != 0) {
      #r_a[is.na(r_a)] <- 0
      #r_a[is.infinite(r_a)] <- 0
      #r_a[r_a == 0] <- NA
      #r_a[is.infinite(r_a)] <- NA

      ## reclassify and plot ##
      # exract all the values present, into a vector.
      v_q <- c(
        -1000,
        -10,
        -5,
        -1,
        -0.5,
        -0.1,
        -0.01,
        0.01,
        0.1,
        0.5,
        1,
        5,
        10,
        1000
      )

      if (global(r_a, min, na.rm = T) < -1000) {
        v_q[1] <- floor(global(r_a, min, na.rm = T)[, 1])
      }
      if (global(r_a, max, na.rm = T) > 1000) {
        v_q[14] <- ceiling(global(r_a, max, na.rm = T)[, 1])
      }

      v_cols <- c(
        grDevices::colorRampPalette(colors = c("#259bdf", "#d8f1ff"))(6),
        "#ececec",
        grDevices::colorRampPalette(colors = c("#ffdad8", "#f8493f"))(6)
      )

      # create a reclassification matrix and labels for the plot.
      m <- matrix(c(v_q[1:13], v_q[2:14], 1:13), ncol = 3)

      # create labels
      v_labs <- unlist(lapply(1:nrow(m), function(x) {
        paste0(round(m[x, 1], 2), "-", round(m[x, 2], 2))
      }))

      # reclassify and extract the levels
      r_a_rc <- terra::classify(r_a, m)
      r_a_rc <- as.factor(r_a_rc)

      names(r_a_rc) <- paste0("file_2 - file_1")

      dt_levs <- levels(r_a_rc)[[1]]

      # subset the colours and the labels based on factors present
      v_cols <- v_cols[unique(dt_levs$ID)]
      v_labs <- v_labs[unique(dt_levs$ID)]
    } else {
      r_a_rc <- copy(r_a)
      r_a_rc[r_a_rc == 0] <- NA
      if (length(v_cols) == 0) {
        v_cols <- "#ececec"
      }
      if (length(v_labs) == 0) v_labs <- "0"
    }

    g_diffabs <- ggplot() +
      geom_spatraster(data = r_a_rc, na.rm = T, maxcell = 1e+06) +
      # scale_fill_gradient2(low = "#16abf5", mid = "white", high = "#ff514e", midpoint = 5, breaks = 1:length(v_labs), labels = v_labs)+
      #scale_fill_brewer(labels = v_labs, palette = "RdBu", direction = brew_d, na.value = "grey90")+
      {
        if (!is.na(global(r_a_rc, sum, na.rm = T)[, 1])) {
          scale_fill_manual(
            values = v_cols,
            labels = v_labs,
            na.value = "transparent"
          )
        }
      } +
      #scale_y_continuous(expand = c(0, 0)) +
      labs(fill = "tonnes") +
      ggtitle(names(r_a_rc)) +
      geom_sf(data = sf_uk, fill = NA, colour = "black") +
      facet_wrap(~lyr) +
      #scale_fill_hypso_d(labels = v_labs, na.value = "transparent", palette = "etopo1")+
      theme_bw() +
      theme(
        strip.text.x = element_blank(),
        plot.title = element_text(size = 18, hjust = 0.5),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 16),
        legend.position = "right",
        legend.box.spacing = unit(2, "mm"),
        legend.margin = margin(0, 0, 0, 0),
        legend.key.height = unit(0.9, "cm"),
        plot.margin = unit(c(1, 1, 1, 1), "mm")
      )

    #fname <- paste0(plot_dir,"/test3.png")
    #ggsave(fname, g_diffabs, width = 7, height = 5)

    ## patchwork them
    p <- p_tots /
      (g_diff + g_diffabs) +
      plot_layout(nrow = 2, heights = c(1, 0.85))

    fname <- paste0(plot_dir, "/", pollutant, "_UKEIRE_2.", i, "_UKSECMAP.png")
    ggsave(fname, p, width = 10.4, height = 12, limitsize = F)

    l[[i]] <- fname
  }

  return(l)
}

###############################################################################
#### function to
comp_map_tot_mon <- function(pollutant, l_m_1, l_m_2, plot_dir) {
  # plot out each monthly total

  l <- list()

  for (i in 1:12) {
    print(i)

    # retrieve data
    s1 <- rast(lapply(l_m_1[["total_month"]], function(x) x[[i]]))
    r1 <- app(s1, sum, na.rm = T)

    s2 <- rast(lapply(l_m_2[["total_month"]], function(x) x[[i]]))
    r2 <- app(s2, sum, na.rm = T)

    ### Plot national totals

    # new stack, with the rasters in order of the filenames provided
    s <- c(r1, r2)

    if (sum(global(s, sum, na.rm = T)[, 1]) > 0) {
      s[s == 0] <- NA
      # s[is.na(s)] <- 0

      ## reclassify and plot ##
      # exract all the values present, into a vector.
      v_v <- values(s)
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
          ceiling(max(global(s, max, na.rm = T)$max, na.rm = T)),
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
      s_rc <- terra::classify(s, m)
      s_rc <- as.factor(s_rc)
    } else {
      s_rc <- copy(s)
    }

    # names
    if (nlyr(s_rc) != 2) {
      stop("There should be 2 layers in the comparison")
    }

    # rename to sectors
    names(s_rc) <- c(
      paste0(
        unique(l_m_1$summary$emis_y),
        "_",
        unique(l_m_1$summary$inv_y),
        "_",
        unique(l_m_1$summary$emep_v)
      ),
      paste0(
        unique(l_m_2$summary$emis_y),
        "_",
        unique(l_m_2$summary$inv_y),
        "_",
        unique(l_m_2$summary$emep_v)
      )
    )

    p_tots <- ggplot() +
      geom_spatraster(data = s_rc, na.rm = T, maxcell = 1e+06) +
      {
        if (sum(global(s, sum, na.rm = T)[, 1]) > 0) {
          scale_fill_brewer(
            labels = v_labs,
            palette = "Spectral",
            breaks = 1:length(v_q),
            direction = brew_d
          )
        }
      } +
      scale_y_continuous(expand = c(0, 0)) +
      scale_x_continuous(expand = c(0, 0)) +
      # ggtitle(paste0(unique(l_m_1$summary$fname)," vs ",unique(l_m_2$summary$fname)))+
      ggtitle(month.name[i]) +
      labs(fill = bquote(tonnes ~ a^-1), y = "", x = "") +
      # ggtitle(bquote("Emissions of"~.(p)~a^-1))+
      geom_sf(data = sf_uk, fill = NA, colour = "black") +
      facet_wrap(~lyr, nrow = 1) +
      theme_bw() +
      {
        if (leg.pos == "bottom") guides(fill = guide_legend(nrow = 2))
      } +
      theme(
        #plot.title = element_text(size = 30, face = "bold"),
        strip.text = element_text(size = 18),
        axis.text = element_text(size = 11),
        plot.title = element_text(size = 28, hjust = 0.5),
        legend.text = element_text(size = 18),
        legend.title = element_text(size = 24),
        legend.position = leg.pos,
        #plot.margin = grid::unit(c(2,2,2,2), "mm"),
        margin(t = 2, r = 2, b = 2, l = 2, unit = "mm")
      )

    #fname <- paste0(plot_dir,"/test1.png")
    #ggsave(fname, p_tots, width = 14, height = 11)

    ### Plot a relative change map - file 2 divided by file 1.

    r_a <- r2 / r1
    r_a[is.na(r_a)] <- 0

    if (global(r_a, sum, na.rm = T)[, 1] > 0) {
      #r_a[is.na(r_a)] <- 0
      #r_a[is.infinite(r_a)] <- 0
      r_a[r_a == 0] <- NA
      r_a[is.infinite(r_a)] <- NA

      ## reclassify and plot ##
      # exract all the values present, into a vector.
      v_q <- c(
        0,
        0.25,
        0.5,
        0.7,
        0.8,
        0.9,
        0.98,
        1.02,
        1.1,
        1.2,
        1.3,
        1.5,
        2,
        3
      )

      v_cols <- c(
        grDevices::colorRampPalette(colors = c("#259bdf", "#d8f1ff"))(6),
        "#ececec",
        grDevices::colorRampPalette(colors = c("#ffdad8", "#f8493f"))(7)
      )

      # create a reclassification matrix and labels for the plot.
      m <- matrix(
        c(
          v_q,
          v_q[2:length(v_q)],
          ceiling(max(global(r_a, max, na.rm = T)$max)),
          1:length(v_q)
        ),
        ncol = 3
      )
      if (is.na(m[nrow(m), 2])) {
        m[nrow(m), 2] <- m[nrow(m), 1] + 1
      }
      if (m[nrow(m), 2] < m[nrow(m), 1]) {
        m[nrow(m), 2] <- m[nrow(m), 1] + 1
      }

      # create labels
      v_labs <- unlist(lapply(1:nrow(m), function(x) {
        paste0(round(m[x, 1], 2), "-", round(m[x, 2], 2))
      }))
      v_labs[length(v_labs)] <- paste0("> ", round(m[nrow(m), 1], 2))

      # reclassify and extract the levels
      r_a_rc <- terra::classify(r_a, m)
      r_a_rc <- as.factor(r_a_rc)

      names(r_a_rc) <- paste0("file_2 / file_1")

      dt_levs <- levels(r_a_rc)[[1]]

      # subset the colours and the labels based on factors present
      v_cols <- v_cols[unique(dt_levs$ID)]
      v_labs <- v_labs[unique(dt_levs$ID)]
    } else {
      r_a_rc <- copy(r_a)
      r_a_rc[r_a_rc == 0] <- NA
      names(r_a_rc) <- paste0("file_2 / file_1")
      v_cols <- "#ececec"
      v_labs <- "1"
    }

    g_diff <- ggplot() +
      geom_spatraster(data = r_a_rc, na.rm = T, maxcell = 1e+06) +
      # scale_fill_gradient2(low = "#16abf5", mid = "white", high = "#ff514e", midpoint = 5, breaks = 1:length(v_labs), labels = v_labs)+
      #scale_fill_brewer(labels = v_labs, palette = "RdBu", direction = brew_d, na.value = "grey90")+
      {
        if (!is.na(global(r_a_rc, sum, na.rm = T)[, 1])) {
          scale_fill_manual(
            values = v_cols,
            labels = v_labs,
            na.value = "transparent"
          )
        }
      } +
      #scale_y_continuous(expand = c(0, 0)) +
      labs(fill = "rel") +
      ggtitle(names(r_a_rc)) +
      geom_sf(data = sf_uk, fill = NA, colour = "black") +
      facet_wrap(~lyr) +
      #scale_fill_hypso_d(labels = v_labs, na.value = "transparent", palette = "etopo1")+
      theme_bw() +
      theme(
        strip.text.x = element_blank(),
        plot.title = element_text(size = 18, hjust = 0.5),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 16),
        legend.position = "right",
        legend.box.spacing = unit(2, "mm"),
        legend.margin = margin(0, 0, 0, 0),
        legend.key.height = unit(0.9, "cm"),
        plot.margin = unit(c(1, 1, 1, 1), "mm")
      )

    #fname <- paste0(plot_dir,"/test2.png")
    #ggsave(fname, g_diff, width = 7, height = 5)

    ### Plot an absolute change map - file 2 divided by file 1.
    r_a <- r2 - r1

    if (global(r_a, sum, na.rm = T)[, 1] != 0) {
      r_a[is.na(r_a)] <- 0
      r_a[is.infinite(r_a)] <- 0
      #r_a[r_a == 0] <- NA
      #r_a[is.infinite(r_a)] <- NA

      ## reclassify and plot ##
      # exract all the values present, into a vector.
      v_q <- c(
        -1000,
        -10,
        -5,
        -1,
        -0.5,
        -0.1,
        -0.01,
        0.01,
        0.1,
        0.5,
        1,
        5,
        10,
        1000
      )

      if (global(r_a, min, na.rm = T) < -1000) {
        v_q[1] <- floor(global(r_a, min, na.rm = T)[, 1])
      }
      if (global(r_a, max, na.rm = T) > 1000) {
        v_q[14] <- ceiling(global(r_a, max, na.rm = T)[, 1])
      }

      v_cols <- c(
        grDevices::colorRampPalette(colors = c("#259bdf", "#d8f1ff"))(6),
        "#ececec",
        grDevices::colorRampPalette(colors = c("#ffdad8", "#f8493f"))(6)
      )

      # create a reclassification matrix and labels for the plot.
      m <- matrix(c(v_q[1:13], v_q[2:14], 1:13), ncol = 3)

      # create labels
      v_labs <- unlist(lapply(1:nrow(m), function(x) {
        paste0(round(m[x, 1], 2), "-", round(m[x, 2], 2))
      }))

      # reclassify and extract the levels
      r_a_rc <- terra::classify(r_a, m)
      r_a_rc <- as.factor(r_a_rc)

      names(r_a_rc) <- paste0("file_2 - file_1")

      dt_levs <- levels(r_a_rc)[[1]]

      # subset the colours and the labels based on factors present
      v_cols <- v_cols[unique(dt_levs$ID)]
      v_labs <- v_labs[unique(dt_levs$ID)]
    } else {
      r_a_rc <- copy(r_a)
      r_a_rc[r_a_rc == 0] <- NA
      names(r_a_rc) <- paste0("file_2 - file_1")
      if (length(v_cols) == 0) {
        v_cols <- "#ececec"
      }
      if (length(v_labs) == 0) v_labs <- "0"
    }

    g_diffabs <- ggplot() +
      geom_spatraster(data = r_a_rc, na.rm = T, maxcell = 1e+06) +
      # scale_fill_gradient2(low = "#16abf5", mid = "white", high = "#ff514e", midpoint = 5, breaks = 1:length(v_labs), labels = v_labs)+
      #scale_fill_brewer(labels = v_labs, palette = "RdBu", direction = brew_d, na.value = "grey90")+
      {
        if (!is.na(global(r_a_rc, sum, na.rm = T)[, 1])) {
          scale_fill_manual(
            values = v_cols,
            labels = v_labs,
            na.value = "transparent"
          )
        }
      } +
      #scale_y_continuous(expand = c(0, 0)) +
      labs(fill = "tonnes") +
      ggtitle(names(r_a_rc)) +
      geom_sf(data = sf_uk, fill = NA, colour = "black") +
      facet_wrap(~lyr) +
      #scale_fill_hypso_d(labels = v_labs, na.value = "transparent", palette = "etopo1")+
      theme_bw() +
      theme(
        strip.text.x = element_blank(),
        plot.title = element_text(size = 18, hjust = 0.5),
        legend.text = element_text(size = 12),
        legend.title = element_text(size = 16),
        legend.position = "right",
        legend.box.spacing = unit(2, "mm"),
        legend.margin = margin(0, 0, 0, 0),
        legend.key.height = unit(0.9, "cm"),
        plot.margin = unit(c(1, 1, 1, 1), "mm")
      )

    #fname <- paste0(plot_dir,"/test3.png")
    #ggsave(fname, g_diffabs, width = 7, height = 5)

    ## patchwork them
    p <- p_tots /
      (g_diff + g_diffabs) +
      plot_layout(nrow = 2, heights = c(1, 0.85))

    fname <- paste0(plot_dir, "/", pollutant, "_UKEIRE_3.", i, "_UKTOTMON.png")
    ggsave(fname, p, width = 10.4, height = 12, limitsize = F)

    l[[i]] <- fname
  }

  return(l)
}

###############################################################################
#### function to plot monthly totals per ISO on a line graph
comp_lin_tot_mon <- function(pollutant, l_m_1, l_m_2, plot_dir) {
  # comparing monthly totals in one plot

  i_time <- 1:12

  dt1 <- l_m_1[["summary"]] %>%
    .[, .(emis_kt = sum(emis_t, na.rm = T) / 1000), by = .(Area, t)]
  dt1[,
    file := paste0(
      unique(l_m_1$summary$emis_y),
      "_",
      unique(l_m_1$summary$inv_y),
      "_",
      unique(l_m_1$summary$emep_v)
    )
  ]
  dt2 <- l_m_2[["summary"]] %>%
    .[, .(emis_kt = sum(emis_t, na.rm = T) / 1000), by = .(Area, t)]
  dt2[,
    file := paste0(
      unique(l_m_2$summary$emis_y),
      "_",
      unique(l_m_2$summary$inv_y),
      "_",
      unique(l_m_2$summary$emep_v)
    )
  ]
  dt <- rbindlist(list(dt1, dt2), use.names = T)

  # plot
  p <- ggplot(data = dt, aes(x = t, y = emis_kt, group = file, colour = file)) +
    geom_line() +
    geom_point() +
    scale_x_continuous(breaks = 1:12) +
    facet_wrap(~Area, scales = "free_y", ncol = 1) +
    labs(y = bquote(kt ~ month^-1)) +
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

  fname <- paste0(plot_dir, "/", pollutant, "_UKEIRE_4_UKLINMON.png")
  ggsave(fname, p, width = 11, height = 8, limitsize = F)

  return(fname)
}

###############################################################################
#### function to plot monthly totals per ISO on line graph composite
comp_lin_sec_mon <- function(pollutant, l_m_1, l_m_2, plot_dir) {
  i_time <- 1:12

  dt1 <- l_m_1[["summary"]] %>%
    .[, .(emis_kt = sum(emis_t, na.rm = T) / 1000), by = .(Area, GNFR, t)]
  dt1[,
    file := paste0(
      unique(l_m_1$summary$emis_y),
      "_",
      unique(l_m_1$summary$inv_y),
      "_",
      unique(l_m_1$summary$emep_v)
    )
  ]
  dt2 <- l_m_2[["summary"]] %>%
    .[, .(emis_kt = sum(emis_t, na.rm = T) / 1000), by = .(Area, GNFR, t)]
  dt2[,
    file := paste0(
      unique(l_m_2$summary$emis_y),
      "_",
      unique(l_m_2$summary$inv_y),
      "_",
      unique(l_m_2$summary$emep_v)
    )
  ]

  dt <- rbindlist(list(dt1, dt2), use.names = T)

  # list for plots
  l_p <- list()

  for (i in as.vector(dt_sec[, GNFRlong][1:13])) {
    g1 <- ggplot(
      data = dt[GNFR == i & Area != "SEA"],
      aes(x = t, y = emis_kt, colour = file, group = file)
    ) +
      geom_line() +
      geom_point() +
      ggtitle(i) +
      labs(y = bquote(kt ~ month^-1)) +
      scale_x_continuous(breaks = 1:12) +
      facet_wrap(~Area, scales = "free_y", ncol = 1) +
      theme_bw() +
      month_sector_theme(sector = i)

    l_p[[i]] <- g1
  }

  # plot
  #label_plot <- ggdraw() + draw_label(paste0("Annual input file:\n using EMEP",
  #                                           emep_version," profiles"),
  #									  x = 0.1, y = 0.65, size = 24)

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
    l_p[[11]] +
    l_p[[12]] +
    l_p[[13]] +
    plot_layout(ncol = 5)

  fname <- paste0(plot_dir, "/", pollutant, "_UKEIRE_5_SECLINMON.png")
  ggsave(fname, p, width = 21, height = 13, limitsize = F)

  return(fname)
}

################################################################################
#### function to plot dominant sector in change
domSNAP_map_tot_ann <- function(pollutant, l_m_1, l_m_2, plot_dir) {
  # retrieve data
  l1 <- l_m_1[["annual_sector"]]
  l2 <- l_m_2[["annual_sector"]]

  # make sectoral stack
  l1 <- lapply(l1, function(x) {
    app(rast(x), sum, na.rm = T)
  })

  l2 <- lapply(l2, function(x) {
    app(rast(x), sum, na.rm = T)
  })

  s1 <- rast(l1)
  s1[s1 == 0] <- NA
  s2 <- rast(l2)
  s2[s2 == 0] <- NA

  # calculate difference
  s_diff <- s2 - s1

  # set any infinite values to NA
  s_diff[is.infinite(s_diff)] <- NA

  # create positive and negative change stacks
  # we need this as which.min might still be a positive value
  s_diff_pos <- s_diff
  s_diff_pos[s_diff_pos < 0] <- NA
  s_diff_neg <- s_diff
  s_diff_neg[s_diff_neg > 0] <- NA

  # make a raster of the max change per cell - positive and negative
  r_diff_posmax <- app(s_diff_pos, which.max, na.rm = T)
  r_diff_posmax <- as.factor(r_diff_posmax)

  r_diff_negmax <- app(s_diff_neg, which.min, na.rm = T)
  r_diff_negmax <- as.factor(r_diff_negmax)

  # extract the levels in both rasters, for the plotting
  v_secID <- c(levels(r_diff_posmax)[[1]]$ID, levels(r_diff_negmax)[[1]]$ID)
  v_secID <- sort(unique(v_secID[!is.na(v_secID)]))

  v_secCols <- c(
    "#46f149",
    "#5950bf",
    "#85c9a7",
    "#4f587c",
    "#a31890",
    "#e79120",
    "#213f13",
    "#9c701e",
    "#f157ff",
    "#2023d8",
    "#bed7f8",
    "#ffee54",
    "#fc4d2a"
  )

  # plot
  s_plot <- c(r_diff_posmax, r_diff_negmax)
  names(s_plot) <- c("Max Abs Positive Change", "Max Abs Negative Change")

  g_change <- ggplot() +
    geom_spatraster(data = s_plot, na.rm = T, maxcell = 1e+06) +
    scale_fill_manual(
      labels = names(s_diff_pos)[v_secID],
      values = v_secCols[v_secID],
      na.value = "transparent"
    ) +
    labs(fill = "Sector") +
    # ggtitle(names(r_a_rc)) +
    # geom_sf(data = sf_uk, fill = NA, colour = "black") +
    facet_wrap(~lyr) +
    #scale_fill_hypso_d(labels = v_labs, na.value = "transparent", palette = "etopo1")+
    theme_bw() +
    theme(
      strip.text.x = element_text(size = 16),
      plot.title = element_text(size = 18, hjust = 0.5),
      legend.text = element_text(size = 12),
      legend.title = element_text(size = 16),
      legend.position = "right",
      legend.box.spacing = unit(2, "mm"),
      legend.margin = margin(0, 0, 0, 0),
      legend.key.height = unit(0.9, "cm"),
      plot.margin = unit(c(1, 1, 1, 1), "mm")
    )

  fname <- paste0(plot_dir, "/", pollutant, "_UKEIRE_7_UKSECCHANGE.png")
  ggsave(fname, g_change, width = 13, height = 8, limitsize = F)

  return(fname)
}


################################################################################
#### function to set theme of ggplot dependent on GNFR - for full 13 GNFR plot
#### specifically for line plots of totals per sector per month.
month_sector_theme <- function(sector) {
  if (sector %in% c("A_PublicPower", "F_RoadTransport")) {
    x <- theme(
      plot.title = element_text(size = 20, hjust = 0.5),
      strip.text.x = element_text(size = 14),
      axis.title.x = element_blank(),
      legend.position = "none",
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
      plot.title = element_text(size = 20, hjust = 0.5),
      strip.text.x = element_text(size = 14),
      axis.title.y = element_blank(),
      axis.title.x = element_blank(),
      legend.position = "none",
      margin(t = 2, r = 2, b = 2, l = 2, unit = "mm")
    )
  } else if (sector %in% c("L_AgriOther")) {
    x <- theme(
      plot.title = element_text(size = 20, hjust = 0.5),
      strip.text.x = element_text(size = 14),
      axis.title.y = element_blank(),
      legend.position = "none",
      margin(t = 2, r = 2, b = 2, l = 2, unit = "mm")
    )
  } else if (sector == "K_AgriLivestock") {
    x <- theme(
      plot.title = element_text(size = 20, hjust = 0.5),
      strip.text.x = element_text(size = 14),
      legend.position = "none",
      margin(t = 2, r = 2, b = 2, l = 2, unit = "mm")
    )
  } else if (sector %in% c("M_Other")) {
    x <- theme(
      plot.title = element_text(size = 20, hjust = 0.5),
      strip.text.x = element_text(size = 14),
      axis.title.y = element_blank(),
      legend.position = "bottom",
      legend.text = element_text(size = 16),
      margin(t = 2, r = 2, b = 2, l = 2, unit = "mm")
    )
  }

  return(x)
}

################################################################################
#### function to plot up the sectoral totals in bars

comp_bar_sec_ann <- function(pollutant, l_m_1, l_m_2, plot_dir) {
  dt_tots_1 <- l_m_1[["summary"]]
  dt_tots_1[, plot_group := paste0(emis_y, "_", inv_y, "_", emep_v)]
  dt_tots_1 <- dt_tots_1[,
    .(emis_kt = sum(emis_t, na.rm = T) / 1000),
    by = .(Area, GNFR, emis_y, inv_y, emep_v, plot_group)
  ]

  dt_tots_2 <- l_m_2[["summary"]]
  dt_tots_2[, plot_group := paste0(emis_y, "_", inv_y, "_", emep_v)]
  dt_tots_2 <- dt_tots_2[,
    .(emis_kt = sum(emis_t, na.rm = T) / 1000),
    by = .(Area, GNFR, emis_y, inv_y, emep_v, plot_group)
  ]

  dt_tots <- rbindlist(list(dt_tots_1, dt_tots_2), use.names = T)

  p <- ggplot(
    dt_tots,
    aes(x = GNFR, y = emis_kt, group = plot_group, fill = plot_group)
  ) +
    geom_bar(
      stat = "identity",
      position = position_dodge2(width = 0.9, preserve = "single"),
      alpha = 0.8
    ) +
    #geom_point()+
    #geom_line()+
    ylab(bquote(kt ~ a^-1)) +
    facet_wrap(~Area, scales = "free_y", nrow = 3) +
    theme_bw() +
    theme(
      legend.title = element_blank(),
      legend.position = "top",
      axis.title.x = element_blank(),
      axis.text.x = element_text(angle = 45, vjust = 1, hjust = 1)
    )

  fname <- paste0(plot_dir, "/", pollutant, "_UKEIRE_6_SECBARANN.png")
  ggsave(fname, p, width = 8, height = 6)

  return(fname)
}
