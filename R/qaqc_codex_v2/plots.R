## Quantile probabilities used to make raster map classes.
qaqc_v2_quantile_probs <- c(0.20, 0.40, 0.55, 0.7, 0.82, 0.92, 0.97, 1)

## Return outline polygons for domains where an overlay improves the map.
## For GLOBAL, select EDGAR or HTAP grid based on data_source parameter.
qaqc_v2_world_polygons <- function(domain, data_source = NA) {
  domain <- qaqc_v2_normalise_domain(domain)
  root <- qaqc_v2_project_root()

  if (domain == "GLOBAL") {
    ## GLOBAL domain uses data-source-specific grid boundaries (EDGAR or HTAP).
    grid_type <- "htap" # Default to HTAP
    if (!is.na(data_source) && data_source != "") {
      data_source_lower <- tolower(data_source)
      if (grepl("edgar", data_source_lower)) {
        grid_type <- "edgar"
      } else if (grepl("htap", data_source_lower)) {
        grid_type <- "htap"
      }
    }
    poly_path <- file.path(
      root,
      "data",
      "spatial",
      "world",
      paste0("global_iso_", grid_type, ".shp")
    )
    if (file.exists(poly_path)) {
      return(sf::st_read(poly_path, quiet = TRUE))
    }
  }

  if (domain == "UKEIRE") {
    poly_path <- file.path(root, "data", "spatial", "UKEire", "UKEIRE_LL.shp")
    if (file.exists(poly_path)) {
      return(sf::st_read(poly_path, quiet = TRUE))
    }
  }

  if (domain == "EU") {
    ## EU domain - look for EU-specific shapefile if it exists
    poly_path <- file.path(root, "data", "spatial", "EU", "EU.shp")
    if (file.exists(poly_path)) {
      return(sf::st_read(poly_path, quiet = TRUE))
    }
    ## Fallback to world borders if EU-specific not available
    poly_path <- file.path(
      root,
      "data",
      "spatial",
      "world",
      "TM_WORLD_BORDERS-0.3.shp"
    )
    if (file.exists(poly_path)) {
      return(sf::st_read(poly_path, quiet = TRUE))
    }
  }

  NULL
}

## Convert positive raster values into quantile-based plotting breaks.
qaqc_v2_class_breaks <- function(r) {
  vals <- terra::values(r, mat = FALSE)
  vals <- vals[is.finite(vals) & vals > 0]
  if (length(vals) == 0) {
    return(c(0, 1))
  }
  brks <- unique(as.numeric(stats::quantile(
    vals,
    probs = qaqc_v2_quantile_probs,
    na.rm = TRUE,
    names = FALSE
  )))
  brks <- unique(c(0, brks))
  if (length(brks) < 2) {
    brks <- c(0, max(vals, na.rm = TRUE))
  }
  brks
}

## Make human-readable labels for the map legend bins.
qaqc_v2_class_labels <- function(breaks) {
  labs <- paste0(round(breaks[-length(breaks)], 2), " - ", round(breaks[-1], 2))
  labs[length(labs)] <- paste0("> ", round(breaks[length(breaks) - 1], 2))
  labs
}

## Turn a raster into a data frame with x/y coordinates and an emissions class.
qaqc_v2_raster_df <- function(path, breaks = NULL) {
  r <- terra::rast(path)
  ## Treat zero emissions as blank map cells.
  r[r == 0] <- NA
  if (is.null(breaks)) {
    breaks <- qaqc_v2_class_breaks(r)
  }
  df <- as.data.frame(r, xy = TRUE, na.rm = TRUE)
  names(df)[3] <- "emis_t"
  df$class <- cut(
    df$emis_t,
    breaks = breaks,
    labels = qaqc_v2_class_labels(breaks),
    include.lowest = TRUE,
    dig.lab = 5
  )
  df
}

## Build the total-emissions raster map for one pollutant/year.
qaqc_v2_total_map_plot <- function(path, domain, species, y, data_source = NA) {
  r <- terra::rast(path)
  breaks <- qaqc_v2_class_breaks(r)
  df <- qaqc_v2_raster_df(path, breaks)
  sf_poly <- qaqc_v2_world_polygons(
    domain,    
    data_source = data_source
  )

  p <- ggplot2::ggplot() +
    ggplot2::geom_tile(data = df, ggplot2::aes(x = x, y = y, fill = class)) +
    ggplot2::scale_fill_brewer(
      palette = "Spectral",
      direction = -1,
      na.value = NA
    ) +
    ggplot2::coord_sf(expand = FALSE) +
	ggplot2::geom_sf(
      data = sf_poly,
      inherit.aes = FALSE,
      fill = NA,
      colour = "black",
      linewidth = 0.15
    ) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::labs(
      fill = bquote(tonnes ~ a^-1),
      y = "",
      x = ""
    ) +
    ggplot2::theme_bw() +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2)) +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 12),
      legend.text = ggplot2::element_text(size = 18),
      legend.title = ggplot2::element_text(size = 24),
      legend.position = "bottom",
      plot.margin = ggplot2::margin(t = 2, r = 2, b = 2, l = 2, unit = "mm")
    )
 
  p
}

## Build faceted raster maps for all sector rasters, with domain-specific overlays.
qaqc_v2_sector_map_plot <- function(
  paths,
  domain,
  species,
  y,
  dt_sec = NULL,
  data_source = NA
) {
  if (length(paths) == 0) {
    return(NULL)
  }
  domain <- qaqc_v2_normalise_domain(domain)

  ## Use one shared set of breaks so sector facets are visually comparable.
  breaks <- qaqc_v2_class_breaks(terra::rast(paths))
  l_df <- lapply(seq_along(paths), function(i) {
    df <- qaqc_v2_raster_df(paths[i], breaks)
    if (nrow(df) == 0) {
      return(NULL)
    }
    sec_match <- sprintf("sec%02d", i)
    label <- paste0("Sector ", i)
    ## Replace generic sector numbers with GNFR names when the lookup is supplied.
    if (!is.null(dt_sec) && nrow(dt_sec) > 0 && "sec" %in% names(dt_sec)) {
      sec_name <- dt_sec[["GNFRlong"]][match(sec_match, dt_sec[["sec"]])][1]
      if (!is.na(sec_name)) {
        label <- sec_name
      }
    }
    df$Sector <- label
    df
  })
  l_df <- Filter(Negate(is.null), l_df)
  if (length(l_df) == 0) {
    return(NULL)
  }
  df <- data.table::rbindlist(l_df)

  ## Add domain-specific outlines for improved visualization.
  sf_poly <- qaqc_v2_world_polygons(
    domain,    
    data_source = data_source
  )

  p <- ggplot2::ggplot(data = df, ggplot2::aes(x = x, y = y, fill = class)) +
    ggplot2::geom_tile() +
    ggplot2::scale_fill_brewer(
      palette = "Spectral",
      direction = -1,
      na.value = NA
    ) +
    ggplot2::coord_sf(expand = FALSE) +
    ggplot2::scale_y_continuous(expand = c(0, 0)) +
    ggplot2::scale_x_continuous(expand = c(0, 0)) +
    ggplot2::facet_wrap(~Sector, ncol = 4) +
    ggplot2::geom_sf(
      data = sf_poly,
      inherit.aes = FALSE,
      fill = NA,
      colour = "black",
      linewidth = 0.15
    ) +
    ggplot2::labs(
      fill = bquote(tonnes ~ a^-1),
      y = "",
      x = ""
    ) +
    ggplot2::theme_bw() +
    ggplot2::guides(fill = ggplot2::guide_legend(nrow = 2)) +
    ggplot2::theme(
      axis.text = ggplot2::element_text(size = 8),
      legend.text = ggplot2::element_text(size = 16),
      legend.title = ggplot2::element_text(size = 20),
      legend.position = "bottom",
      strip.text = ggplot2::element_text(size = 10),
      plot.margin = ggplot2::margin(t = 2, r = 2, b = 2, l = 2, unit = "mm")
    )

  p
}

## Create all plot image files used by the QAQC v2 R Markdown report.
qaqc_v2_write_plots <- function(
  domain,
  y,
  species,
  folname,
  summary_paths,
  raster_paths,
  dt_sec = NULL,
  data_source = NA,
  inv = NA
) {
  plot_dir <- file.path(folname, "plots", paste0("e", y))
  dir.create(plot_dir, recursive = TRUE, showWarnings = FALSE)

  ## Initialise every expected output path as missing; plots fill these in when created.
  out <- list(
    stage = NA_character_,
    sector_bar = NA_character_,
    raster_total = NA_character_,
    raster_sector = NA_character_,
    timeseries_total = NA_character_,
    timeseries_sector = NA_character_
  )

  ## Compare each processing stage against inventory totals by area.
  dt_stage <- qaqc_v2_stage_totals(summary_paths)
  if (nrow(dt_stage) > 0) {
    dt <- dt_stage[,
      .(emis_kt = sum(emis_t, na.rm = TRUE) / 1000),
      by = .(Area, stage)
    ]
    dt <- dt[!is.na(Area) & Area != "SUM_ALL"]
    dt[, inventory_kt := emis_kt[stage == "inventory"][1], by = Area]
    dt <- dt[!is.na(inventory_kt) & inventory_kt > 0]
    dt[, ratio_to_inventory := emis_kt / inventory_kt]
    dt[,
      stage := factor(
        stage,
        levels = c(
          "inventory",
          "masked",
          "processed",
          "netcdf_input",
          "netcdf_output"
        )
      )
    ]
    ## Split many areas into groups so labels remain legible.
    area_order <- dt[
      stage == "inventory",
      .(inventory_kt = max(inventory_kt, na.rm = TRUE)),
      by = Area
    ][
      order(-inventory_kt),
      Area
    ]
    dt[, Area := factor(Area, levels = area_order)]
    dt[, rank := as.integer(Area)]

    if (domain == "UKEIRE") {
      cut_length <- 1
      cut_labels <- 1:1
    } else if (domain == "EU") {
      cut_length <- 3
      cut_labels <- 1:2
    } else if (domain == "GLOBAL") {
      cut_length <- 5
      cut_labels <- 1:4
    }

    if (domain == "UKEIRE") {
      dt[, group := 1]
    } else {
      dt[,
        group := cut(
          rank,
          breaks = unique(round(seq(
            0,
            length(area_order),
            length.out = cut_length
          ))),
          include.lowest = TRUE,
          labels = paste("ISO group", cut_labels)
        )
      ]
    }

    p <- ggplot2::ggplot(
      dt,
      ggplot2::aes(Area, ratio_to_inventory, fill = stage)
    ) +
      ggplot2::geom_col(
        position = ggplot2::position_dodge(width = 0.8),
        width = 0.72
      ) +
      ggplot2::geom_hline(
        yintercept = 1,
        colour = "grey35",
        linetype = "dashed",
        linewidth = 0.25
      ) +
      ggplot2::facet_wrap(~group, ncol = 1, scales = "free_x") +
      ggplot2::labs(x = NULL, y = "ratio to inventory", fill = "Stage") +
      ggplot2::theme_bw() +
      ggplot2::theme(
        legend.position = "bottom",
        axis.text.x = ggplot2::element_text(
          angle = 90,
          hjust = 1,
          vjust = 0.5,
          size = 5
        ),
        strip.text = ggplot2::element_text(size = 9)
      )
    out$stage <- file.path(
      plot_dir,
      paste0(species, "_stage_totals_by_iso.png")
    )
    ggplot2::ggsave(out$stage, p, width = 12, height = 10)
  }

  ## Plot sector composition for the largest emitting areas.
  dt_sector <- qaqc_v2_top_iso_sector_table(summary_paths, top_n = 10)
  if (nrow(dt_sector) > 0) {
    dt_long <- data.table::melt(
      dt_sector,
      id.vars = "Sector",
      variable.name = "Area",
      value.name = "emis_kt"
    )
    dt_long <- dt_long[Sector != "Total"]
    iso_order <- names(dt_sector)[-1]
    dt_long[, Area := factor(Area, levels = iso_order)]
    p <- ggplot2::ggplot(dt_long, ggplot2::aes(Area, emis_kt, fill = Sector)) +
      ggplot2::geom_col(position = "stack") +
      ggplot2::labs(x = NULL, y = "kt a-1", fill = "Sector") +
      ggplot2::theme_bw() +
      ggplot2::theme(
        legend.position = "bottom",
        axis.text.x = ggplot2::element_text(angle = 35, hjust = 1)
      )
    out$sector_bar <- file.path(
      plot_dir,
      paste0(species, "_top10_iso_sector_totals.png")
    )
    ggplot2::ggsave(out$sector_bar, p, width = 9, height = 5.8)
  }

  ## Total raster map is skipped quietly if the expected file was not produced.
  if (!is.na(raster_paths$total) && file.exists(raster_paths$total)) {
    p <- qaqc_v2_total_map_plot(
      raster_paths$total,
      domain,
      species,
      y,
      data_source
    )
    out$raster_total <- file.path(
      plot_dir,
      paste0(species, "_total_quantile_map.png")
    )
    ggplot2::ggsave(out$raster_total, p, width = 10, height = 5.8)
  }

  ## Sector raster map is skipped when no sector rasters are available.
  if (length(raster_paths$sector) > 0) {
    p <- qaqc_v2_sector_map_plot(
      raster_paths$sector,
      domain,
      species,
      y,
      dt_sec,
      data_source
    )
    out$raster_sector <- file.path(
      plot_dir,
      paste0(species, "_sector_quantile_maps.png")
    )
    ggplot2::ggsave(out$raster_sector, p, width = 11, height = 8.2)
  }

  ## Historical time-series plots are currently only supported for GLOBAL inventories.
  if (qaqc_v2_normalise_domain(domain) == "GLOBAL") {
    ts <- qaqc_v2_global_timeseries(data_source, inv, species, y)
    if (nrow(ts$data) > 0) {
      p <- ggplot2::ggplot(ts$data, ggplot2::aes(Year, emis_kt)) +
        ggplot2::geom_line() +
        ggplot2::geom_point(size = 1.8) +
        ggplot2::geom_vline(
          xintercept = y,
          linetype = "dashed",
          colour = "grey50"
        ) +
        ggplot2::labs(
          x = NULL,
          y = "kt a-1",
          title = paste(species, "global inventory total")
        ) +
        ggplot2::theme_bw()
      out$timeseries_total <- file.path(
        plot_dir,
        paste0(species, "_global_total_timeseries.png")
      )
      ggplot2::ggsave(out$timeseries_total, p, width = 8, height = 4.8)
    }
    if (!is.null(ts$sector) && nrow(ts$sector) > 0) {
      p <- ggplot2::ggplot(
        ts$sector,
        ggplot2::aes(Year, emis_kt, colour = GNFR)
      ) +
        ggplot2::geom_line() +
        ggplot2::geom_vline(
          xintercept = y,
          linetype = "dashed",
          colour = "grey50"
        ) +
        ggplot2::labs(
          x = NULL,
          y = "kt a-1",
          colour = "GNFR",
          title = paste(species, "global sector totals")
        ) +
        ggplot2::theme_bw() +
        ggplot2::theme(legend.position = "bottom")
      out$timeseries_sector <- file.path(
        plot_dir,
        paste0(species, "_global_sector_timeseries.png")
      )
      ggplot2::ggsave(out$timeseries_sector, p, width = 9, height = 5.2)
    }
  }

  out
}
