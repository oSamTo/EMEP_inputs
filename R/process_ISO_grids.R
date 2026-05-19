require(terra)
require(data.table)
require(parallel)
n_cores <- 16

i <- "EDGAR"

dt_iso <- fread(paste0("data/lookups/dt_iso_", i, ".csv"))

# territories
r <- rast(paste0("data/spatial/iso_map_", i, ".tif"))

v_r <- as.vector(unique(values(r)))

make_iso_rast <- function(r, v, i, dt_iso) {
    if (is.na(v)) {
        next
    }
    if (is.nan(v)) {
        next
    }

    rv <- copy(r)
    rv[rv != v] <- NA

    rv[!is.na(rv)] <- 1

    iso <- dt_iso[ISO_N3_EH == v, ISO_A3_EH]

    fname <- paste0("data/spatial/", i, "_iso_grids/iso_grid_", iso, ".tif")

    writeRaster(rv, filename = fname, overwrite = TRUE)

    fname
}

iso_chunks <- split(
    v_r,
    ceiling(seq_along(v_r) / n_cores)
)

l_out_all <- list()


for (k in seq_along(iso_chunks)) {
    v_iso <- iso_chunks[[k]]

    l_out <- parallel::mclapply(
        v_iso,
        function(x) {
            suppressMessages({
                make_iso_rast(r = r, x, i = i, dt_iso = dt_iso)
            })
        },
        mc.cores = n_cores,
        mc.preschedule = FALSE
    )
    l_out_all <- c(l_out_all, l_out)

    gc()
    terra::tmpFiles(orphan = TRUE, old = FALSE, remove = TRUE)
}

v_out_all <- list.files("data/spatial/EDGAR_iso_grids", pattern = ".tif$")
v_out_all <- gsub("iso_grid_", "", v_out_all)
v_out_all <- gsub(".tif", "", v_out_all)

length(v_r)

v_out_all %in% dt_iso[, ISO_A3_EH]
v_out_all[!(v_out_all %in% dt_iso[, ISO_A3_EH])]

dt_iso[, ISO_A3_EH][!(dt_iso[, ISO_A3_EH] %in% v_out_all)]
dt_iso[ISO_N3_EH %in% v_r, ISO_A3_EH] %in% v_out_all

sort(dt_iso[ISO_N3_EH %in% v_r, ISO_A3_EH])
length(dt_iso[ISO_N3_EH %in% v_r, ISO_A3_EH])
length(unique(dt_iso[ISO_N3_EH %in% v_r, ISO_A3_EH]))


# ships
rs <- rast(paste0("data/spatial/iso_map_", i, "_ships.tif"))

rs[is.na(rs)] <- 1

fname <- paste0("data/spatial/", i, "_iso_grids/iso_grid_SEA.tif")

writeRaster(rs, filename = fname, overwrite = TRUE)
