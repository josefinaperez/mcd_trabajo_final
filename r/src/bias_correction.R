# ============================================================
# File: grid_thinning.R
# Purpose: Grid-based thinning (spatial rarefaction) for GBIF
# Author: (you)
# ============================================================

grid_thin_gbif <- function(
    df,
    grid_sizes_km = c(10, 25, 50),
    out_dir = NULL,
    prefix = "occ",
    clip_to_argentina = TRUE,
    seed = 42,
    max_per_cell = 1,
    grid_over = c("argentina", "points"),
    equal_area_crs = "+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 +datum=WGS84 +units=m +no_defs",
    lon_col = "decimalLongitude",
    lat_col = "decimalLatitude",
    keep_cols = NULL,          # NULL = keep all columns
    drop_dupe_coords = FALSE   # TRUE = remove duplicate lon/lat rows before thinning
) {
  # ---- dependencies ----
  req <- c("sf", "dplyr")
  missing <- req[!vapply(req, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing) > 0) {
    stop("Missing packages: ", paste(missing, collapse = ", "),
         ". Install them first (install.packages).")
  }
  
  grid_over <- match.arg(grid_over)
  
  if (!all(c(lon_col, lat_col) %in% names(df))) {
    stop("Input df must contain columns: ", lon_col, " and ", lat_col)
  }
  
  # ---- select columns (optional) ----
  if (!is.null(keep_cols)) {
    keep_cols <- unique(c(keep_cols, lon_col, lat_col))
    df <- df[, intersect(names(df), keep_cols), drop = FALSE]
  }
  
  # ---- basic cleaning of coordinates ----
  df2 <- df
  df2[[lon_col]] <- suppressWarnings(as.numeric(df2[[lon_col]]))
  df2[[lat_col]] <- suppressWarnings(as.numeric(df2[[lat_col]]))
  
  df2 <- df2[is.finite(df2[[lon_col]]) & is.finite(df2[[lat_col]]), , drop = FALSE]
  df2 <- df2[df2[[lon_col]] >= -180 & df2[[lon_col]] <= 180, , drop = FALSE]
  df2 <- df2[df2[[lat_col]] >= -90 & df2[[lat_col]] <= 90, , drop = FALSE]
  
  if (nrow(df2) == 0) stop("No valid coordinates after filtering.")
  
  if (drop_dupe_coords) {
    df2 <- df2[!duplicated(df2[, c(lon_col, lat_col)]), , drop = FALSE]
  }
  
  # ---- sf conversion (WGS84) ----
  occ_sf <- sf::st_as_sf(df2, coords = c(lon_col, lat_col), crs = 4326, remove = FALSE)
  occ_sf$occ_id <- seq_len(nrow(occ_sf))
  
  # ---- Argentina clip (optional) ----
  arg_sf <- NULL
  if (clip_to_argentina) {
    # require rnaturalearth only if clipping
    if (!requireNamespace("rnaturalearth", quietly = TRUE) ||
        !requireNamespace("rnaturalearthdata", quietly = TRUE)) {
      stop("To clip_to_argentina=TRUE, install packages: rnaturalearth, rnaturalearthdata")
    }
    
    arg_sf <- rnaturalearth::ne_countries(country = "Argentina", returnclass = "sf")
    arg_sf <- sf::st_make_valid(arg_sf)
    
    inside <- sf::st_within(occ_sf, arg_sf, sparse = FALSE)[, 1]
    occ_sf <- occ_sf[inside, ]
    if (nrow(occ_sf) == 0) stop("No points remain after clipping to Argentina.")
  }
  
  # ---- project to equal-area (meters) ----
  occ_ea <- sf::st_transform(occ_sf, equal_area_crs)
  arg_ea <- if (!is.null(arg_sf)) sf::st_transform(arg_sf, equal_area_crs) else NULL
  
  # ---- thinning helper ----
  thin_one <- function(points_ea, cell_km) {
    set.seed(seed)
    cell_m <- cell_km * 1000
    
    grid_geom <- if (grid_over == "argentina") {
      if (is.null(arg_ea)) stop("grid_over='argentina' requires clip_to_argentina=TRUE.")
      arg_ea
    } else {
      points_ea
    }
    
    grid <- sf::st_make_grid(grid_geom, cellsize = cell_m, square = TRUE)
    grid_sf <- sf::st_sf(cell_id = seq_along(grid), geometry = grid)
    
    joined <- sf::st_join(points_ea, grid_sf, join = sf::st_within, left = FALSE)
    if (nrow(joined) == 0) stop("No points could be assigned to grid cells. Check CRS/geometry.")
    
    # sample up to max_per_cell per cell
    keep_ids <- joined |>
      sf::st_drop_geometry() |>
      dplyr::group_by(cell_id) |>
      dplyr::slice_sample(n = min(max_per_cell, dplyr::n())) |>
      dplyr::ungroup() |>
      dplyr::pull(occ_id)
    
    points_ea[points_ea$occ_id %in% keep_ids, ]
  }
  
  # ---- run thinning for all grid sizes ----
  thinned_ea <- lapply(grid_sizes_km, function(km) thin_one(occ_ea, km))
  names(thinned_ea) <- paste0("grid_", grid_sizes_km, "km")
  
  # ---- build outputs in WGS84 data.frames ----
  thinned_sf_wgs <- lapply(thinned_ea, function(x) sf::st_transform(x, 4326))
  
  thinned_df <- lapply(thinned_sf_wgs, function(x) {
    coords <- sf::st_coordinates(x)
    out <- sf::st_drop_geometry(x)
    out[[lon_col]] <- coords[, 1]
    out[[lat_col]] <- coords[, 2]
    out
  })
  
  # ---- summary ----
  summary_df <- data.frame(
    grid_km = grid_sizes_km,
    n_points = vapply(thinned_df, nrow, integer(1)),
    stringsAsFactors = FALSE
  )
  
  # ---- write to disk (optional) ----
  if (!is.null(out_dir)) {
    if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
    
    # write each dataset
    for (nm in names(thinned_df)) {
      csv_path <- file.path(out_dir, paste0(prefix, "_", nm, ".csv"))
      utils::write.csv(thinned_df[[nm]], csv_path, row.names = FALSE)
    }
    
    # write summary
    utils::write.csv(
      summary_df,
      file.path(out_dir, paste0(prefix, "_grid_thinning_summary.csv")),
      row.names = FALSE
    )
  }
  
  # ---- return ----
  list(
    summary = summary_df,
    thinned_sf = thinned_sf_wgs,
    thinned_df = thinned_df
  )
}