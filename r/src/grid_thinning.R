# ============================================================
# File: grid_thinning.R
# Purpose: Grid-based thinning (spatial rarefaction) for GBIF
# ============================================================

grid_thin_gbif <- function(
    df,
    grid_sizes_km = c(10, 25, 50),
    out_dir = NULL,
    prefix = "occ",
    seed = 42,
    max_per_cell = 1,
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
  
  # ---- input validation ----
  if (!is.data.frame(df)) stop("df must be a data.frame / tibble.")
  if (!all(c(lon_col, lat_col) %in% names(df))) {
    stop("Input df must contain columns: ", lon_col, " and ", lat_col)
  }
  if (!is.numeric(grid_sizes_km) || any(grid_sizes_km <= 0)) {
    stop("grid_sizes_km must be a numeric vector of positive km values.")
  }
  if (!is.numeric(max_per_cell) || max_per_cell < 1) {
    stop("max_per_cell must be >= 1.")
  }
  
  # ---- select columns (optional) ----
  if (!is.null(keep_cols)) {
    keep_cols <- unique(c(keep_cols, lon_col, lat_col))
    df <- df[, intersect(names(df), keep_cols), drop = FALSE]
  }
  
  # ---- basic coordinate cleaning ----
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
  if (!inherits(occ_sf, "sf")) stop("st_as_sf failed: occ_sf is not an sf object.")
  occ_sf$occ_id <- seq_len(nrow(occ_sf))
  
  # ---- project to equal-area (meters) ----
  occ_ea <- sf::st_transform(occ_sf, equal_area_crs)
  if (!inherits(occ_ea, "sf")) stop("st_transform failed: occ_ea is not an sf object.")
  
  # ---- thinning helper: grid over points extent ----
  thin_one <- function(points_ea, cell_km) {
    set.seed(seed)
    cell_m <- cell_km * 1000
    
    grid <- sf::st_make_grid(points_ea, cellsize = cell_m, square = TRUE)
    grid_sf <- sf::st_sf(cell_id = seq_along(grid), geometry = grid)
    
    joined <- sf::st_join(points_ea, grid_sf, join = sf::st_within, left = FALSE)
    if (nrow(joined) == 0) stop("No points could be assigned to grid cells. Check CRS/geometry.")
    
    # sample max_per_cell per cell (random)
    keep_ids <- joined |>
      sf::st_drop_geometry() |>
      dplyr::group_by(cell_id) |>
      dplyr::slice_sample(n = max_per_cell) |>
      dplyr::ungroup() |>
      dplyr::pull(occ_id)
    
    points_ea[points_ea$occ_id %in% keep_ids, ]
  }
  
  # ---- run thinning for all grid sizes ----
  thinned_ea <- lapply(grid_sizes_km, function(km) thin_one(occ_ea, km))
  names(thinned_ea) <- paste0("grid_", grid_sizes_km, "km")
  
  # ---- outputs in WGS84 data.frames ----
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
    
    for (nm in names(thinned_df)) {
      csv_path <- file.path(out_dir, paste0(prefix, "_", nm, ".csv"))
      utils::write.csv(thinned_df[[nm]], csv_path, row.names = FALSE)
    }
    
    utils::write.csv(
      summary_df,
      file.path(out_dir, paste0(prefix, "_grid_thinning_summary.csv")),
      row.names = FALSE
    )
  }
  
  list(
    summary = summary_df,
    thinned_sf = thinned_sf_wgs,
    thinned_df = thinned_df
  )
}