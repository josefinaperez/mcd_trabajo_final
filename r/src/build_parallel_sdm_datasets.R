# ============================================================
# File: build_parallel_sdm_datasets.R
# Purpose: Build parallel SDM datasets from workflow configs
# Initial BP method: random
# ============================================================

suppressPackageStartupMessages({
  library(sf)
  library(dplyr)
  library(terra)
  library(purrr)
  library(readr)
  library(tibble)
  library(stringr)
})

# ------------------------------------------------------------
# OPTIONAL: source your thinning function if you already saved it
# source("grid_thinning.R")
# ------------------------------------------------------------

# ============================================================
# 1) HELPERS
# ============================================================

ensure_dir <- function(path) {
  if (!dir.exists(path)) dir.create(path, recursive = TRUE, showWarnings = FALSE)
}

safe_slug <- function(x) {
  x |>
    tolower() |>
    str_replace_all("[^a-z0-9]+", "_") |>
    str_replace_all("^_|_$", "")
}

read_occurrences_csv <- function(path,
                                 lon_col = "decimalLongitude",
                                 lat_col = "decimalLatitude") {
  df <- readr::read_csv(path, show_col_types = FALSE)
  
  stopifnot(all(c(lon_col, lat_col) %in% names(df)))
  
  df <- df |>
    mutate(
      !!lon_col := as.numeric(.data[[lon_col]]),
      !!lat_col := as.numeric(.data[[lat_col]])
    ) |>
    filter(
      is.finite(.data[[lon_col]]),
      is.finite(.data[[lat_col]])
    )
  
  df
}

df_to_sf <- function(df,
                     lon_col = "decimalLongitude",
                     lat_col = "decimalLatitude",
                     crs = 4326) {
  st_as_sf(df, coords = c(lon_col, lat_col), crs = crs, remove = FALSE)
}

sample_random_background <- function(mask_raster,
                                     n = 10000,
                                     seed = 42,
                                     lon_col = "decimalLongitude",
                                     lat_col = "decimalLatitude") {
  set.seed(seed)
  
  # sample only from non-NA cells
  pts <- terra::spatSample(
    x = mask_raster,
    size = n,
    method = "random",
    na.rm = TRUE,
    as.points = TRUE,
    values = FALSE
  )
  
  if (is.null(pts) || nrow(pts) == 0) {
    stop("Could not generate random background points from mask raster.")
  }
  
  pts_df <- as.data.frame(terra::crds(pts))
  names(pts_df) <- c(lon_col, lat_col)
  
  pts_df
}

extract_env_values <- function(points_df,
                               env_rast,
                               lon_col = "decimalLongitude",
                               lat_col = "decimalLatitude") {
  stopifnot(all(c(lon_col, lat_col) %in% names(points_df)))
  
  v <- terra::vect(points_df[, c(lon_col, lat_col)], geom = c(lon_col, lat_col), crs = "EPSG:4326")
  vals <- terra::extract(env_rast, v)
  
  # remove terra ID column
  vals <- vals[, setdiff(names(vals), "ID"), drop = FALSE]
  
  bind_cols(points_df, vals)
}

make_model_ready_dataset <- function(dataset,
                                     predictor_cols,
                                     class_col = "class",
                                     lon_col = "decimalLongitude",
                                     lat_col = "decimalLatitude",
                                     keep_coords = TRUE) {
  cols <- c(class_col, predictor_cols)
  
  if (keep_coords) {
    cols <- c(lon_col, lat_col, cols)
  }
  
  cols_present <- intersect(cols, names(dataset))
  
  if (!(class_col %in% cols_present)) {
    stop("class column not found in dataset.")
  }
  
  dataset[, cols_present, drop = FALSE]
}

# ============================================================
# 2) SPATIAL BIAS CORRECTION METHODS
# ============================================================

apply_spatial_bias_method <- function(occ_df,
                                      method = "none",
                                      method_params = list(),
                                      lon_col = "decimalLongitude",
                                      lat_col = "decimalLatitude") {
  method <- tolower(method)
  
  if (method == "none") {
    return(occ_df)
  }
  
  if (method == "grid_thin") {
    if (!exists("grid_thin_gbif")) {
      stop("grid_thin_gbif() not found. Source your grid_thinning.R before running.")
    }
    
    grid_km <- method_params$grid_km %||% 25
    
    res <- grid_thin_gbif(
      df = occ_df,
      grid_sizes_km = c(grid_km),
      out_dir = NULL,
      prefix = "tmp",
      lon_col = lon_col,
      lat_col = lat_col
    )
    
    out_name <- paste0("grid_", grid_km, "km")
    return(res$thinned_df[[out_name]])
  }
  
  stop("Unknown spatial bias correction method: ", method)
}

# little helper for NULL default
`%||%` <- function(x, y) if (is.null(x)) y else x

# ============================================================
# 3) BACKGROUND-POINT METHODS
# ============================================================

### No tiene en cuenta shp de arg por ahora
### depende de los rasters

get_bp_n <- function(bp_n_strategy = "fixed",
                     n_presences,
                     fixed_bp_n = 10000L) {
  bp_n_strategy <- tolower(bp_n_strategy)
  
  if (bp_n_strategy == "fixed") {
    return(as.integer(fixed_bp_n))
  }
  
  if (bp_n_strategy == "match_presence") {
    return(as.integer(n_presences))
  }
  
  stop("Unknown bp_n_strategy: ", bp_n_strategy)
}


generate_background_points <- function(bp_method = "random",
                                       bp_params = list(),
                                       mask_raster,
                                       occ_df = NULL,
                                       lon_col = "decimalLongitude",
                                       lat_col = "decimalLatitude") {
  bp_method <- tolower(bp_method)
  
  if (bp_method == "random") {
    n <- bp_params$n %||% 10000
    seed <- bp_params$seed %||% 42
    
    return(sample_random_background(
      mask_raster = mask_raster,
      n = n,
      seed = seed,
      lon_col = lon_col,
      lat_col = lat_col
    ))
  }
  
  # placeholders for future methods
  if (bp_method == "spatially_constrained") {
    stop("spatially_constrained BP not implemented yet.")
  }
  
  if (bp_method == "environmentally_dissimilar") {
    stop("environmentally_dissimilar BP not implemented yet.")
  }
  
  if (bp_method == "three_step") {
    stop("three_step BP not implemented yet.")
  }
  
  stop("Unknown BP generation method: ", bp_method)
}


###### sample_random_background_from_shp
#### Provisorio para sacar puntos de un shp
#### Luego debería hacerse un híbrido, sacar puntos de la intersección
#### del shp de arg con los rasters de vars ambientales!!!

sample_random_background_from_shp <- function(shp_path,
                                              n = 10000,
                                              seed = 42,
                                              lon_col = "decimalLongitude",
                                              lat_col = "decimalLatitude") {
  set.seed(seed)
  
  # Leer shapefile
  study_area <- st_read(shp_path, quiet = TRUE)
  
  # Asegurar geometrías válidas
  study_area <- st_make_valid(study_area)
  
  # Unificar en una sola geometría por si hay varias partes
  study_area <- st_union(study_area)
  
  # Convertir a sf
  study_area <- st_as_sf(study_area)
  
  # Muestrear puntos aleatorios dentro del polígono
  pts <- st_sample(study_area, size = n, type = "random")
  
  if (length(pts) == 0) {
    stop("No se pudieron generar puntos aleatorios dentro del shapefile.")
  }
  
  pts_sf <- st_as_sf(pts)
  pts_sf <- st_set_crs(pts_sf, st_crs(study_area))
  
  # Pasar a lon/lat si hiciera falta
  pts_sf <- st_transform(pts_sf, 4326)
  
  coords <- st_coordinates(pts_sf)
  
  pts_df <- setNames(
    data.frame(coords[, 1], coords[, 2]),
    c(lon_col, lat_col)
  )
  
  pts_df
}

# ============================================================
# 4) BUILD ONE DATASET CONFIG
# ============================================================

build_one_sdm_dataset <- function(config_row,
                                  env_sets,
                                  occ_dir,
                                  out_dir,
                                  lon_col = "decimalLongitude",
                                  lat_col = "decimalLatitude",
                                  fixed_bp_n = 10000L,
                                  cols_to_keep = NA) {
  # config_row is one-row tibble/data.frame
  config_row <- as.list(config_row)
  
  species        <- config_row$species
  occ_file       <- config_row$occ_file
  bias_method    <- config_row$bias_method
  bias_param     <- config_row$bias_param
  bp_method      <- config_row$bp_method
  bp_n_strategy  <- config_row$bp_n_strategy
  env_set        <- config_row$env_set
  run_id         <- config_row$run_id
  
  # env set object
  env_info <- env_sets[[env_set]]
  if (is.null(env_info)) stop("env_set not found in env_sets: ", env_set)
  
  env_rast   <- terra::rast(env_info$files)
  mask_layer <- env_rast[[1]]  # use first layer as valid-area mask
  
  # paths
  run_dir <- file.path(out_dir, run_id)
  ensure_dir(run_dir)
  
  # 1) read occurrences
  occ_path <- file.path(occ_dir, occ_file)
  occ_df <- read_occurrences_csv(occ_path, lon_col = lon_col, lat_col = lat_col)
  
  # 2) bias correction
  occ_processed <- apply_spatial_bias_method(
    occ_df = occ_df,
    method = bias_method,
    method_params = list(grid_km = bias_param),
    lon_col = lon_col,
    lat_col = lat_col
  ) |>
    mutate(class = 1L)
  
  # 3) background generation
  
  n_presences <- nrow(occ_processed)
  
  bp_n <- get_bp_n(
    bp_n_strategy = bp_n_strategy,
    n_presences = n_presences,
    fixed_bp_n = fixed_bp_n
  )

  # bg_df <- generate_background_points(
  #   bp_method = bp_method,
  #   bp_params = list(n = bp_n, seed = 42),
  #   mask_raster = mask_layer,
  #   occ_df = occ_processed,
  #   lon_col = lon_col,
  #   lat_col = lat_col
  # ) |>
  #   mutate(class = 0L)
  
  bg_df <- sample_random_background_from_shp(
    shp_path = "data/shp/argentina/argentina.shp",
    n = bp_n,
    seed = 42,
    lon_col = lon_col,
    lat_col = lat_col
  ) |>
    mutate(class = 0L)
  
  # 4) extract env vars
  occ_env <- extract_env_values(
    points_df = occ_processed,
    env_rast = env_rast,
    lon_col = lon_col,
    lat_col = lat_col
  )
  
  bg_env <- extract_env_values(
    points_df = bg_df,
    env_rast = env_rast,
    lon_col = lon_col,
    lat_col = lat_col
  )
  
  # 5) merge dataset
  dataset <- bind_rows(occ_env, bg_env)
  
  # optional: drop rows with NA env values
  dataset <- dataset |> filter(if_all(all_of(names(env_rast)), ~ !is.na(.x)))
  
  # model-ready dataset: class + predictors (+ coords if desired)
  model_ready_dataset <- make_model_ready_dataset(
    dataset = dataset,
    predictor_cols = names(env_rast),
    class_col = "class",
    lon_col = lon_col,
    lat_col = lat_col,
    keep_coords = TRUE
  )
  
  readr::write_csv(occ_processed, file.path(run_dir, "occ_processed.csv"))
  readr::write_csv(bg_df,         file.path(run_dir, "background_points.csv"))
  readr::write_csv(dataset,       file.path(run_dir, "sdm_dataset.csv"))
  readr::write_csv(model_ready_dataset,       file.path(run_dir, "sdm_dataset_model_ready.csv"))
  
  # 7) return manifest row
  tibble(
    run_id = run_id,
    species = species,
    occ_file = occ_file,
    bias_method = bias_method,
    bias_param = bias_param,
    bp_method = bp_method,
    bp_n_strategy = bp_n_strategy,
    bp_n = bp_n,
    env_set = env_set,
    n_occ_after_bias = nrow(occ_processed),
    n_background = nrow(bg_df),
    n_final_rows = nrow(dataset),
    output_dir = run_dir
  )
}

# ============================================================
# 5) BUILD MANY CONFIGS IN PARALLEL (SEQUENTIALLY)
# ============================================================

build_parallel_sdm_datasets <- function(config_table,
                                        env_sets,
                                        occ_dir,
                                        out_dir = "outputs/sdm_datasets",
                                        lon_col = "decimalLongitude",
                                        lat_col = "decimalLatitude",
                                        fixed_bp_n = 10000L,
                                        cols_to_keep = NA) {
  ensure_dir(out_dir)
  
  manifest <- purrr::map_dfr(seq_len(nrow(config_table)), function(i) {
    message("Running config ", i, "/", nrow(config_table), " -> ", config_table$run_id[i])
    
    build_one_sdm_dataset(
      config_row = config_table[i, , drop = FALSE],
      env_sets = env_sets,
      occ_dir = occ_dir,
      out_dir = out_dir,
      lon_col = lon_col,
      lat_col = lat_col,
      fixed_bp_n = fixed_bp_n,
      cols_to_keep = cols_to_keep
    )
    
  })
  
  readr::write_csv(manifest, file.path(out_dir, "manifest.csv"))
  manifest
}

# ============================================================
# 6) CONFIG GENERATOR
# ============================================================
make_config_table <- function(species_table,
                              bp_methods = c("random"),
                              bp_n_strategies = c("fixed", "match_presence"),
                              fixed_bp_n = 10000L,
                              env_sets = c("bioclim_30s"),
                              grid_sizes_km = c(10, 25, 50)) {
  stopifnot(all(c("species", "occ_file") %in% names(species_table)))
  
  none_rows <- species_table |>
    tidyr::crossing(
      bias_method   = "none",
      bias_param    = NA_real_,
      bp_method     = bp_methods,
      bp_n_strategy = bp_n_strategies,
      fixed_bp_n    = fixed_bp_n,
      env_set       = env_sets
    )
  
  grid_rows <- species_table |>
    tidyr::crossing(
      bias_method   = "grid_thin",
      bias_param    = grid_sizes_km,
      bp_method     = bp_methods,
      bp_n_strategy = bp_n_strategies,
      fixed_bp_n    = fixed_bp_n,
      env_set       = env_sets
    )
  
  configs <- bind_rows(none_rows, grid_rows) |>
    mutate(
      bp_n_strategy = as.character(bp_n_strategy),
      bp_n_label = ifelse(
        bp_n_strategy == "fixed",
        paste0("fixed_", fixed_bp_n),
        bp_n_strategy
      ),
      run_id = paste(
        safe_slug(species),
        paste0("bias-", bias_method),
        ifelse(is.na(bias_param), "", paste0("p", bias_param)),
        paste0("bp-", bp_method),
        paste0("bpn-", bp_n_label),
        paste0("env-", env_set),
        sep = "__"
      ) |>
        stringr::str_replace_all("__+", "__") |>
        stringr::str_replace_all("__$", "")
    )
  
  configs
}
