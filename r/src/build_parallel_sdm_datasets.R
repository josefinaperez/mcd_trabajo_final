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

# Artefactos que un dataset-run completo deja en su run_dir. Si están los
# cuatro, el run se considera ya construido (idempotencia).
DATASET_ARTIFACTS <- c("occ_processed.csv", "background_points.csv",
                       "sdm_dataset.csv", "sdm_dataset_model_ready.csv")
dataset_artifacts_exist <- function(run_dir) {
  all(file.exists(file.path(run_dir, DATASET_ARTIFACTS)))
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
                                     lat_col = "decimalLatitude",
                                     max_attempts = 8) {
  set.seed(seed)

  # spatSample(na.rm = TRUE) puede devolver menos de `size` porque descarta
  # las celdas NA del bounding box (todo lo externo a Argentina + huecos de
  # validez). Oversampleamos y acumulamos hasta juntar n celdas válidas únicas.
  have    <- NULL
  attempt <- 0L
  while ((is.null(have) || nrow(have) < n) && attempt < max_attempts) {
    attempt <- attempt + 1L
    pts <- terra::spatSample(
      x = mask_raster, size = n * 3L, method = "random",
      na.rm = TRUE, as.points = TRUE, values = FALSE
    )
    if (!is.null(pts) && nrow(pts) > 0) {
      d    <- as.data.frame(terra::crds(pts))
      have <- if (is.null(have)) d else unique(rbind(have, d))
    }
  }

  if (is.null(have) || nrow(have) == 0) {
    stop("Could not generate random background points from mask raster.")
  }
  if (nrow(have) < n) {
    warning(sprintf("sample_random_background: solo %d celdas válidas únicas (< n = %d).",
                    nrow(have), n))
  }

  pts_df <- have[seq_len(min(n, nrow(have))), , drop = FALSE]
  names(pts_df) <- c(lon_col, lat_col)
  rownames(pts_df) <- NULL
  pts_df
}

# #48: background target-group (Phillips et al. 2009). En vez de muestrear el
# fondo de forma uniforme, lo muestrea con probabilidad ∝ una superficie de
# sesgo (weight_raster), de modo que el background comparta el sesgo espacial
# de muestreo de las presencias y éste se cancele en el contraste
# presencia/background. Muestrea centros de celda SIN reemplazo con prob ∝ peso,
# sobre las celdas válidas de la máscara (no-NA). El weight_raster se alinea a la
# grilla de la máscara si hace falta.
sample_weighted_background <- function(mask_raster,
                                       weight_raster,
                                       n = 10000,
                                       seed = 42,
                                       lon_col = "decimalLongitude",
                                       lat_col = "decimalLatitude") {
  set.seed(seed)

  if (!terra::compareGeom(weight_raster, mask_raster, stopOnError = FALSE)) {
    weight_raster <- terra::resample(weight_raster, mask_raster, method = "bilinear")
  }

  mask1 <- mask_raster[[1]]
  valid <- which(!is.na(terra::values(mask1, mat = FALSE)))
  if (length(valid) == 0) stop("sample_weighted_background: máscara sin celdas válidas.")

  w  <- terra::values(weight_raster, mat = FALSE)[valid]
  ok <- is.finite(w) & w > 0
  cells <- valid[ok]
  w     <- w[ok]
  if (length(cells) == 0) {
    stop("sample_weighted_background: ninguna celda válida con peso > 0.")
  }
  if (length(cells) < n) {
    warning(sprintf("sample_weighted_background: solo %d celdas con peso > 0 (< n = %d).",
                    length(cells), n))
  }

  size <- min(n, length(cells))
  idx  <- sample(cells, size = size, replace = FALSE, prob = w / sum(w))
  xy   <- terra::xyFromCell(mask1, idx)

  pts_df <- as.data.frame(xy)
  names(pts_df) <- c(lon_col, lat_col)
  rownames(pts_df) <- NULL
  pts_df
}

# #52/#54: helper puro — máscara con un buffer de exclusión geográfica de radio
# buffer_km alrededor de las presencias (celdas dentro del buffer -> NA). El
# buffer se calcula en AEA (km). Lo reusan sample_spatially_constrained_background
# (#52) y sample_three_step_background (#54).
build_exclusion_mask <- function(mask_raster, occ_df, buffer_km = 20,
                                 lon_col = "decimalLongitude",
                                 lat_col = "decimalLatitude") {
  aea <- "+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 +datum=WGS84 +units=m +no_defs"

  occ_v   <- terra::vect(occ_df[, c(lon_col, lat_col)],
                         geom = c(lon_col, lat_col), crs = "EPSG:4326")
  occ_aea <- terra::project(occ_v, aea)
  buf_aea <- terra::buffer(occ_aea, width = buffer_km * 1000)        # metros
  buf     <- terra::aggregate(terra::project(buf_aea, terra::crs(mask_raster)))

  terra::mask(mask_raster, buf, inverse = TRUE, updatevalue = NA)
}

# #52: background "spatially constrained" (Miyaji estrategia ii; Lobo et al.
# 2010). Buffer de exclusión geográfica (build_exclusion_mask) + muestreo
# aleatorio FUERA del buffer (hereda oversampling y degradación elegante).
sample_spatially_constrained_background <- function(mask_raster,
                                                    occ_df,
                                                    buffer_km = 20,
                                                    n = 10000,
                                                    seed = 42,
                                                    lon_col = "decimalLongitude",
                                                    lat_col = "decimalLatitude") {
  mask_excl <- build_exclusion_mask(mask_raster, occ_df, buffer_km, lon_col, lat_col)

  sample_random_background(
    mask_raster = mask_excl,
    n = n, seed = seed,
    lon_col = lon_col, lat_col = lat_col
  )
}

# #53/#54: helper puro — One-Class SVM (Schölkopf et al. 2001) sobre el env de
# las presencias; devuelve los ÍNDICES DE CELDA outlier (fuera del envelope del
# nicho = ambientalmente disímiles). nu es la cota superior de la fracción de
# presencias rechazadas: nu chico (0.1) = envelope permisivo (casi todas las
# presencias adentro), de modo que solo se etiquetan como disímiles las celdas
# claramente fuera del nicho. Lo reusan sample_environmentally_dissimilar_background
# (#53) y sample_three_step_background (#54). scale=TRUE guarda el centrado/escala
# del entrenamiento y lo reaplica en predict().
ocsvm_dissimilar_cells <- function(env_rast, occ_df,
                                   nu = 0.1, gamma = NULL,
                                   lon_col = "decimalLongitude",
                                   lat_col = "decimalLatitude") {
  if (!requireNamespace("e1071", quietly = TRUE)) {
    stop("environmentally_dissimilar: falta el paquete 'e1071' (OCSVM). ",
         "Instalar con install.packages('e1071').")
  }
  var_names <- names(env_rast)

  # env en las presencias -> matriz sin NA (define el envelope del nicho)
  occ_v <- terra::vect(occ_df[, c(lon_col, lat_col)],
                       geom = c(lon_col, lat_col), crs = "EPSG:4326")
  pres  <- terra::extract(env_rast, occ_v)
  pres  <- pres[, var_names, drop = FALSE]
  pres  <- pres[stats::complete.cases(pres), , drop = FALSE]
  if (nrow(pres) < 2) {
    stop("environmentally_dissimilar: muy pocas presencias con env completo para entrenar el OCSVM.")
  }
  pres_mat <- as.matrix(pres)
  if (is.null(gamma)) gamma <- 1 / ncol(pres_mat)   # default RBF sobre datos escalados

  ocsvm <- e1071::svm(x = pres_mat, type = "one-classification",
                      kernel = "radial", nu = nu, gamma = gamma, scale = TRUE)

  all_vals <- terra::values(env_rast, mat = TRUE)
  valid    <- which(stats::complete.cases(all_vals))
  if (length(valid) == 0) stop("environmentally_dissimilar: el stack no tiene celdas válidas.")
  pred <- predict(ocsvm, all_vals[valid, var_names, drop = FALSE])

  valid[!pred]   # outliers = fuera del envelope = disímiles
}

# #53: background "environmentally dissimilar" (Miyaji estrategia iii; Chefaoui &
# Lobo 2008). Muestrea uniformemente entre las celdas disímiles (outliers del
# OCSVM, ocsvm_dissimilar_cells).
sample_environmentally_dissimilar_background <- function(env_rast, occ_df,
                                                         nu = 0.1, gamma = NULL,
                                                         n = 10000, seed = 42,
                                                         lon_col = "decimalLongitude",
                                                         lat_col = "decimalLatitude") {
  set.seed(seed)
  cand <- ocsvm_dissimilar_cells(env_rast, occ_df, nu = nu, gamma = gamma,
                                 lon_col = lon_col, lat_col = lat_col)
  if (length(cand) == 0) {
    stop("environmentally_dissimilar: ninguna celda quedó fuera del envelope ",
         "(probá un nu mayor para ceñir el envelope al núcleo del nicho).")
  }
  if (length(cand) < n) {
    warning(sprintf("sample_environmentally_dissimilar_background: solo %d celdas disímiles (< n = %d).",
                    length(cand), n))
  }
  size <- min(n, length(cand))
  idx  <- sample(cand, size = size, replace = FALSE)
  xy   <- terra::xyFromCell(env_rast[[1]], idx)

  pts_df <- as.data.frame(xy)
  names(pts_df) <- c(lon_col, lat_col)
  rownames(pts_df) <- NULL
  pts_df
}

# #54: reparte `size` muestras entre los clusters de K-means de la forma más
# UNIFORME posible (round-robin: cada ronda suma 1 a cada cluster con cupo),
# capando por el tamaño de cada cluster y redistribuyendo el déficit a los que
# todavía tienen celdas. Devuelve los índices de celda muestreados.
allocate_uniform_per_cluster <- function(cells, clusters, size) {
  ids   <- sort(unique(clusters))
  by_cl <- lapply(ids, function(c) cells[clusters == c])
  avail <- vapply(by_cl, length, integer(1))
  quota <- integer(length(ids))

  remaining <- size
  repeat {
    open <- which(quota < avail)
    if (remaining <= 0L || length(open) == 0L) break
    take <- min(remaining, length(open))
    quota[open[seq_len(take)]] <- quota[open[seq_len(take)]] + 1L
    remaining <- remaining - take
  }

  # muestreo dentro de cada cluster (guarda contra el bug de sample() con length-1)
  safe_sample <- function(x, q) if (q <= 0L) integer(0) else if (length(x) == 1L) x else sample(x, q)
  unlist(Map(safe_sample, by_cl, quota), use.names = FALSE)
}

# #54: estrategia (iv) Three-Step (Senay, Worner & Ikeda 2013). Compone el buffer
# espacial (#52) + el envelope OCSVM (#53) + K-means: el pool candidato es
# (fuera del buffer) ∩ (outliers OCSVM); K-means estratifica ese pool en el
# espacio ambiental y se muestrea ~n/k por cluster (cobertura uniforme del
# espacio disímil → menos redundancia/autocorrelación del background).
sample_three_step_background <- function(env_rast, occ_df,
                                         buffer_km = 20, nu = 0.1, gamma = NULL,
                                         k = 10, n = 10000, seed = 42,
                                         lon_col = "decimalLongitude",
                                         lat_col = "decimalLatitude") {
  set.seed(seed)
  var_names <- names(env_rast)

  # 1) espacial: celdas fuera del buffer de exclusión
  mask_excl <- build_exclusion_mask(env_rast[[1]], occ_df, buffer_km, lon_col, lat_col)
  outside   <- which(!is.na(terra::values(mask_excl, mat = FALSE)))

  # 2) ambiental: celdas outlier del OCSVM
  outliers <- ocsvm_dissimilar_cells(env_rast, occ_df, nu = nu, gamma = gamma,
                                     lon_col = lon_col, lat_col = lat_col)

  # 3) intersección
  cand <- intersect(outside, outliers)
  if (length(cand) == 0) {
    stop("three_step: pool vacío (fuera del buffer ∩ disímiles). ",
         "Probá un buffer menor o un nu mayor.")
  }
  if (length(cand) < n) {
    warning(sprintf("sample_three_step_background: solo %d celdas en el pool (< n = %d).",
                    length(cand), n))
  }
  size <- min(n, length(cand))

  # 4) clustering en el espacio ambiental escalado del pool
  env_mat <- terra::values(env_rast, mat = TRUE)[cand, var_names, drop = FALSE]
  env_sc  <- scale(env_mat)
  env_sc[is.na(env_sc)] <- 0          # columnas de varianza cero -> 0 (no aportan)
  k_eff <- max(1L, min(as.integer(k), length(cand)))
  km    <- stats::kmeans(env_sc, centers = k_eff)

  idx <- allocate_uniform_per_cluster(cand, km$cluster, size)

  xy  <- terra::xyFromCell(env_rast[[1]], idx)
  pts_df <- as.data.frame(xy)
  names(pts_df) <- c(lon_col, lat_col)
  rownames(pts_df) <- NULL
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

# #48: superficie de accesibilidad por defecto para el background target-group
# (inverso de travel_time). Es la capa antrópica armonizada de #47.
TRAVEL_TIME_PATH <- "data/features/env_2.5m_ar/anthropic/travel_time.tif"

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
                                       env_rast = NULL,
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
  
  # #48: background target-group ponderado por accesibilidad (inverso de
  # travel_time). El peso decae exponencialmente con el tiempo de viaje:
  # w = exp(-tt / tau). tau (escala de decaimiento) se calibra con la MEDIANA
  # del travel_time en las presencias, de modo que el background imite la
  # accesibilidad observada del muestreo. Así el sesgo de accesibilidad queda
  # igual en presencia y fondo, y se cancela en el contraste.
  if (bp_method == "bias_weighted") {
    n     <- bp_params$n %||% 10000
    seed  <- bp_params$seed %||% 42
    wpath <- bp_params$weight_raster_path %||% TRAVEL_TIME_PATH
    if (!file.exists(wpath)) {
      stop("bias_weighted: falta la capa de accesibilidad (", wpath,
           "). Preparar las capas antrópicas (#47) primero.")
    }
    tt <- terra::rast(wpath)

    tau <- bp_params$tau
    if (is.null(tau)) {
      if (is.null(occ_df)) stop("bias_weighted: se requiere occ_df para calibrar tau.")
      occ_v <- terra::vect(occ_df[, c(lon_col, lat_col)],
                           geom = c(lon_col, lat_col), crs = "EPSG:4326")
      pv  <- terra::extract(tt, occ_v)[[2]]
      tau <- stats::median(pv, na.rm = TRUE)
      if (!is.finite(tau) || tau <= 0) {
        tau <- stats::median(terra::values(tt, mat = FALSE), na.rm = TRUE)
      }
    }
    message(sprintf("    bias_weighted: tau (mediana tt presencias) = %.1f", tau))
    weight_r <- exp(-tt / tau)

    return(sample_weighted_background(
      mask_raster   = mask_raster,
      weight_raster = weight_r,
      n             = n,
      seed          = seed,
      lon_col       = lon_col,
      lat_col       = lat_col
    ))
  }

  # #52: estrategia (ii) Spatially Constrained. Buffer de exclusión geográfica
  # (buffer_km, default 20) alrededor de las presencias; PA fuera del buffer.
  if (bp_method == "spatially_constrained") {
    if (is.null(occ_df)) {
      stop("spatially_constrained: requiere occ_df para construir el buffer.")
    }
    buffer_km <- bp_params$buffer_km %||% 20
    n    <- bp_params$n %||% 10000
    seed <- bp_params$seed %||% 42
    return(sample_spatially_constrained_background(
      mask_raster = mask_raster,
      occ_df = occ_df,
      buffer_km = buffer_km,
      n = n, seed = seed,
      lon_col = lon_col, lat_col = lat_col
    ))
  }
  
  # #53: estrategia (iii) Environmentally Dissimilar. OCSVM sobre el env de las
  # presencias; PA en las celdas que quedan fuera del envelope del nicho. Requiere
  # el stack del env_set (env_rast), no solo la máscara de validez.
  if (bp_method == "environmentally_dissimilar") {
    if (is.null(env_rast)) {
      stop("environmentally_dissimilar: requiere env_rast (stack del env_set).")
    }
    if (is.null(occ_df)) {
      stop("environmentally_dissimilar: requiere occ_df para definir el nicho.")
    }
    nu    <- bp_params$nu    %||% 0.1
    gamma <- bp_params$gamma %||% NULL
    n     <- bp_params$n     %||% 10000
    seed  <- bp_params$seed  %||% 42
    return(sample_environmentally_dissimilar_background(
      env_rast = env_rast,
      occ_df   = occ_df,
      nu = nu, gamma = gamma,
      n = n, seed = seed,
      lon_col = lon_col, lat_col = lat_col
    ))
  }

  # #54: estrategia (iv) Three-Step. Buffer (#52) + OCSVM (#53) + K-means.
  # Requiere el stack del env_set (env_rast) y las presencias (occ_df).
  if (bp_method == "three_step") {
    if (is.null(env_rast)) {
      stop("three_step: requiere env_rast (stack del env_set).")
    }
    if (is.null(occ_df)) {
      stop("three_step: requiere occ_df para el buffer y el OCSVM.")
    }
    buffer_km <- bp_params$buffer_km %||% 20
    nu        <- bp_params$nu        %||% 0.1
    gamma     <- bp_params$gamma     %||% NULL
    k         <- bp_params$k         %||% 10
    n         <- bp_params$n         %||% 10000
    seed      <- bp_params$seed      %||% 42
    return(sample_three_step_background(
      env_rast = env_rast, occ_df = occ_df,
      buffer_km = buffer_km, nu = nu, gamma = gamma, k = k,
      n = n, seed = seed,
      lon_col = lon_col, lat_col = lat_col
    ))
  }
  
  stop("Unknown BP generation method: ", bp_method)
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
                                  cols_to_keep = NA,
                                  force = FALSE) {
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

  # paths
  run_dir <- file.path(out_dir, run_id)
  ensure_dir(run_dir)

  # Idempotencia: si el run ya está construido, reconstruir la fila de manifest
  # desde disco (counts + bp_n recomputado) y devolver sin recomputar nada.
  if (!force && dataset_artifacts_exist(run_dir)) {
    message("  skip build (ya existe): ", run_id)
    occ_n <- nrow(readr::read_csv(file.path(run_dir, "occ_processed.csv"), show_col_types = FALSE))
    bg_n  <- nrow(readr::read_csv(file.path(run_dir, "background_points.csv"), show_col_types = FALSE))
    ds_n  <- nrow(readr::read_csv(file.path(run_dir, "sdm_dataset.csv"), show_col_types = FALSE))
    return(tibble(
      run_id = run_id, species = species, occ_file = occ_file,
      bias_method = bias_method, bias_param = bias_param,
      bp_method = bp_method, bp_n_strategy = bp_n_strategy,
      bp_n = get_bp_n(bp_n_strategy, n_presences = occ_n, fixed_bp_n = fixed_bp_n),
      env_set = env_set,
      n_occ_after_bias = occ_n, n_background = bg_n, n_final_rows = ds_n,
      output_dir = run_dir
    ))
  }

  env_rast   <- terra::rast(env_info$files)
  # #37: máscara de validez = celdas con dato en TODAS las capas del env_set.
  # Como los rásters de env_2.5m_ar/ ya vienen recortados a Argentina, esto
  # equivale a la intersección validez-ráster ∩ polígono del país. sum()
  # propaga NA: una celda con NA en cualquier capa queda NA y no se muestrea
  # como background (evita filas que luego se descartarían). La intersección
  # es por env_set, de modo que cada conjunto muestrea su dominio real (p. ej.
  # las capas de suelo, con más NA, restringen el fondo de los env_set de suelo).
  valid_mask <- sum(env_rast)

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

  bg_df <- generate_background_points(
    bp_method = bp_method,
    bp_params = list(n = bp_n, seed = 42),
    mask_raster = valid_mask,
    occ_df = occ_processed,
    env_rast = env_rast,
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
                                        cols_to_keep = NA,
                                        force = FALSE) {
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
      cols_to_keep = cols_to_keep,
      force = force
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
                              env_sets = c("bioclim"),
                              grid_sizes_km = c(10, 25, 50),
                              bias_weighted_env_sets = character(0),
                              spatially_constrained_env_sets = character(0),
                              environmentally_dissimilar_env_sets = character(0),
                              three_step_env_sets = character(0)) {
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

  # #48: background target-group (bias_weighted) SOLO con bias_method=none y
  # solo sobre los env_sets ecológicos indicados (sin antrópicos). Se compara
  # contra el background aleatorio de none_rows sobre los mismos env_sets.
  bw_rows <- if (length(bias_weighted_env_sets) > 0) {
    species_table |>
      tidyr::crossing(
        bias_method   = "none",
        bias_param    = NA_real_,
        bp_method     = "bias_weighted",
        bp_n_strategy = bp_n_strategies,
        fixed_bp_n    = fixed_bp_n,
        env_set       = bias_weighted_env_sets
      )
  } else {
    NULL
  }

  # #52: background spatially_constrained SOLO con bias_method=none, sobre los
  # env_sets ecológicos indicados. Se compara contra el background aleatorio
  # (none_rows) sobre los mismos env_sets. buffer_km queda como constante en el
  # sampler (no entra al run_id porque usamos un único radio).
  sc_rows <- if (length(spatially_constrained_env_sets) > 0) {
    species_table |>
      tidyr::crossing(
        bias_method   = "none",
        bias_param    = NA_real_,
        bp_method     = "spatially_constrained",
        bp_n_strategy = bp_n_strategies,
        fixed_bp_n    = fixed_bp_n,
        env_set       = spatially_constrained_env_sets
      )
  } else {
    NULL
  }

  # #53: background environmentally_dissimilar SOLO con bias_method=none, sobre
  # los env_sets ecológicos indicados. Se compara contra el background aleatorio
  # (none_rows) sobre los mismos env_sets. nu/gamma quedan como defaults en el
  # sampler (no entran al run_id porque usamos un único nu).
  ed_rows <- if (length(environmentally_dissimilar_env_sets) > 0) {
    species_table |>
      tidyr::crossing(
        bias_method   = "none",
        bias_param    = NA_real_,
        bp_method     = "environmentally_dissimilar",
        bp_n_strategy = bp_n_strategies,
        fixed_bp_n    = fixed_bp_n,
        env_set       = environmentally_dissimilar_env_sets
      )
  } else {
    NULL
  }

  # #54: background three_step SOLO con bias_method=none, sobre los env_sets
  # ecológicos indicados. Compone buffer+OCSVM+K-means; se compara contra el
  # background aleatorio (none_rows) sobre los mismos env_sets.
  ts_rows <- if (length(three_step_env_sets) > 0) {
    species_table |>
      tidyr::crossing(
        bias_method   = "none",
        bias_param    = NA_real_,
        bp_method     = "three_step",
        bp_n_strategy = bp_n_strategies,
        fixed_bp_n    = fixed_bp_n,
        env_set       = three_step_env_sets
      )
  } else {
    NULL
  }

  configs <- bind_rows(none_rows, grid_rows, bw_rows, sc_rows, ed_rows, ts_rows) |>
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
