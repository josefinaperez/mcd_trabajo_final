# ============================================================
# File: predict_distribution_map.R
# Purpose: Funciones puras para proyectar un modelo SDM
#          entrenado sobre la grilla WorldClim de Argentina,
#          binarizar la predicción con un umbral, generar el
#          panel cartográfico (continuo + binario) y persistir
#          los artefactos. Sin estado global; el orquestador
#          (predict_pipeline.R) compone estas funciones.
# ============================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tibble)
  library(ggplot2)
  library(terra)
  library(sf)
  library(tidyterra)
  library(patchwork)
  library(maxnet)   # registra predict.maxnet para terra::predict
})

WORLDCLIM_DIR  <- "data/features/worldclim/wc2.1_30s_bio"
ARGENTINA_SHP  <- "data/shp/argentina/argentina.shp"

# ------------------------------------------------------------
# 1) ENV STACK LOADING
# ------------------------------------------------------------
#
# Levanta los 19 rasters BIO de WorldClim, los apila en un
# SpatRaster, los recorta (crop) al bounding box de Argentina
# y los enmascara (mask) al polígono del país. Las capas se
# nombran por el basename del .tif (ej. "wc2.1_30s_bio_1"),
# lo que coincide exactamente con las columnas usadas por el
# modelo entrenado.
# ------------------------------------------------------------

load_env_stack <- function(worldclim_dir = WORLDCLIM_DIR,
                           shp_path      = ARGENTINA_SHP) {
  tif_paths <- list.files(worldclim_dir, pattern = "\\.tif$", full.names = TRUE)
  if (length(tif_paths) == 0) {
    stop("load_env_stack: no .tif files in ", worldclim_dir)
  }

  env <- terra::rast(tif_paths)
  ar  <- sf::st_read(shp_path, quiet = TRUE) |> sf::st_union()
  ar_v <- terra::vect(ar)

  env |>
    terra::crop(ar_v) |>
    terra::mask(ar_v)
}

# ------------------------------------------------------------
# 2) RASTER PREDICTION
# ------------------------------------------------------------
#
# Proyecta un modelo maxnet sobre el SpatRaster apilado.
# Usa terra::predict con un wrapper que invoca el predict.maxnet
# subyacente con type = "cloglog" y clamp = TRUE (idéntico al
# usado en train_maxent.R para garantizar que las idoneidades
# del mapa sean comparables con las del set de prueba).
# Devuelve un SpatRaster mono-capa llamado "suitability".
# ------------------------------------------------------------

predict_suitability_raster <- function(model, env_stack) {
  predict_chunk <- function(model, data, ...) {
    predict(model, newdata = as.data.frame(data),
            type = "cloglog", clamp = TRUE)
  }

  out <- terra::predict(
    object = env_stack,
    model  = model,
    fun    = predict_chunk,
    na.rm  = TRUE
  )
  names(out) <- "suitability"
  out
}

# ------------------------------------------------------------
# 3) BINARIZATION
# ------------------------------------------------------------
#
# Aplica el umbral tau (en unidades de score cloglog) a un
# raster continuo de idoneidad. Píxeles con score >= tau → 1
# ("apto"), resto → 0 ("no apto"); los NA se preservan como
# NA. El tau se lee del metrics.csv del run (columna
# threshold_max_tss = τ* de Youden, consistente con §4.5
# del documento metodológico).
# ------------------------------------------------------------

binarize_raster <- function(suit_r, threshold) {
  stopifnot(is.numeric(threshold), length(threshold) == 1L)
  out <- terra::classify(
    suit_r,
    rcl = matrix(c(-Inf, threshold,        0,
                   threshold,        Inf,  1),
                 ncol = 3, byrow = TRUE),
    include.lowest = FALSE,
    right = FALSE
  )
  names(out) <- "binary_presence"
  out
}
