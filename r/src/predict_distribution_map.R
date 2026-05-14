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
