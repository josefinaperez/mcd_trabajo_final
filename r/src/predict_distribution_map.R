# ============================================================
# File: predict_distribution_map.R
# Purpose: Funciones puras para proyectar un modelo SDM
#          entrenado sobre la grilla WorldClim de Argentina,
#          binarizar la predicción con un umbral, generar el
#          panel cartográfico (continuo + binario) y persistir
#          los artefactos. Sin estado global; el orquestador
#          (predict_pipeline.R) compone estas funciones.
# ============================================================

source("r/src/xai_predict.R")  # make_predict_fn (score de presencia [0,1], agnóstico)

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
# Proyecta un modelo SDM sobre el SpatRaster apilado. La
# predicción se delega en make_predict_fn() (xai_predict.R), que
# despacha por algoritmo y normaliza a score de presencia [0,1]
# — la misma función usada en el entrenamiento, de modo que las
# idoneidades del mapa quedan en idéntica escala que el umbral
# τ* del set de prueba para cualquier algoritmo (maxnet/ranger/
# xgboost). Devuelve un SpatRaster mono-capa llamado "suitability".
# ------------------------------------------------------------

predict_suitability_raster <- function(model, env_stack) {
  pf <- make_predict_fn(model)
  predict_chunk <- function(model, data, ...) {
    pf(model, as.data.frame(data))
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
# Aplica el umbral tau (en unidades del score de presencia
# [0,1], agnóstico al algoritmo) a un raster continuo de idoneidad. Píxeles con score >= tau → 1
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

# ------------------------------------------------------------
# 4) PANEL CARTOGRÁFICO
# ------------------------------------------------------------
#
# Compone dos paneles lado a lado: idoneidad continua con
# escala viridis y mapa binario en τ*. Sobre ambos se
# superponen las presencias GBIF del run (alpha bajo). Se
# usa tidyterra::geom_spatraster para que los SpatRaster
# se integren limpiamente con ggplot.
# ------------------------------------------------------------

plot_map_panel <- function(suit_r, bin_r, run_id, threshold, occ_points) {
  stopifnot(
    inherits(suit_r, "SpatRaster"),
    inherits(bin_r,  "SpatRaster"),
    all(c("decimalLongitude", "decimalLatitude") %in% names(occ_points))
  )

  p_cont <- ggplot() +
    tidyterra::geom_spatraster(data = suit_r) +
    scale_fill_viridis_c(
      name   = "Idoneidad\n[0–1]",
      limits = c(0, 1),
      na.value = "transparent"
    ) +
    geom_point(
      data = occ_points,
      aes(x = decimalLongitude, y = decimalLatitude),
      colour = "white", fill = "black",
      shape = 21, size = 0.6, stroke = 0.2, alpha = 0.55
    ) +
    coord_sf(crs = 4326) +
    labs(title = "Idoneidad ambiental (continua)") +
    theme_minimal(base_size = 10) +
    theme(legend.position = "right")

  p_bin <- ggplot() +
    tidyterra::geom_spatraster(data = as.factor(bin_r)) +
    scale_fill_manual(
      name   = paste0("Presencia\n(τ* = ", sprintf("%.3f", threshold), ")"),
      values = c("0" = "#e5e5e5", "1" = "#2c7a3e"),
      labels = c("0" = "no apto", "1" = "apto"),
      na.value = "transparent"
    ) +
    geom_point(
      data = occ_points,
      aes(x = decimalLongitude, y = decimalLatitude),
      colour = "white", fill = "black",
      shape = 21, size = 0.6, stroke = 0.2, alpha = 0.55
    ) +
    coord_sf(crs = 4326) +
    labs(title = "Presencia / no apto (binario)") +
    theme_minimal(base_size = 10) +
    theme(legend.position = "right")

  (p_cont | p_bin) +
    patchwork::plot_annotation(
      title    = paste0("Distribución potencial — ", run_id),
      subtitle = "Predicción MaxEnt sobre Argentina (WorldClim 30 arc-sec, EPSG:4326)"
    )
}

# ------------------------------------------------------------
# 5) PERSIST ARTIFACTS
# ------------------------------------------------------------
#
# Persiste los 3 artefactos cartográficos en run_dir:
#   - suitability.tif       : raster continuo (DEFLATE)
#   - binary_presence.tif   : raster 0/1 (DEFLATE)
#   - map_panel.png         : figura ggplot 1600x900
# Devuelve invisible() la ruta del run_dir.
# ------------------------------------------------------------

save_map_artifacts <- function(run_dir, suit_r, bin_r, fig) {
  dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)

  terra::writeRaster(
    suit_r,
    file.path(run_dir, "suitability.tif"),
    overwrite = TRUE,
    gdal = c("COMPRESS=DEFLATE", "PREDICTOR=2")
  )
  terra::writeRaster(
    bin_r,
    file.path(run_dir, "binary_presence.tif"),
    overwrite = TRUE,
    datatype  = "INT1U",
    gdal      = c("COMPRESS=DEFLATE")
  )
  ggplot2::ggsave(
    filename = file.path(run_dir, "map_panel.png"),
    plot     = fig,
    width    = 14, height = 7, dpi = 110
  )

  invisible(run_dir)
}
