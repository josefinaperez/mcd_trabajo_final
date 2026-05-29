# ============================================================
# File: r/src/prepare_env_layers.R
# Purpose: Construye la grilla de referencia 2.5 arc-min recortada
#          a Argentina (#24) y las capas de vegetación alineadas
#          (#18). Modular y a prueba de descargas parciales: la
#          grilla se arma siempre; la vegetación solo si están TODOS
#          sus insumos crudos. Escribe a data/features/env_2.5m_ar/.
# Ejecutar desde repo root:
#   source("r/src/prepare_env_layers.R")
# ============================================================

suppressPackageStartupMessages(library(terra))
source("r/src/env_layers.R")

SHP_PATH    <- "data/shp/argentina/argentina.shp"
BIO_RAW_DIR <- "data/features/worldclim/wc2.1_2.5m_bio"
WC_DIR      <- "data/features/vegetation_raw/worldcover"
NDVI_DIR    <- "data/features/vegetation_raw/modis_ndvi"
CANOPY_PATH <- "data/features/vegetation_raw/inta_forest/canopy_height.tif"
OUT_BIO     <- "data/features/env_2.5m_ar/bioclim"
OUT_VEG     <- "data/features/env_2.5m_ar/vegetation"

# ---- #24: grilla de referencia (WorldClim 2.5m recortado a Argentina) ----
# Devuelve el template (capa de referencia para alinear todo lo demás).
build_reference_grid <- function() {
  dir.create(OUT_BIO, recursive = TRUE, showWarnings = FALSE)
  ar <- terra::vect(SHP_PATH)
  bio_raw <- terra::rast(list.files(BIO_RAW_DIR, pattern = "tif$", full.names = TRUE))
  bio_ar <- crop_mask_to_region(bio_raw, ar)
  terra::writeRaster(bio_ar, file.path(OUT_BIO, paste0(names(bio_ar), ".tif")),
                     overwrite = TRUE)
  message(sprintf("grilla de referencia: %d capas escritas en %s", terra::nlyr(bio_ar), OUT_BIO))
  bio_ar[[1]]
}

# ---- #18: capas de vegetación alineadas al template ----
build_vegetation_layers <- function(template) {
  dir.create(OUT_VEG, recursive = TRUE, showWarnings = FALSE)
  ar <- terra::vect(SHP_PATH)

  # tree cover % (ESA WorldCover, clase árbol = 10)
  # Mosaico virtual (VRT) de los tiles: distintos extents no se pueden apilar
  # con rast(); el VRT los une sin copiar datos y resample los lee por bloques.
  wc <- terra::vrt(list.files(WC_DIR, pattern = "tif$", full.names = TRUE),
                   filename = tempfile(fileext = ".vrt"), overwrite = TRUE)
  tree_pct <- crop_mask_to_region(cover_fraction(wc, target_class = 10, template = template), ar)
  names(tree_pct) <- "tree_cover_pct"
  terra::writeRaster(tree_pct, file.path(OUT_VEG, "tree_cover_pct.tif"), overwrite = TRUE)

  # NDVI medio y estacionalidad (MODIS MOD13A3)
  ndvi_stack <- terra::rast(list.files(NDVI_DIR, pattern = "tif$", full.names = TRUE))
  ndvi_mean <- crop_mask_to_region(align_to_template(temporal_mean(ndvi_stack), template), ar)
  ndvi_seas <- crop_mask_to_region(align_to_template(temporal_amplitude(ndvi_stack), template), ar)
  names(ndvi_mean) <- "ndvi_mean"; names(ndvi_seas) <- "ndvi_seasonality"
  terra::writeRaster(ndvi_mean, file.path(OUT_VEG, "ndvi_mean.tif"), overwrite = TRUE)
  terra::writeRaster(ndvi_seas, file.path(OUT_VEG, "ndvi_seasonality.tif"), overwrite = TRUE)

  # altura de dosel (INTA, 30 m). Agrega primero (~9e9 celdas) y luego alinea.
  # NA -> 0 (sin bosque nativo); re-enmascara océano a NA.
  canopy_raw <- terra::rast(CANOPY_PATH)
  fact <- max(1, floor(terra::res(template)[1] / terra::res(canopy_raw)[1]))
  canopy_agg <- terra::aggregate(canopy_raw, fact = fact, fun = mean, na.rm = TRUE)
  canopy <- crop_mask_to_region(fill_na(align_to_template(canopy_agg, template), 0), ar)
  names(canopy) <- "canopy_height"
  terra::writeRaster(canopy, file.path(OUT_VEG, "canopy_height.tif"), overwrite = TRUE)

  message("capas de vegetación: tree_cover_pct, ndvi_mean, ndvi_seasonality, canopy_height escritas")
}

# ---- main ----
template <- build_reference_grid()

veg_ready <- length(list.files(WC_DIR, pattern = "tif$")) > 0 &&
             length(list.files(NDVI_DIR, pattern = "tif$")) > 0 &&
             file.exists(CANOPY_PATH)

if (veg_ready) {
  build_vegetation_layers(template)
  message("prepare_env_layers: grilla + vegetación OK")
} else {
  message("prepare_env_layers: grilla OK. Vegetación OMITIDA (faltan insumos: ",
          paste(c(if (!length(list.files(WC_DIR, pattern = "tif$"))) "worldcover",
                  if (!length(list.files(NDVI_DIR, pattern = "tif$"))) "modis_ndvi",
                  if (!file.exists(CANOPY_PATH)) "canopy_height"), collapse = ", "),
          "). Re-correr cuando estén.")
}
