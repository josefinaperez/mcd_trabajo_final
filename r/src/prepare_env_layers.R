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

suppressPackageStartupMessages({
  library(terra)
  library(sf)
})
source("r/src/env_layers.R")

SHP_PATH    <- "data/shp/argentina/argentina.shp"
BIO_RAW_DIR <- "data/features/worldclim/wc2.1_2.5m_bio"
WC_DIR      <- "data/features/vegetation_raw/worldcover"
NDVI_DIR    <- "data/features/vegetation_raw/modis_ndvi"
CANOPY_PATH <- "data/features/vegetation_raw/inta_forest/canopy_height.tif"
OUT_BIO     <- "data/features/env_2.5m_ar/bioclim"
OUT_VEG     <- "data/features/env_2.5m_ar/vegetation"
TOPO_RAW_DIR <- "data/features/topography_raw"   # subdirs: cti, slope, tpi, tri
TOPO_VARS    <- c("cti", "slope", "tpi", "tri")  # cti = TWI/CTI; tri = ruggedness
OUT_TOPO     <- "data/features/env_2.5m_ar/topography"

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
  # VRT (mosaico virtual de los 70 tiles, sin copiar datos) + warp GDAL a ~270m
  # con -r near: submuestrea ~1/256 de los píxeles 10m en una sola pasada por
  # bloques (minutos). Hacer cover_fraction directo a 10m leía miles de millones
  # de celdas (horas). cover_fraction luego promedia 270m -> 2.5 arc-min.
  wc_vrt_path  <- tempfile(fileext = ".vrt")
  wc_down_path <- tempfile(fileext = ".tif")
  terra::vrt(list.files(WC_DIR, pattern = "tif$", full.names = TRUE),
             filename = wc_vrt_path, overwrite = TRUE)
  sf::gdal_utils("warp", source = wc_vrt_path, destination = wc_down_path,
                 options = c("-tr", "0.0025", "0.0025", "-r", "near",
                             "-ot", "Byte", "-co", "COMPRESS=LZW"))
  tree_pct <- crop_mask_to_region(
    cover_fraction(terra::rast(wc_down_path), target_class = 10, template = template), ar)
  names(tree_pct) <- "tree_cover_pct"
  terra::writeRaster(tree_pct, file.path(OUT_VEG, "tree_cover_pct.tif"), overwrite = TRUE)

  # NDVI medio y estacionalidad (MODIS MOD13A3)
  ndvi_stack <- terra::rast(list.files(NDVI_DIR, pattern = "tif$", full.names = TRUE))
  ndvi_mean <- crop_mask_to_region(align_to_template(temporal_mean(ndvi_stack), template), ar)
  ndvi_seas <- crop_mask_to_region(align_to_template(temporal_amplitude(ndvi_stack), template), ar)
  names(ndvi_mean) <- "ndvi_mean"; names(ndvi_seas) <- "ndvi_seasonality"
  terra::writeRaster(ndvi_mean, file.path(OUT_VEG, "ndvi_mean.tif"), overwrite = TRUE)
  terra::writeRaster(ndvi_seas, file.path(OUT_VEG, "ndvi_seasonality.tif"), overwrite = TRUE)

  # altura de dosel (INTA, 30 m, 34 GB). warp GDAL a ~270m con -r average:
  # usa las overviews (.ovr) del .tif para promediar rápido (minutos), en vez
  # de terra::aggregate que recorre las ~9e9 celdas nativas (horas).
  # NA -> 0 (sin bosque nativo); crop_mask re-enmascara océano a NA.
  canopy_down_path <- tempfile(fileext = ".tif")
  sf::gdal_utils("warp", source = CANOPY_PATH, destination = canopy_down_path,
                 options = c("-tr", "0.0025", "0.0025", "-r", "average",
                             "-co", "COMPRESS=LZW"))
  canopy <- crop_mask_to_region(
    fill_na(align_to_template(terra::rast(canopy_down_path), template), 0), ar)
  names(canopy) <- "canopy_height"
  terra::writeRaster(canopy, file.path(OUT_VEG, "canopy_height.tif"), overwrite = TRUE)

  message("capas de vegetación: tree_cover_pct, ndvi_mean, ndvi_seasonality, canopy_height escritas")
}

# Procesamiento común para familias de capas continuas que se downsamplean
# agregando: por cada variable, VRT(tiles) -> warp GDAL a ~270m con -r average
# (usa overviews si existen) -> align_to_template (270m -> 2.5 arc-min) ->
# crop_mask (recorta y re-enmascara océano a NA). NO rellena NA: un NA es
# "sin dato", no una categoría ecológica; crop_mask deja el océano como NA.
build_aggregated_layers <- function(raw_dir, vars, out_dir, template, ar) {
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  for (v in vars) {
    tiles <- list.files(file.path(raw_dir, v), pattern = "tif$", full.names = TRUE)
    vrt_path  <- tempfile(fileext = ".vrt")
    down_path <- tempfile(fileext = ".tif")
    terra::vrt(tiles, filename = vrt_path, overwrite = TRUE)
    sf::gdal_utils("warp", source = vrt_path, destination = down_path,
                   options = c("-tr", "0.0025", "0.0025", "-r", "average",
                               "-co", "COMPRESS=LZW"))
    layer <- crop_mask_to_region(
      align_to_template(terra::rast(down_path), template), ar)
    names(layer) <- v
    terra::writeRaster(layer, file.path(out_dir, paste0(v, ".tif")), overwrite = TRUE)
  }
}

# ---- #25: capas topográficas (Geomorpho90m) alineadas al template ----
build_topography_layers <- function(template) {
  ar <- terra::vect(SHP_PATH)
  build_aggregated_layers(TOPO_RAW_DIR, TOPO_VARS, OUT_TOPO, template, ar)
  message("capas topográficas: ", paste(TOPO_VARS, collapse = ", "), " escritas")
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

topo_ready <- all(vapply(TOPO_VARS, function(v)
  length(list.files(file.path(TOPO_RAW_DIR, v), pattern = "tif$")) > 0, logical(1)))

if (topo_ready) {
  build_topography_layers(template)
  message("prepare_env_layers: topografía OK")
} else {
  faltan <- TOPO_VARS[!vapply(TOPO_VARS, function(v)
    length(list.files(file.path(TOPO_RAW_DIR, v), pattern = "tif$")) > 0, logical(1))]
  message("prepare_env_layers: topografía OMITIDA (faltan insumos: ",
          paste(faltan, collapse = ", "), "). Re-correr cuando estén.")
}
