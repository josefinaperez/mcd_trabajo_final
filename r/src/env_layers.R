# ============================================================
# File: r/src/env_layers.R
# Purpose: Funciones puras para armonizar rasters ambientales a
#          una grilla de referencia común (template). Sin I/O.
# ============================================================

suppressPackageStartupMessages(library(terra))

# Recorta y enmascara un raster al polígono de una región.
crop_mask_to_region <- function(rast, region_vect) {
  r <- terra::crop(rast, region_vect)
  terra::mask(r, region_vect)
}

# Alinea un raster a la grilla de un template (mismo extent/res/CRS).
# Reproyecta si el CRS difiere; si no, solo resamplea.
align_to_template <- function(rast, template, method = "bilinear") {
  if (!terra::same.crs(rast, template)) {
    terra::project(rast, template, method = method)
  } else {
    terra::resample(rast, template, method = method)
  }
}

# Fracción (%) de celdas-fuente de una clase dada, llevada a la grilla
# del template. Usa resample con method="average" (una pasada GDAL que
# promedia las celdas-fuente dentro de cada celda-destino), eficiente
# incluso cuando el template es mucho más grueso que la fuente (10m -> 4.6km).
cover_fraction <- function(class_rast, target_class, template) {
  mask01 <- terra::ifel(class_rast == target_class, 1, 0)
  frac <- align_to_template(mask01, template, method = "average")
  frac * 100
}

# Media píxel-a-píxel de un stack multi-temporal (p. ej. NDVI anual).
temporal_mean <- function(stack) {
  terra::app(stack, fun = mean, na.rm = TRUE)
}

# Amplitud intra-anual (máx - mín) como proxy de estacionalidad.
temporal_amplitude <- function(stack) {
  mx <- terra::app(stack, fun = max, na.rm = TRUE)
  mn <- terra::app(stack, fun = min, na.rm = TRUE)
  mx - mn
}

# Reemplaza NA por un valor (p. ej. canopy_height = 0 donde no hay bosque).
fill_na <- function(rast, value = 0) {
  terra::ifel(is.na(rast), value, rast)
}
