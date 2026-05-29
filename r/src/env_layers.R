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
