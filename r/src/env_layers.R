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
