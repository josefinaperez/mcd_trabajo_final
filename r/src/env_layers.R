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

# Fracción (%) de celdas-fuente de una clase dada, agregada a la
# grilla del template. Útil para "tree cover %" desde una clasificación.
# Agrega la máscara binaria a ~la resolución del template (media) y luego
# la alinea exactamente a la grilla; resample("average") no agrega bien
# cuando el template es mucho más grueso que la fuente.
cover_fraction <- function(class_rast, target_class, template) {
  mask01 <- terra::ifel(class_rast == target_class, 1, 0)
  fact <- pmax(1, round(terra::res(template) / terra::res(mask01)))
  agg <- terra::aggregate(mask01, fact = fact, fun = mean, na.rm = TRUE)
  frac <- terra::resample(agg, template, method = "bilinear")
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
