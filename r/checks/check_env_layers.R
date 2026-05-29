# ============================================================
# File: r/checks/check_env_layers.R
# Purpose: Chequeo de las funciones puras de armonización de
#          capas ambientales (env_layers.R) sobre rasters
#          sintéticos. No descarga datos.
# Ejecutar desde repo root:
#   Rscript r/checks/check_env_layers.R
# ============================================================

suppressPackageStartupMessages(library(terra))
source("r/src/env_layers.R")

ok <- function(msg) cat("OK:", msg, "\n")

# ---- crop_mask_to_region ----
r <- terra::rast(nrows = 20, ncols = 20,
                 xmin = -70, xmax = -60, ymin = -40, ymax = -30,
                 crs = "EPSG:4326")
terra::values(r) <- 1
# triángulo: garantiza celdas enmascaradas dentro del bbox
poly <- terra::vect("POLYGON((-68 -38, -62 -38, -62 -32, -68 -38))",
                    crs = "EPSG:4326")
out <- crop_mask_to_region(r, poly)
stopifnot(terra::xmax(out) - terra::xmin(out) < 10)        # recortado
stopifnot(sum(is.na(terra::values(out))) > 0)               # enmascarado
stopifnot(sum(!is.na(terra::values(out))) > 0)              # algo queda
ok("crop_mask_to_region")

# ---- align_to_template ----
template <- terra::rast(nrows = 10, ncols = 10,
                        xmin = -68, xmax = -62, ymin = -38, ymax = -32,
                        crs = "EPSG:4326")
terra::values(template) <- 0
src <- terra::rast(nrows = 60, ncols = 60,
                   xmin = -68, xmax = -62, ymin = -38, ymax = -32,
                   crs = "EPSG:4326")
terra::values(src) <- seq_len(terra::ncell(src))
aligned <- align_to_template(src, template, method = "bilinear")
stopifnot(terra::compareGeom(aligned, template, stopOnError = FALSE))
ok("align_to_template")

cat("\nTODOS LOS CHEQUEOS OK\n")
