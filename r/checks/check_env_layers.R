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

# ---- cover_fraction ----
tmpl1 <- terra::rast(nrows = 1, ncols = 1,
                     xmin = 0, xmax = 10, ymin = 0, ymax = 10,
                     crs = "EPSG:4326")
terra::values(tmpl1) <- 0
cls <- terra::rast(nrows = 10, ncols = 10,
                   xmin = 0, xmax = 10, ymin = 0, ymax = 10,
                   crs = "EPSG:4326")
vv <- rep(2L, 100); vv[1:50] <- 1L   # 50% clase 1 ("árbol")
terra::values(cls) <- vv
frac <- cover_fraction(cls, target_class = 1L, template = tmpl1)
stopifnot(abs(terra::values(frac)[1] - 50) < 1e-6)
ok("cover_fraction")

# ---- temporal_mean / temporal_amplitude ----
b <- terra::rast(nrows = 2, ncols = 2,
                 xmin = 0, xmax = 2, ymin = 0, ymax = 2, crs = "EPSG:4326")
l1 <- b; terra::values(l1) <- 0
l2 <- b; terra::values(l2) <- 4
l3 <- b; terra::values(l3) <- 2
st <- c(l1, l2, l3)
stopifnot(all(terra::values(temporal_mean(st)) == 2))
stopifnot(all(terra::values(temporal_amplitude(st)) == 4))   # max - min = 4 - 0
ok("temporal_mean / temporal_amplitude")

# ---- fill_na ----
rna <- terra::rast(nrows = 2, ncols = 2,
                   xmin = 0, xmax = 2, ymin = 0, ymax = 2, crs = "EPSG:4326")
terra::values(rna) <- c(NA, 5, NA, 7)
filled <- fill_na(rna, 0)
stopifnot(all(terra::values(filled) == c(0, 5, 0, 7)))
ok("fill_na")

cat("\nTODOS LOS CHEQUEOS OK\n")
