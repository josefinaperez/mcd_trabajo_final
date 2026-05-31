# ============================================================
# File: r/checks/check_topography_layers.R
# Purpose: Verifica las capas topográficas preparadas (#25) contra
#          la grilla de referencia: existencia, alineación geométrica
#          y cobertura (NA interior ~ 0). Guardado: si las capas aún
#          no fueron preparadas, omite sin fallar.
# Ejecutar desde repo root (después de source("r/src/prepare_env_layers.R")):
#   Rscript r/checks/check_topography_layers.R
# ============================================================

suppressPackageStartupMessages({ library(terra) })
source("r/src/env_layers.R")

OUT_TOPO  <- "data/features/env_2.5m_ar/topography"
OUT_BIO   <- "data/features/env_2.5m_ar/bioclim"
SHP_PATH  <- "data/shp/argentina/argentina.shp"
TOPO_VARS <- c("cti", "slope", "tpi", "tri")
NA_TOL    <- 0.02   # tolerancia de NA interior (2%)

ok <- function(msg) cat("OK:", msg, "\n")

topo_paths <- file.path(OUT_TOPO, paste0(TOPO_VARS, ".tif"))
if (!all(file.exists(topo_paths))) {
  faltan <- TOPO_VARS[!file.exists(topo_paths)]
  cat("OMITIDO: faltan capas topográficas preparadas (", paste(faltan, collapse = ", "),
      "). Correr source(\"r/src/prepare_env_layers.R\") primero.\n", sep = "")
  quit(save = "no", status = 0)
}

bio_paths <- list.files(OUT_BIO, pattern = "tif$", full.names = TRUE)
stopifnot(length(bio_paths) > 0)
template <- terra::rast(bio_paths[[1]])
ar <- terra::vect(SHP_PATH)

for (v in TOPO_VARS) {
  layer <- terra::rast(file.path(OUT_TOPO, paste0(v, ".tif")))
  stopifnot(terra::compareGeom(layer, template, stopOnError = FALSE))
  ok(paste0("alineación geométrica: ", v))
  frac_na <- interior_na_fraction(layer, ar)
  if (is.na(frac_na) || frac_na > NA_TOL) {
    stop(sprintf("capa %s: NA interior = %.3f supera tolerancia %.3f", v, frac_na, NA_TOL))
  }
  ok(sprintf("cobertura (NA interior %.3f <= %.2f): %s", frac_na, NA_TOL, v))
}

cat("\nCAPAS TOPOGRÁFICAS OK\n")
