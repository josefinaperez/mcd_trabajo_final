# ============================================================
# File: r/checks/check_anthropic_layers.R
# Purpose: Verifica las capas antrópicas preparadas (#47) contra la
#          grilla de referencia: existencia, alineación geométrica
#          y cobertura (NA interior <= tolerancia). Guardado: si las
#          capas aún no fueron preparadas, omite sin fallar.
# Ejecutar desde repo root (después de source("r/src/prepare_env_layers.R")):
#   Rscript r/checks/check_anthropic_layers.R
# ============================================================

suppressPackageStartupMessages({ library(terra) })
source("r/src/env_layers.R")

OUT_ANTHRO  <- "data/features/env_2.5m_ar/anthropic"
OUT_BIO     <- "data/features/env_2.5m_ar/bioclim"
SHP_PATH    <- "data/shp/argentina/argentina.shp"
ANTHRO_VARS <- c("travel_time", "human_modification")
NA_TOL      <- 0.05   # tolerancia de NA interior (5%): travel-time y gHM son
                      # continuas y están definidas en toda la tierra; se espera
                      # NA interior ~0 (sólo cuerpos de agua). NO se rellena
                      # (NA = sin dato); esas filas las descarta el builder.

ok <- function(msg) cat("OK:", msg, "\n")

anthro_paths <- file.path(OUT_ANTHRO, paste0(ANTHRO_VARS, ".tif"))
if (!all(file.exists(anthro_paths))) {
  faltan <- ANTHRO_VARS[!file.exists(anthro_paths)]
  cat("OMITIDO: faltan capas antrópicas preparadas (", paste(faltan, collapse = ", "),
      "). Correr source(\"r/src/prepare_env_layers.R\") primero.\n", sep = "")
  quit(save = "no", status = 0)
}

bio_paths <- list.files(OUT_BIO, pattern = "tif$", full.names = TRUE)
stopifnot(length(bio_paths) > 0)
template <- terra::rast(bio_paths[[1]])
ar <- terra::vect(SHP_PATH)

for (v in ANTHRO_VARS) {
  layer <- terra::rast(file.path(OUT_ANTHRO, paste0(v, ".tif")))
  stopifnot(terra::compareGeom(layer, template, stopOnError = FALSE))
  ok(paste0("alineación geométrica: ", v))
  frac_na <- interior_na_fraction(layer, ar)
  if (is.na(frac_na) || frac_na > NA_TOL) {
    stop(sprintf("capa %s: NA interior = %.3f supera tolerancia %.3f", v, frac_na, NA_TOL))
  }
  ok(sprintf("cobertura (NA interior %.3f <= %.2f): %s", frac_na, NA_TOL, v))
}

cat("\nCAPAS ANTRÓPICAS OK\n")
