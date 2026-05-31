# ============================================================
# File: r/checks/check_soil_layers.R
# Purpose: Verifica las capas de suelo preparadas (#26) contra la
#          grilla de referencia: existencia, alineación geométrica
#          y cobertura (NA interior <= tolerancia). Guardado: si las
#          capas aún no fueron preparadas, omite sin fallar.
# Ejecutar desde repo root (después de source("r/src/prepare_env_layers.R")):
#   Rscript r/checks/check_soil_layers.R
# ============================================================

suppressPackageStartupMessages({ library(terra) })
source("r/src/env_layers.R")

OUT_SOIL  <- "data/features/env_2.5m_ar/soil"
OUT_BIO   <- "data/features/env_2.5m_ar/bioclim"
SHP_PATH  <- "data/shp/argentina/argentina.shp"
SOIL_VARS <- c("phh2o", "soc", "sand", "silt", "clay", "cec", "bdod")
NA_TOL    <- 0.05   # tolerancia de NA interior (5%): empíricamente SoilGrids
                    # tiene ~3% de NA sobre tierra en Argentina (cuerpos de agua,
                    # salares). NO se rellena (NA = sin dato de suelo, no una
                    # categoría); esas filas las descarta el filtro del builder.

ok <- function(msg) cat("OK:", msg, "\n")

soil_paths <- file.path(OUT_SOIL, paste0(SOIL_VARS, ".tif"))
if (!all(file.exists(soil_paths))) {
  faltan <- SOIL_VARS[!file.exists(soil_paths)]
  cat("OMITIDO: faltan capas de suelo preparadas (", paste(faltan, collapse = ", "),
      "). Correr source(\"r/src/prepare_env_layers.R\") primero.\n", sep = "")
  quit(save = "no", status = 0)
}

bio_paths <- list.files(OUT_BIO, pattern = "tif$", full.names = TRUE)
stopifnot(length(bio_paths) > 0)
template <- terra::rast(bio_paths[[1]])
ar <- terra::vect(SHP_PATH)

for (v in SOIL_VARS) {
  layer <- terra::rast(file.path(OUT_SOIL, paste0(v, ".tif")))
  stopifnot(terra::compareGeom(layer, template, stopOnError = FALSE))
  ok(paste0("alineación geométrica: ", v))
  frac_na <- interior_na_fraction(layer, ar)
  if (is.na(frac_na) || frac_na > NA_TOL) {
    stop(sprintf("capa %s: NA interior = %.3f supera tolerancia %.3f", v, frac_na, NA_TOL))
  }
  ok(sprintf("cobertura (NA interior %.3f <= %.2f): %s", frac_na, NA_TOL, v))
}

cat("\nCAPAS DE SUELO OK\n")
