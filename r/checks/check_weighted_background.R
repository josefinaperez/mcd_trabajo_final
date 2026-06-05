# ============================================================
# File: r/checks/check_weighted_background.R
# Purpose: Chequea sample_weighted_background() (#48) sobre rasters
#          sintéticos: el muestreo se concentra donde el peso es alto,
#          respeta la máscara de validez (no muestrea celdas NA) y
#          devuelve coordenadas dentro de la región.
# Ejecutar desde repo root:
#   Rscript r/checks/check_weighted_background.R
# ============================================================

suppressPackageStartupMessages({ library(terra) })
source("r/src/build_parallel_sdm_datasets.R")

ok <- function(msg) cat("OK:", msg, "\n")

# Raster 20x20. Mitad izquierda (x < 10) peso ALTO, mitad derecha peso BAJO.
mask_r   <- terra::rast(nrows = 20, ncols = 20, xmin = 0, xmax = 20,
                        ymin = 0, ymax = 20, vals = 1)
weight_r <- terra::rast(mask_r)
xs <- terra::xFromCell(weight_r, seq_len(terra::ncell(weight_r)))
terra::values(weight_r) <- ifelse(xs < 10, 100, 1)   # 100:1 a favor de la izquierda

bg <- sample_weighted_background(mask_r, weight_r, n = 150, seed = 1)
stopifnot(nrow(bg) == 150)
stopifnot(all(c("decimalLongitude", "decimalLatitude") %in% names(bg)))
frac_izq <- mean(bg$decimalLongitude < 10)
cat(sprintf("  fracción en zona de peso alto (izq): %.3f\n", frac_izq))
stopifnot(frac_izq > 0.90)   # con 100:1, la inmensa mayoría cae a la izquierda
ok("sample_weighted_background concentra el muestreo donde el peso es alto")

# Respeta la máscara: NA en la mitad inferior -> ningún punto con y < 10.
mask_na <- mask_r
terra::values(mask_na)[terra::yFromCell(mask_na, seq_len(terra::ncell(mask_na))) < 10] <- NA
bg2 <- sample_weighted_background(mask_na, weight_r, n = 100, seed = 2)
stopifnot(all(bg2$decimalLatitude >= 10))
ok("sample_weighted_background no muestrea celdas NA de la máscara")

# Peso 0/NA en parte de la región -> no se muestrea ahí.
w_zero <- weight_r
terra::values(w_zero)[xs >= 10] <- 0   # peso 0 a la derecha
bg3 <- sample_weighted_background(mask_r, w_zero, n = 100, seed = 3)
stopifnot(all(bg3$decimalLongitude < 10))
ok("sample_weighted_background ignora celdas con peso 0")

# Coordenadas dentro de la extensión.
stopifnot(all(bg$decimalLongitude >= 0 & bg$decimalLongitude <= 20),
          all(bg$decimalLatitude  >= 0 & bg$decimalLatitude  <= 20))
ok("coordenadas dentro de la región")

cat("\nTodos los chequeos de sample_weighted_background (#48) pasaron.\n")
