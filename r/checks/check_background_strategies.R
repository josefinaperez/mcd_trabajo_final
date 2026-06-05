# ============================================================
# File: r/checks/check_background_strategies.R
# Purpose: Chequea las estrategias de background "spatially_constrained" (#52)
#          sobre rasters/presencias sintéticas: ningún PA cae dentro del buffer
#          de exclusión, devuelve n puntos, degrada con gracia si el pool < n,
#          y coordenadas dentro de la región.
# Ejecutar desde repo root:
#   Rscript r/checks/check_background_strategies.R
# ============================================================

suppressPackageStartupMessages({ library(terra) })
source("r/src/build_parallel_sdm_datasets.R")

ok <- function(msg) cat("OK:", msg, "\n")

# CRS AEA (mismo de grid_thinning.R) para medir distancias en metros.
AEA <- "+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 +datum=WGS84 +units=m +no_defs"

# Máscara 60x60 en lon/lat alrededor de Argentina central (todo válido = 1).
mask_r <- terra::rast(nrows = 60, ncols = 60, xmin = -66, xmax = -60,
                      ymin = -36, ymax = -30, vals = 1)

# Tres presencias bien separadas.
occ <- data.frame(
  decimalLongitude = c(-64, -62, -63),
  decimalLatitude  = c(-34, -32, -33)
)

buffer_km <- 20
bg <- sample_spatially_constrained_background(mask_r, occ, buffer_km = buffer_km,
                                              n = 200, seed = 1)

stopifnot(nrow(bg) == 200)
stopifnot(all(c("decimalLongitude", "decimalLatitude") %in% names(bg)))
ok("sample_spatially_constrained_background devuelve n puntos con las columnas esperadas")

# Ningún PA dentro del buffer: distancia (en AEA) de cada PA a la presencia más
# cercana debe ser >= buffer_km.
to_aea <- function(df) {
  v <- terra::vect(df[, c("decimalLongitude", "decimalLatitude")],
                   geom = c("decimalLongitude", "decimalLatitude"), crs = "EPSG:4326")
  terra::project(v, AEA)
}
d <- terra::distance(to_aea(bg), to_aea(occ))   # matriz PA x presencias (m)
min_d_km <- min(apply(d, 1, min)) / 1000
cat(sprintf("  distancia mínima PA->presencia: %.1f km (buffer = %d km)\n", min_d_km, buffer_km))
stopifnot(min_d_km >= buffer_km)
ok("ningún PA cae dentro del buffer de exclusión")

# Coordenadas dentro de la extensión de la máscara.
stopifnot(all(bg$decimalLongitude >= -66 & bg$decimalLongitude <= -60),
          all(bg$decimalLatitude  >= -36 & bg$decimalLatitude  <= -30))
ok("coordenadas dentro de la región")

# Degradación elegante: si se pide más background que celdas disponibles fuera
# del buffer, no aborta -> warning y devuelve lo que puede (< n). La máscara tiene
# 60*60 = 3600 celdas; pedimos 5000 con el buffer normal de 20 km.
bg_deg <- withCallingHandlers(
  sample_spatially_constrained_background(mask_r, occ, buffer_km = 20,
                                          n = 5000, seed = 2),
  warning = function(w) { cat("  (warning esperado):", conditionMessage(w), "\n"); invokeRestart("muffleWarning") }
)
stopifnot(nrow(bg_deg) < 5000, nrow(bg_deg) > 0)
ok("degradación elegante cuando el pool fuera del buffer < n")

# Dispatcher: bp_method = "spatially_constrained" llega al sampler nuevo.
bg_disp <- generate_background_points(
  bp_method = "spatially_constrained",
  bp_params = list(n = 150, seed = 3, buffer_km = 20),
  mask_raster = mask_r,
  occ_df = occ
)
stopifnot(nrow(bg_disp) == 150)
d2 <- terra::distance(to_aea(bg_disp), to_aea(occ))
stopifnot(min(apply(d2, 1, min)) / 1000 >= 20)
ok("dispatcher despacha bp_method='spatially_constrained' al sampler")

cat("\nTodos los chequeos de spatially_constrained (#52) pasaron.\n")
