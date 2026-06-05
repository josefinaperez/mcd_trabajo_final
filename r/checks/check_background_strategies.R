# ============================================================
# File: r/checks/check_background_strategies.R
# Purpose: Chequea las estrategias de background "spatially_constrained" (#52) y
#          "environmentally_dissimilar" (#53) sobre rasters/presencias sintéticas:
#          ningún PA cae dentro del buffer de exclusión (#52); las candidatas
#          disímiles caen en el modo ambiental opuesto al nicho (#53); ambas
#          devuelven n puntos, degradan con gracia si el pool < n, y dejan las
#          coordenadas dentro de la región; y three_step (#54): PA fuera del
#          buffer, disímiles y repartidas entre clusters de K-means.
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

# ============================================================
# #53 — environmentally_dissimilar (OCSVM)
# ============================================================
# Stack ambiental sintético BIMODAL: dos capas con dos modos bien separados
# (mitad izquierda ~0, mitad derecha ~10). Las presencias viven SOLO en el modo
# izquierdo, así que el envelope del OCSVM debe rodear la izquierda y las
# candidatas disímiles (outliers) deben caer en la mitad derecha.
set.seed(1)
tmpl <- terra::rast(nrows = 40, ncols = 40, xmin = -66, xmax = -60,
                    ymin = -36, ymax = -30)
xy   <- terra::xyFromCell(tmpl, seq_len(terra::ncell(tmpl)))
left <- xy[, 1] < -63
nc   <- terra::ncell(tmpl)
r1 <- terra::setValues(tmpl, ifelse(left, 0, 10) + rnorm(nc, 0, 0.3)); names(r1) <- "v1"
r2 <- terra::setValues(tmpl, ifelse(left, 0, 10) + rnorm(nc, 0, 0.3)); names(r2) <- "v2"
env <- c(r1, r2)

# Presencias repartidas por todo el modo IZQUIERDO.
occ_ed <- data.frame(
  decimalLongitude = runif(120, -65.8, -63.3),
  decimalLatitude  = runif(120, -35.8, -30.2)
)

bg_ed <- sample_environmentally_dissimilar_background(env, occ_ed, nu = 0.1,
                                                      n = 150, seed = 1)
stopifnot(nrow(bg_ed) == 150)
stopifnot(all(c("decimalLongitude", "decimalLatitude") %in% names(bg_ed)))
ok("sample_environmentally_dissimilar_background devuelve n puntos con las columnas esperadas")

# Las candidatas disímiles caen en el modo opuesto (mitad derecha, lon > -63).
# Nota: nu=0.1 etiqueta por construcción ~10-15% de las celdas tipo-nicho como
# outliers, así que la mayoría (no el 100%) cae en el modo disímil.
frac_der <- mean(bg_ed$decimalLongitude > -63)
cat(sprintf("  fracción de PA en el modo disímil (derecha): %.2f\n", frac_der))
stopifnot(frac_der > 0.7)
# Aserción fuerte: el env medio del background está mucho más cerca del modo
# disímil (~10) que del nicho de las presencias (~0).
bg_env <- terra::extract(env, terra::vect(bg_ed, geom = c("decimalLongitude", "decimalLatitude"),
                                          crs = "EPSG:4326"))
cat(sprintf("  env medio del background: v1=%.1f v2=%.1f (presencias ~0)\n",
            mean(bg_env$v1), mean(bg_env$v2)))
stopifnot(mean(bg_env$v1) > 5, mean(bg_env$v2) > 5)
ok("las candidatas son verificablemente disímiles al nicho (modo ambiental opuesto)")

# Coordenadas dentro de la región.
stopifnot(all(bg_ed$decimalLongitude >= -66 & bg_ed$decimalLongitude <= -60),
          all(bg_ed$decimalLatitude  >= -36 & bg_ed$decimalLatitude  <= -30))
ok("coordenadas dentro de la región")

# Degradación elegante: la mitad derecha tiene ~800 celdas; pedir 5000 -> warning
# y devuelve lo que puede (< n) sin abortar.
bg_ed_deg <- withCallingHandlers(
  sample_environmentally_dissimilar_background(env, occ_ed, nu = 0.1, n = 5000, seed = 2),
  warning = function(w) { cat("  (warning esperado):", conditionMessage(w), "\n"); invokeRestart("muffleWarning") }
)
stopifnot(nrow(bg_ed_deg) < 5000, nrow(bg_ed_deg) > 0)
ok("degradación elegante cuando el pool de celdas disímiles < n")

# Dispatcher: bp_method = "environmentally_dissimilar" llega al sampler con env_rast.
bg_ed_disp <- generate_background_points(
  bp_method   = "environmentally_dissimilar",
  bp_params   = list(n = 120, seed = 3, nu = 0.1),
  mask_raster = sum(env),
  occ_df      = occ_ed,
  env_rast    = env
)
stopifnot(nrow(bg_ed_disp) == 120)
stopifnot(mean(bg_ed_disp$decimalLongitude > -63) > 0.7)
ok("dispatcher despacha bp_method='environmentally_dissimilar' al sampler (con env_rast)")

# El dispatcher debe abortar si falta env_rast.
err <- tryCatch(
  generate_background_points(bp_method = "environmentally_dissimilar",
                             bp_params = list(n = 10), mask_raster = sum(env),
                             occ_df = occ_ed, env_rast = NULL),
  error = function(e) conditionMessage(e)
)
stopifnot(grepl("env_rast", err))
ok("el dispatcher aborta con mensaje claro si falta env_rast")

cat("\nTodos los chequeos de environmentally_dissimilar (#53) pasaron.\n")

# ============================================================
# #54 — three_step (buffer + OCSVM + K-means)
# ============================================================
# Stack con TRES zonas por longitud:
#   - nicho:        lon < -64          (env ~0)   -> presencias acá
#   - disímil B:    -64 <= lon < -60.3 (env ~10)  -> modo disímil GRANDE
#   - disímil A:    lon >= -60.3       (env ~20)  -> modo disímil CHICO
# El pool candidato (fuera del buffer ∩ outliers OCSVM) = B ∪ A. Con k=2 el
# K-means separa A de B; la asignación uniforme ~n/k debe darle a A (área chica)
# MUCHA más representación que su fracción de área (eso es el aporte del paso 3).
set.seed(1)
tmpl3 <- terra::rast(nrows = 60, ncols = 60, xmin = -66, xmax = -60,
                     ymin = -36, ymax = -30)
xy3   <- terra::xyFromCell(tmpl3, seq_len(terra::ncell(tmpl3)))
zone  <- ifelse(xy3[, 1] < -64, 0,
         ifelse(xy3[, 1] < -60.3, 10, 20))      # nicho / B / A
nc3 <- terra::ncell(tmpl3)
w1 <- terra::setValues(tmpl3, zone + rnorm(nc3, 0, 0.3)); names(w1) <- "v1"
w2 <- terra::setValues(tmpl3, zone + rnorm(nc3, 0, 0.3)); names(w2) <- "v2"
env3 <- c(w1, w2)

occ3 <- data.frame(
  decimalLongitude = runif(120, -65.8, -64.3),  # presencias en el nicho
  decimalLatitude  = runif(120, -35.8, -30.2)
)

bg3 <- sample_three_step_background(env3, occ3, buffer_km = 20, nu = 0.1,
                                    k = 2, n = 200, seed = 1)

stopifnot(nrow(bg3) == 200)
stopifnot(all(c("decimalLongitude", "decimalLatitude") %in% names(bg3)))
ok("sample_three_step_background devuelve n puntos con las columnas esperadas")

# (1) Disímiles: env medio del background lejos del nicho (~0).
bg3_env <- terra::extract(env3, terra::vect(bg3, geom = c("decimalLongitude", "decimalLatitude"),
                                            crs = "EPSG:4326"))
cat(sprintf("  env medio del background: v1=%.1f (presencias ~0)\n", mean(bg3_env$v1)))
stopifnot(mean(bg3_env$v1) > 5)
ok("PA ambientalmente disímiles al nicho")

# (2) Fuera del buffer: distancia AEA de cada PA a la presencia más cercana >= buffer_km.
to_aea3 <- function(df) terra::project(
  terra::vect(df[, c("decimalLongitude", "decimalLatitude")],
              geom = c("decimalLongitude", "decimalLatitude"), crs = "EPSG:4326"), AEA)
dd <- terra::distance(to_aea3(bg3), to_aea3(occ3))
stopifnot(min(apply(dd, 1, min)) / 1000 >= 20)
ok("ningún PA cae dentro del buffer de exclusión")

# (3) Reparto entre clusters: el modo disímil CHICO (A, lon >= -60.3, ~5% del
# área candidata) recibe una fracción de PA muy superior a su área (cobertura
# uniforme de K-means; un muestreo proporcional lo dejaría casi vacío).
frac_A <- mean(bg3$decimalLongitude >= -60.3)
cat(sprintf("  fracción de PA en el modo disímil chico (A): %.2f\n", frac_A))
stopifnot(frac_A > 0.3)
ok("K-means reparte el background entre clusters (cobertura uniforme del espacio disímil)")

# (4) Degradación elegante: pedir más que el pool -> warning y < n.
bg3_deg <- withCallingHandlers(
  sample_three_step_background(env3, occ3, buffer_km = 20, nu = 0.1, k = 2, n = 100000, seed = 2),
  warning = function(w) { cat("  (warning esperado):", conditionMessage(w), "\n"); invokeRestart("muffleWarning") }
)
stopifnot(nrow(bg3_deg) < 100000, nrow(bg3_deg) > 0)
ok("degradación elegante cuando el pool < n")

# (5) Dispatcher: bp_method = "three_step" llega al sampler con env_rast.
bg3_disp <- generate_background_points(
  bp_method   = "three_step",
  bp_params   = list(n = 120, seed = 3, buffer_km = 20, nu = 0.1, k = 2),
  mask_raster = sum(env3),
  occ_df      = occ3,
  env_rast    = env3
)
stopifnot(nrow(bg3_disp) == 120)
ok("dispatcher despacha bp_method='three_step' al sampler (con env_rast)")

cat("\nTodos los chequeos de three_step (#54) pasaron.\n")
