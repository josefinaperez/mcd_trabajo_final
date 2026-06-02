# ============================================================
# File: r/checks/check_residual_autocorrelation.R
# Purpose: Chequeo de las funciones puras de autocorrelación
#          espacial de residuos (residual_autocorrelation.R)
#          sobre datos sintéticos. No lee artefactos de disco.
# Ejecutar desde repo root:
#   Rscript r/checks/check_residual_autocorrelation.R
# ============================================================

suppressPackageStartupMessages(library(tibble))
source("r/src/residual_autocorrelation.R")

ok <- function(msg) cat("OK:", msg, "\n")

set.seed(42)

# ---- compute_residuals ----
r <- compute_residuals(class = c(1, 0, 1, 0),
                       score = c(0.8, 0.3, 0.4, 0.1))
stopifnot(all(abs(r - c(0.2, -0.3, 0.6, -0.1)) < 1e-9))
res <- tryCatch(compute_residuals(c(1, 2), c(0.1, 0.2)), error = function(e) "err")
stopifnot(identical(res, "err"))  # clase no binaria -> error
ok("compute_residuals")

# ---- project_coords_aea ----
# lon/lat dentro de Argentina -> coords finitas en metros, n x 2.
lon <- c(-64, -60, -68)
lat <- c(-32, -38, -28)
xy  <- project_coords_aea(lon, lat)
stopifnot(is.matrix(xy), nrow(xy) == 3, ncol(xy) == 2, all(is.finite(xy)))
# rango plausible para la AEA centrada en (-60,-32): metros, |x|,|y| < ~3000 km
stopifnot(all(abs(xy) < 3e6))
ok("project_coords_aea")

# Grilla regular de puntos para los tests de Moran -----------
g  <- expand.grid(gx = 1:18, gy = 1:18)
n  <- nrow(g)
# coords en "metros": separación de 20 km entre nodos vecinos
coords_m <- as.matrix(g) * 20e3

# ---- build_knn_listw ----
listw <- build_knn_listw(coords_m, k = 8)
stopifnot(inherits(listw, "listw"))
stopifnot(length(listw$neighbours) == n)
stopifnot(all(spdep::card(listw$neighbours) == 8))  # exactamente k vecinos
ok("build_knn_listw")

# ---- moran_test_residuals: campo CON autocorrelación ----
# Residuo = gradiente espacial suave -> Moran's I fuertemente positivo.
grad <- (g$gx + g$gy) / max(g$gx + g$gy)
mt_signal <- moran_test_residuals(grad, listw, nsim = 199, k = 8)
stopifnot(nrow(mt_signal) == 1)
stopifnot(mt_signal$moran_i > 0.5)        # estructura espacial fuerte
stopifnot(mt_signal$p_value <= 0.05)      # significativa
stopifnot(abs(mt_signal$expectation + 1 / (n - 1)) < 1e-12)
ok("moran_test_residuals (señal espacial -> I positivo significativo)")

# ---- moran_test_residuals: ruido i.i.d. ----
noise <- rnorm(n)
mt_noise <- moran_test_residuals(noise, listw, nsim = 199, k = 8)
stopifnot(abs(mt_noise$moran_i) < 0.2)    # I cerca de E[I] ~ 0
stopifnot(mt_noise$p_value > 0.05)        # no significativo
ok("moran_test_residuals (ruido -> I ~ E[I], no significativo)")

# ---- residual_distance_correlogram ----
# Bandas de 0 a 120 km en pasos de 40 km sobre el gradiente.
cg <- residual_distance_correlogram(
  coords_m, grad,
  breaks = seq(0, 120e3, by = 40e3),
  nsim   = 99
)
stopifnot(nrow(cg) == 3)
stopifnot(all(c("dist_lo", "dist_hi", "dist_mid",
                "moran_i", "p_value", "n_links") %in% names(cg)))
stopifnot(all(cg$n_links > 0))
# La banda de menor distancia (vecinos inmediatos del gradiente) es
# positiva y significativa.
band1 <- cg[1, ]
stopifnot(band1$moran_i > 0.3, band1$p_value <= 0.05)
ok("residual_distance_correlogram")

cat("\nTodos los chequeos de residual_autocorrelation.R pasaron.\n")
