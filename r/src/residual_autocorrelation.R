# ============================================================
# File: residual_autocorrelation.R
# Purpose: Funciones puras (sin I/O de disco) para evaluar la
#          autocorrelación espacial de los residuos de un modelo
#          SDM ya entrenado. Operan sobre (class, score, coords)
#          leídos de predictions_test.csv, por lo que son
#          reutilizables para cualquier algoritmo.
#
#          Residuo crudo = class - score (presencia/background).
#          Moran's I con pesos k-vecinos-más-cercanos y p-valor
#          por permutación Monte Carlo (spdep::moran.mc). El
#          correlograma reporta Moran's I por bandas de distancia.
#
# Testeado sobre datos sintéticos por
#   r/checks/check_residual_autocorrelation.R
# ============================================================

suppressPackageStartupMessages({
  library(sf)
  library(spdep)
  library(tibble)
})

# CRS equal-area centrada en Argentina (Albers). Misma que
# spatial_cv.R / grid_thinning.R; las distancias salen en metros.
AEA_ARGENTINA <- paste0(
  "+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 ",
  "+datum=WGS84 +units=m +no_defs"
)

# ------------------------------------------------------------
# 1) RESIDUOS
# ------------------------------------------------------------
#
# Residuo crudo: observado (1 = presencia, 0 = background) menos
# el score de idoneidad predicho ∈ [0, 1]. Vector numérico.
# ------------------------------------------------------------

compute_residuals <- function(class, score) {
  stopifnot(length(class) == length(score))
  class <- as.numeric(class)
  if (!all(class %in% c(0, 1))) {
    stop("compute_residuals: 'class' debe ser binaria (0/1).")
  }
  class - as.numeric(score)
}

# ------------------------------------------------------------
# 2) PROYECCIÓN A EQUAL-AREA
# ------------------------------------------------------------
#
# Proyecta coordenadas lon/lat (EPSG:4326) a la AEA de Argentina
# para que las vecindades y distancias se computen en metros.
# Devuelve una matriz n x 2 (x, y) en metros.
# ------------------------------------------------------------

project_coords_aea <- function(lon, lat) {
  stopifnot(length(lon) == length(lat))
  pts <- sf::st_as_sf(
    data.frame(lon = lon, lat = lat),
    coords = c("lon", "lat"),
    crs    = 4326
  )
  pts <- sf::st_transform(pts, AEA_ARGENTINA)
  sf::st_coordinates(pts)
}

# ------------------------------------------------------------
# 3) PESOS ESPACIALES K-VECINOS-MÁS-CERCANOS
# ------------------------------------------------------------
#
# coords_m: matriz n x 2 en metros. Devuelve un objeto listw
# (estilo "W", fila-estandarizado) con los k vecinos más
# cercanos de cada punto. Robusto a densidad irregular (las
# presencias clusterizadas y el background disperso conviven).
# ------------------------------------------------------------

build_knn_listw <- function(coords_m, k = 8) {
  coords_m <- as.matrix(coords_m)
  n <- nrow(coords_m)
  stopifnot(ncol(coords_m) == 2, k >= 1, k < n)
  knn <- spdep::knearneigh(coords_m, k = k)
  nb  <- spdep::knn2nb(knn)
  spdep::nb2listw(nb, style = "W")
}

# ------------------------------------------------------------
# 4) TEST DE MORAN SOBRE LOS RESIDUOS
# ------------------------------------------------------------
#
# Moran's I con p-valor por permutación Monte Carlo (moran.mc):
# se permutan los residuos nsim veces para construir la
# distribución nula y se compara el I observado. Alternativa
# "greater" (interesa autocorrelación POSITIVA: estructura
# residual no explicada).
#
# Devuelve un tibble de una fila:
#   moran_i      : I observado
#   expectation  : E[I] = -1/(n-1)
#   sd           : desvío de la distribución de permutación
#   p_value      : p-valor de permutación (cola superior)
#   n            : nº de puntos
#   k            : nº de vecinos usado
#   nsim         : nº de permutaciones
#   alternative  : "greater"
# ------------------------------------------------------------

moran_test_residuals <- function(residuals, listw, nsim = 999, k = NA_integer_) {
  stopifnot(length(residuals) == length(listw$neighbours))
  mc <- spdep::moran.mc(
    x           = residuals,
    listw       = listw,
    nsim        = nsim,
    alternative = "greater",
    zero.policy = TRUE
  )
  n <- length(residuals)
  tibble::tibble(
    moran_i     = as.numeric(mc$statistic),
    expectation = -1 / (n - 1),
    sd          = stats::sd(mc$res),
    p_value     = as.numeric(mc$p.value),
    n           = n,
    k           = as.integer(k),
    nsim        = as.integer(nsim),
    alternative = "greater"
  )
}

# ------------------------------------------------------------
# 5) CORRELOGRAMA POR BANDAS DE DISTANCIA
# ------------------------------------------------------------
#
# Para cada anillo [breaks[i], breaks[i+1]) en metros construye
# una vecindad por umbral de distancia (dnearneigh) y computa
# Moran's I + p-valor de permutación sobre los residuos. Muestra
# a qué escala espacial queda (o no) estructura residual.
#
# coords_m : matriz n x 2 en metros
# breaks   : vector creciente de cortes de distancia en metros
#            (longitud >= 2). Por defecto 0..500 km en pasos de
#            50 km.
#
# Devuelve un tibble con una fila por banda:
#   dist_lo, dist_hi, dist_mid (m), moran_i, p_value, n_links
# Las bandas sin pares (o con un solo punto conectado) devuelven
# moran_i = NA.
# ------------------------------------------------------------

residual_distance_correlogram <- function(coords_m,
                                          residuals,
                                          breaks = seq(0, 500e3, by = 50e3),
                                          nsim   = 499) {
  coords_m <- as.matrix(coords_m)
  stopifnot(ncol(coords_m) == 2,
            length(residuals) == nrow(coords_m),
            length(breaks) >= 2,
            all(diff(breaks) > 0))

  n_bands <- length(breaks) - 1L
  rows <- vector("list", n_bands)

  for (i in seq_len(n_bands)) {
    lo <- breaks[i]
    hi <- breaks[i + 1]
    nb <- tryCatch(
      spdep::dnearneigh(coords_m, d1 = lo, d2 = hi),
      error = function(e) NULL
    )

    n_links <- if (is.null(nb)) 0L else sum(spdep::card(nb))
    moran_i <- NA_real_
    p_value <- NA_real_

    if (!is.null(nb) && n_links > 0L) {
      listw <- spdep::nb2listw(nb, style = "W", zero.policy = TRUE)
      mc <- tryCatch(
        spdep::moran.mc(residuals, listw, nsim = nsim,
                        alternative = "greater", zero.policy = TRUE),
        error = function(e) NULL
      )
      if (!is.null(mc)) {
        moran_i <- as.numeric(mc$statistic)
        p_value <- as.numeric(mc$p.value)
      }
    }

    rows[[i]] <- tibble::tibble(
      dist_lo  = lo,
      dist_hi  = hi,
      dist_mid = (lo + hi) / 2,
      moran_i  = moran_i,
      p_value  = p_value,
      n_links  = n_links
    )
  }

  do.call(rbind, rows)
}
