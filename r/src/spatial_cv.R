# ============================================================
# File: spatial_cv.R
# Purpose: Particiones espaciales para evaluación honesta de
#          SDMs (Roberts et al. 2017, Valavi et al. 2019).
#          Provee dos primitivas:
#            - calibrate_block_size(): estima el rango de
#              autocorrelación espacial de las variables
#              ambientales, base para fijar el tamaño del bloque.
#            - assign_spatial_folds(): asigna cada punto del
#              sdm_dataset a un fold (1..k) sobre una grilla
#              proyectada en equal-area Argentina.
#
#          Las funciones son puras: leen, transforman, devuelven.
#          La persistencia (folds.csv, plot de bloques) la maneja
#          el orquestador en train_pipeline.R.
# ============================================================

suppressPackageStartupMessages({
  library(sf)
  library(terra)
  library(blockCV)
  library(dplyr)
  library(tibble)
})

# CRS equal-area centrada en Argentina (Albers).
# Misma que usa grid_thinning.R; las distancias salen en metros.
AEA_ARGENTINA <- paste0(
  "+proj=aea +lat_1=-5 +lat_2=-42 +lat_0=-32 +lon_0=-60 ",
  "+datum=WGS84 +units=m +no_defs"
)

# ------------------------------------------------------------
# 1) CALIBRACIÓN DEL TAMAÑO DEL BLOQUE
# ------------------------------------------------------------
#
# Estima el rango de autocorrelación espacial de las variables
# bioclimáticas (variograma empírico por variable, mediana del
# range) y elige un tamaño de bloque acotado por
# `size_cap_km`. El cap existe porque las BIO de WorldClim a
# escala continental tienen rangos de autocorrelación enormes
# (cientos a miles de km, dominados por gradientes latitudinales
# de temperatura): si el bloque iguala al range, en Argentina
# (~3700 × 1500 km) caben tan pocos bloques que no se pueden
# armar k folds balanceados. Tomar el rango más bajo "realista"
# (excluyendo variogramas no convergidos) y caparlo es el
# trade-off estándar en SDM block-CV (Roberts et al. 2017,
# Valavi et al. 2019).
#
# Devuelve:
#   size_m           : tamaño efectivo a usar para los bloques (m)
#   autocor_range_m  : range usado para derivarlo (lowest > 50km)
#   range_table      : tabla por variable (range, sill, nugget)
#   capped           : TRUE si autocor_range_m > size_cap_km*1000
# ------------------------------------------------------------

calibrate_block_size <- function(env_raster_paths,
                                 argentina_shp  = "data/shp/argentina/argentina.shp",
                                 num_sample     = 1000L,
                                 aggregate_fact = 10L,
                                 size_cap_km    = 300L,
                                 seed           = 42L) {
  set.seed(seed)

  r <- terra::rast(env_raster_paths)

  # 1) Recortar al bbox de Argentina: los WorldClim vienen globales.
  arg <- sf::st_read(argentina_shp, quiet = TRUE)
  arg_bbox <- sf::st_bbox(arg)
  r <- terra::crop(r, terra::ext(arg_bbox["xmin"], arg_bbox["xmax"],
                                 arg_bbox["ymin"], arg_bbox["ymax"]))

  # 2) Agregar resolución: a 30s nativo, el variograma sobre 19 capas
  #    es O(n²) y se vuelve impracticable. Aggregate por factor 10
  #    pasa de ~1 km a ~10 km — suficiente para estimar el range
  #    de autocorrelación de variables climáticas.
  if (aggregate_fact > 1L) {
    r <- terra::aggregate(r, fact = aggregate_fact, fun = "mean", na.rm = TRUE)
  }

  ac <- blockCV::cv_spatial_autocor(
    r          = r,
    num_sample = num_sample,
    plot       = FALSE,
    progress   = FALSE
  )

  range_table <- tibble::as_tibble(ac$range_table) |>
    dplyr::arrange(range)

  # Tomar el menor rango "realista": excluimos variogramas que
  # explotan (range > 5000 km es claramente no convergido) y
  # los que dan ~0 (range < 50 km, ruido). El menor de los
  # plausibles minimiza la correlación residual entre folds.
  plausible <- range_table$range[
    range_table$range > 50e3 & range_table$range < 5e6
  ]
  autocor_range_m <- if (length(plausible) > 0L) {
    min(plausible)
  } else {
    as.numeric(ac$range)
  }

  size_cap_m <- size_cap_km * 1000
  size_m     <- min(autocor_range_m, size_cap_m)

  list(
    size_m          = size_m,
    autocor_range_m = autocor_range_m,
    range_table     = range_table,
    capped          = autocor_range_m > size_cap_m
  )
}

# ------------------------------------------------------------
# 2) ASIGNACIÓN DE FOLDS
# ------------------------------------------------------------
#
# Construye una grilla regular de bloques cuadrados de lado
# `size_m` sobre el bbox de los puntos proyectados a AEA
# Argentina, y asigna cada bloque a uno de los k folds. Cada
# punto hereda el fold de su bloque. La asignación de bloques
# a folds se hace por selección aleatoria con `iteration`
# intentos, eligiendo la configuración que minimiza el
# desbalance de presencias entre folds.
#
# Devuelve:
#   fold_id : vector entero (1..k) de largo nrow(df)
#   blocks  : sf con la grilla de bloques + columna folds
#   size_m  : tamaño usado, metros
#   k       : número de folds
# ------------------------------------------------------------

assign_spatial_folds <- function(df,
                                 size_m,
                                 k         = 5L,
                                 seed      = 42L,
                                 iteration = 50L) {
  stopifnot(all(c("decimalLongitude", "decimalLatitude", "class") %in% names(df)))

  points_sf <- sf::st_as_sf(
    df |> mutate(.row_id = seq_len(n())),
    coords = c("decimalLongitude", "decimalLatitude"),
    crs    = 4326,
    remove = FALSE
  )

  set.seed(seed)

  sb <- blockCV::cv_spatial(
    x          = points_sf,
    column     = "class",
    k          = k,
    size       = size_m,
    hexagon    = FALSE,
    selection  = "random",
    iteration  = iteration,
    seed       = seed,
    biomod2    = FALSE,
    progress   = FALSE,
    report     = FALSE,
    plot       = FALSE
  )

  list(
    fold_id = as.integer(sb$folds_ids),
    blocks  = sb$blocks,
    size_m  = size_m,
    k       = as.integer(k)
  )
}

# ------------------------------------------------------------
# 3) PLOT DE VALIDACIÓN
# ------------------------------------------------------------
#
# Mapa de Argentina + bloques coloreados por fold + presencias.
# Permite verificar visualmente que ningún fold quedó
# concentrado en una sola ecorregión.
# ------------------------------------------------------------

plot_spatial_folds <- function(blocks,
                               df,
                               fold_id,
                               argentina_shp = "data/shp/argentina/argentina.shp") {
  suppressPackageStartupMessages({
    library(ggplot2)
    library(sf)
  })

  arg <- sf::st_read(argentina_shp, quiet = TRUE) |>
    sf::st_transform(sf::st_crs(blocks))

  pres <- df |>
    mutate(fold_id = factor(fold_id)) |>
    filter(class == 1L) |>
    sf::st_as_sf(coords = c("decimalLongitude", "decimalLatitude"), crs = 4326) |>
    sf::st_transform(sf::st_crs(blocks))

  blocks_plot <- blocks |>
    mutate(fold_id = factor(folds))

  ggplot() +
    geom_sf(data = arg, fill = "grey95", color = "grey60", linewidth = 0.2) +
    geom_sf(data = blocks_plot, aes(fill = fold_id),
            color = "white", linewidth = 0.1, alpha = 0.55) +
    geom_sf(data = pres, aes(color = fold_id), size = 0.7, alpha = 0.85) +
    scale_fill_brewer(palette = "Set2", name = "Fold (bloque)") +
    scale_color_brewer(palette = "Set2", name = "Fold (presencia)") +
    labs(title = "Spatial-block CV: bloques y presencias por fold",
         subtitle = "Equal-area Argentina (AEA)") +
    theme_minimal(base_size = 10) +
    theme(legend.position = "bottom")
}
