# ============================================================
# File: xai_lime.R
# Purpose: LIME para maxnet — incluye:
#          (1) métodos S3 para que lime reconozca maxnet como
#              clasificador binario,
#          (2) select_lime_points() para elegir las instancias
#              a explicar (presencias por cuantil + puntos
#              críticos del raster),
#          (3) compute_lime() y plot_lime_panel().
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(ggplot2)
  library(patchwork)
  library(lime)
  library(terra)
})

# ------------------------------------------------------------
# (1) S3 methods para que lime trate a maxnet como
#     clasificador binario con dos clases (presence / background).
#
# lime::model_type() y lime::predict_model() son los dos hooks
# necesarios; predict_model devuelve un data.frame con una
# columna por clase y probabilidades.
# ------------------------------------------------------------

model_type.maxnet <- function(x, ...) "classification"

predict_model.maxnet <- function(x, newdata, type, ...) {
  pred <- predict_fn(x, newdata)
  data.frame(presence   = pred,
             background = 1 - pred,
             check.names = FALSE)
}

# Registrar globalmente para que lime los encuentre vía S3
# dispatch incluso si no se exporta el namespace.
.S3method("model_type",    "maxnet", model_type.maxnet)
.S3method("predict_model", "maxnet", predict_model.maxnet)

# ------------------------------------------------------------
# (2) select_lime_points: arma 8 puntos por run.
#
# Reglas:
#   - 3 presencias del test set con score alto (top quintil)
#   - 3 presencias del test set con score bajo (bottom quintil)
#   - 2 puntos críticos del raster (coordenadas fijas por especie)
#
# Args:
#   predictions_test : data.frame con (decimalLongitude,
#                      decimalLatitude, class, score)
#   ds_model_ready   : data.frame con coords + predictoras
#                      (todas las filas del dataset)
#   env_stack        : SpatRaster con las 19 BIO (para extraer
#                      features en los puntos críticos)
#   critical_points  : tibble con (point_id, lon, lat, region)
#                      por especie
#
# Returns:
#   tibble con columnas: point_id, lon, lat, score, origin,
#   class_observed + las 19 columnas wc2.1_30s_bio_*.
# ------------------------------------------------------------

select_lime_points <- function(predictions_test, ds_model_ready,
                               env_stack, critical_points) {
  pred_cols <- grep("^wc2.1_30s_bio_", names(ds_model_ready), value = TRUE)

  pres_test <- predictions_test |>
    dplyr::filter(class == 1) |>
    dplyr::arrange(dplyr::desc(score))

  n_pres <- nrow(pres_test)
  if (n_pres < 6) {
    stop("select_lime_points: faltan presencias en test (", n_pres, ")")
  }

  set.seed(42)
  high_idx <- sample(seq_len(max(1, ceiling(n_pres * 0.2))), size = 3)
  low_idx  <- sample(seq(n_pres - ceiling(n_pres * 0.2) + 1, n_pres), size = 3)

  high_pres <- pres_test[high_idx, ] |>
    dplyr::mutate(point_id = paste0("pres_high_", dplyr::row_number()),
                  origin   = "presencia_score_alto")
  low_pres <- pres_test[low_idx, ] |>
    dplyr::mutate(point_id = paste0("pres_low_", dplyr::row_number()),
                  origin   = "presencia_score_bajo")

  pres_sel <- dplyr::bind_rows(high_pres, low_pres) |>
    dplyr::rename(lon = decimalLongitude, lat = decimalLatitude,
                  class_observed = class)

  pres_feat <- ds_model_ready |>
    dplyr::rename(lon = decimalLongitude, lat = decimalLatitude) |>
    dplyr::select(lon, lat, dplyr::all_of(pred_cols))

  pres_sel <- pres_sel |>
    dplyr::inner_join(pres_feat, by = c("lon", "lat"))

  if (nrow(pres_sel) != 6) {
    stop("select_lime_points: join de features falló (esperaba 6, hay ",
         nrow(pres_sel), ")")
  }

  # Extraer features de los puntos críticos del raster
  crit_xy   <- as.matrix(critical_points[, c("lon", "lat")])
  crit_vals <- terra::extract(env_stack, crit_xy)
  crit_df <- tibble::tibble(
    point_id       = critical_points$point_id,
    lon            = critical_points$lon,
    lat            = critical_points$lat,
    score          = NA_real_,
    origin         = paste0("raster_", critical_points$region),
    class_observed = NA_integer_
  ) |>
    dplyr::bind_cols(tibble::as_tibble(crit_vals))

  missing_cols <- setdiff(pred_cols, names(crit_df))
  if (length(missing_cols) > 0) {
    stop("select_lime_points: faltan columnas tras extract: ",
         paste(missing_cols, collapse = ", "))
  }

  out <- dplyr::bind_rows(
    pres_sel |> dplyr::select(point_id, lon, lat, score, origin,
                              class_observed, dplyr::all_of(pred_cols)),
    crit_df  |> dplyr::select(point_id, lon, lat, score, origin,
                              class_observed, dplyr::all_of(pred_cols))
  )

  if (anyNA(out |> dplyr::select(dplyr::all_of(pred_cols)))) {
    warning("select_lime_points: algún punto crítico cae fuera del raster (NA en features). ",
            "Revisar coordenadas.")
  }

  out
}
