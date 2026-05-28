# ============================================================
# File: xai_lime.R
# Purpose: LIME para modelos SDM (maxnet/ranger/xgboost) — incluye:
#          (1) métodos S3 para que lime reconozca cada modelo como
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
# (1) S3 methods para que lime trate a cada modelo como
#     clasificador binario (presence / background).
#
# lime::model_type() y lime::predict_model() son los dos hooks
# necesarios; predict_model devuelve un data.frame con una
# columna por clase y probabilidades. Ambos delegan en
# make_predict_fn() (xai_predict.R), así que el comportamiento
# es idéntico para maxnet, ranger y xgboost.
# ------------------------------------------------------------

.lime_predict_df <- function(x, newdata) {
  pred <- make_predict_fn(x)(x, newdata)
  data.frame(presence   = pred,
             background = 1 - pred,
             check.names = FALSE)
}

model_type.maxnet      <- function(x, ...) "classification"
model_type.ranger      <- function(x, ...) "classification"
model_type.xgb.Booster <- function(x, ...) "classification"

predict_model.maxnet      <- function(x, newdata, type, ...) .lime_predict_df(x, newdata)
predict_model.ranger      <- function(x, newdata, type, ...) .lime_predict_df(x, newdata)
predict_model.xgb.Booster <- function(x, newdata, type, ...) .lime_predict_df(x, newdata)

# Registrar globalmente para que lime los encuentre vía S3
# dispatch incluso si no se exporta el namespace. Sobreescribe
# cualquier método propio de lime para estas clases, garantizando
# la normalización a score de presencia en [0, 1].
.S3method("model_type",    "maxnet",      model_type.maxnet)
.S3method("model_type",    "ranger",      model_type.ranger)
.S3method("model_type",    "xgb.Booster", model_type.xgb.Booster)
.S3method("predict_model", "maxnet",      predict_model.maxnet)
.S3method("predict_model", "ranger",      predict_model.ranger)
.S3method("predict_model", "xgb.Booster", predict_model.xgb.Booster)

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

# ------------------------------------------------------------
# (3) compute_lime: corre lime sobre los puntos elegidos.
#
# Args:
#   model          : modelo SDM (maxnet/ranger/xgboost)
#   points         : tibble de select_lime_points()
#   X_train        : data.frame con las columnas predictoras
#                    (entrenamiento del modelo)
#   n_features     : nº de features locales por punto (default 5)
#   n_permutations : default 5000
#
# Returns:
#   tibble largo: point_id, feature, weight, feature_value.
# ------------------------------------------------------------

compute_lime <- function(model, points, X_train,
                         n_features = 5, n_permutations = 5000) {
  pred_cols <- names(X_train)
  set.seed(42)
  explainer <- lime::lime(
    x              = as.data.frame(X_train),
    model          = model,
    bin_continuous = TRUE,
    n_bins         = 4
  )

  X_explain <- as.data.frame(points |> dplyr::select(dplyr::all_of(pred_cols)))
  rownames(X_explain) <- points$point_id

  set.seed(42)
  expl <- lime::explain(
    x              = X_explain,
    explainer      = explainer,
    n_features     = n_features,
    n_permutations = n_permutations,
    labels         = "presence"
  )

  tibble::as_tibble(expl) |>
    dplyr::transmute(
      point_id      = case,
      feature       = feature,
      weight        = feature_weight,
      feature_value = feature_value
    )
}

# ------------------------------------------------------------
# (4) plot_lime_panel: 8 subplots, barras horizontales de pesos.
#     Verde = empuja a presence; rojo = empuja a background.
# ------------------------------------------------------------

plot_lime_panel <- function(lime_df, points, run_id) {
  meta <- points |> dplyr::select(point_id, origin, score, lon, lat)

  make_subplot <- function(pid) {
    sub <- lime_df |> dplyr::filter(point_id == pid)
    m   <- meta   |> dplyr::filter(point_id == pid)
    titlestr <- if (is.na(m$score)) {
      sprintf("%s\n(%.2f, %.2f) — %s", pid, m$lon, m$lat, m$origin)
    } else {
      sprintf("%s\nscore=%.2f — %s", pid, m$score, m$origin)
    }
    sub |>
      dplyr::mutate(sign = ifelse(weight >= 0, "presence", "background")) |>
      ggplot(aes(x = stats::reorder(feature, weight),
                 y = weight, fill = sign)) +
      geom_col() +
      coord_flip() +
      scale_fill_manual(values = c(presence = "#2ca02c",
                                   background = "#d62728"),
                        guide = "none") +
      labs(x = NULL, y = "peso LIME",
           title = titlestr) +
      theme_minimal(base_size = 8) +
      theme(plot.title = element_text(size = 8))
  }

  plots <- lapply(points$point_id, make_subplot)
  patchwork::wrap_plots(plots, ncol = 4) +
    patchwork::plot_annotation(
      title = paste0("LIME — ", run_id),
      theme = ggplot2::theme(plot.title = element_text(size = 11))
    )
}
