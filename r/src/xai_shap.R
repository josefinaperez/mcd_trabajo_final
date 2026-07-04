# ============================================================
# File: xai_shap.R
# Purpose: Cómputo de valores SHAP (muestreo Monte Carlo) vía fastshap
#          y plot resumen (barplot |SHAP| medio + beeswarm).
#          Consume el predict_fn de xai_predict.R.
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(ggplot2)
  library(ggbeeswarm)
  library(patchwork)
  library(fastshap)
})

# ------------------------------------------------------------
# compute_shap: corre KernelSHAP sobre X_explain usando
# X_background como referencia, con el predict_fn provisto.
#
# Args:
#   model       : modelo SDM (maxnet/ranger/xgboost)
#   X_explain   : data.frame de instancias a explicar (solo
#                 columnas predictoras)
#   X_background: data.frame de referencia (solo predictoras)
#   pred_fn     : función predict_fn(model, newdata) → numeric
#   nsim        : nº de simulaciones de fastshap (default 100)
#
# Returns:
#   tibble con una columna por predictor y una fila por
#   instancia de X_explain (los valores SHAP).
# ------------------------------------------------------------

compute_shap <- function(model, X_explain, X_background,
                         pred_fn = predict_fn, nsim = 100) {
  stopifnot(identical(sort(names(X_explain)), sort(names(X_background))))
  set.seed(42)
  # API de fastshap >= 0.1: `X` es la distribución de referencia
  # (background); `newdata` son las instancias a explicar.
  # nrow(shap) == nrow(X_explain).
  shap <- fastshap::explain(
    object       = model,
    X            = as.data.frame(X_background),
    pred_wrapper = pred_fn,
    nsim         = nsim,
    newdata      = as.data.frame(X_explain)
  )
  # fastshap devuelve un objeto con clase 'explain' que hereda
  # de matrix; lo aplanamos a tibble para que downstream
  # (pivot_longer + ggplot) lo procese sin warnings.
  tibble::as_tibble(as.data.frame(unclass(shap)))
}

# ------------------------------------------------------------
# importance_from_shap: ranking de variables por |SHAP| medio.
# ------------------------------------------------------------

importance_from_shap <- function(shap_df) {
  shap_df |>
    tidyr::pivot_longer(everything(),
                        names_to = "variable",
                        values_to = "shap_value") |>
    dplyr::group_by(variable) |>
    dplyr::summarise(mean_abs_shap = mean(abs(shap_value)), .groups = "drop") |>
    dplyr::arrange(dplyr::desc(mean_abs_shap)) |>
    dplyr::mutate(rank = dplyr::row_number())
}

# ------------------------------------------------------------
# plot_shap_summary: panel doble — barplot horizontal de
# |SHAP| medio (izquierda) + beeswarm coloreado por valor
# del feature (derecha). Estilo Lundberg.
#
# Args:
#   shap_df     : tibble de compute_shap()
#   X_explain   : data.frame original (para el color del beeswarm)
#   importance  : tibble de importance_from_shap()
#   run_id      : string para el título del panel
#
# Returns:
#   patchwork ggplot.
# ------------------------------------------------------------

pretty_var <- function(x) gsub("wc2[._]1[._]2[._]5m[._]", "", x)

plot_shap_summary <- function(shap_df, X_explain, importance, run_id) {
  importance <- dplyr::mutate(importance, variable = pretty_var(variable))
  p_bar <- importance |>
    ggplot(aes(x = stats::reorder(variable, mean_abs_shap),
               y = mean_abs_shap)) +
    geom_col() +
    coord_flip() +
    labs(x = NULL, y = "|SHAP| medio",
         title = "Importancia global") +
    theme_minimal(base_size = 9)

  shap_long <- shap_df |>
    dplyr::mutate(.row = dplyr::row_number()) |>
    tidyr::pivot_longer(-".row", names_to = "variable",
                        values_to = "shap_value") |>
    dplyr::mutate(variable = pretty_var(variable))

  feat_long <- X_explain |>
    dplyr::as_tibble() |>
    dplyr::mutate(.row = dplyr::row_number()) |>
    tidyr::pivot_longer(-".row", names_to = "variable",
                        values_to = "feature_value") |>
    dplyr::mutate(variable = pretty_var(variable)) |>
    dplyr::group_by(variable) |>
    dplyr::mutate(feature_scaled = (feature_value - min(feature_value)) /
                                   (max(feature_value) - min(feature_value) + 1e-12)) |>
    dplyr::ungroup()

  bee_df <- dplyr::inner_join(shap_long, feat_long,
                              by = c(".row", "variable"))

  var_order <- importance$variable

  p_bee <- bee_df |>
    dplyr::mutate(variable = factor(variable, levels = rev(var_order))) |>
    ggplot(aes(x = shap_value, y = variable, color = feature_scaled)) +
    ggbeeswarm::geom_quasirandom(groupOnX = FALSE, size = 0.6, alpha = 0.7) +
    scale_color_gradient(low = "#3b4cc0", high = "#b40426",
                         name = "Feature\n(escalado)") +
    geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
    labs(x = "Valor SHAP", y = NULL,
         title = "Distribución de contribuciones") +
    theme_minimal(base_size = 9) +
    theme(legend.position = "right")

  (p_bar | p_bee)
}

# ============================================================
# SHAP local: explicación por instancia sobre puntos elegidos.
# Reutiliza compute_shap() (los valores SHAP ya son por instancia)
# y una selección de puntos agnóstica al método.
# ============================================================

# ------------------------------------------------------------
# select_local_points: arma 4 puntos por run para la explicación
# local.
#
# Reglas:
#   - 1 presencia del test set con score alto (top quintil)
#   - 1 presencia del test set con score bajo (bottom quintil)
#   - 2 puntos diagnósticos del raster (coordenadas fijas por especie)
#
# Args:
#   predictions_test : data.frame con (decimalLongitude,
#                      decimalLatitude, class, score)
#   ds_model_ready   : data.frame con coords + predictoras
#   env_stack        : SpatRaster con los predictores del run
#                      (para extraer features en los puntos diagnósticos)
#   critical_points  : tibble con (point_id, lon, lat, region)
#   model            : modelo SDM del run, para puntuar los puntos
#                      diagnósticos con la idoneidad del modelo
#
# Returns:
#   tibble con: point_id, lon, lat, score, origin, class_observed
#   + las columnas de predictores del run.
# ------------------------------------------------------------

select_local_points <- function(predictions_test, ds_model_ready,
                                 env_stack, critical_points, model) {
  pred_cols <- setdiff(names(ds_model_ready), c("class", "decimalLongitude", "decimalLatitude"))

  # Join de features ANTES de samplear: una fracción de las coords de
  # predictions_test no casa exacto contra el dataset por precisión de floats
  # al escribir los CSV. Se arma el pool de presencias YA matcheadas (orden por
  # score desc) y se samplea 1 alta / 1 baja de ahí.
  pres_feat <- ds_model_ready |>
    dplyr::rename(lon = decimalLongitude, lat = decimalLatitude) |>
    dplyr::select(lon, lat, dplyr::all_of(pred_cols))

  pres_pool <- predictions_test |>
    dplyr::filter(class == 1) |>
    dplyr::rename(lon = decimalLongitude, lat = decimalLatitude,
                  class_observed = class) |>
    dplyr::inner_join(pres_feat, by = c("lon", "lat")) |>
    dplyr::distinct(lon, lat, .keep_all = TRUE) |>
    dplyr::arrange(dplyr::desc(score))

  n_pres <- nrow(pres_pool)
  if (n_pres < 2) {
    stop("select_local_points: faltan presencias con features (", n_pres, ")")
  }

  set.seed(42)
  high_idx <- sample(seq_len(max(1, ceiling(n_pres * 0.2))), size = 1)
  low_idx  <- sample(seq(n_pres - ceiling(n_pres * 0.2) + 1, n_pres), size = 1)

  high_pres <- pres_pool[high_idx, ] |>
    dplyr::mutate(point_id = paste0("pres_high_", dplyr::row_number()),
                  origin   = "presencia_score_alto")
  low_pres <- pres_pool[low_idx, ] |>
    dplyr::mutate(point_id = paste0("pres_low_", dplyr::row_number()),
                  origin   = "presencia_score_bajo")

  pres_sel <- dplyr::bind_rows(high_pres, low_pres)

  # Extraer features de los puntos diagnósticos del raster
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
    stop("select_local_points: faltan columnas tras extract: ",
         paste(missing_cols, collapse = ", "))
  }

  # Idoneidad del modelo en los puntos diagnósticos (misma escala [0,1] que el
  # mapa). Las presencias ya traen su score del set de prueba; los puntos
  # diagnósticos no son presencias, así que se los puntúa con el modelo.
  predict_fn <- make_predict_fn(model)
  crit_ok <- stats::complete.cases(crit_df[, pred_cols, drop = FALSE])
  if (any(crit_ok)) {
    crit_df$score[crit_ok] <- predict_fn(
      model, as.data.frame(crit_df[crit_ok, pred_cols, drop = FALSE]))
  }

  out <- dplyr::bind_rows(
    pres_sel |> dplyr::select(point_id, lon, lat, score, origin,
                              class_observed, dplyr::all_of(pred_cols)),
    crit_df  |> dplyr::select(point_id, lon, lat, score, origin,
                              class_observed, dplyr::all_of(pred_cols))
  )

  # Algunos puntos diagnósticos pueden caer en celdas NA del stack -> features
  # NA que romperían el cálculo. Se descartan esos puntos (no las presencias,
  # que vienen del join del dataset).
  n_before <- nrow(out)
  out <- out |>
    dplyr::filter(!dplyr::if_any(dplyr::all_of(pred_cols), is.na))
  n_dropped <- n_before - nrow(out)
  if (n_dropped > 0) {
    warning("select_local_points: ", n_dropped, " punto(s) diagnóstico(s) en celdas NA ",
            "descartado(s) (features incompletas).")
  }

  out
}

# ------------------------------------------------------------
# geocode_provincia: geocodifica cada punto a su provincia
# (GADM nivel 1) para rotular los subplots con un lugar legible.
# Si falta el shapefile o el paquete sf, devuelve NA.
# ------------------------------------------------------------

geocode_provincia <- function(meta) {
  shp <- "data/shp/argentina/gadm41_ARG_1.shp"
  if (!requireNamespace("sf", quietly = TRUE) || !file.exists(shp)) {
    return(rep(NA_character_, nrow(meta)))
  }
  prov <- sf::st_read(shp, quiet = TRUE)
  pts  <- sf::st_transform(
    sf::st_as_sf(meta, coords = c("lon", "lat"), crs = 4326),
    sf::st_crs(prov))
  idx  <- sf::st_within(pts, prov)
  vapply(idx, function(i) if (length(i)) prov$NAME_1[i[1]] else NA_character_,
         character(1))
}

# ------------------------------------------------------------
# plot_shap_local_panel: 4 subplots (2x2) de contribución SHAP
# por predictor en cada punto. Verde = empuja a presencia
# (SHAP > 0); rojo = empuja a fondo (SHAP < 0). Como SHAP es
# aditivo, se muestran todos los predictores (sin recorte).
#
# Args:
#   shap_local_df : tibble con una columna point_id + una columna
#                   por predictor (valores SHAP por instancia).
#   points        : tibble de select_local_points() (para el título).
#   run_id        : string (no se imprime; se conserva por simetría).
#
# Returns:
#   patchwork ggplot.
# ------------------------------------------------------------

plot_shap_local_panel <- function(shap_local_df, points, run_id) {
  shap_long <- shap_local_df |>
    tidyr::pivot_longer(-"point_id", names_to = "feature",
                        values_to = "shap_value") |>
    dplyr::mutate(feature = pretty_var(feature))

  # Valor de cada predictor en cada punto, para anotar la etiqueta del eje
  # ("bio_11 = 11,5"). Se formatea a 3 cifras significativas, con coma decimal.
  meta_cols <- c("point_id", "lon", "lat", "score", "origin", "class_observed")
  fmt_val   <- function(v) sub("\\.", ",", trimws(formatC(v, format = "fg", digits = 3)))
  feat_long <- points |>
    dplyr::select(point_id, dplyr::all_of(setdiff(names(points), meta_cols))) |>
    tidyr::pivot_longer(-"point_id", names_to = "feature",
                        values_to = "feature_value") |>
    dplyr::mutate(feature = pretty_var(feature))

  shap_long <- shap_long |>
    dplyr::left_join(feat_long, by = c("point_id", "feature")) |>
    dplyr::mutate(label = paste0(feature, " = ", fmt_val(feature_value)))

  meta <- points |> dplyr::select(point_id, origin, score, lon, lat)
  meta$provincia <- geocode_provincia(meta)

  make_subplot <- function(pid) {
    sub <- shap_long |> dplyr::filter(point_id == pid)
    m   <- meta      |> dplyr::filter(point_id == pid)
    titlestr <- if (grepl("^raster_", m$origin)) {
      reg <- sub("^raster_", "", m$origin)
      sc  <- if (!is.na(m$score))
               sprintf(" (idoneidad %s)", sub("\\.", ",", sprintf("%.2f", m$score)))
             else ""
      sprintf("%s\npunto diagnóstico%s", reg, sc)
    } else {
      tipo <- if (grepl("alto", m$origin)) "bien predicha" else "mal predicha"
      loc  <- if (!is.na(m$provincia)) m$provincia
              else sprintf("(%.1f, %.1f)", m$lon, m$lat)
      sprintf("%s\n%s (score %s)", loc, tipo,
              sub("\\.", ",", sprintf("%.2f", m$score)))
    }
    sub |>
      dplyr::mutate(sign = ifelse(shap_value >= 0, "presence", "background")) |>
      ggplot(aes(x = stats::reorder(label, shap_value),
                 y = shap_value, fill = sign)) +
      geom_col() +
      coord_flip() +
      scale_fill_manual(values = c(presence = "#2ca02c",
                                   background = "#d62728"),
                        guide = "none") +
      labs(x = NULL, y = "contribución SHAP",
           title = titlestr) +
      theme_minimal(base_size = 11) +
      theme(plot.title = element_text(size = 11))
  }

  plots <- lapply(points$point_id, make_subplot)
  patchwork::wrap_plots(plots, ncol = 2)
}
