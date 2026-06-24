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

plot_shap_summary <- function(shap_df, X_explain, importance, run_id) {
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
                        values_to = "shap_value")

  feat_long <- X_explain |>
    dplyr::as_tibble() |>
    dplyr::mutate(.row = dplyr::row_number()) |>
    tidyr::pivot_longer(-".row", names_to = "variable",
                        values_to = "feature_value") |>
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

  (p_bar | p_bee) +
    patchwork::plot_annotation(
      title = paste0("SHAP — ", run_id),
      theme = ggplot2::theme(plot.title = element_text(size = 11))
    )
}
