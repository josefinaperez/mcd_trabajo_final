# ============================================================
# File: xai_pdp.R
# Purpose: Cómputo de curvas Partial Dependence con pdp::partial
#          para cada predictor y plot facet ordenado por
#          importancia SHAP.
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(ggplot2)
  library(pdp)
  library(purrr)
})

# ------------------------------------------------------------
# compute_pdp: corre pdp::partial sobre cada variable de
# predictors y devuelve un data.frame largo (variable, x, yhat).
#
# Args:
#   model            : modelo maxnet
#   X_train          : data.frame con solo columnas predictoras
#   predictors       : character vector con nombres de variables
#   pred_fn          : predict_fn(model, newdata) → numeric
#   grid_resolution  : nº de puntos por variable (default 50)
#
# Returns:
#   tibble largo con columnas variable, x, yhat.
# ------------------------------------------------------------

compute_pdp <- function(model, X_train, predictors,
                        pred_fn = predict_fn,
                        grid_resolution = 50) {
  X <- as.data.frame(X_train)

  # pdp::partial trata un pred.fun que devuelve un vector como
  # ICE (una curva por fila de train) y un pred.fun que devuelve
  # un escalar como PDP (promedio sobre train). Para PDP marginal
  # queremos el promedio.
  pdp_wrapper <- function(object, newdata) {
    mean(pred_fn(object, newdata))
  }

  purrr::map_dfr(predictors, function(v) {
    part <- pdp::partial(
      object          = model,
      pred.var        = v,
      train           = X,
      pred.fun        = pdp_wrapper,
      grid.resolution = grid_resolution,
      plot            = FALSE,
      progress        = FALSE
    )
    tibble::tibble(
      variable = v,
      x        = part[[v]],
      yhat     = part[["yhat"]]
    )
  })
}

# ------------------------------------------------------------
# plot_pdp_grid: facet por variable, ordenado por importancia
# SHAP (top-izquierda = más importante). Eje Y compartido [0,1].
#
# Args:
#   pdp_df     : tibble de compute_pdp()
#   importance : tibble de importance_from_shap() (para orden)
#   run_id     : string para el título
# ------------------------------------------------------------

plot_pdp_grid <- function(pdp_df, importance, run_id) {
  var_order <- importance$variable
  pdp_df |>
    dplyr::mutate(variable = factor(variable, levels = var_order)) |>
    ggplot(aes(x = x, y = yhat)) +
    geom_line(color = "#b40426", linewidth = 0.6) +
    facet_wrap(~ variable, scales = "free_x", ncol = 5) +
    coord_cartesian(ylim = c(0, 1)) +
    labs(x = "Valor de la variable",
         y = "Idoneidad cloglog (PDP)",
         title = paste0("PDP — ", run_id),
         subtitle = "Variables ordenadas por |SHAP| medio") +
    theme_minimal(base_size = 9)
}
