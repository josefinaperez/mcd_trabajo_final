# ============================================================
# File: xai_predict.R
# Purpose: Wrapper de predicción agnóstico al algoritmo,
#          compartido por las tres técnicas XAI (SHAP, PDP,
#          LIME). Cada algoritmo devuelve un score de presencia
#          en [0, 1] con la misma firma (object, newdata), de
#          modo que fastshap, pdp y lime lo consuman sin cambios.
# ============================================================

# ------------------------------------------------------------
# make_predict_fn: factory que despacha por la clase del modelo
# y devuelve una closure predict(object, newdata) -> numeric en
# [0, 1] (probabilidad de presencia).
#
# Algoritmos soportados:
#   - maxnet       : type = "cloglog", clamp = TRUE (igual que
#                    Etapas 1-3, para coherencia).
#   - ranger       : forest de probabilidad; se lee la columna
#                    de la clase "1" (presencia).
#   - xgb.Booster  : objective binary:logistic; predict exige
#                    matriz con las columnas en el orden del
#                    entrenamiento (model$feature_names).
#
# Args:
#   model : modelo entrenado (maxnet / ranger / xgb.Booster).
#
# Returns:
#   function(object, newdata) -> numeric vector en [0, 1].
# ------------------------------------------------------------

make_predict_fn <- function(model) {
  if (inherits(model, "maxnet")) {
    function(object, newdata) {
      # maxnet::predict con clamp = TRUE hace matrix-indexing
      # sobre newdata; tibble lo rechaza. Forzamos data.frame.
      as.numeric(predict(object, newdata = as.data.frame(newdata),
                         type = "cloglog", clamp = TRUE))
    }

  } else if (inherits(model, "ranger")) {
    if (!requireNamespace("ranger", quietly = TRUE)) {
      stop("make_predict_fn: falta el paquete 'ranger'")
    }
    function(object, newdata) {
      pr <- predict(object, data = as.data.frame(newdata))$predictions
      # Forest de probabilidad: matriz con una columna por clase.
      if (is.matrix(pr)) as.numeric(pr[, "1"]) else as.numeric(pr)
    }

  } else if (inherits(model, "xgb.Booster")) {
    if (!requireNamespace("xgboost", quietly = TRUE)) {
      stop("make_predict_fn: falta el paquete 'xgboost'")
    }
    # xgboost < 2 guarda los nombres en $feature_names; la API
    # nueva (>= 3) los expone vía variable.names().
    feats <- model$feature_names
    if (is.null(feats) || length(feats) == 0L) {
      feats <- tryCatch(stats::variable.names(model), error = function(e) NULL)
    }
    if (is.null(feats) || length(feats) == 0L) {
      stop("make_predict_fn: no se pudieron resolver los feature names del ",
           "xgb.Booster; entrenar con una matriz de columnas nombradas")
    }
    function(object, newdata) {
      nd <- as.data.frame(newdata)
      missing <- setdiff(feats, names(nd))
      if (length(missing) > 0) {
        stop("make_predict_fn (xgboost): faltan columnas en newdata: ",
             paste(missing, collapse = ", "))
      }
      m <- as.matrix(nd[, feats, drop = FALSE])
      as.numeric(predict(object, newdata = m))
    }

  } else {
    stop("make_predict_fn: clase de modelo no soportada: ",
         paste(class(model), collapse = "/"))
  }
}

# ------------------------------------------------------------
# predict_fn: wrapper universal retrocompatible. Resuelve el
# algoritmo en cada llamada vía make_predict_fn, de modo que
# el default `pred_fn = predict_fn` de compute_shap() funcione
# para cualquier algoritmo sin tocar el orquestador.
# ------------------------------------------------------------

predict_fn <- function(object, newdata) {
  make_predict_fn(object)(object, newdata)
}
