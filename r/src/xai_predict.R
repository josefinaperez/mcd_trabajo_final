# ============================================================
# File: xai_predict.R
# Purpose: Wrapper único de predicción cloglog para maxnet,
#          compartido por las tres técnicas XAI (SHAP, PDP,
#          LIME). Asegura coherencia con Etapas 1-3, que usan
#          el mismo type = "cloglog" y clamp = TRUE.
# ============================================================

suppressPackageStartupMessages({
  library(maxnet)
})

# ------------------------------------------------------------
# predict_fn: invoca maxnet::predict con la firma esperada por
# fastshap (pred_wrapper), pdp (pred.fun) y lime (a través del
# método S3 predict_model.maxnet definido en xai_lime.R).
#
# Args:
#   object : modelo maxnet entrenado
#   newdata: data.frame con las columnas predictoras esperadas
#            por el modelo (mismos nombres que en el training)
#
# Returns:
#   numeric vector con scores cloglog ∈ [0, 1].
# ------------------------------------------------------------

predict_fn <- function(object, newdata) {
  # maxnet::predict con clamp = TRUE hace matrix-indexing sobre
  # newdata; tibble lo rechaza. Forzamos data.frame plano.
  as.numeric(predict(object, newdata = as.data.frame(newdata),
                     type = "cloglog", clamp = TRUE))
}
