# ============================================================
# File: r/checks/check_predict_fn.R
# Purpose: Chequeo de que la cadena XAI (make_predict_fn + SHAP
#          global + SHAP local) es agnóstica al algoritmo. Entrena
#          modelos triviales de maxnet, ranger y xgboost sobre datos
#          sintéticos y verifica que cada wrapper devuelve scores de
#          presencia en [0, 1] y que SHAP (global y local) corre sin
#          tocar el orquestador.
#
# Ejecutar desde repo root:
#   Rscript r/checks/check_predict_fn.R
# ============================================================

suppressPackageStartupMessages({
  library(maxnet)
  library(ranger)
  library(xgboost)
})

source("r/src/xai_predict.R")
source("r/src/xai_shap.R")

set.seed(42)

# ---- Datos sintéticos: 2 predictores con señal binaria ----
n <- 300L
preds <- c("v1", "v2")
X <- data.frame(v1 = rnorm(n), v2 = rnorm(n))
lin <- 1.5 * X$v1 - 1.0 * X$v2
y <- rbinom(n, 1, plogis(lin))            # clase 0/1
df <- cbind(class = y, X)

# ---- Entrenar un modelo trivial por algoritmo ----
m_maxnet <- maxnet::maxnet(p = y, data = X)

m_ranger <- ranger::ranger(
  formula     = class ~ .,
  data        = transform(df, class = factor(class)),
  probability = TRUE,
  num.trees   = 100
)

# API nueva de xgboost (>= 3): x/y, y factor dispara clasificación
# binaria; predict devuelve P(clase positiva = "1").
m_xgb <- xgboost::xgboost(
  x = as.matrix(X), y = factor(y), nrounds = 10
)

models <- list(maxnet = m_maxnet, ranger = m_ranger, xgboost = m_xgb)

# ---- Helper de aserción ----
fail <- character(0)
check <- function(cond, msg) {
  status <- if (isTRUE(cond)) "OK  " else { fail <<- c(fail, msg); "FAIL" }
  cat(sprintf("  [%s] %s\n", status, msg))
}
in01 <- function(v) is.numeric(v) && all(is.finite(v)) && all(v >= 0 & v <= 1)

newX <- X[1:20, , drop = FALSE]

for (algo in names(models)) {
  cat("== ", algo, " ==\n", sep = "")
  model <- models[[algo]]

  # (1) make_predict_fn -> scores en [0,1]
  pf <- make_predict_fn(model)
  s  <- pf(model, newX)
  check(length(s) == nrow(newX) && in01(s),
        sprintf("%s: make_predict_fn devuelve %d scores en [0,1]", algo, nrow(newX)))

  # (2) predict_fn universal coincide con make_predict_fn
  check(isTRUE(all.equal(s, predict_fn(model, newX))),
        sprintf("%s: predict_fn universal == make_predict_fn", algo))

  # (3) SHAP corre con el default pred_fn = predict_fn
  shap <- compute_shap(model, X_explain = newX, X_background = X[1:50, ], nsim = 10)
  check(nrow(shap) == nrow(newX) && identical(sort(names(shap)), sort(preds)),
        sprintf("%s: compute_shap devuelve %d filas × %d vars", algo, nrow(newX), length(preds)))

  # (4) SHAP local: compute_shap sobre 4 puntos + panel sin error
  pts_local <- data.frame(
    point_id       = paste0("p", 1:4),
    lon            = c(-58.4, -60.5, -59.5, -67.7),
    lat            = c(-34.6, -32.5, -35.5, -53.8),
    score          = c(0.9, 0.2, NA, NA),
    origin         = c("presencia_score_alto", "presencia_score_bajo",
                       "raster_Pampa", "raster_Patagonia"),
    class_observed = c(1L, 1L, NA, NA)
  )
  pts_local <- cbind(pts_local, X[1:4, , drop = FALSE])
  shap_loc  <- compute_shap(model, X_explain = pts_local[, preds],
                            X_background = X[1:50, ], nsim = 10)
  shap_loc$point_id <- pts_local$point_id
  p_local <- plot_shap_local_panel(shap_loc, pts_local, run_id = "test")
  check(inherits(p_local, c("patchwork", "gg", "ggplot")),
        sprintf("%s: plot_shap_local_panel devuelve un ggplot", algo))
}

cat("\n")
if (length(fail) == 0) {
  cat("TODO OK — la cadena XAI es agnóstica al algoritmo.\n")
} else {
  cat(sprintf("%d CHEQUEO(S) FALLARON:\n", length(fail)))
  cat(paste0("  - ", fail, collapse = "\n"), "\n")
  quit(status = 1)
}
