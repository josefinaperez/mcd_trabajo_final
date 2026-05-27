# ============================================================
# File: r/checks/check_predict_fn.R
# Purpose: Chequeo de que la cadena XAI (make_predict_fn + SHAP +
#          PDP + dispatch S3 de LIME) es agnóstica al algoritmo.
#          Entrena modelos triviales de maxnet, ranger y xgboost
#          sobre datos sintéticos y verifica que cada wrapper
#          devuelve scores de presencia en [0, 1] y que SHAP/PDP/
#          LIME corren sin tocar el orquestador.
#
# Pre-requisito de #15 (sumar algoritmos). Ejecutar desde repo root:
#   Rscript r/checks/check_predict_fn.R
# ============================================================

suppressPackageStartupMessages({
  library(maxnet)
  library(ranger)
  library(xgboost)
})

source("r/src/xai_predict.R")
source("r/src/xai_shap.R")
source("r/src/xai_pdp.R")
source("r/src/xai_lime.R")

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

  # (4) PDP corre con el default pred_fn = predict_fn
  pdp_df <- compute_pdp(model, X_train = X, predictors = preds, grid_resolution = 10)
  check(all(preds %in% pdp_df$variable) && in01(pdp_df$yhat),
        sprintf("%s: compute_pdp devuelve yhat en [0,1] para %d vars", algo, length(preds)))

  # (5) Dispatch S3 de LIME: predict_model devuelve df presence/background
  pm <- predict_model(model, newdata = newX, type = "prob")
  check(is.data.frame(pm) && all(c("presence", "background") %in% names(pm)) &&
          in01(pm$presence) && isTRUE(all.equal(pm$presence + pm$background, rep(1, nrow(pm)))),
        sprintf("%s: lime predict_model -> presence+background = 1", algo))
}

cat("\n")
if (length(fail) == 0) {
  cat("TODO OK — la cadena XAI es agnóstica al algoritmo.\n")
} else {
  cat(sprintf("%d CHEQUEO(S) FALLARON:\n", length(fail)))
  cat(paste0("  - ", fail, collapse = "\n"), "\n")
  quit(status = 1)
}
