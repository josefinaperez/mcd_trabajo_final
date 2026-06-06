# ============================================================
# File: tune_models.R
# Purpose: Funciones PURAS para el tuneo de hiperparámetros (#8).
#          Construyen la grilla por algoritmo, puntúan un combo por
#          TSS pooled sobre folds espaciales internos, eligen el
#          mejor (desempate al modelo más simple) y convierten entre
#          la lista `hp` y una fila de best_hp.csv. Sin I/O: la
#          persistencia la maneja tune_pipeline.R.
#
#          Reusa fit_model/predict_score (train_models.R) y
#          compute_dual_metrics (evaluate_model.R): no hay modelado
#          nuevo, sólo orquestación sobre la grilla.
# ============================================================

source("r/src/train_models.R")    # fit_model, predict_score, SUPPORTED_ALGOS, ALGO_DEFAULTS
source("r/src/evaluate_model.R")   # compute_dual_metrics

suppressPackageStartupMessages({
  library(dplyr)
  library(tibble)
  library(purrr)
})

# Columnas de hiperparámetros que viven en best_hp.csv (unión de todos los algos).
HP_PARAM_COLS <- c("regmult", "classes", "num.trees", "mtry", "min.node.size",
                   "max_depth", "learning_rate", "nrounds")

# ------------------------------------------------------------
# 1) GRILLA DE HP POR ALGORITMO  (amplitud "Standard", ~12 combos)
# ------------------------------------------------------------
# Devuelve una lista de combos (cada uno una lista `hp`). `mtry`
# (ranger) es relativo al nº de predictores del dataset, así que
# puede colapsar a menos combos cuando floor(sqrt(p)) == floor(p/3).
build_hp_grid <- function(algo, n_predictors) {
  algo <- match.arg(algo, SUPPORTED_ALGOS)

  if (algo == "maxnet") {
    grid <- expand.grid(regmult = c(0.5, 1, 2, 4),
                        classes = c("lq", "lqh", "lqhp"),
                        stringsAsFactors = FALSE)
    purrr::pmap(grid, function(regmult, classes)
      list(regmult = regmult, classes = classes))

  } else if (algo == "ranger") {
    mtry_vals <- sort(unique(pmax(1L, as.integer(
      c(floor(sqrt(n_predictors)), floor(n_predictors / 3))))))
    grid <- expand.grid(num.trees     = c(500L, 1000L),
                        mtry          = mtry_vals,
                        min.node.size = c(1L, 5L, 10L),
                        stringsAsFactors = FALSE)
    purrr::pmap(grid, function(num.trees, mtry, min.node.size)
      list(num.trees = num.trees, mtry = mtry, min.node.size = min.node.size))

  } else { # xgboost
    grid <- expand.grid(max_depth     = c(3L, 6L),
                        learning_rate = c(0.05, 0.1, 0.3),
                        nrounds       = c(200L, 400L),
                        stringsAsFactors = FALSE)
    purrr::pmap(grid, function(max_depth, learning_rate, nrounds)
      list(max_depth = max_depth, learning_rate = learning_rate, nrounds = nrounds))
  }
}

# ------------------------------------------------------------
# 2) RANKING DE COMPLEJIDAD  (menor = más simple = preferido en empate)
# ------------------------------------------------------------
# "Más simple" = más regularizado: en maxnet mayor regmult y menos
# clases; en ranger hojas más grandes (min.node.size), menor mtry y
# menos árboles; en xgboost menor profundidad y menos rounds.
complexity_rank <- function(algo, hp) {
  algo <- match.arg(algo, SUPPORTED_ALGOS)
  if (algo == "maxnet") {
    cls <- c(lq = 1, lqh = 2, lqhp = 3)[[hp$classes]]
    cls * 10 - hp$regmult
  } else if (algo == "ranger") {
    hp$mtry * 100 + hp$num.trees / 500 - hp$min.node.size
  } else { # xgboost
    hp$max_depth * 1000 + hp$nrounds
  }
}
