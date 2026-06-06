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

# ------------------------------------------------------------
# 3) SCORING DE UN COMBO  (TSS pooled sobre folds internos)
# ------------------------------------------------------------
# Entrena un modelo por fold (train = resto), concatena las
# predicciones de test de los folds evaluables y computa el TSS
# en el umbral de Youden sobre ese pool (mismo criterio que el
# reporte externo). Un fold con < 5 presencias en train o sin
# ambas clases en test se omite. Con < 2 folds evaluables -> NA.
score_hp_combo <- function(algo, ds, fold_id, hp, seed = 42) {
  k <- max(fold_id)

  fold_preds <- purrr::map(seq_len(k), function(f) {
    test_mask  <- fold_id == f
    train_mask <- !test_mask
    if (sum(ds$y[train_mask] == 1L) < 5L ||
        length(unique(ds$y[test_mask])) < 2L) {
      return(NULL)
    }
    model_f  <- fit_model(algo,
                          ds$X[train_mask, , drop = FALSE],
                          ds$y[train_mask],
                          hp = hp, seed = seed)
    scores_f <- predict_score(model_f, ds$X[test_mask, , drop = FALSE])
    tibble::tibble(class = ds$y[test_mask], score = scores_f)
  })

  evaluable <- purrr::compact(fold_preds)
  if (length(evaluable) < 2L) return(NA_real_)

  pooled <- dplyr::bind_rows(evaluable)
  if (length(unique(pooled$class)) < 2L) return(NA_real_)

  compute_dual_metrics(pooled$class, pooled$score)$tss
}

# ------------------------------------------------------------
# 4) TUNEO SOBRE LA GRILLA
# ------------------------------------------------------------
# Devuelve list(best_hp, scores_table, best_tss). Desempate: mayor
# TSS, luego menor complexity_rank (modelo más simple). Si ningún
# combo es evaluable, cae a ALGO_DEFAULTS con un warning.
tune_hp_for_dataset <- function(algo, ds, fold_id, grid = NULL, seed = 42) {
  if (is.null(grid)) grid <- build_hp_grid(algo, length(ds$predictors))

  tss <- vapply(grid, function(hp) score_hp_combo(algo, ds, fold_id, hp, seed = seed),
                numeric(1))

  scores_table <- purrr::map2_dfr(grid, tss, function(hp, t)
    dplyr::mutate(tibble::as_tibble(hp), tss = t))

  if (all(is.na(tss))) {
    warning("tune_hp_for_dataset: ningún combo evaluable para ", algo,
            "; se usan ALGO_DEFAULTS.")
    return(list(best_hp = ALGO_DEFAULTS[[algo]],
                scores_table = scores_table, best_tss = NA_real_))
  }

  best_tss <- max(tss, na.rm = TRUE)
  cand     <- which(!is.na(tss) & tss == best_tss)
  ranks    <- vapply(cand, function(i) complexity_rank(algo, grid[[i]]), numeric(1))
  best_idx <- cand[which.min(ranks)]

  list(best_hp = grid[[best_idx]], scores_table = scores_table, best_tss = best_tss)
}

# ------------------------------------------------------------
# 5) CONVERSORES hp <-> fila de best_hp.csv
# ------------------------------------------------------------
# hp_to_row: arma una fila con TODAS las HP_PARAM_COLS (NA donde no
# aplica al algoritmo) + metadatos. hp_row_to_list: reconstruye la
# lista hp tomando sólo las columnas no-NA. train_pipeline.R reusa
# hp_row_to_list para pasar los HP a fit_model.
hp_to_row <- function(run_id, algorithm, best_hp,
                      best_tss, n_combos, k_internal) {
  get <- function(nm, na_val) if (!is.null(best_hp[[nm]])) best_hp[[nm]] else na_val
  tibble::tibble(
    run_id            = run_id,
    algorithm         = algorithm,
    regmult           = get("regmult",       NA_real_),
    classes           = get("classes",       NA_character_),
    num.trees         = get("num.trees",     NA_integer_),
    mtry              = get("mtry",          NA_integer_),
    min.node.size     = get("min.node.size", NA_integer_),
    max_depth         = get("max_depth",     NA_integer_),
    learning_rate     = get("learning_rate", NA_real_),
    nrounds           = get("nrounds",       NA_integer_),
    best_internal_tss = best_tss,
    n_combos          = n_combos,
    k_internal        = k_internal
  )
}

hp_row_to_list <- function(row) {
  cols <- intersect(HP_PARAM_COLS, names(row))
  vals <- as.list(row[1, cols, drop = FALSE])
  vals[!vapply(vals, function(v) length(v) == 0L || is.na(v), logical(1))]
}
