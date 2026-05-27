# ============================================================
# File: train_models.R
# Purpose: Entrenar un SDM sobre un dataset model-ready y persistir
#          artefactos (modelo, predicciones test, métricas, curvas
#          de respuesta). Agnóstico al algoritmo: maxnet, ranger
#          (Random Forest) y xgboost comparten la misma lógica de
#          holdout y spatial-block CV; lo único que cambia es
#          fit_model() (entrenamiento) y la predicción, que se
#          delega en make_predict_fn() de xai_predict.R.
# ============================================================

source("r/src/xai_predict.R")  # make_predict_fn (score de presencia [0,1])

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tibble)
  library(tidyr)
  library(purrr)
  library(ggplot2)
  library(maxnet)
  library(pROC)
})

COORD_COLS <- c("decimalLongitude", "decimalLatitude")
CLASS_COL  <- "class"

# Hiperparámetros por defecto por algoritmo. NULL = dejar el
# default de la librería. fit_model() mergea `hp` sobre estos.
ALGO_DEFAULTS <- list(
  maxnet  = list(regmult = 1, classes = NULL),
  ranger  = list(num.trees = 1000L, mtry = NULL, min.node.size = NULL),
  xgboost = list(nrounds = 300L, max_depth = 4L,
                 learning_rate = 0.1, min_child_weight = 1)
)
SUPPORTED_ALGOS <- names(ALGO_DEFAULTS)

# ------------------------------------------------------------
# 1) DATA LOADING
# ------------------------------------------------------------

load_model_ready_dataset <- function(path, predictor_cols = NULL) {
  df <- readr::read_csv(path, show_col_types = FALSE)

  required <- c(CLASS_COL, COORD_COLS)
  missing_required <- setdiff(required, names(df))
  if (length(missing_required) > 0) {
    stop(
      "Missing required column(s) in dataset (", path, "): ",
      paste(missing_required, collapse = ", ")
    )
  }

  if (is.null(predictor_cols)) {
    predictor_cols <- setdiff(names(df), required)
    if (length(predictor_cols) == 0) {
      stop("No predictor columns found in dataset (", path, ").")
    }
  } else {
    missing_pred <- setdiff(predictor_cols, names(df))
    if (length(missing_pred) > 0) {
      stop(
        "Missing predictor column(s) in dataset (", path, "): ",
        paste(missing_pred, collapse = ", ")
      )
    }
  }

  df <- df |>
    select(all_of(c(COORD_COLS, CLASS_COL, predictor_cols))) |>
    drop_na(all_of(predictor_cols))

  list(
    X          = df |> select(all_of(predictor_cols)),
    y          = as.integer(df[[CLASS_COL]]),
    coords     = df |> select(all_of(COORD_COLS)),
    predictors = predictor_cols
  )
}

# ------------------------------------------------------------
# 2) STRATIFIED HOLD-OUT SPLIT
# ------------------------------------------------------------

stratified_split <- function(y, p_train = 0.7, seed = 42) {
  set.seed(seed)

  train_idx <- tibble(idx = seq_along(y), cls = y) |>
    group_by(cls) |>
    slice_sample(prop = p_train) |>
    pull(idx)

  list(
    train = sort(train_idx),
    test  = setdiff(seq_along(y), train_idx)
  )
}

# ------------------------------------------------------------
# 3) TRAIN / PREDICT
# ------------------------------------------------------------

# fit_model: entrena `algo` sobre (X, y) con hiperparámetros `hp`
# (mergeados sobre ALGO_DEFAULTS). Devuelve el objeto nativo de
# cada librería; su clase guía a make_predict_fn() y a LIME.
fit_model <- function(algo, X, y, hp = list(), seed = 42) {
  algo <- match.arg(algo, SUPPORTED_ALGOS)
  hp   <- utils::modifyList(ALGO_DEFAULTS[[algo]], hp)

  if (algo == "maxnet") {
    args <- list(p = y, data = as.data.frame(X), regmult = hp$regmult)
    if (!is.null(hp$classes)) args$classes <- hp$classes
    do.call(maxnet::maxnet, args)

  } else if (algo == "ranger") {
    if (!requireNamespace("ranger", quietly = TRUE)) stop("falta el paquete 'ranger'")
    df <- as.data.frame(X)
    df[[".class"]] <- factor(y, levels = c(0L, 1L))
    do.call(ranger::ranger, c(
      list(formula = stats::as.formula(".class ~ ."),
           data = df, probability = TRUE, seed = seed),
      Filter(Negate(is.null), hp)
    ))

  } else { # xgboost: y factor dispara clasificación; predict -> P(clase "1")
    if (!requireNamespace("xgboost", quietly = TRUE)) stop("falta el paquete 'xgboost'")
    do.call(xgboost::xgboost, c(
      list(x = as.matrix(X), y = factor(y, levels = c(0L, 1L)),
           objective = "binary:logistic", seed = seed, verbosity = 0L),
      Filter(Negate(is.null), hp)
    ))
  }
}

# predict_score: probabilidad de presencia ∈ [0,1] para cualquier
# algoritmo, vía el factory de xai_predict.R.
predict_score <- function(model, X) {
  make_predict_fn(model)(model, as.data.frame(X))
}

# ------------------------------------------------------------
# 4) THRESHOLD-INDEPENDENT METRICS (AUC informativo)
# ------------------------------------------------------------

compute_basic_metrics <- function(y_train, y_test, scores_test) {
  auc_test <- if (length(unique(y_test)) < 2L) {
    NA_real_
  } else {
    as.numeric(pROC::auc(
      response  = y_test,
      predictor = scores_test,
      levels    = c(0, 1),
      direction = "<",
      quiet     = TRUE
    ))
  }

  tibble(
    n_train_pres = sum(y_train == 1L),
    n_train_bg   = sum(y_train == 0L),
    n_test_pres  = sum(y_test == 1L),
    n_test_bg    = sum(y_test == 0L),
    auc_test     = auc_test
  )
}

# ------------------------------------------------------------
# 5) RESPONSE CURVES
# ------------------------------------------------------------

extract_response_curves <- function(model, X_train, n_points = 100) {
  X_train <- as_tibble(X_train)

  medians <- X_train |>
    summarise(across(everything(), \(v) median(v, na.rm = TRUE)))

  purrr::map_dfr(names(X_train), function(var) {
    grid <- seq(min(X_train[[var]], na.rm = TRUE),
                max(X_train[[var]], na.rm = TRUE),
                length.out = n_points)

    newdata <- medians |>
      slice(rep(1, n_points)) |>
      mutate(!!var := grid)

    tibble(
      variable = var,
      value    = grid,
      score    = predict_score(model, newdata)
    )
  })
}

plot_response_curves <- function(curves_df) {
  # Orden numérico para variables tipo "<prefix>_<n>" (ej. wc2.1_30s_bio_1..19);
  # si no matchea, cae a orden alfabético del input.
  vars  <- unique(curves_df$variable)
  nums  <- suppressWarnings(as.integer(sub(".*_(\\d+)$", "\\1", vars)))
  ordered_vars <- if (all(!is.na(nums))) vars[order(nums)] else vars

  curves_df$variable <- factor(curves_df$variable, levels = ordered_vars)

  ggplot(curves_df, aes(x = value, y = score)) +
    geom_line(color = "#2c7fb8", linewidth = 0.6) +
    facet_wrap(~ variable, scales = "free_x") +
    labs(x = "Variable value", y = "Predicted suitability") +
    theme_minimal(base_size = 9)
}

# ------------------------------------------------------------
# 6) PERSIST ARTIFACTS
# ------------------------------------------------------------
#
# `cv_scheme` define el subdirectorio dentro de `out_root/run_id/`.
# Cada esquema vive aislado para no pisar artefactos.
# ------------------------------------------------------------

save_model_artifacts <- function(run_id,
                                 cv_scheme,
                                 model,
                                 predictions_test,
                                 metrics,
                                 response_curves,
                                 out_root,
                                 extra_files = list()) {
  run_dir <- file.path(out_root, run_id, cv_scheme)
  dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)

  saveRDS(model, file.path(run_dir, "model.rds"))
  readr::write_csv(predictions_test, file.path(run_dir, "predictions_test.csv"))
  readr::write_csv(metrics,          file.path(run_dir, "metrics.csv"))
  readr::write_csv(response_curves,  file.path(run_dir, "response_curves.csv"))

  ggplot2::ggsave(
    filename = file.path(run_dir, "response_curves.png"),
    plot     = plot_response_curves(response_curves),
    width    = 10, height = 7, dpi = 120
  )

  # Artefactos extra (ej. folds.csv, fold_metrics.csv en spatial_block).
  for (name in names(extra_files)) {
    obj <- extra_files[[name]]
    path <- file.path(run_dir, name)
    if (inherits(obj, "data.frame")) {
      readr::write_csv(obj, path)
    } else {
      saveRDS(obj, path)
    }
  }

  invisible(run_dir)
}

# ------------------------------------------------------------
# 7) ONE-SHOT WRAPPER (holdout)
# ------------------------------------------------------------

run_model_holdout <- function(algo,
                              run_id,
                              ds,
                              out_root,
                              p_train = 0.7,
                              seed    = 42,
                              hp      = list()) {
  split <- stratified_split(ds$y, p_train = p_train, seed = seed)

  X_train <- ds$X |> slice(split$train)
  y_train <- ds$y[split$train]
  X_test  <- ds$X |> slice(split$test)
  y_test  <- ds$y[split$test]

  t0 <- Sys.time()
  model <- fit_model(algo, X_train, y_train, hp = hp, seed = seed)
  train_secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  scores_test <- predict_score(model, X_test)

  predictions_test <- ds$coords |>
    slice(split$test) |>
    mutate(class = y_test, score = scores_test)

  metrics <- compute_basic_metrics(y_train, y_test, scores_test) |>
    mutate(run_id    = run_id,
           algorithm = algo,
           cv_scheme = "holdout",
           train_secs = train_secs,
           .before    = 1)

  curves <- extract_response_curves(model, X_train)

  save_model_artifacts(
    run_id           = run_id,
    cv_scheme        = "holdout",
    model            = model,
    predictions_test = predictions_test,
    metrics          = metrics,
    response_curves  = curves,
    out_root         = out_root
  )

  metrics
}

# ------------------------------------------------------------
# 8) ONE-SHOT WRAPPER (spatial-block CV)
# ------------------------------------------------------------
#
# Entrena k modelos (uno por fold), persiste:
#   - model.rds            : modelo refit-on-all (para mapping/XAI)
#   - predictions_test.csv : test preds concatenadas (con fold_id)
#   - folds.csv            : asignación fold_id por punto
#   - fold_metrics.csv     : métricas básicas por fold
#   - metrics.csv          : agregado (single row) sobre preds pooled
#   - response_curves.csv  : del modelo refit-on-all
#
# Las columnas threshold-dependent (TSS, FNR, etc.) las completa
# evaluate_run_dir() leyendo predictions_test.csv pooled.
# ------------------------------------------------------------

run_model_spatial_block <- function(algo,
                                    run_id,
                                    ds,
                                    fold_id,
                                    out_root,
                                    seed = 42,
                                    hp   = list()) {
  stopifnot(length(fold_id) == length(ds$y))
  k <- max(fold_id)

  t0 <- Sys.time()

  fold_results <- purrr::map(seq_len(k), function(f) {
    test_mask  <- fold_id == f
    train_mask <- !test_mask

    if (sum(ds$y[train_mask] == 1L) < 5L || sum(ds$y[test_mask] == 1L) < 1L) {
      warning("Fold ", f, " de ", run_id,
              " con muy pocas presencias (train=", sum(ds$y[train_mask] == 1L),
              ", test=", sum(ds$y[test_mask] == 1L), "); se omite.")
      return(NULL)
    }

    model_f <- fit_model(
      algo,
      ds$X[train_mask, , drop = FALSE],
      ds$y[train_mask],
      hp = hp, seed = seed
    )
    scores_f <- predict_score(model_f, ds$X[test_mask, , drop = FALSE])

    fold_basic <- compute_basic_metrics(
      ds$y[train_mask],
      ds$y[test_mask],
      scores_f
    ) |>
      mutate(fold = f, .before = 1)

    list(
      preds = ds$coords[test_mask, , drop = FALSE] |>
        mutate(class   = ds$y[test_mask],
               score   = scores_f,
               fold_id = f),
      metrics = fold_basic
    )
  }) |> purrr::compact()

  if (length(fold_results) == 0L) {
    stop("Spatial-block para ", run_id, ": ningún fold entrenable.")
  }

  preds_pool   <- bind_rows(purrr::map(fold_results, "preds"))
  fold_metrics <- bind_rows(purrr::map(fold_results, "metrics"))

  # Modelo final refit-on-all para downstream (mapping/XAI/response curves).
  refit <- fit_model(algo, ds$X, ds$y, hp = hp, seed = seed)
  curves <- extract_response_curves(refit, ds$X)

  train_secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  # Métricas agregadas sobre el pool de predicciones test (cada punto
  # fue test en exactamente un fold). AUC pooled + mean/sd per-fold.
  auc_pooled <- if (length(unique(preds_pool$class)) < 2L) NA_real_ else
    as.numeric(pROC::auc(
      response  = preds_pool$class,
      predictor = preds_pool$score,
      levels    = c(0, 1),
      direction = "<",
      quiet     = TRUE
    ))

  metrics <- tibble(
    run_id       = run_id,
    algorithm    = algo,
    cv_scheme    = "spatial_block",
    train_secs   = train_secs,
    n_train_pres = sum(ds$y == 1L),  # refit-on-all
    n_train_bg   = sum(ds$y == 0L),
    n_test_pres  = sum(preds_pool$class == 1L),
    n_test_bg    = sum(preds_pool$class == 0L),
    auc_test     = auc_pooled,
    auc_test_sd  = sd(fold_metrics$auc_test, na.rm = TRUE),
    k_folds      = nrow(fold_metrics)
  )

  folds_csv <- ds$coords |>
    mutate(class = ds$y, fold_id = fold_id)

  save_model_artifacts(
    run_id           = run_id,
    cv_scheme        = "spatial_block",
    model            = refit,
    predictions_test = preds_pool,
    metrics          = metrics,
    response_curves  = curves,
    out_root         = out_root,
    extra_files      = list(
      "folds.csv"        = folds_csv,
      "fold_metrics.csv" = fold_metrics
    )
  )

  metrics
}

# ------------------------------------------------------------
# 9) ENTRY POINT
# ------------------------------------------------------------

run_model_for_dataset <- function(algo,
                                  run_id,
                                  dataset_path,
                                  out_root,
                                  cv_scheme = c("holdout", "spatial_block"),
                                  fold_id   = NULL,
                                  p_train   = 0.7,
                                  seed      = 42,
                                  hp        = list()) {
  cv_scheme <- match.arg(cv_scheme)
  ds <- load_model_ready_dataset(dataset_path)

  if (cv_scheme == "holdout") {
    run_model_holdout(
      algo = algo, run_id = run_id, ds = ds, out_root = out_root,
      p_train = p_train, seed = seed, hp = hp
    )
  } else {
    if (is.null(fold_id)) {
      stop("cv_scheme = 'spatial_block' requiere `fold_id` precomputado ",
           "(usar spatial_cv.R::assign_spatial_folds).")
    }
    run_model_spatial_block(
      algo = algo, run_id = run_id, ds = ds, fold_id = fold_id,
      out_root = out_root, seed = seed, hp = hp
    )
  }
}
