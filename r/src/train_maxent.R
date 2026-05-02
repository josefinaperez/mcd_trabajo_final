# ============================================================
# File: train_maxent.R
# Purpose: Train MaxEnt (maxnet) on a model-ready SDM dataset
#          and persist artifacts (model, test predictions,
#          metrics, response curves).
# ============================================================

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

train_maxnet <- function(X_train, y_train, regmult = 1, classes = NULL) {
  args <- list(
    p       = y_train,
    data    = as.data.frame(X_train),
    regmult = regmult
  )
  if (!is.null(classes)) args$classes <- classes

  do.call(maxnet::maxnet, args)
}

predict_maxnet <- function(model, X, type = "cloglog") {
  as.numeric(predict(model, newdata = as.data.frame(X), type = type, clamp = TRUE))
}

# ------------------------------------------------------------
# 4) THRESHOLD-INDEPENDENT METRICS (AUC informativo)
# ------------------------------------------------------------

compute_basic_metrics <- function(y_train, y_test, scores_test) {
  auc_test <- as.numeric(pROC::auc(
    response  = y_test,
    predictor = scores_test,
    levels    = c(0, 1),
    direction = "<",
    quiet     = TRUE
  ))

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
      score    = predict_maxnet(model, newdata)
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
    labs(x = "Variable value", y = "Predicted suitability (cloglog)") +
    theme_minimal(base_size = 9)
}

# ------------------------------------------------------------
# 6) PERSIST ARTIFACTS
# ------------------------------------------------------------

save_model_artifacts <- function(run_id,
                                 model,
                                 predictions_test,
                                 metrics,
                                 response_curves,
                                 out_root) {
  run_dir <- file.path(out_root, run_id)
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

  invisible(run_dir)
}

# ------------------------------------------------------------
# 7) ONE-SHOT WRAPPER
# ------------------------------------------------------------

run_maxent_for_dataset <- function(run_id,
                                   dataset_path,
                                   out_root,
                                   p_train = 0.7,
                                   seed    = 42,
                                   regmult = 1,
                                   classes = NULL) {
  ds <- load_model_ready_dataset(dataset_path)

  split <- stratified_split(ds$y, p_train = p_train, seed = seed)

  X_train <- ds$X |> slice(split$train)
  y_train <- ds$y[split$train]
  X_test  <- ds$X |> slice(split$test)
  y_test  <- ds$y[split$test]

  t0 <- Sys.time()
  model <- train_maxnet(X_train, y_train, regmult = regmult, classes = classes)
  train_secs <- as.numeric(difftime(Sys.time(), t0, units = "secs"))

  scores_test <- predict_maxnet(model, X_test)

  predictions_test <- ds$coords |>
    slice(split$test) |>
    mutate(class = y_test, score = scores_test)

  metrics <- compute_basic_metrics(y_train, y_test, scores_test) |>
    mutate(run_id = run_id, train_secs = train_secs, .before = 1)

  curves <- extract_response_curves(model, X_train)

  save_model_artifacts(
    run_id           = run_id,
    model            = model,
    predictions_test = predictions_test,
    metrics          = metrics,
    response_curves  = curves,
    out_root         = out_root
  )

  metrics
}
