# ============================================================
# File: evaluate_model.R
# Purpose: Threshold-dependent evaluation for any trained SDM
#          model. Operates only on (y, score) pairs read from
#          predictions_test.csv, so it is reusable for MaxEnt,
#          Random Forest, SVM, etc. Computes TSS / FNR at the
#          Youden-optimal threshold and exposes helpers for
#          pipeline-level aggregation.
# ============================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tibble)
  library(purrr)
  library(ggplot2)
  library(pROC)
})

# ------------------------------------------------------------
# 1) CORE METRIC COMPUTATION
# ------------------------------------------------------------
#
# Given binary labels (1 = presence, 0 = background) and a
# continuous suitability score, find the threshold that
# maximises Youden's J = sensitivity + specificity - 1
# (equivalently: maximises TSS) and report the metrics at
# that threshold.
#
# Returns a one-row tibble with:
#   threshold_max_tss : tau* in score units
#   sensitivity       : TP / (TP + FN)  at tau*
#   specificity       : TN / (TN + FP)  at tau*
#   tss               : sens + spec - 1
#   fnr               : 1 - sens
# ------------------------------------------------------------

compute_dual_metrics <- function(y, scores) {
  stopifnot(length(y) == length(scores))
  if (length(unique(y)) < 2L) {
    stop("compute_dual_metrics: need both classes (0 and 1) in y.")
  }

  roc_obj <- pROC::roc(
    response  = y,
    predictor = scores,
    levels    = c(0, 1),
    direction = "<",
    quiet     = TRUE
  )

  best <- pROC::coords(
    roc_obj,
    x           = "best",
    best.method = "youden",
    ret         = c("threshold", "sensitivity", "specificity"),
    transpose   = FALSE
  )
  # In case of ties, coords() may return >1 row; take the first.
  best <- best[1, , drop = FALSE]

  sens <- as.numeric(best$sensitivity)
  spec <- as.numeric(best$specificity)

  tibble(
    threshold_max_tss = as.numeric(best$threshold),
    sensitivity       = sens,
    specificity       = spec,
    tss               = sens + spec - 1,
    fnr               = 1 - sens
  )
}

# ------------------------------------------------------------
# 2) PER-RUN EVALUATION
# ------------------------------------------------------------
#
# Reads predictions_test.csv from a single run directory and
# returns a one-row tibble with run_id + dual metrics.
# ------------------------------------------------------------

evaluate_run_dir <- function(run_dir) {
  preds_path <- file.path(run_dir, "predictions_test.csv")
  if (!file.exists(preds_path)) {
    stop("evaluate_run_dir: missing predictions_test.csv at ", preds_path)
  }

  preds <- readr::read_csv(preds_path, show_col_types = FALSE)
  required <- c("class", "score")
  missing_cols <- setdiff(required, names(preds))
  if (length(missing_cols) > 0) {
    stop(
      "evaluate_run_dir: predictions_test.csv at ", preds_path,
      " is missing column(s): ", paste(missing_cols, collapse = ", ")
    )
  }

  metrics <- compute_dual_metrics(
    y      = as.integer(preds$class),
    scores = as.numeric(preds$score)
  )

  tibble(run_id = basename(run_dir)) |>
    bind_cols(metrics)
}

# ------------------------------------------------------------
# 3) BATCH EVALUATION OVER A MODELS ROOT
# ------------------------------------------------------------
#
# Walks the immediate subdirectories of out_root, evaluating
# every one that contains a predictions_test.csv. Returns a
# tibble with one row per run_id.
# ------------------------------------------------------------

evaluate_all_runs <- function(out_root) {
  if (!dir.exists(out_root)) {
    stop("evaluate_all_runs: directory not found: ", out_root)
  }

  run_dirs <- list.dirs(out_root, full.names = TRUE, recursive = FALSE)
  run_dirs <- run_dirs[file.exists(file.path(run_dirs, "predictions_test.csv"))]

  if (length(run_dirs) == 0) {
    warning("evaluate_all_runs: no run directories with predictions_test.csv found in ", out_root)
    return(tibble(
      run_id            = character(),
      threshold_max_tss = numeric(),
      sensitivity       = numeric(),
      specificity       = numeric(),
      tss               = numeric(),
      fnr               = numeric()
    ))
  }

  purrr::map_dfr(run_dirs, evaluate_run_dir)
}

# ------------------------------------------------------------
# 4) COMPARATIVE PLOT (FNR vs TSS)
# ------------------------------------------------------------
#
# Produces a scatter plot in (FNR, TSS) space. The upper-left
# quadrant identifies configurations that are simultaneously
# consistent (low FNR) and discriminatory (high TSS) — the
# region from which the dual scheme will pick the winner.
#
# `summary_df` should contain columns: run_id, fnr, tss, and
# (optionally) bias_label, bp_label for visual encoding.
# ------------------------------------------------------------

make_dual_metrics_plot <- function(summary_df,
                                   color_var = "bias_label",
                                   shape_var = "bp_label",
                                   label_var = "run_id") {
  stopifnot(all(c("fnr", "tss") %in% names(summary_df)))

  has_color <- color_var %in% names(summary_df)
  has_shape <- shape_var %in% names(summary_df)
  has_label <- label_var %in% names(summary_df)

  aes_point <- if (has_color && has_shape) {
    aes(x = fnr, y = tss,
        color = .data[[color_var]],
        shape = .data[[shape_var]])
  } else if (has_color) {
    aes(x = fnr, y = tss, color = .data[[color_var]])
  } else {
    aes(x = fnr, y = tss)
  }

  p <- ggplot(summary_df, aes_point) +
    geom_point(size = 3, alpha = 0.85) +
    scale_x_continuous(limits = c(0, 1)) +
    scale_y_continuous(limits = c(0, 1)) +
    labs(
      x = "FNR (1 - sensitivity)  -  lower is better",
      y = "TSS (sens + spec - 1)  -  higher is better",
      title = "Dual evaluation: TSS vs FNR per configuration",
      subtitle = "Upper-left quadrant = consistent and discriminatory"
    ) +
    theme_minimal(base_size = 11) +
    theme(legend.position = "bottom")

  if (has_label) {
    p <- p + geom_text(
      aes(label = .data[[label_var]]),
      size = 2.6, vjust = -0.9, hjust = 0.5,
      show.legend = FALSE
    )
  }

  p
}
