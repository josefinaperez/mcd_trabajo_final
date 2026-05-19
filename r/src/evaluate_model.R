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
# 2) CONTINUOUS BOYCE INDEX (Hirzel et al. 2006)
# ------------------------------------------------------------
#
# Métrica recomendada para datos de solo presencia: mide la
# correlación monótona entre el score predicho y la frecuencia
# observada de presencias relativa a la disponibilidad
# ambiental (aproximada acá por los puntos de background).
#
# Procedimiento (ventana móvil sobre [s_min, s_max]):
#   1. Bins con ancho window_width * (s_max - s_min).
#   2. Por bin i: F_i = (frac presencias en bin) / (frac BG en bin).
#   3. Devuelve Spearman ρ entre midpoint del bin y F_i,
#      ignorando bins sin disponibilidad (F_i = NA o Inf).
#
# Rango: [-1, 1]. Modelo "trivial" (score constante) ⇒ NA.
# ------------------------------------------------------------

compute_boyce <- function(scores_pres,
                          scores_bg,
                          n_bins       = 101L,
                          window_width = 0.1) {
  stopifnot(length(scores_pres) > 0, length(scores_bg) > 0,
            window_width > 0, window_width < 1)

  s_min <- min(c(scores_pres, scores_bg), na.rm = TRUE)
  s_max <- max(c(scores_pres, scores_bg), na.rm = TRUE)
  if (!is.finite(s_min) || !is.finite(s_max) || s_max <= s_min) {
    return(NA_real_)
  }

  w <- (s_max - s_min) * window_width
  midpoints <- seq(s_min + w / 2, s_max - w / 2, length.out = n_bins)

  pe <- vapply(midpoints, function(m) {
    lo <- m - w / 2
    hi <- m + w / 2
    fp <- mean(scores_pres >= lo & scores_pres <= hi)
    fb <- mean(scores_bg   >= lo & scores_bg   <= hi)
    if (fb == 0) NA_real_ else fp / fb
  }, numeric(1))

  ok <- is.finite(pe)
  if (sum(ok) < 3L) return(NA_real_)

  suppressWarnings(
    stats::cor(midpoints[ok], pe[ok], method = "spearman")
  )
}

# ------------------------------------------------------------
# 3) PER-RUN EVALUATION
# ------------------------------------------------------------
#
# Lee predictions_test.csv en un directorio cualquiera y
# devuelve un único tibble de métricas duales + Boyce. No
# añade run_id ni cv_scheme: el caller los agrega externamente
# desde el manifest (separa parsing de paths de cómputo).
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

  y <- as.integer(preds$class)
  s <- as.numeric(preds$score)

  dual <- compute_dual_metrics(y = y, scores = s)
  boyce <- compute_boyce(
    scores_pres = s[y == 1L],
    scores_bg   = s[y == 0L]
  )

  dual |> mutate(boyce = boyce)
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
