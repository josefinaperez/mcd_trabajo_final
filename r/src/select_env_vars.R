# ============================================================
# File: select_env_vars.R
# Purpose: Reduce colinealidad de predictores ambientales por
#          correlación pairwise. Implementación equivalente a
#          caret::findCorrelation(cutoff = 0.7) en base R.
# ============================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(tibble)
  library(tidyr)
  library(ggplot2)
})

# Devuelve los nombres de variables a descartar para que ninguna
# pareja sobreviviente tenga |r| > cutoff. Criterio de desempate:
# entre las dos variables del par más correlacionado, se descarta
# la que tenga mayor correlación promedio con el resto.
find_correlated <- function(cor_mat, cutoff = 0.7) {
  stopifnot(is.matrix(cor_mat), nrow(cor_mat) == ncol(cor_mat))
  abs_mat <- abs(cor_mat)
  diag(abs_mat) <- NA_real_

  drop <- character(0)
  repeat {
    remaining <- setdiff(rownames(abs_mat), drop)
    if (length(remaining) < 2) break
    sub <- abs_mat[remaining, remaining, drop = FALSE]
    mx <- suppressWarnings(max(sub, na.rm = TRUE))
    if (!is.finite(mx) || mx <= cutoff) break
    idx <- which(sub == mx, arr.ind = TRUE)[1, ]
    i <- remaining[idx[1]]
    j <- remaining[idx[2]]
    mean_i <- mean(sub[i, ], na.rm = TRUE)
    mean_j <- mean(sub[j, ], na.rm = TRUE)
    drop <- c(drop, if (mean_i >= mean_j) i else j)
  }
  drop
}

# Pipeline puro: dataset con clase + predictores → artefactos
# (matriz de correlación tidy + vars seleccionadas + heatmap).
compute_env_selection <- function(dataset,
                                  predictor_cols,
                                  cutoff = 0.7) {
  stopifnot(all(predictor_cols %in% names(dataset)))
  X <- dataset[, predictor_cols, drop = FALSE]
  X <- X[complete.cases(X), , drop = FALSE]
  if (nrow(X) < 2) stop("Dataset sin filas completas para correlación.")

  cor_mat <- cor(as.matrix(X), method = "pearson")
  dropped <- find_correlated(cor_mat, cutoff = cutoff)
  selected <- setdiff(predictor_cols, dropped)

  cor_tidy <- as.data.frame(cor_mat) |>
    tibble::rownames_to_column("var_i") |>
    tidyr::pivot_longer(-"var_i", names_to = "var_j", values_to = "r")

  list(
    cor_matrix = cor_mat,
    cor_tidy   = cor_tidy,
    dropped    = dropped,
    selected   = selected,
    cutoff     = cutoff,
    n_rows     = nrow(X)
  )
}

plot_corr_heatmap <- function(cor_tidy, selected, cutoff = 0.7) {
  clean_lab <- function(v) sub("^wc2\\.1_2\\.5m_", "", v)
  cor_tidy$var_i <- clean_lab(cor_tidy$var_i)
  cor_tidy$var_j <- clean_lab(cor_tidy$var_j)
  selected <- clean_lab(selected)
  vars_order <- unique(cor_tidy$var_i)
  cor_tidy |>
    mutate(
      var_i = factor(var_i, levels = vars_order),
      var_j = factor(var_j, levels = vars_order),
      kept_i = var_i %in% selected,
      kept_j = var_j %in% selected,
      kept_pair = kept_i & kept_j
    ) |>
    ggplot(aes(var_i, var_j, fill = r)) +
    geom_tile(color = "grey90") +
    geom_text(aes(label = formatC(r, format = "f", digits = 2, decimal.mark = ","),
                  fontface = ifelse(abs(r) > cutoff & var_i != var_j, "bold", "plain")),
              size = 2.4) +
    scale_fill_gradient2(low = "#2166AC", mid = "white", high = "#B2182B",
                        midpoint = 0, limits = c(-1, 1),
                        labels = scales::label_number(decimal.mark = ",")) +
    coord_equal() +
    theme_minimal(base_size = 10) +
    theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
    labs(x = NULL, y = NULL,
        title = sprintf("Correlación pairwise (Pearson) — cutoff |r| > %s", formatC(cutoff, format = "f", digits = 2, decimal.mark = ",")),
        subtitle = sprintf("Conservadas: %d / %d", length(selected), length(vars_order)))
}
