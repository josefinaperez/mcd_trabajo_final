source("r/src/select_env_vars.R")

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

out_dir <- "data/outputs/env_selection"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

DATASETS_ROOT <- "data/outputs/sdm_parallel"

# Corre la reducción de colinealidad sobre el dataset de referencia de un
# env_set y persiste corr_matrix<suffix>.csv, selected_vars<suffix>.csv y
# corr_heatmap<suffix>.png. predictor_cols_fn elige qué columnas entran al
# análisis (solo bioclim vs bioclim+vegetación).
run_selection <- function(ref_run_id, predictor_cols_fn, suffix = "", cutoff = 0.7) {
  ref_path <- file.path(DATASETS_ROOT, ref_run_id, "sdm_dataset_model_ready.csv")
  if (!file.exists(ref_path)) {
    stop("Dataset de referencia no encontrado. Correr dataset_pipeline.R primero: ",
         ref_path)
  }
  ds <- readr::read_csv(ref_path, show_col_types = FALSE)
  predictor_cols <- predictor_cols_fn(ds)
  message(sprintf("Referencia%s: %s\nFilas: %d | Predictores: %d",
                  suffix, ref_path, nrow(ds), length(predictor_cols)))

  sel <- compute_env_selection(
    dataset        = ds,
    predictor_cols = predictor_cols,
    cutoff         = cutoff
  )

  readr::write_csv(sel$cor_tidy, file.path(out_dir, paste0("corr_matrix", suffix, ".csv")))
  readr::write_csv(
    tibble::tibble(variable = sel$selected, status = "kept") |>
      bind_rows(tibble::tibble(variable = sel$dropped, status = "dropped")),
    file.path(out_dir, paste0("selected_vars", suffix, ".csv"))
  )
  p <- plot_corr_heatmap(sel$cor_tidy, sel$selected, cutoff = sel$cutoff)
  ggplot2::ggsave(file.path(out_dir, paste0("corr_heatmap", suffix, ".png")),
                  p, width = 9, height = 8, dpi = 150)

  message(sprintf("Seleccionadas (%d): %s",
                  length(sel$selected), paste(sel$selected, collapse = ", ")))
  message(sprintf("Descartadas (%d): %s\n",
                  length(sel$dropped), paste(sel$dropped, collapse = ", ")))
  invisible(sel)
}

predictors_minus_meta <- function(ds) {
  setdiff(names(ds), c("class", "decimalLongitude", "decimalLatitude"))
}

# 1) Solo bioclim -> selected_vars.csv (alimenta el env_set bioclim_reduced).
run_selection(
  ref_run_id        = "polyporaceae__bias-none__bp-random__bpn-fixed_10000__env-bioclim",
  predictor_cols_fn = function(ds) grep("^wc2", names(ds), value = TRUE),
  suffix            = ""
)

# 2) Bioclim + vegetación -> selected_vars_veg.csv (alimenta bioclim_veg_reduced).
run_selection(
  ref_run_id        = "polyporaceae__bias-none__bp-random__bpn-fixed_10000__env-bioclim_veg",
  predictor_cols_fn = predictors_minus_meta,
  suffix            = "_veg"
)

# 3) Bioclim + topografía -> selected_vars_topo.csv (alimenta bioclim_topo_reduced).
run_selection(
  ref_run_id        = "polyporaceae__bias-none__bp-random__bpn-fixed_10000__env-bioclim_topo",
  predictor_cols_fn = predictors_minus_meta,
  suffix            = "_topo"
)

# 4) Bioclim + vegetación + topografía -> selected_vars_veg_topo.csv
#    (alimenta bioclim_veg_topo_reduced).
run_selection(
  ref_run_id        = "polyporaceae__bias-none__bp-random__bpn-fixed_10000__env-bioclim_veg_topo",
  predictor_cols_fn = predictors_minus_meta,
  suffix            = "_veg_topo"
)
