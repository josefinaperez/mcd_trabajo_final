source("r/src/select_env_vars.R")

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})

out_dir <- "data/outputs/env_selection"
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)

ref_dataset_path <- file.path(
  "data/outputs/sdm_parallel",
  "polyporaceae__bias-none__bp-random__bpn-fixed_10000__env-bioclim_30s",
  "sdm_dataset_model_ready.csv"
)
if (!file.exists(ref_dataset_path)) {
  stop("Dataset de referencia no encontrado. Correr dataset_pipeline.R primero: ",
       ref_dataset_path)
}

ref_dataset <- readr::read_csv(ref_dataset_path, show_col_types = FALSE)
predictor_cols <- grep("^wc2", names(ref_dataset), value = TRUE)
message(sprintf("Referencia: %s\nFilas: %d | Predictores: %d",
                ref_dataset_path, nrow(ref_dataset), length(predictor_cols)))

sel <- compute_env_selection(
  dataset        = ref_dataset,
  predictor_cols = predictor_cols,
  cutoff         = 0.7
)

readr::write_csv(sel$cor_tidy, file.path(out_dir, "corr_matrix.csv"))
readr::write_csv(
  tibble::tibble(variable = sel$selected, status = "kept") |>
    bind_rows(tibble::tibble(variable = sel$dropped, status = "dropped")),
  file.path(out_dir, "selected_vars.csv")
)

p <- plot_corr_heatmap(sel$cor_tidy, sel$selected, cutoff = sel$cutoff)
ggplot2::ggsave(file.path(out_dir, "corr_heatmap.png"),
                p, width = 9, height = 8, dpi = 150)

message(sprintf("\nSeleccionadas (%d): %s",
                length(sel$selected), paste(sel$selected, collapse = ", ")))
message(sprintf("\nDescartadas (%d): %s",
                length(sel$dropped), paste(sel$dropped, collapse = ", ")))
