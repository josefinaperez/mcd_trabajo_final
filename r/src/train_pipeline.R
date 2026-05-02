# ============================================================
# File: train_pipeline.R
# Purpose: Entrenar MaxEnt sobre todos los datasets del manifest
#          generado por dataset_pipeline.R y persistir todos los
#          outputs necesarios para visualizar resultados después
#          (sin necesidad de re-correr nada).
#
# Run from repo root:
#   source("r/src/train_pipeline.R")
# ============================================================

source("r/src/train_maxent.R")

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(purrr)
  library(ggplot2)
})

# ------------------------------------------------------------
# Configuración
# ------------------------------------------------------------

DATASETS_ROOT <- "data/outputs/sdm_parallel"
MODELS_ROOT   <- "data/outputs/sdm_models"
SEED          <- 42
P_TRAIN       <- 0.7
REGMULT       <- 1

dir.create(MODELS_ROOT, recursive = TRUE, showWarnings = FALSE)

# ------------------------------------------------------------
# 1) Manifest de datasets
# ------------------------------------------------------------

datasets_manifest_path <- file.path(DATASETS_ROOT, "manifest.csv")
if (!file.exists(datasets_manifest_path)) {
  stop("No se encontró el manifest de datasets en ", datasets_manifest_path,
       ". Corré primero r/src/dataset_pipeline.R")
}

datasets_manifest <- read_csv(datasets_manifest_path, show_col_types = FALSE)

# ------------------------------------------------------------
# 2) Entrenar MaxEnt para cada dataset
# ------------------------------------------------------------

metrics_per_run <- purrr::map_dfr(seq_len(nrow(datasets_manifest)), function(i) {
  run_id       <- datasets_manifest$run_id[i]
  dataset_path <- file.path(DATASETS_ROOT, run_id, "sdm_dataset_model_ready.csv")

  message("[", i, "/", nrow(datasets_manifest), "] ", run_id)

  run_maxent_for_dataset(
    run_id       = run_id,
    dataset_path = dataset_path,
    out_root     = MODELS_ROOT,
    p_train      = P_TRAIN,
    seed         = SEED,
    regmult      = REGMULT
  )
})

# ------------------------------------------------------------
# 3) Manifest global de modelos (dataset + métricas)
# ------------------------------------------------------------

models_manifest <- datasets_manifest |>
  left_join(metrics_per_run, by = "run_id") |>
  mutate(model_dir = file.path(MODELS_ROOT, run_id))

write_csv(models_manifest, file.path(MODELS_ROOT, "manifest.csv"))

# ------------------------------------------------------------
# 4) Tabla resumen lista para mostrar (Rmd la levanta tal cual)
# ------------------------------------------------------------

summary_table <- models_manifest |>
  mutate(
    bias_label = if_else(bias_method == "none",
                         "sin corrección",
                         paste0("grid ", bias_param, "km")),
    bp_label   = if_else(bp_n_strategy == "fixed",
                         paste0("BG = ", bp_n),
                         "BG = n presencias")
  ) |>
  select(run_id, species, bias_label, bp_label,
         n_train_pres, n_test_pres, n_train_bg, n_test_bg,
         auc_test, train_secs)

write_csv(summary_table, file.path(MODELS_ROOT, "summary_table.csv"))

# ------------------------------------------------------------
# 5) Plot resumen pre-renderizado (AUC por config)
# ------------------------------------------------------------

auc_plot <- summary_table |>
  ggplot(aes(x = bias_label, y = auc_test, fill = bp_label)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey40") +
  facet_wrap(~ species) +
  labs(x = "Corrección de sesgo espacial",
       y = "AUC (test)",
       fill = "Background points",
       title = "Comparación de AUC por configuración") +
  theme_minimal()

ggsave(
  filename = file.path(MODELS_ROOT, "auc_comparison.png"),
  plot     = auc_plot,
  width    = 9, height = 5, dpi = 120
)

message("OK. Outputs en: ", MODELS_ROOT)
print(summary_table)
