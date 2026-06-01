# ============================================================
# File: train_pipeline.R
# Purpose: Entrenar MaxEnt sobre todos los datasets del manifest
#          generado por dataset_pipeline.R, evaluando con:
#            - spatial_block : k=5 folds sobre grilla AEA Argentina,
#                              tamaño de bloque derivado de la
#                              autocorrelación de las BIO.
#
# Run from repo root:
#   source("r/src/train_pipeline.R")
# ============================================================

source("r/src/train_models.R")
source("r/src/evaluate_model.R")
source("r/src/spatial_cv.R")

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(purrr)
  library(ggplot2)
})

# ------------------------------------------------------------
# Configuración
# ------------------------------------------------------------

DATASETS_ROOT  <- "data/outputs/sdm_parallel"
MODELS_ROOT    <- "data/outputs/sdm_models"
ENV_DIR        <- "data/features/env_2.5m_ar/bioclim"
ARGENTINA_SHP  <- "data/shp/argentina/argentina.shp"

SEED           <- 42
P_TRAIN        <- 0.7
REGMULT        <- 1
K_FOLDS        <- 5L
BLOCK_SIZE_CAP_KM <- 300L

CV_SCHEMES     <- c("spatial_block")
ALGOS          <- c("maxnet", "ranger", "xgboost")

# Idempotencia: por defecto se saltea cualquier (run × cv × algo) ya entrenado
# en disco. Forzar reentrenamiento completo con SDM_FORCE=1 en el entorno.
# Los helpers model_artifacts_exist() / read_basic_metrics() viven en
# train_models.R (módulo puro, testeable).
FORCE <- tolower(Sys.getenv("SDM_FORCE", "")) %in% c("1", "true", "t", "yes", "y")

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
# 2) Calibración global del tamaño de bloque
# ------------------------------------------------------------
#
# La autocorrelación espacial se computa sobre las BIO una sola
# vez para toda la corrida: depende del paisaje climático, no
# del dataset específico. El tamaño efectivo se acota a
# BLOCK_SIZE_CAP_KM (las BIO climáticas a escala continental
# tienen ranges enormes; el cap mantiene los bloques manejables
# dentro de Argentina).

env_paths <- list.files(ENV_DIR, pattern = "\\.tif$", full.names = TRUE)
if (length(env_paths) == 0) {
  stop("No se encontraron rasters de BIO en ", ENV_DIR)
}

message("Calibrando tamaño de bloque por autocorrelación...")
block_calib <- calibrate_block_size(
  env_raster_paths = env_paths,
  argentina_shp    = ARGENTINA_SHP,
  size_cap_km      = BLOCK_SIZE_CAP_KM,
  seed             = SEED
)
BLOCK_SIZE_M <- block_calib$size_m
message(sprintf(
  "  autocor range (menor plausible): %.0f km  |  size efectivo: %.0f km  |  capped: %s",
  block_calib$autocor_range_m / 1000,
  BLOCK_SIZE_M / 1000,
  block_calib$capped
))

# Persistir diagnóstico de autocor (para citar en docs)
write_csv(block_calib$range_table, file.path(MODELS_ROOT, "autocor_range_table.csv"))
write_csv(
  tibble(
    size_m            = BLOCK_SIZE_M,
    autocor_range_m   = block_calib$autocor_range_m,
    size_cap_km       = BLOCK_SIZE_CAP_KM,
    capped            = block_calib$capped,
    k_folds           = K_FOLDS
  ),
  file.path(MODELS_ROOT, "spatial_cv_config.csv")
)

# ------------------------------------------------------------
# 3) Entrenamiento: cv_scheme × algo × run
#     Los folds spatial_block dependen solo del dataset (no del
#     algoritmo): se computan UNA vez por run_id y se reusan.
# ------------------------------------------------------------

# Cache de folds por run_id (solo para spatial_block).
fold_cache <- new.env(parent = emptyenv())

get_folds <- function(run_id, dataset_path) {
  if (!is.null(fold_cache[[run_id]])) return(fold_cache[[run_id]])
  ds_df <- read_csv(dataset_path, show_col_types = FALSE)
  sb <- assign_spatial_folds(df = ds_df, size_m = BLOCK_SIZE_M,
                             k = K_FOLDS, seed = SEED)
  # Plot de bloques (una vez por run, a nivel cv_scheme)
  p <- plot_spatial_folds(sb$blocks, ds_df, sb$fold_id,
                          argentina_shp = ARGENTINA_SHP)
  fold_plot_dir <- file.path(MODELS_ROOT, run_id, "spatial_block")
  dir.create(fold_plot_dir, recursive = TRUE, showWarnings = FALSE)
  ggsave(file.path(fold_plot_dir, "blocks_map.png"), p,
         width = 7, height = 8, dpi = 110)
  fold_cache[[run_id]] <- sb$fold_id
  sb$fold_id
}

train_one <- function(i, cv_scheme, algo) {
  run_id       <- datasets_manifest$run_id[i]
  model_dir    <- file.path(MODELS_ROOT, run_id, cv_scheme, algo)

  # Idempotencia: si el modelo ya está entrenado, devolver su fila básica desde
  # disco sin reentrenar. El paso dual (evaluate_run_dir) igual relee las
  # predicciones, así que el summary sale completo.
  if (!FORCE && model_artifacts_exist(model_dir)) {
    message("[", i, "/", nrow(datasets_manifest), "] ", run_id,
            "  (", cv_scheme, " / ", algo, ")  [skip: ya entrenado]")
    return(read_basic_metrics(model_dir))
  }

  dataset_path <- file.path(DATASETS_ROOT, run_id, "sdm_dataset_model_ready.csv")
  hp           <- if (algo == "maxnet") list(regmult = REGMULT) else list()

  message("[", i, "/", nrow(datasets_manifest), "] ", run_id,
          "  (", cv_scheme, " / ", algo, ")")

  if (cv_scheme == "spatial_block") {
    run_model_for_dataset(
      algo = algo, run_id = run_id, dataset_path = dataset_path,
      out_root = MODELS_ROOT, cv_scheme = "spatial_block",
      fold_id = get_folds(run_id, dataset_path), hp = hp
    )
  } else {
    run_model_for_dataset(
      algo = algo, run_id = run_id, dataset_path = dataset_path,
      out_root = MODELS_ROOT, cv_scheme = "holdout",
      p_train = P_TRAIN, seed = SEED, hp = hp
    )
  }
}

train_grid <- tidyr::expand_grid(
  cv_scheme = CV_SCHEMES,
  algo      = ALGOS,
  i         = seq_len(nrow(datasets_manifest))
)

basic_metrics_per_run <- purrr::pmap_dfr(
  train_grid,
  function(cv_scheme, algo, i) train_one(i, cv_scheme, algo)
)

# ------------------------------------------------------------
# 4) Evaluación dual (TSS / FNR @ Youden + Boyce) por run
# ------------------------------------------------------------

dual_metrics <- basic_metrics_per_run |>
  select(run_id, cv_scheme, algorithm) |>
  mutate(
    model_dir = file.path(MODELS_ROOT, run_id, cv_scheme, algorithm),
    dual = purrr::map(model_dir, evaluate_run_dir)
  ) |>
  tidyr::unnest(dual)

metrics_per_run <- basic_metrics_per_run |>
  left_join(dual_metrics, by = c("run_id", "cv_scheme", "algorithm"))

# Persistir metrics.csv extendido en cada run_dir
purrr::walk(seq_len(nrow(metrics_per_run)), function(i) {
  row <- metrics_per_run[i, , drop = FALSE]
  write_csv(row, file.path(row$model_dir, "metrics.csv"))
})

# ------------------------------------------------------------
# 5) Manifest global de modelos (dataset × cv_scheme)
# ------------------------------------------------------------

models_manifest <- datasets_manifest |>
  inner_join(metrics_per_run, by = "run_id")

write_csv(models_manifest, file.path(MODELS_ROOT, "manifest.csv"))

# ------------------------------------------------------------
# 6) Tabla resumen y plots comparativos
# ------------------------------------------------------------

# Orden de env_sets por familia (base → +veg → +topo → +soil → combinados),
# con el full inmediatamente seguido de su _reduced. Así la paleta "Paired"
# (pares claro/oscuro) asigna a cada familia un par de colores y full/reduced
# quedan visualmente emparentados. Solo se conservan los niveles presentes.
ENV_SET_ORDER <- c(
  "bioclim",          "bioclim_reduced",
  "bioclim_veg",      "bioclim_veg_reduced",
  "bioclim_topo",     "bioclim_topo_reduced",
  "bioclim_soil",     "bioclim_soil_reduced",
  "bioclim_veg_topo", "bioclim_veg_topo_reduced",
  "bioclim_veg_soil", "bioclim_veg_soil_reduced"
)

summary_table <- models_manifest |>
  mutate(
    env_set    = factor(env_set, levels = intersect(ENV_SET_ORDER, unique(env_set))),
    bias_label = if_else(bias_method == "none",
                         "sin corrección",
                         paste0("grid ", bias_param, "km")),
    bp_label   = if_else(bp_n_strategy == "fixed",
                         paste0("BG = ", bp_n),
                         "BG = n presencias")
  ) |>
  select(run_id, cv_scheme, algorithm, species, env_set, bias_label, bp_label,
         n_train_pres, n_test_pres, n_train_bg, n_test_bg,
         auc_test, threshold_max_tss, sensitivity, specificity,
         tss, fnr, boyce, train_secs) |>
  arrange(cv_scheme, algorithm, desc(tss))

write_csv(summary_table, file.path(MODELS_ROOT, "summary_table.csv"))

# 6a) AUC por env_set, barras por algoritmo, faceteado por bias × background.
#     env_set es el eje protagonista: deja ver si veg/topo/suelo aportan
#     sobre bioclim, en cada combinación de sesgo y background.
auc_plot <- summary_table |>
  ggplot(aes(x = env_set, y = auc_test, fill = algorithm)) +
  geom_col(position = position_dodge(width = 0.8), width = 0.7) +
  geom_hline(yintercept = 0.5, linetype = "dashed", color = "grey40") +
  facet_grid(bias_label ~ bp_label) +
  labs(x = "Conjunto de predictores (env_set)",
       y = "AUC (test)",
       fill = "Algoritmo",
       title = "AUC por env_set — sesgo × background",
       subtitle = "Facet: corrección de sesgo (filas) × background (columnas)") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1))

ggsave(file.path(MODELS_ROOT, "auc_comparison.png"),
       auc_plot, width = 14, height = 8, dpi = 120)

# 6b) TSS vs FNR (criterio Miyaji), color por env_set, forma por algoritmo,
#     facet por corrección de sesgo. Es el plot de decisión: el cuadrante
#     sup-izq (TSS alto, FNR bajo) muestra qué env_set discrimina mejor.
dual_plot <- summary_table |>
  ggplot(aes(x = fnr, y = tss, color = env_set, shape = algorithm)) +
  geom_point(size = 3, alpha = 0.85) +
  facet_wrap(~ bias_label) +
  scale_color_brewer(palette = "Paired", drop = FALSE) +
  scale_x_continuous(limits = c(0, 1)) +
  scale_y_continuous(limits = c(0, 1)) +
  labs(x = "FNR  (menor es mejor)",
       y = "TSS  (mayor es mejor)",
       color = "env_set",
       shape = "Algoritmo",
       title = "TSS vs FNR — por env_set y algoritmo",
       subtitle = "Cuadrante sup-izq: consistente y discriminatorio · facet por corrección de sesgo") +
  theme_minimal(base_size = 11) +
  theme(legend.position = "right")

ggsave(file.path(MODELS_ROOT, "dual_metrics_comparison.png"),
       dual_plot, width = 13, height = 6, dpi = 120)

# 6c) Ranking por TSS dentro de cada (cv_scheme, algorithm)
rank_compare <- summary_table |>
  group_by(cv_scheme, algorithm) |>
  mutate(rank_tss = rank(-tss, ties.method = "min")) |>
  ungroup() |>
  select(run_id, cv_scheme, algorithm, env_set, rank_tss, tss, fnr, boyce) |>
  arrange(cv_scheme, algorithm, rank_tss)

write_csv(rank_compare, file.path(MODELS_ROOT, "rank_compare_spatial_block.csv"))

message("OK. Outputs en: ", MODELS_ROOT)
print(summary_table)
print(rank_compare)
