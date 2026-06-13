# ============================================================
# File: tune_pipeline.R
# Purpose: Tuneo de hiperparámetros (#8) sobre todos los datasets
#          del manifest, una sola vez por (run_id × algoritmo).
#          CV interno espacial K=3, métrica TSS (umbral de Youden
#          sobre el pool de folds). Persiste best_hp.csv (consumido
#          por train_pipeline.R y por #6) y la grilla completa por
#          run×algo. Manifest-driven con el mismo pareo algo↔
#          background (#46) que train_pipeline.R.
#
#          DEBE correrse con Rscript directamente (guarda el bloque
#          principal con sys.nframe(), como predict/xai/residuals):
#            Rscript r/src/tune_pipeline.R
#          Skip-if-exists por (run×algo) ya en best_hp.csv; forzar
#          recálculo con SDM_FORCE=1.
# ============================================================

source("r/src/tune_models.R")
source("r/src/spatial_cv.R")

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(purrr)
  library(tidyr)
  library(tibble)
})

# ------------------------------------------------------------
# Configuración
# ------------------------------------------------------------
DATASETS_ROOT     <- "data/outputs/sdm_parallel"
MODELS_ROOT       <- "data/outputs/sdm_models"
ENV_DIR           <- "data/features/env_2.5m_ar/bioclim"
ARGENTINA_SHP     <- "data/shp/argentina/argentina.shp"

SEED              <- 42L
K_INTERNAL        <- 3L
BLOCK_SIZE_CAP_KM <- 300L

ALGOS             <- c("maxnet", "ranger", "xgboost")
# #46: cada algoritmo tunea sólo sobre los datasets de su estrategia de background.
ALGO_BP_STRATEGY  <- c(maxnet = "fixed", ranger = "match_presence",
                       xgboost = "match_presence")

BEST_HP_PATH      <- file.path(MODELS_ROOT, "best_hp.csv")

FORCE <- tolower(Sys.getenv("SDM_FORCE", "")) %in% c("1", "true", "t", "yes", "y")

# ------------------------------------------------------------
# Tamaño de bloque: reusar el global de train_pipeline si existe
# (spatial_cv_config.csv), para que el block size sea IDÉNTICO entre
# tuneo y reporte. Si no existe, calibrar acá.
# ------------------------------------------------------------
get_block_size_m <- function() {
  cfg <- file.path(MODELS_ROOT, "spatial_cv_config.csv")
  if (file.exists(cfg)) {
    size_m <- readr::read_csv(cfg, show_col_types = FALSE)$size_m[1]
    message("Block size leído de spatial_cv_config.csv: ", round(size_m / 1000), " km")
    return(size_m)
  }
  message("spatial_cv_config.csv ausente; calibrando block size...")
  env_paths <- list.files(ENV_DIR, pattern = "\\.tif$", full.names = TRUE)
  if (length(env_paths) == 0) stop("No se encontraron rasters de BIO en ", ENV_DIR)
  calibrate_block_size(env_raster_paths = env_paths, argentina_shp = ARGENTINA_SHP,
                       size_cap_km = BLOCK_SIZE_CAP_KM, seed = SEED)$size_m
}

# ------------------------------------------------------------
# Cache de folds internos (K=3) por run_id: dependen sólo del dataset.
# ------------------------------------------------------------
.fold_cache <- new.env(parent = emptyenv())
get_internal_folds <- function(run_id, dataset_path, block_size_m) {
  if (!is.null(.fold_cache[[run_id]])) return(.fold_cache[[run_id]])
  ds_df <- readr::read_csv(dataset_path, show_col_types = FALSE)
  sb <- assign_spatial_folds(df = ds_df, size_m = block_size_m,
                             k = K_INTERNAL, seed = SEED)
  .fold_cache[[run_id]] <- sb$fold_id
  sb$fold_id
}

# ------------------------------------------------------------
# Tunea un (run × algo) o reusa su fila previa.
# ------------------------------------------------------------
tune_one <- function(i, algo, manifest, existing_best_hp, block_size_m) {
  run_id <- manifest$run_id[i]

  if (!FORCE && !is.null(existing_best_hp)) {
    prev <- dplyr::filter(existing_best_hp,
                          run_id == !!run_id, algorithm == !!algo)
    if (nrow(prev) > 0) {
      message("[", i, "] ", run_id, " / ", algo, "  [skip: ya tuneado]")
      return(prev[1, , drop = FALSE])
    }
  }

  dataset_path <- file.path(DATASETS_ROOT, run_id, "sdm_dataset_model_ready.csv")
  ds      <- load_model_ready_dataset(dataset_path)   # de train_models.R
  fold_id <- get_internal_folds(run_id, dataset_path, block_size_m)
  grid    <- build_hp_grid(algo, length(ds$predictors))

  res <- tune_hp_for_dataset(algo, ds, fold_id, grid = grid, seed = SEED)

  # Persistir la grilla completa (evidencia de búsqueda para el informe).
  tune_dir <- file.path(MODELS_ROOT, run_id, "tune")
  dir.create(tune_dir, recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(res$scores_table, file.path(tune_dir, paste0(algo, "_scores.csv")))

  message("[", i, "] ", run_id, " / ", algo,
          "  best TSS = ", round(res$best_tss, 3))
  hp_to_row(run_id, algo, res$best_hp, res$best_tss,
            n_combos = length(grid), k_internal = K_INTERNAL)
}

# ------------------------------------------------------------
# Bloque principal (sólo bajo Rscript directo)
# ------------------------------------------------------------
if (sys.nframe() == 0L) {
  dir.create(MODELS_ROOT, recursive = TRUE, showWarnings = FALSE)

  manifest_path <- file.path(DATASETS_ROOT, "manifest.csv")
  if (!file.exists(manifest_path)) {
    stop("No se encontró el manifest de datasets en ", manifest_path,
         ". Corré primero r/src/dataset_pipeline.R")
  }
  datasets_manifest <- read_csv(manifest_path, show_col_types = FALSE)

  # Filtro opcional por especie: SDM_SPECIES="Coprinus comatus" (o lista
  # separada por comas) tunea sólo esas especies. Vacío = todas (default).
  # Espejo del filtro de train_pipeline.R (commit 01f6778).
  species_filter <- trimws(Sys.getenv("SDM_SPECIES", ""))
  if (nzchar(species_filter)) {
    wanted <- trimws(strsplit(species_filter, ",")[[1]])
    datasets_manifest <- dplyr::filter(datasets_manifest,
                                       tolower(species) %in% tolower(wanted))
    message("Filtro SDM_SPECIES activo -> ", paste(wanted, collapse = ", "),
            " (", nrow(datasets_manifest), " datasets)")
    if (nrow(datasets_manifest) == 0) {
      stop("SDM_SPECIES no coincide con ninguna especie del manifest.")
    }
  }

  block_size_m <- get_block_size_m()

  existing_best_hp <- if (file.exists(BEST_HP_PATH)) {
    read_csv(BEST_HP_PATH, show_col_types = FALSE)
  } else NULL

  # Grid de pares (run × algo) tras el pareo algo↔background (#46).
  tune_grid <- tidyr::expand_grid(
    algo = ALGOS,
    i    = seq_len(nrow(datasets_manifest))
  ) |>
    mutate(bp_n_strategy = datasets_manifest$bp_n_strategy[i]) |>
    filter(bp_n_strategy == ALGO_BP_STRATEGY[algo]) |>
    select(-bp_n_strategy)

  message(sprintf("Tuneando %d pares (run × algo)...", nrow(tune_grid)))

  best_hp <- purrr::pmap_dfr(
    tune_grid,
    function(algo, i) tune_one(i, algo, datasets_manifest,
                               existing_best_hp, block_size_m)
  )

  # Si se filtró por especie, preservar las filas ya tuneadas de las demás
  # especies; de lo contrario write_csv sobrescribiría best_hp.csv con sólo
  # las nuevas.
  if (nzchar(species_filter) && !is.null(existing_best_hp)) {
    keep <- dplyr::anti_join(existing_best_hp, best_hp,
                             by = c("run_id", "algorithm"))
    best_hp <- dplyr::bind_rows(keep, best_hp)
  }
  write_csv(best_hp, BEST_HP_PATH)
  message("OK. best_hp.csv escrito en: ", BEST_HP_PATH)
  print(best_hp)
}
