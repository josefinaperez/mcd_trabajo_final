# ============================================================
# File: predict_pipeline.R
# Purpose: Orquesta la Etapa 3 — selecciona el modelo ganador
#          según el esquema dual TSS+FNR (§4.5 de la
#          metodología), genera el mapa de distribución del
#          ganador y de los sobrevivientes del filtro τ_FNR,
#          y persiste manifest + análisis de robustez.
#          Ejecutar desde repo root.
# ============================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tibble)
  library(purrr)
})

source("r/src/predict_distribution_map.R")

# Configuración de Etapa 3 ------------------------------------
TAU_FNR_MAIN <- 0.20                              # umbral principal a posteriori
TAU_FNR_GRID <- c(0.15, 0.20, 0.25, 0.30)         # grilla de robustez

MODELS_ROOT   <- "data/outputs/sdm_models"
DATASETS_ROOT <- "data/outputs/sdm_parallel"
MAPS_ROOT     <- "data/outputs/sdm_maps"

# ------------------------------------------------------------
# 1) SELECCIÓN: filtro FNR + argmax TSS
# ------------------------------------------------------------
#
# Dado el summary de Etapa 2, marca los runs que pasan el
# filtro de consistencia (fnr <= tau_fnr) y, entre ellos,
# el ganador (argmax tss). Devuelve el summary enriquecido
# con dos columnas booleanas: passes_filter, is_winner.
# Si ningún run pasa el filtro, ambas columnas son FALSE.
# ------------------------------------------------------------

select_runs <- function(summary_df, tau_fnr) {
  stopifnot(all(c("run_id", "tss", "fnr") %in% names(summary_df)))

  out <- summary_df |>
    mutate(passes_filter = fnr <= tau_fnr)

  winner_id <- out |>
    filter(passes_filter) |>
    slice_max(tss, n = 1, with_ties = FALSE) |>
    pull(run_id)

  out |> mutate(is_winner = run_id %in% winner_id)
}

# ------------------------------------------------------------
# 2) ROBUSTEZ DE τ_FNR
# ------------------------------------------------------------
#
# Aplica select_runs() sobre una grilla de valores de τ_FNR
# y reporta, para cada uno, el ganador resultante, su TSS
# y la cantidad de sobrevivientes. Es la evidencia empírica
# de robustez prometida en §4.5.3 del documento metodológico.
# ------------------------------------------------------------

compute_tau_fnr_robustness <- function(summary_df, tau_fnr_grid) {
  purrr::map_dfr(tau_fnr_grid, function(t) {
    sel <- select_runs(summary_df, t)
    winner <- sel |> filter(is_winner)
    tibble(
      tau_fnr        = t,
      n_survivors    = sum(sel$passes_filter),
      winner_run_id  = if (nrow(winner) == 1L) winner$run_id else NA_character_,
      winner_tss     = if (nrow(winner) == 1L) winner$tss   else NA_real_,
      winner_fnr     = if (nrow(winner) == 1L) winner$fnr   else NA_real_
    )
  })
}

# ------------------------------------------------------------
# 3) MAPEO DE UN RUN INDIVIDUAL
# ------------------------------------------------------------
#
# Lee model.rds, metrics.csv y occ_processed.csv del run,
# genera los 3 artefactos cartográficos vía las funciones
# de predict_distribution_map.R, y devuelve una fila del
# manifest con paths absolutos.
# ------------------------------------------------------------

map_one_run <- function(run_id, env_stack, models_root, datasets_root, maps_root) {
  model_path   <- file.path(models_root,   run_id, "model.rds")
  metrics_path <- file.path(models_root,   run_id, "metrics.csv")
  occ_path     <- file.path(datasets_root, run_id, "occ_processed.csv")

  if (!file.exists(model_path))   stop("missing model.rds for ", run_id)
  if (!file.exists(metrics_path)) stop("missing metrics.csv for ", run_id)
  if (!file.exists(occ_path))     stop("missing occ_processed.csv for ", run_id)

  model   <- readRDS(model_path)
  metrics <- readr::read_csv(metrics_path, show_col_types = FALSE)
  occ     <- readr::read_csv(occ_path,     show_col_types = FALSE)
  tau     <- as.numeric(metrics$threshold_max_tss)

  suit_r <- predict_suitability_raster(model, env_stack)
  bin_r  <- binarize_raster(suit_r, tau)
  fig    <- plot_map_panel(suit_r, bin_r, run_id, tau, occ)

  run_dir <- file.path(maps_root, run_id)
  save_map_artifacts(run_dir, suit_r, bin_r, fig)

  tibble(
    run_id            = run_id,
    threshold_max_tss = tau,
    suit_path         = normalizePath(file.path(run_dir, "suitability.tif"),    mustWork = TRUE),
    bin_path          = normalizePath(file.path(run_dir, "binary_presence.tif"), mustWork = TRUE),
    png_path          = normalizePath(file.path(run_dir, "map_panel.png"),       mustWork = TRUE)
  )
}

# ------------------------------------------------------------
# 4) MAIN
# ------------------------------------------------------------

main <- function() {
  if (!dir.exists(MODELS_ROOT)) {
    stop("Falta data/outputs/sdm_models — corré primero r/src/train_pipeline.R")
  }
  dir.create(MAPS_ROOT, recursive = TRUE, showWarnings = FALSE)

  summary_df <- readr::read_csv(file.path(MODELS_ROOT, "summary_table.csv"),
                                show_col_types = FALSE)

  sel <- select_runs(summary_df, TAU_FNR_MAIN)
  if (!any(sel$passes_filter)) {
    stop("Ningún modelo pasa el filtro τ_FNR = ", TAU_FNR_MAIN,
         ". Revisar la distribución empírica de FNR.")
  }

  rob <- compute_tau_fnr_robustness(summary_df, TAU_FNR_GRID)
  readr::write_csv(rob, file.path(MAPS_ROOT, "tau_fnr_robustness.csv"))

  to_map <- sel |> filter(passes_filter) |> pull(run_id)
  message("Mapeando ", length(to_map), " run(s): ", paste(to_map, collapse = ", "))

  env <- load_env_stack()

  rows <- purrr::map(to_map, function(rid) {
    tryCatch(
      map_one_run(rid, env, MODELS_ROOT, DATASETS_ROOT, MAPS_ROOT),
      error = function(e) {
        message("ERROR mapeando ", rid, ": ", conditionMessage(e))
        NULL
      }
    )
  }) |> purrr::compact() |> dplyr::bind_rows()

  manifest <- rows |>
    mutate(
      tau_fnr_main = TAU_FNR_MAIN,
      is_winner    = run_id %in% sel$run_id[sel$is_winner]
    ) |>
    relocate(run_id, is_winner, tau_fnr_main, threshold_max_tss)

  readr::write_csv(manifest, file.path(MAPS_ROOT, "manifest.csv"))

  winner_summary <- sel |>
    filter(is_winner) |>
    transmute(
      run_id,
      tau_fnr_main      = TAU_FNR_MAIN,
      threshold_max_tss,
      tss,
      fnr,
      auc_test
    )
  readr::write_csv(winner_summary, file.path(MAPS_ROOT, "winner_summary.csv"))

  message("Listo. Artefactos en ", MAPS_ROOT)
  invisible(manifest)
}

if (sys.nframe() == 0L) {
  main()
}
