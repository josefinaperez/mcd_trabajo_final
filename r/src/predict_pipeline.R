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

# #47: los env_sets antrópicos son DIAGNÓSTICO de sesgo (¿domina la
# accesibilidad en XAI?), no predictores ecológicos del mapa final. Se
# excluyen de la candidatura a ganador para que el mapa de distribución no
# quede apoyado en travel_time (proxy de esfuerzo de muestreo). Siguen
# entrenándose y mapeándose como sobrevivientes/comparación.
WINNER_EXCLUDE_REGEX <- "_anthro"

# ------------------------------------------------------------
# 1) SELECCIÓN: filtro FNR + argmax TSS por cv_scheme
# ------------------------------------------------------------
#
# Dado el summary de Etapa 2, marca los runs que pasan el
# filtro de consistencia (fnr <= tau_fnr) y, entre ellos,
# el ganador (argmax tss) DENTRO de cada cv_scheme. Devuelve
# el summary enriquecido con:
#   - passes_filter (lógico)
#   - is_winner     (lógico, ganador dentro del cv_scheme)
#   - winner_role   (character: "winner_<cv_scheme>" o NA)
# Si ningún run pasa el filtro en un esquema, sus winners
# quedan en NA/FALSE para ese esquema.
# ------------------------------------------------------------

select_runs <- function(summary_df, tau_fnr, winner_exclude_regex = NULL) {
  stopifnot(all(c("run_id", "cv_scheme", "bp_method", "algorithm", "tss", "fnr") %in% names(summary_df)))

  out <- summary_df |>
    mutate(passes_filter = fnr <= tau_fnr)

  # Ganador cross-algoritmo: argmax TSS sobre todos los (run_id, algorithm)
  # que pasan el filtro DENTRO de cada (cv_scheme, bp_method) (#57). Se declara
  # un ganador por estrategia de background porque el TSS no es comparable ENTRE
  # bp_method (los backgrounds disímiles inflan el contraste) pero sí DENTRO de
  # uno. La comparación entre ganadores (Boyce como árbitro) es del análisis.
  # El ganador del grupo puede ser cualquier algoritmo. winner_exclude_regex
  # (#47) saca de la candidatura a los env_sets de diagnóstico (antrópicos), que
  # igual quedan como sobrevivientes (passes_filter) pero nunca is_winner.
  winner_pool <- out |> filter(passes_filter)
  if (!is.null(winner_exclude_regex)) {
    winner_pool <- winner_pool |> filter(!grepl(winner_exclude_regex, run_id))
  }
  winners <- winner_pool |>
    group_by(cv_scheme, bp_method) |>
    slice_max(tss, n = 1, with_ties = FALSE) |>
    ungroup() |>
    transmute(run_id, cv_scheme, algorithm, .is_winner_row = TRUE)

  out |>
    left_join(winners, by = c("run_id", "cv_scheme", "algorithm")) |>
    mutate(
      is_winner   = !is.na(.is_winner_row),
      winner_role = ifelse(is_winner,
                           paste0("winner_", cv_scheme, "_", bp_method),
                           NA_character_)
    ) |>
    select(-.is_winner_row)
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

compute_tau_fnr_robustness <- function(summary_df, tau_fnr_grid, winner_exclude_regex = NULL) {
  purrr::map_dfr(tau_fnr_grid, function(t) {
    sel <- select_runs(summary_df, t, winner_exclude_regex)
    survivors <- sel |>
      group_by(cv_scheme, bp_method) |>
      summarise(n_survivors = sum(passes_filter), .groups = "drop")
    sel |>
      filter(is_winner) |>
      transmute(
        tau_fnr          = t,
        cv_scheme,
        bp_method,
        winner_run_id    = run_id,
        winner_algorithm = algorithm,
        winner_tss       = tss,
        winner_fnr       = fnr
      ) |>
      left_join(survivors, by = c("cv_scheme", "bp_method"))
  })
}

# ------------------------------------------------------------
# 3) MAPEO DE UN PAR (run_id, cv_scheme)
# ------------------------------------------------------------
#
# Lee model.rds, metrics.csv y occ_processed.csv del trío
# (run_id, cv_scheme, algorithm), genera los 3 artefactos
# cartográficos y devuelve una fila del manifest con paths
# absolutos. El layout en disco es
# models_root/<run_id>/<cv_scheme>/<algorithm>/... y
# maps_root/<run_id>/<cv_scheme>/<algorithm>/...
# ------------------------------------------------------------

map_one_run <- function(run_id, cv_scheme, algorithm, env_stack,
                        models_root, datasets_root, maps_root) {
  model_path   <- file.path(models_root,   run_id, cv_scheme, algorithm, "model.rds")
  metrics_path <- file.path(models_root,   run_id, cv_scheme, algorithm, "metrics.csv")
  occ_path     <- file.path(datasets_root, run_id, "occ_processed.csv")
  mr_path      <- file.path(datasets_root, run_id, "sdm_dataset_model_ready.csv")

  tag <- paste(run_id, cv_scheme, algorithm, sep = " / ")
  if (!file.exists(model_path))   stop("missing model.rds for ", tag)
  if (!file.exists(metrics_path)) stop("missing metrics.csv for ", tag)
  if (!file.exists(occ_path))     stop("missing occ_processed.csv for ", run_id)
  if (!file.exists(mr_path))      stop("missing sdm_dataset_model_ready.csv for ", run_id)

  model   <- readRDS(model_path)
  metrics <- readr::read_csv(metrics_path, show_col_types = FALSE)
  occ     <- readr::read_csv(occ_path,     show_col_types = FALSE)
  tau     <- as.numeric(metrics$threshold_max_tss)

  # Subsetea el stack a los predictores reales de este run (env_set),
  # leídos del header del dataset. Así un modelo bioclim no hereda el
  # extent (ni los NA) de las capas de vegetación del superset.
  predictors <- setdiff(
    names(readr::read_csv(mr_path, n_max = 0, show_col_types = FALSE)),
    c("class", "decimalLongitude", "decimalLatitude")
  )
  missing_layers <- setdiff(predictors, names(env_stack))
  if (length(missing_layers) > 0) {
    stop("env_stack no tiene capas para ", tag, ": ",
         paste(missing_layers, collapse = ", "))
  }

  suit_r <- predict_suitability_raster(model, env_stack[[predictors]])
  bin_r  <- binarize_raster(suit_r, tau)
  fig    <- plot_map_panel(suit_r, bin_r, tag, tau, occ)

  run_dir <- file.path(maps_root, run_id, cv_scheme, algorithm)
  save_map_artifacts(run_dir, suit_r, bin_r, fig)

  tibble(
    run_id            = run_id,
    cv_scheme         = cv_scheme,
    algorithm         = algorithm,
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

  sel <- select_runs(summary_df, TAU_FNR_MAIN, WINNER_EXCLUDE_REGEX)
  if (!any(sel$passes_filter)) {
    stop("Ningún modelo pasa el filtro τ_FNR = ", TAU_FNR_MAIN,
         ". Revisar la distribución empírica de FNR.")
  }

  rob <- compute_tau_fnr_robustness(summary_df, TAU_FNR_GRID, WINNER_EXCLUDE_REGEX)
  readr::write_csv(rob, file.path(MAPS_ROOT, "tau_fnr_robustness.csv"))

  to_map <- sel |> filter(passes_filter) |> select(run_id, cv_scheme, algorithm)
  message("Mapeando ", nrow(to_map), " trío(s) (run_id, cv_scheme, algorithm).")

  env <- load_env_stack()

  rows <- purrr::pmap(to_map, function(run_id, cv_scheme, algorithm) {
    tryCatch(
      map_one_run(run_id, cv_scheme, algorithm, env, MODELS_ROOT, DATASETS_ROOT, MAPS_ROOT),
      error = function(e) {
        message("ERROR mapeando ", run_id, " / ", cv_scheme, " / ", algorithm,
                ": ", conditionMessage(e))
        NULL
      }
    )
  }) |> purrr::compact() |> dplyr::bind_rows()

  manifest <- rows |>
    left_join(
      sel |> select(run_id, cv_scheme, algorithm, is_winner, winner_role),
      by = c("run_id", "cv_scheme", "algorithm")
    ) |>
    mutate(tau_fnr_main = TAU_FNR_MAIN) |>
    relocate(run_id, cv_scheme, algorithm, is_winner, winner_role,
             tau_fnr_main, threshold_max_tss)

  readr::write_csv(manifest, file.path(MAPS_ROOT, "manifest.csv"))

  winner_summary <- sel |>
    filter(is_winner) |>
    transmute(
      run_id,
      cv_scheme,
      bp_method,
      algorithm,
      winner_role,
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
