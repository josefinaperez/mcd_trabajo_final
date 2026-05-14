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
