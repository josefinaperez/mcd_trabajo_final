# ============================================================
# File: build_final_manifest.R
# Purpose: Vista consolidada de las cuatro etapas del pipeline en
#          un único manifest, una fila por configuración
#          (run_id × cv_scheme × algorithm). Cruza:
#            - dataset + modelo  (sdm_models/manifest.csv)
#            - mapa              (sdm_maps/manifest.csv)
#            - XAI               (sdm_xai/manifest.csv)
#          mediante left-join anclado en el manifest de modelos
#          (set autoritativo de configuraciones entrenadas), de
#          modo que filas stale de etapas posteriores no inflen
#          la vista. Escribe data/outputs/final_manifest.csv.
#          Ejecutar desde repo root con
#          `Rscript r/src/build_final_manifest.R`.
# ============================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
})

MODELS_MANIFEST <- "data/outputs/sdm_models/manifest.csv"
MAPS_MANIFEST   <- "data/outputs/sdm_maps/manifest.csv"
XAI_MANIFEST    <- "data/outputs/sdm_xai/manifest.csv"
OUT_PATH        <- "data/outputs/final_manifest.csv"

KEY <- c("run_id", "cv_scheme", "algorithm")

build_final_manifest <- function(models_path = MODELS_MANIFEST,
                                 maps_path   = MAPS_MANIFEST,
                                 xai_path    = XAI_MANIFEST) {
  if (!file.exists(models_path)) {
    stop("Falta el manifest de modelos (", models_path,
         "). Corré r/src/train_pipeline.R primero.")
  }

  # Base: dataset + modelo + métricas (una fila por run × cv × algo).
  base <- read_csv(models_path, show_col_types = FALSE) |>
    select(
      all_of(KEY), species, env_set,
      bias_method, bias_param, bp_n_strategy, bp_n,
      n_train_pres, n_test_pres, n_background,
      auc_test, threshold_max_tss, tss, fnr, boyce,
      sensitivity, specificity, train_secs, model_dir
    )

  # Etapa mapa: flags de ganador + rutas de artefactos cartográficos.
  if (file.exists(maps_path)) {
    maps <- read_csv(maps_path, show_col_types = FALSE) |>
      select(all_of(KEY), is_winner, winner_role, tau_fnr_main,
             suit_path, bin_path, map_png_path = png_path)
    base <- left_join(base, maps, by = KEY)
  } else {
    message("(sin sdm_maps/manifest.csv: columnas de mapa vacías)")
  }

  # Etapa XAI: flag de presencia + rutas de artefactos de interpretabilidad.
  if (file.exists(xai_path)) {
    xai <- read_csv(xai_path, show_col_types = FALSE) |>
      transmute(across(all_of(KEY)), has_xai = TRUE,
                xai_role = role,
                shap_path, shap_importance_path = importance_path,
                pdp_png_path, lime_panel_path)
    base <- base |>
      left_join(xai, by = KEY) |>
      mutate(has_xai = !is.na(has_xai) & has_xai)
  } else {
    base <- mutate(base, has_xai = FALSE)
    message("(sin sdm_xai/manifest.csv: columnas de XAI vacías)")
  }

  base |>
    relocate(all_of(KEY), env_set) |>
    arrange(cv_scheme, desc(tss))
}

# ------------------------------------------------------------
# MAIN
# ------------------------------------------------------------

if (sys.nframe() == 0L) {
  fm <- build_final_manifest()
  write_csv(fm, OUT_PATH)
  n_win <- sum(fm$is_winner %in% TRUE, na.rm = TRUE)
  message(sprintf(
    "Manifest final: %d filas | %d con mapa | %d con XAI | %d ganador(es) -> %s",
    nrow(fm),
    sum(!is.na(fm$map_png_path)),
    sum(fm$has_xai %in% TRUE),
    n_win, OUT_PATH
  ))
}
