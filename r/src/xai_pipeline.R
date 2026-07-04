# ============================================================
# File: xai_pipeline.R
# Purpose: Orquesta la Etapa 4 — corre SHAP global + SHAP local
#          sobre el ganador (de Etapa 3) y un modelo de contraste
#          con corrección de sesgo. Persiste todos los artefactos
#          en data/outputs/sdm_xai/ y escribe manifest global.
#          Ejecutar desde repo root.
# ============================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tibble)
  library(purrr)
  library(ggplot2)
})

source("r/src/xai_predict.R")
source("r/src/xai_shap.R")
source("r/src/predict_distribution_map.R")  # load_env_stack

# ------------------------------------------------------------
# Configuración (constantes explícitas para la POC)
# ------------------------------------------------------------

MODELS_ROOT   <- "data/outputs/sdm_models"
DATASETS_ROOT <- "data/outputs/sdm_parallel"
MAPS_ROOT     <- "data/outputs/sdm_maps"
XAI_ROOT      <- "data/outputs/sdm_xai"

# Runs a explicar: se derivan del ganador cross-algoritmo por CV scheme
# que selecciona predict_pipeline (Etapa 3) y persiste en
# sdm_maps/winner_summary.csv. El trío (run_id, cv_scheme, algorithm)
# determina de qué subcarpeta del modelo se lee model.rds y
# predictions_test.csv; winner_role etiqueta las figuras.
read_xai_runs <- function(maps_root = MAPS_ROOT) {
  ws_path <- file.path(maps_root, "winner_summary.csv")
  if (!file.exists(ws_path)) {
    stop("Falta ", ws_path, " — corré Etapa 3 (predict_pipeline.R) primero")
  }
  ws <- readr::read_csv(ws_path, show_col_types = FALSE)
  stopifnot(all(c("run_id", "cv_scheme", "algorithm", "winner_role") %in% names(ws)))
  ws |> dplyr::transmute(run_id, cv_scheme, algorithm, role = winner_role)
}

# Puntos diagnósticos por especie (coordenadas fijas) para la
# explicación local (SHAP local).
DIAGNOSTIC_POINTS <- list(
  polyporaceae = tibble::tribble(
    ~point_id,           ~lon,    ~lat,    ~region,
    "yungas_NOA",        -64.85, -23.65,   "Yungas (Calilegua)",
    "andean_patagonia",  -71.5,  -41.0,    "Bosque andino-patagonico (Nahuel Huapi)"
  ),
  # Coprinus comatus: saprotrofo de suelo, no asociado a un bosque. Dos sitios
  # dentro de su rango registrado en AR: el nucleo pampeano (alta idoneidad) y
  # la Patagonia fueguina (presente pero ambiente contrastante, clima frio).
  coprinus_comatus = tibble::tribble(
    ~point_id,           ~lon,    ~lat,    ~region,
    "pampa_humeda",      -59.5,  -35.5,    "Pampa húmeda (núcleo de distribución)",
    "patagonia_fueguina", -67.7, -53.8,    "Patagonia fueguina (Río Grande, Tierra del Fuego)"
  )
)

# Hiperparámetros XAI (alineados con el spec).
SHAP_N_BACKGROUND <- 200L
SHAP_NSIM         <- 100L

# ------------------------------------------------------------
# Helpers
# ------------------------------------------------------------

species_from_run_id <- function(run_id) {
  sub("__.*$", "", run_id)
}

# ------------------------------------------------------------
# explain_one_run: corre SHAP global + SHAP local para un run y
# persiste todos los artefactos en data/outputs/sdm_xai/<run_id>/.
# Devuelve una fila del manifest.
# ------------------------------------------------------------

explain_one_run <- function(run_id, cv_scheme, algorithm, role, env_stack,
                            models_root   = MODELS_ROOT,
                            datasets_root = DATASETS_ROOT,
                            xai_root      = XAI_ROOT) {
  message("=== Explaining ", run_id, " / ", cv_scheme, " / ", algorithm, " (", role, ")")

  model_path  <- file.path(models_root,   run_id, cv_scheme, algorithm, "model.rds")
  preds_path  <- file.path(models_root,   run_id, cv_scheme, algorithm, "predictions_test.csv")
  ds_path     <- file.path(datasets_root, run_id, "sdm_dataset_model_ready.csv")
  stopifnot(file.exists(model_path), file.exists(preds_path), file.exists(ds_path))

  model <- readRDS(model_path)
  preds <- readr::read_csv(preds_path, show_col_types = FALSE)
  ds    <- readr::read_csv(ds_path,    show_col_types = FALSE)
  pred_cols <- setdiff(names(ds), c("class", "decimalLongitude", "decimalLatitude"))
  stopifnot(length(pred_cols) >= 1L)

  run_dir <- file.path(xai_root, run_id, cv_scheme, algorithm)
  dir.create(run_dir, recursive = TRUE, showWarnings = FALSE)

  # ---- SHAP ----
  X_explain <- preds |>
    dplyr::filter(class == 1) |>
    dplyr::select(decimalLongitude, decimalLatitude) |>
    dplyr::inner_join(
      ds |> dplyr::select(decimalLongitude, decimalLatitude,
                          dplyr::all_of(pred_cols)),
      by = c("decimalLongitude", "decimalLatitude")
    ) |>
    dplyr::select(dplyr::all_of(pred_cols))

  set.seed(42)
  X_bg <- ds |>
    dplyr::filter(class == 0) |>
    dplyr::slice_sample(n = min(SHAP_N_BACKGROUND, sum(ds$class == 0))) |>
    dplyr::select(dplyr::all_of(pred_cols))

  message("  SHAP: n_explain=", nrow(X_explain),
          " n_background=", nrow(X_bg), " nsim=", SHAP_NSIM)
  shap_df <- compute_shap(model, X_explain, X_bg, nsim = SHAP_NSIM)
  imp     <- importance_from_shap(shap_df)
  p_shap  <- plot_shap_summary(shap_df, X_explain, imp, run_id)

  readr::write_csv(shap_df, file.path(run_dir, "shap_values.csv"))
  readr::write_csv(imp,     file.path(run_dir, "importance_ranking.csv"))
  ggsave(file.path(run_dir, "shap_summary.png"),
         p_shap, width = 11, height = 6, dpi = 150)

  # ---- SHAP local ----
  # Los valores SHAP ya son por instancia: la explicación local reusa
  # compute_shap() sobre los puntos elegidos (2 presencias por cuantil de score
  # + 2 puntos diagnósticos de coordenadas fijas) con la misma referencia X_bg.
  species <- species_from_run_id(run_id)
  if (!species %in% names(DIAGNOSTIC_POINTS)) {
    stop("explain_one_run: faltan DIAGNOSTIC_POINTS para la especie '",
         species, "'. Agregar entrada al orquestador.")
  }
  crit <- DIAGNOSTIC_POINTS[[species]]

  message("  SHAP local: seleccionando 4 puntos + explicando")
  pts        <- select_local_points(preds, ds, env_stack, crit, model)
  X_local    <- pts |> dplyr::select(dplyr::all_of(pred_cols))
  shap_local <- compute_shap(model, X_local, X_bg, nsim = SHAP_NSIM)
  shap_local$point_id <- pts$point_id
  p_shap_local <- plot_shap_local_panel(shap_local, pts, run_id)

  readr::write_csv(pts |> dplyr::select(point_id, lon, lat, score,
                                        origin, class_observed),
                   file.path(run_dir, "shap_local_points.csv"))
  readr::write_csv(shap_local, file.path(run_dir, "shap_local_values.csv"))
  ggsave(file.path(run_dir, "shap_local_panel.png"),
         p_shap_local, width = 10, height = 8, dpi = 150)
  n_local <- nrow(pts)

  # ---- Manifest row ----
  tibble::tibble(
    run_id                 = run_id,
    cv_scheme              = cv_scheme,
    algorithm              = algorithm,
    role                   = role,
    is_winner              = startsWith(role, "winner_"),
    shap_path              = normalizePath(file.path(run_dir, "shap_values.csv"),          mustWork = TRUE),
    importance_path        = normalizePath(file.path(run_dir, "importance_ranking.csv"),   mustWork = TRUE),
    shap_local_points_path = normalizePath(file.path(run_dir, "shap_local_points.csv"),    mustWork = TRUE),
    shap_local_values_path = normalizePath(file.path(run_dir, "shap_local_values.csv"),    mustWork = TRUE),
    shap_local_panel_path  = normalizePath(file.path(run_dir, "shap_local_panel.png"),     mustWork = TRUE),
    n_explained_shap       = nrow(X_explain),
    n_explained_shap_local = n_local
  )
}

# ------------------------------------------------------------
# MAIN
# ------------------------------------------------------------

main <- function() {
  if (!dir.exists(MODELS_ROOT)) {
    stop("Falta ", MODELS_ROOT, " — corré Etapas 1-3 primero")
  }
  if (!dir.exists(MAPS_ROOT)) {
    stop("Falta ", MAPS_ROOT, " — corré Etapa 3 (predict_pipeline.R) primero")
  }
  dir.create(XAI_ROOT, recursive = TRUE, showWarnings = FALSE)

  set.seed(42)
  env <- load_env_stack()

  xai_runs <- read_xai_runs()
  message("Explicando ", nrow(xai_runs), " ganador(es) cross-algoritmo.")

  rows <- purrr::pmap(xai_runs, function(run_id, cv_scheme, algorithm, role) {
    tryCatch(
      explain_one_run(run_id, cv_scheme, algorithm, role, env_stack = env),
      error = function(e) {
        message("ERROR explicando ", run_id, " / ", cv_scheme, " / ", algorithm,
                ": ", conditionMessage(e))
        NULL
      }
    )
  }) |> purrr::compact() |> dplyr::bind_rows()

  if (nrow(rows) == 0L) {
    stop("Ningún run explicado exitosamente")
  }

  manifest <- rows |>
    dplyr::relocate(run_id, cv_scheme, algorithm, is_winner, role,
                    n_explained_shap, n_explained_shap_local)
  readr::write_csv(manifest, file.path(XAI_ROOT, "manifest.csv"))

  message("Listo. Artefactos en ", XAI_ROOT)
  invisible(manifest)
}

if (sys.nframe() == 0L) {
  main()
}
