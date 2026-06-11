# ============================================================
# File: xai_pipeline.R
# Purpose: Orquesta la Etapa 4 — corre SHAP + PDP + LIME sobre
#          el ganador (de Etapa 3) y un modelo de contraste con
#          corrección de sesgo. Persiste todos los artefactos
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
source("r/src/xai_pdp.R")
source("r/src/xai_lime.R")
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

# Puntos críticos de LIME por especie.
LIME_CRITICAL_POINTS <- list(
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
    "pampa_humeda",      -59.5,  -35.5,    "Pampa humeda (nucleo de distribucion)",
    "patagonia_fueguina", -67.7, -53.8,    "Patagonia fueguina (Rio Grande, Tierra del Fuego)"
  )
)

# Hiperparámetros XAI (alineados con el spec).
SHAP_N_BACKGROUND <- 200L
SHAP_NSIM         <- 100L
PDP_GRID_RES      <- 50L
LIME_N_FEATURES   <- 5L
LIME_N_PERMS      <- 5000L

# ------------------------------------------------------------
# Helpers
# ------------------------------------------------------------

species_from_run_id <- function(run_id) {
  sub("__.*$", "", run_id)
}

# ------------------------------------------------------------
# explain_one_run: corre SHAP + PDP + LIME para un run y
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

  # ---- PDP ----
  message("  PDP: ", length(pred_cols), " vars × grid=", PDP_GRID_RES)
  X_train <- ds |> dplyr::select(dplyr::all_of(pred_cols))
  pdp_df  <- compute_pdp(model, X_train, predictors = pred_cols,
                         grid_resolution = PDP_GRID_RES)
  p_pdp   <- plot_pdp_grid(pdp_df, imp, run_id)
  readr::write_csv(pdp_df, file.path(run_dir, "pdp_long.csv"))
  ggsave(file.path(run_dir, "pdp_grid.png"),
         p_pdp, width = 12, height = 10, dpi = 150)

  # ---- LIME ----
  species <- species_from_run_id(run_id)
  if (!species %in% names(LIME_CRITICAL_POINTS)) {
    stop("explain_one_run: faltan LIME_CRITICAL_POINTS para la especie '",
         species, "'. Agregar entrada al orquestador.")
  }
  crit <- LIME_CRITICAL_POINTS[[species]]

  # LIME es el XAI secundario (SHAP es el primario). Su integración con lime
  # falla en algunos modelos (p. ej. ranger con predict_model custom -> "NAs in
  # V(mu)" de glmnet). Se aísla en tryCatch para que una falla de LIME no tire
  # abajo el run: SHAP + PDP igual quedan persistidos.
  message("  LIME: seleccionando 8 puntos + explicando")
  lime_ok <- tryCatch({
    pts <- select_lime_points(preds, ds, env_stack, crit)
    lime_df <- compute_lime(model, pts, X_train,
                            n_features = LIME_N_FEATURES,
                            n_permutations = LIME_N_PERMS)
    p_lime <- plot_lime_panel(lime_df, pts, run_id)

    readr::write_csv(pts |> dplyr::select(point_id, lon, lat, score,
                                          origin, class_observed),
                     file.path(run_dir, "lime_points.csv"))
    readr::write_csv(lime_df, file.path(run_dir, "lime_weights.csv"))
    ggsave(file.path(run_dir, "lime_panel.png"),
           p_lime, width = 14, height = 7, dpi = 150)
    nrow(pts)
  }, error = function(e) {
    warning("explain_one_run: LIME falló para ", run_id, " / ", algorithm,
            " (", conditionMessage(e), "). Se persisten SHAP + PDP igual.")
    NA_integer_
  })

  lime_path <- function(f) if (is.na(lime_ok)) NA_character_ else
    normalizePath(file.path(run_dir, f), mustWork = TRUE)

  # ---- Manifest row ----
  tibble::tibble(
    run_id            = run_id,
    cv_scheme         = cv_scheme,
    algorithm         = algorithm,
    role              = role,
    is_winner         = startsWith(role, "winner_"),
    shap_path         = normalizePath(file.path(run_dir, "shap_values.csv"),         mustWork = TRUE),
    importance_path   = normalizePath(file.path(run_dir, "importance_ranking.csv"),  mustWork = TRUE),
    pdp_long_path     = normalizePath(file.path(run_dir, "pdp_long.csv"),            mustWork = TRUE),
    pdp_png_path      = normalizePath(file.path(run_dir, "pdp_grid.png"),            mustWork = TRUE),
    lime_points_path  = lime_path("lime_points.csv"),
    lime_weights_path = lime_path("lime_weights.csv"),
    lime_panel_path   = lime_path("lime_panel.png"),
    n_explained_shap  = nrow(X_explain),
    n_explained_lime  = lime_ok
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
                    n_explained_shap, n_explained_lime)
  readr::write_csv(manifest, file.path(XAI_ROOT, "manifest.csv"))

  message("Listo. Artefactos en ", XAI_ROOT)
  invisible(manifest)
}

if (sys.nframe() == 0L) {
  main()
}
