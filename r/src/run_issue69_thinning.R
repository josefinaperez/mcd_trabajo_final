# ============================================================
# File: r/src/run_issue69_thinning.R
# Issue #69 — Resultados Paso 2: efecto del thinning (corrección
# de sesgo espacial) para Cyttaria hariotii.
#
# Corre un subconjunto ENFOCADO y NO-DESTRUCTIVO: bias_method
# {none, grid 5/15/30 km} × env_set {bioclim, bioclim_veg} ×
# background random, con el pareo algo–fondo estándar (maxnet→fixed,
# ranger/xgboost→match_presence). 8 configs (bias×env) → 24 modelos.
# CV espacial K=5 en todos los niveles (block size global reutilizado
# de spatial_cv_config.csv); la inestabilidad a 15/30 km se reporta.
#
# NO toca el manifest/summary globales: escribe a un dir propio
#   data/outputs/issue69_thinning/{thinning_summary.csv, presences_by_thinning.csv}
#   data/outputs/issue69_thinning/models/<run_id>/...
# Los datasets van a data/outputs/sdm_parallel/<run_id> (run_ids únicos,
# reutilizables por el run completo vía skip-if-exists).
#
# Rscript-directo (guarda con sys.nframe()):
#   Rscript r/src/run_issue69_thinning.R
# ============================================================

run_issue69_thinning <- function(species   = "Cyttaria hariotii",
                                  occ_file  = "df_cyttaria_hariotii.csv",
                                  block_size_m = 300000L,
                                  k_folds   = 5L) {
  out_dir      <- "data/outputs/issue69_thinning"
  out_models   <- file.path(out_dir, "models")
  out_parallel <- "data/outputs/sdm_parallel"
  dir.create(out_models, recursive = TRUE, showWarnings = FALSE)

  bioclim <- list.files("data/features/env_2.5m_ar/bioclim", "tif$", full.names = TRUE)
  veg     <- list.files("data/features/env_2.5m_ar/vegetation", "tif$", full.names = TRUE)
  env_sets <- list(
    bioclim     = list(files = bioclim),
    bioclim_veg = list(files = c(bioclim, veg))
  )

  sp_tab <- tibble::tibble(species = species, occ_file = occ_file)
  config <- make_config_table(
    sp_tab,
    bp_methods      = "random",
    bp_n_strategies = c("fixed", "match_presence"),
    fixed_bp_n      = 10000L,
    env_sets        = c("bioclim", "bioclim_veg"),
    grid_sizes_km   = c(5, 15, 30)
  )

  # ---- mapa antes/después: presencias por nivel de thinning (display map) ----
  occ_clean <- read_occurrences_csv(file.path("data/ocurrences/processed", occ_file))
  pres_levels <- purrr::map_dfr(c(0, 5, 15, 30), function(km) {
    p <- if (km == 0) occ_clean else
      apply_spatial_bias_method(occ_clean, "grid_thin", list(grid_km = km))
    tibble::tibble(decimalLongitude = p$decimalLongitude,
                   decimalLatitude  = p$decimalLatitude,
                   thinning_km = km)
  })
  readr::write_csv(pres_levels, file.path(out_dir, "presences_by_thinning.csv"))

  # ---- build + train por config ----
  rows <- list()
  for (i in seq_len(nrow(config))) {
    cr <- config[i, ]
    thin_km <- if (cr$bias_method == "none") 0 else cr$bias_param
    algos   <- if (cr$bp_n_strategy == "fixed") "maxnet" else c("ranger", "xgboost")

    message(sprintf("[%d/%d] %s", i, nrow(config), cr$run_id))
    man <- build_one_sdm_dataset(cr, env_sets = env_sets,
            occ_dir = "data/ocurrences/processed",
            out_dir = out_parallel, fixed_bp_n = 10000L)
    mr <- file.path(out_parallel, cr$run_id, "sdm_dataset_model_ready.csv")

    base_row <- tibble::tibble(
      species = species, env_set = cr$env_set, bias_method = cr$bias_method,
      thinning_km = thin_km, bp_n_strategy = cr$bp_n_strategy,
      n_presences = man$n_occ_after_bias, n_background = man$n_background
    )

    ds_df <- readr::read_csv(mr, show_col_types = FALSE)
    folds <- tryCatch(assign_spatial_folds(ds_df, size_m = block_size_m, k = k_folds),
                      error = function(e) { message("  folds FAIL: ", conditionMessage(e)); NULL })

    for (algo in algos) {
      if (is.null(folds)) {
        rows[[length(rows) + 1L]] <- base_row |>
          dplyr::mutate(algo = algo, tss = NA_real_, boyce = NA_real_,
                        auc = NA_real_, auc_sd = NA_real_, k_folds = NA_integer_,
                        status = "fold_assign_fail")
        next
      }
      r <- tryCatch({
        m <- run_model_for_dataset(algo, run_id = cr$run_id, dataset_path = mr,
              out_root = out_models, cv_scheme = "spatial_block",
              fold_id = folds$fold_id)
        ev <- evaluate_run_dir(file.path(out_models, cr$run_id, "spatial_block", algo))
        base_row |>
          dplyr::mutate(algo = algo, tss = ev$tss, boyce = ev$boyce,
                        auc = m$auc_test, auc_sd = m$auc_test_sd,
                        k_folds = m$k_folds, status = "ok")
      }, error = function(e) {
        base_row |>
          dplyr::mutate(algo = algo, tss = NA_real_, boyce = NA_real_,
                        auc = NA_real_, auc_sd = NA_real_, k_folds = NA_integer_,
                        status = paste0("train_fail: ", conditionMessage(e)))
      })
      rows[[length(rows) + 1L]] <- r
    }
  }

  summary <- dplyr::bind_rows(rows) |>
    dplyr::arrange(env_set, thinning_km, algo)
  readr::write_csv(summary, file.path(out_dir, "thinning_summary.csv"))
  message("\nEscrito: ", file.path(out_dir, "thinning_summary.csv"))
  print(summary, n = nrow(summary))
  invisible(summary)
}

if (sys.nframe() == 0) {
  suppressMessages({
    library(terra); library(dplyr); library(readr); library(tibble)
    library(stringr); library(purrr)
  })
  source("r/src/download_gbif.R");            source("r/src/preprocessing.R")
  source("r/src/grid_thinning.R");            source("r/src/build_parallel_sdm_datasets.R")
  source("r/src/train_models.R");             source("r/src/evaluate_model.R")
  source("r/src/spatial_cv.R")
  run_issue69_thinning()
}
