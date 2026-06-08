# ============================================================
# File: ensemble_pipeline.R
# Purpose: Orquesta el ensemble bootstrap (#58). Para cada ganador
#          por bp_method (winner_summary.csv de predict_pipeline.R),
#          reajusta el modelo N veces sobre bootstraps estratificados
#          del dataset, predice el raster de idoneidad en cada réplica
#          y agrega a media (idoneidad robusta) + SD (incertidumbre
#          espacial). El bootstrap de los datos —no la semilla del
#          modelo— genera la variación, así que funciona también para
#          maxnet, que es determinista.
#
#          Ejecutar desde repo root CON Rscript (guarda en
#          sys.nframe()): Rscript r/src/ensemble_pipeline.R
#          Idempotente (skip-if-exists). SDM_FORCE=1 fuerza recálculo.
#          Depende de predict_pipeline.R (lee winner_summary.csv).
# ============================================================

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(tibble); library(purrr)
  library(ggplot2); library(terra); library(patchwork)
})

source("r/src/predict_distribution_map.R")  # load_env_stack, predict_suitability_raster
source("r/src/train_models.R")              # fit_model, predict_score
source("r/src/ensemble_models.R")           # fit_ensemble_member, aggregate_cell_stats
source("r/src/tune_models.R")               # hp_row_to_list

# Configuración -----------------------------------------------
N_REPLICATES  <- 25L
ENSEMBLE_SEED <- 42L

MODELS_ROOT   <- "data/outputs/sdm_models"
DATASETS_ROOT <- "data/outputs/sdm_parallel"
MAPS_ROOT     <- "data/outputs/sdm_maps"
ENSEMBLE_ROOT <- "data/outputs/sdm_ensemble"

force_recompute <- function() {
  v <- tolower(Sys.getenv("SDM_FORCE", ""))
  v %in% c("1", "true", "yes")
}

# ------------------------------------------------------------
# 1) HP tuneados del ganador (best_hp.csv) o fallback a defaults
# ------------------------------------------------------------
load_winner_hp <- function(run_id, algorithm, models_root = MODELS_ROOT) {
  bh_path <- file.path(models_root, "best_hp.csv")
  if (!file.exists(bh_path)) return(list())
  bh <- readr::read_csv(bh_path, show_col_types = FALSE)
  row <- bh |> dplyr::filter(run_id == !!run_id, algorithm == !!algorithm)
  if (nrow(row) == 0L) return(list())
  hp_row_to_list(row[1, ])
}

# ------------------------------------------------------------
# 2) PANEL media | SD
# ------------------------------------------------------------
plot_ensemble_panel <- function(mean_r, sd_r, tag, occ_points) {
  stopifnot(inherits(mean_r, "SpatRaster"), inherits(sd_r, "SpatRaster"),
            all(c("decimalLongitude", "decimalLatitude") %in% names(occ_points)))

  base_pts <- geom_point(
    data = occ_points,
    aes(x = decimalLongitude, y = decimalLatitude),
    colour = "white", fill = "#e31a1c",
    shape = 21, size = 1.0, stroke = 0.2, alpha = 0.8
  )

  p_mean <- ggplot() +
    tidyterra::geom_spatraster(data = mean_r) +
    scale_fill_viridis_c(name = "Idoneidad\nmedia", limits = c(0, 1),
                         na.value = "transparent") +
    base_pts + coord_sf(crs = 4326) +
    labs(title = paste0("Idoneidad media (", N_REPLICATES, " réplicas)")) +
    theme_minimal(base_size = 10) + theme(legend.position = "right")

  p_sd <- ggplot() +
    tidyterra::geom_spatraster(data = sd_r) +
    scale_fill_viridis_c(name = "SD", option = "magma",
                         na.value = "transparent") +
    base_pts + coord_sf(crs = 4326) +
    labs(title = "Incertidumbre (SD por celda)") +
    theme_minimal(base_size = 10) + theme(legend.position = "right")

  (p_mean | p_sd) +
    patchwork::plot_annotation(title = tag,
                               theme = theme(plot.title = element_text(face = "bold")))
}

# ------------------------------------------------------------
# 3) ENSEMBLE DE UN GANADOR
# ------------------------------------------------------------
ensemble_one_winner <- function(run_id, cv_scheme, algorithm, bp_method,
                                 env_stack, n_replicates = N_REPLICATES,
                                 models_root = MODELS_ROOT,
                                 datasets_root = DATASETS_ROOT,
                                 ensemble_root = ENSEMBLE_ROOT) {
  out_dir   <- file.path(ensemble_root, run_id, cv_scheme, algorithm)
  mean_path <- file.path(out_dir, "suitability_mean.tif")
  sd_path   <- file.path(out_dir, "suitability_sd.tif")
  png_path  <- file.path(out_dir, "ensemble_panel.png")
  tag       <- paste(bp_method, algorithm, sep = " / ")

  if (file.exists(mean_path) && file.exists(sd_path) && !force_recompute()) {
    message("  skip (existe): ", tag)
    sd_r <- terra::rast(sd_path)
    return(tibble(
      run_id = run_id, cv_scheme = cv_scheme, bp_method = bp_method,
      algorithm = algorithm, n_replicates = n_replicates,
      mean_path = normalizePath(mean_path), sd_path = normalizePath(sd_path),
      png_path  = if (file.exists(png_path)) normalizePath(png_path) else NA_character_,
      mean_sd   = as.numeric(terra::global(sd_r, "mean", na.rm = TRUE)[1, 1])
    ))
  }

  mr_path  <- file.path(datasets_root, run_id, "sdm_dataset_model_ready.csv")
  occ_path <- file.path(datasets_root, run_id, "occ_processed.csv")
  if (!file.exists(mr_path))  stop("missing sdm_dataset_model_ready.csv for ", run_id)
  if (!file.exists(occ_path)) stop("missing occ_processed.csv for ", run_id)

  ds  <- readr::read_csv(mr_path, show_col_types = FALSE)
  occ <- readr::read_csv(occ_path, show_col_types = FALSE)
  predictors <- setdiff(names(ds), c("class", "decimalLongitude", "decimalLatitude"))
  X <- ds[, predictors, drop = FALSE]
  y <- as.integer(ds$class)

  missing_layers <- setdiff(predictors, names(env_stack))
  if (length(missing_layers) > 0) {
    stop("env_stack no tiene capas para ", tag, ": ",
         paste(missing_layers, collapse = ", "))
  }
  env_sub <- env_stack[[predictors]]

  hp <- load_winner_hp(run_id, algorithm, models_root)

  message("  ensemble ", tag, " — ", n_replicates, " réplicas bootstrap")
  layers <- purrr::map(seq_len(n_replicates), function(b) {
    model <- fit_ensemble_member(algorithm, X, y, hp = hp, seed = ENSEMBLE_SEED + b)
    predict_suitability_raster(model, env_sub)
  })
  stk <- terra::rast(layers)

  mean_r <- terra::app(stk, fun = mean, na.rm = TRUE); names(mean_r) <- "suitability_mean"
  sd_r   <- terra::app(stk, fun = sd,   na.rm = TRUE); names(sd_r)   <- "suitability_sd"

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  terra::writeRaster(mean_r, mean_path, overwrite = TRUE)
  terra::writeRaster(sd_r,   sd_path,   overwrite = TRUE)

  fig <- plot_ensemble_panel(mean_r, sd_r, tag, occ)
  ggsave(png_path, fig, width = 11, height = 7, dpi = 150)

  tibble(
    run_id = run_id, cv_scheme = cv_scheme, bp_method = bp_method,
    algorithm = algorithm, n_replicates = n_replicates,
    mean_path = normalizePath(mean_path), sd_path = normalizePath(sd_path),
    png_path  = normalizePath(png_path),
    mean_sd   = as.numeric(terra::global(sd_r, "mean", na.rm = TRUE)[1, 1])
  )
}

# ------------------------------------------------------------
# 4) MAIN
# ------------------------------------------------------------
main <- function() {
  winner_path <- file.path(MAPS_ROOT, "winner_summary.csv")
  if (!file.exists(winner_path)) {
    stop("Falta ", winner_path, " — corré primero Rscript r/src/predict_pipeline.R")
  }
  winners <- readr::read_csv(winner_path, show_col_types = FALSE)
  stopifnot(all(c("run_id", "cv_scheme", "algorithm", "bp_method") %in% names(winners)))

  dir.create(ENSEMBLE_ROOT, recursive = TRUE, showWarnings = FALSE)
  message("Ensemble bootstrap de ", nrow(winners), " ganador(es) por bp_method.")

  env <- load_env_stack()

  rows <- purrr::pmap(
    winners |> dplyr::select(run_id, cv_scheme, algorithm, bp_method),
    function(run_id, cv_scheme, algorithm, bp_method) {
      tryCatch(
        ensemble_one_winner(run_id, cv_scheme, algorithm, bp_method, env),
        error = function(e) {
          message("ERROR ensemble ", run_id, " / ", algorithm, ": ",
                  conditionMessage(e))
          NULL
        }
      )
    }
  ) |> purrr::compact() |> dplyr::bind_rows()

  readr::write_csv(rows, file.path(ENSEMBLE_ROOT, "ensemble_summary.csv"))
  message("Listo. Artefactos en ", ENSEMBLE_ROOT)
  invisible(rows)
}

if (sys.nframe() == 0L) {
  main()
}
