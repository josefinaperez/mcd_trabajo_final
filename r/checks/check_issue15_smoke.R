# Smoke test #15: un run_id, 3 algos, 2 esquemas. Verifica que
# se escriben los artefactos esperados y que metrics tiene AUC válido.
# TEMPORAL: borrar al cerrar el issue.
source("r/src/train_models.R")
source("r/src/spatial_cv.R")

suppressPackageStartupMessages({ library(readr); library(dplyr) })

DATASETS_ROOT <- "data/outputs/sdm_parallel"
OUT_ROOT      <- file.path(tempdir(), "issue15_smoke")
ALGOS         <- c("maxnet", "ranger", "xgboost")

manifest <- read_csv(file.path(DATASETS_ROOT, "manifest.csv"), show_col_types = FALSE)
run_id   <- manifest$run_id[1]
ds_path  <- file.path(DATASETS_ROOT, run_id, "sdm_dataset_model_ready.csv")
cat("Smoke run_id:", run_id, "\n")

# fold_id para spatial_block: bloque chico fijo para que sea rápido
ds_df  <- read_csv(ds_path, show_col_types = FALSE)
sb     <- assign_spatial_folds(df = ds_df, size_m = 200000, k = 5L, seed = 42)

ok <- TRUE
for (algo in ALGOS) {
  for (scheme in c("holdout", "spatial_block")) {
    m <- run_model_for_dataset(
      algo = algo, run_id = run_id, dataset_path = ds_path,
      out_root = OUT_ROOT, cv_scheme = scheme,
      fold_id = if (scheme == "spatial_block") sb$fold_id else NULL,
      hp = if (algo == "maxnet") list(regmult = 1) else list()
    )
    dir <- file.path(OUT_ROOT, run_id, scheme, algo)
    files_ok <- all(file.exists(file.path(dir,
      c("model.rds","predictions_test.csv","metrics.csv","response_curves.csv","response_curves.png"))))
    auc_ok <- is.finite(m$auc_test) && m$auc_test >= 0 && m$auc_test <= 1
    algo_ok <- identical(m$algorithm, algo)
    cat(sprintf("  %-8s %-14s files=%s auc=%.3f(%s) algo_col=%s\n",
                algo, scheme, files_ok, m$auc_test, auc_ok, algo_ok))
    ok <- ok && files_ok && auc_ok && algo_ok
  }
}
cat(if (ok) "SMOKE OK\n" else "SMOKE FAIL\n")
if (!ok) quit(status = 1)
