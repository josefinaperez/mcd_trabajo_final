# ============================================================
# File: r/checks/check_idempotency.R
# Purpose: Chequeo de los predicados de idempotencia (skip-if-exists)
#          de build/train sobre directorios sintéticos temporales.
#          No descarga ni entrena nada.
# Ejecutar desde repo root:
#   Rscript r/checks/check_idempotency.R
# ============================================================

suppressPackageStartupMessages({ library(readr) })
source("r/src/build_parallel_sdm_datasets.R")  # dataset_artifacts_exist, DATASET_ARTIFACTS
source("r/src/train_models.R")                  # model_artifacts_exist, read_basic_metrics, BASIC_METRIC_COLS

ok <- function(msg) cat("OK:", msg, "\n")
touch <- function(path) { dir.create(dirname(path), recursive = TRUE, showWarnings = FALSE); invisible(file.create(path)) }

# ---- dataset_artifacts_exist ----
run_dir <- file.path(tempdir(), "run_ds")
unlink(run_dir, recursive = TRUE); dir.create(run_dir)
# faltando el 4º artefacto -> FALSE
for (f in DATASET_ARTIFACTS[1:3]) touch(file.path(run_dir, f))
stopifnot(isFALSE(dataset_artifacts_exist(run_dir)))
# completo -> TRUE
touch(file.path(run_dir, DATASET_ARTIFACTS[4]))
stopifnot(isTRUE(dataset_artifacts_exist(run_dir)))
ok("dataset_artifacts_exist")

# ---- model_artifacts_exist ----
model_dir <- file.path(tempdir(), "run_model", "spatial_block", "maxnet")
unlink(file.path(tempdir(), "run_model"), recursive = TRUE)
dir.create(model_dir, recursive = TRUE)
touch(file.path(model_dir, "model.rds"))
touch(file.path(model_dir, "predictions_test.csv"))
stopifnot(isFALSE(model_artifacts_exist(model_dir)))   # falta metrics.csv
touch(file.path(model_dir, "metrics.csv"))
stopifnot(isTRUE(model_artifacts_exist(model_dir)))
ok("model_artifacts_exist")

# ---- read_basic_metrics ----
# metrics.csv "extendido" (básicas + threshold-dependent). Debe devolver SOLO
# las básicas, descartando tss/fnr/boyce/etc.
extended <- data.frame(
  run_id = "r1", algorithm = "maxnet", cv_scheme = "spatial_block",
  train_secs = 1.2, n_train_pres = 100L, n_train_bg = 9000L,
  n_test_pres = 25L, n_test_bg = 2000L, auc_test = 0.81,
  auc_test_sd = 0.03, k_folds = 5L,
  threshold_max_tss = 0.4, sensitivity = 0.8, specificity = 0.7,
  tss = 0.5, fnr = 0.2, boyce = 0.6
)
write_csv(extended, file.path(model_dir, "metrics.csv"))
basic <- read_basic_metrics(model_dir)
stopifnot(setequal(names(basic), BASIC_METRIC_COLS))
stopifnot(nrow(basic) == 1L, basic$auc_test == 0.81, basic$k_folds == 5L)
stopifnot(!any(c("tss", "fnr", "boyce") %in% names(basic)))
ok("read_basic_metrics (descarta columnas duales)")

cat("\nIDEMPOTENCIA OK\n")
