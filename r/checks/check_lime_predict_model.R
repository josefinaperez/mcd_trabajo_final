# ============================================================
# File: r/checks/check_lime_predict_model.R
# Purpose: Regresión de #50 — compute_lime() debe funcionar para
#          los 3 algoritmos (maxnet, ranger, xgboost). lime trae
#          predict_model propios para ranger/xgb.Booster que
#          devolvían columnas "0"/"1" en vez de "presence"/
#          "background" -> explain(labels="presence") tiraba
#          "NAs in V(mu)". xai_lime.R fuerza el predict custom en
#          el namespace de lime; este check lo verifica.
# Guardado: si faltan los modelos entrenados, omite sin fallar.
# Ejecutar desde repo root:
#   Rscript r/checks/check_lime_predict_model.R
# ============================================================

suppressPackageStartupMessages({
  library(readr); library(dplyr); library(lime)
  library(maxnet); library(ranger); library(xgboost)  # registran sus predict.*
})
source("r/src/xai_predict.R")
source("r/src/xai_lime.R")

ok <- function(msg) cat("OK:", msg, "\n")

MODELS <- "data/outputs/sdm_models"
DSROOT <- "data/outputs/sdm_parallel"
ENV    <- "bioclim_veg_reduced"
# Pareo algo->background de #46: maxnet usa fixed; ranger/xgboost match_presence.
CASES <- list(
  maxnet  = sprintf("polyporaceae__bias-none__bp-random__bpn-fixed_10000__env-%s", ENV),
  ranger  = sprintf("polyporaceae__bias-none__bp-random__bpn-match_presence__env-%s", ENV),
  xgboost = sprintf("polyporaceae__bias-none__bp-random__bpn-match_presence__env-%s", ENV)
)

ran_any <- FALSE
for (algo in names(CASES)) {
  run_id <- CASES[[algo]]
  mpath  <- file.path(MODELS, run_id, "spatial_block", algo, "model.rds")
  dpath  <- file.path(DSROOT, run_id, "sdm_dataset_model_ready.csv")
  if (!file.exists(mpath) || !file.exists(dpath)) {
    cat("OMITIDO:", algo, "- falta modelo/dataset (", run_id, ")\n")
    next
  }
  ran_any <- TRUE
  model <- readRDS(mpath)
  ds    <- read_csv(dpath, show_col_types = FALSE)
  pred_cols <- setdiff(names(ds), c("class", "decimalLongitude", "decimalLatitude"))
  X_train <- as.data.frame(ds[, pred_cols])

  # 6 presencias como puntos a explicar (estructura mínima que espera compute_lime)
  pts <- ds |>
    dplyr::filter(class == 1) |>
    head(6) |>
    dplyr::transmute(point_id = paste0("p", dplyr::row_number()),
                     dplyr::across(dplyr::all_of(pred_cols)))

  res <- compute_lime(model, pts, X_train, n_features = 5, n_permutations = 300)
  stopifnot(is.data.frame(res), nrow(res) > 0)
  stopifnot(all(c("point_id", "feature", "weight", "feature_value") %in% names(res)))
  stopifnot(length(unique(res$point_id)) == 6)   # los 6 puntos explicados
  stopifnot(all(is.finite(res$weight)))          # sin NA/NaN/Inf en los pesos
  ok(sprintf("compute_lime %s: %d filas, %d puntos, pesos finitos",
             algo, nrow(res), length(unique(res$point_id))))
}

if (!ran_any) {
  cat("OMITIDO: no hay modelos entrenados para", ENV, "- correr train_pipeline.R\n")
  quit(save = "no", status = 0)
}
cat("\nLIME OK para los algoritmos disponibles (#50).\n")
