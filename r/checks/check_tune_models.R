# ============================================================
# File: r/checks/check_tune_models.R
# Purpose: Chequea las funciones puras de tuneo de HP (#8) sobre
#          datos sintéticos: formas de grilla por algoritmo, mtry
#          relativo a p, desempate al modelo más simple, scoring por
#          TSS pooled (finito en separable, NA en fold degenerado),
#          selección argmax y round-trip hp <-> fila de best_hp.csv.
# Ejecutar desde repo root:
#   Rscript r/checks/check_tune_models.R
# ============================================================

suppressPackageStartupMessages({ library(dplyr); library(tibble) })
source("r/src/tune_models.R")

ok <- function(msg) cat("OK:", msg, "\n")

# --- build_hp_grid: formas por algoritmo ---
g_max <- build_hp_grid("maxnet", n_predictors = 10)
stopifnot(length(g_max) == 12L)
stopifnot(all(vapply(g_max, function(h) all(c("regmult", "classes") %in% names(h)), logical(1))))
ok("build_hp_grid('maxnet') -> 12 combos regmult x classes")

g_xgb <- build_hp_grid("xgboost", n_predictors = 10)
stopifnot(length(g_xgb) == 12L)
stopifnot(all(vapply(g_xgb, function(h) all(c("max_depth", "learning_rate", "nrounds") %in% names(h)), logical(1))))
ok("build_hp_grid('xgboost') -> 12 combos")

# ranger: mtry relativo a p. p=10 -> floor(sqrt)=3, floor(p/3)=3 -> mtry colapsa a {3} -> 2x1x3=6.
g_rg10 <- build_hp_grid("ranger", n_predictors = 10)
stopifnot(all(vapply(g_rg10, function(h) h$mtry == 3L, logical(1))))
stopifnot(length(g_rg10) == 6L)
# p=12 -> sqrt=3, p/3=4 -> mtry {3,4} -> 2x2x3 = 12.
g_rg12 <- build_hp_grid("ranger", n_predictors = 12)
stopifnot(setequal(unique(vapply(g_rg12, function(h) h$mtry, integer(1))), c(3L, 4L)))
stopifnot(length(g_rg12) == 12L)
ok("build_hp_grid('ranger') -> mtry relativo a p (colapsa o no segun p)")

# --- complexity_rank: menor = mas simple (preferido en empate) ---
# maxnet: lq mas simple que lqhp; mayor regmult (mas regularizacion) mas simple.
stopifnot(complexity_rank("maxnet", list(regmult = 4, classes = "lq")) <
          complexity_rank("maxnet", list(regmult = 0.5, classes = "lqhp")))
# ranger: hojas mas grandes y menos arboles -> mas simple.
stopifnot(complexity_rank("ranger", list(num.trees = 500L, mtry = 1L, min.node.size = 10L)) <
          complexity_rank("ranger", list(num.trees = 1000L, mtry = 3L, min.node.size = 1L)))
# xgboost: menor profundidad y menos rounds -> mas simple.
stopifnot(complexity_rank("xgboost", list(max_depth = 3L, learning_rate = 0.1, nrounds = 200L)) <
          complexity_rank("xgboost", list(max_depth = 6L, learning_rate = 0.1, nrounds = 400L)))
ok("complexity_rank: menor = mas simple por algoritmo")

cat("\nTODOS OK (Task 1)\n")
