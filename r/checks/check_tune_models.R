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

# --- dataset sintético separable para scoring ---
set.seed(1)
n  <- 90L
x1 <- c(rnorm(45, 2), rnorm(45, -2))   # separa clases
x2 <- rnorm(n)
y  <- c(rep(1L, 45), rep(0L, 45))
ds <- list(
  X          = data.frame(x1 = x1, x2 = x2),
  y          = y,
  coords     = data.frame(decimalLongitude = runif(n, -64, -60),
                          decimalLatitude  = runif(n, -36, -32)),
  predictors = c("x1", "x2")
)
# 3 folds que mezclan ambas clases (cada fold ~15 pres / ~15 bg).
fold_id <- rep(1:3, length.out = n)

# score_hp_combo: TSS finito y > 0 en datos separables (ranger es robusto).
tss <- score_hp_combo("ranger", ds, fold_id,
                      hp = list(num.trees = 200L, mtry = 1L, min.node.size = 5L),
                      seed = 42)
stopifnot(is.finite(tss), tss >= -1, tss <= 1, tss > 0)
ok("score_hp_combo: TSS pooled finito y > 0 en dataset separable")

# Fold degenerado: cada fold de test es de una sola clase -> < 2 folds
# evaluables -> NA (no error).
fold_bad <- ifelse(ds$y == 1L, 1L, 2L)
stopifnot(is.na(score_hp_combo("ranger", ds, fold_bad,
                               hp = list(num.trees = 200L, mtry = 1L, min.node.size = 5L))))
ok("score_hp_combo: NA cuando hay < 2 folds evaluables")

# tune_hp_for_dataset: best_hp en la grilla, best_tss == max(scores_table$tss).
grid <- build_hp_grid("ranger", length(ds$predictors))
res  <- tune_hp_for_dataset("ranger", ds, fold_id, grid = grid, seed = 42)
stopifnot(nrow(res$scores_table) == length(grid))
stopifnot("tss" %in% names(res$scores_table))
stopifnot(isTRUE(all.equal(res$best_tss, max(res$scores_table$tss, na.rm = TRUE))))
in_grid <- any(vapply(grid, function(h) isTRUE(all.equal(h, res$best_hp)), logical(1)))
stopifnot(in_grid)
ok("tune_hp_for_dataset: elige el argmax de TSS y best_hp pertenece a la grilla")

# --- round-trip hp <-> fila de best_hp.csv, por algoritmo ---
check_roundtrip <- function(algo, hp) {
  row  <- hp_to_row("r1", algo, hp, best_tss = 0.7, n_combos = 12L, k_internal = 3L)
  back <- hp_row_to_list(row)
  stopifnot(setequal(names(back), names(hp)))
  for (nm in names(hp)) stopifnot(isTRUE(all.equal(back[[nm]], hp[[nm]])))
  stopifnot(row$run_id == "r1", row$algorithm == algo, row$best_internal_tss == 0.7)
}
check_roundtrip("maxnet",  list(regmult = 2, classes = "lqh"))
check_roundtrip("ranger",  list(num.trees = 1000L, mtry = 3L, min.node.size = 5L))
check_roundtrip("xgboost", list(max_depth = 6L, learning_rate = 0.05, nrounds = 400L))
ok("hp_to_row / hp_row_to_list: round-trip por algoritmo")

cat("\nTODOS OK (Task 2)\n")
