# ============================================================
# File: r/checks/check_ensemble_models.R
# Purpose: Chequea las funciones puras del ensemble bootstrap (#58)
#          sobre datos sintéticos: bootstrap estratificado (preserva
#          conteos por clase, reproducible, con reemplazo),
#          fit_ensemble_member agnóstico al algoritmo, la prueba
#          CLAVE de que dos seeds dan modelos maxnet DISTINTOS (el
#          bootstrap rompe la degeneración determinista de maxnet),
#          y aggregate_cell_stats (media/SD por celda).
#
# Ejecutar desde repo root:
#   Rscript r/checks/check_ensemble_models.R
# ============================================================

suppressPackageStartupMessages({
  library(maxnet); library(ranger); library(xgboost)
})
source("r/src/train_models.R")     # fit_model, predict_score
source("r/src/ensemble_models.R")  # bajo prueba

ok <- function(msg) cat("OK:", msg, "\n")

set.seed(1)
n_pres <- 40L; n_bg <- 80L
y <- c(rep(1L, n_pres), rep(0L, n_bg))
# Dos predictores separables-ish para que maxnet aprenda algo no trivial.
X <- data.frame(
  b1 = c(rnorm(n_pres, 2, 1), rnorm(n_bg, 0, 1)),
  b2 = c(rnorm(n_pres, -1, 1), rnorm(n_bg, 1, 1))
)

# --- stratified_bootstrap_indices: preserva conteos por clase ---
idx <- stratified_bootstrap_indices(y, seed = 10)
stopifnot(length(idx) == length(y))
stopifnot(sum(y[idx] == 1L) == n_pres)
stopifnot(sum(y[idx] == 0L) == n_bg)
ok("stratified_bootstrap_indices preserva n_pres y n_bg")

# --- reproducible con el mismo seed, distinto con otro ---
stopifnot(identical(idx, stratified_bootstrap_indices(y, seed = 10)))
idx2 <- stratified_bootstrap_indices(y, seed = 11)
stopifnot(!identical(idx, idx2))
ok("reproducible por seed; seeds distintos -> draws distintos")

# --- con reemplazo: hay índices repetidos ---
stopifnot(anyDuplicated(idx) > 0L)
ok("muestreo con reemplazo (hay repeticiones)")

# --- fit_ensemble_member corre para los 3 algoritmos ---
for (algo in c("maxnet", "ranger", "xgboost")) {
  m <- fit_ensemble_member(algo, X, y, hp = list(), seed = 7)
  p <- predict_score(m, X)
  stopifnot(length(p) == nrow(X), all(is.finite(p)), all(p >= 0 & p <= 1))
}
ok("fit_ensemble_member agnóstico al algoritmo, scores en [0,1]")

# --- CLAVE: dos seeds dan modelos maxnet DISTINTOS via bootstrap ---
# maxnet es determinista dada la data; si el bootstrap varía la data,
# las predicciones deben diferir entre réplicas con distinto seed.
m_a <- fit_ensemble_member("maxnet", X, y, hp = list(), seed = 100)
m_b <- fit_ensemble_member("maxnet", X, y, hp = list(), seed = 200)
pa <- predict_score(m_a, X); pb <- predict_score(m_b, X)
stopifnot(max(abs(pa - pb)) > 1e-6)
ok("dos seeds -> modelos maxnet distintos (bootstrap rompe el determinismo)")

# control negativo: SIN bootstrap (fit_model directo) maxnet ES idéntico
mc1 <- fit_model("maxnet", X, y); mc2 <- fit_model("maxnet", X, y)
stopifnot(max(abs(predict_score(mc1, X) - predict_score(mc2, X))) < 1e-12)
ok("control: maxnet sin bootstrap es determinista (SD=0)")

# --- aggregate_cell_stats: media/SD por celda (columna = réplica) ---
M <- matrix(c(0.0, 0.2, 0.4,
              0.0, 0.6, 0.8), nrow = 2, byrow = TRUE)  # 2 celdas x 3 réplicas
st <- aggregate_cell_stats(M)
stopifnot(length(st$mean) == 2L, length(st$sd) == 2L)
stopifnot(isTRUE(all.equal(st$mean, c(0.2, 0.4666667), tolerance = 1e-5)))
stopifnot(isTRUE(all.equal(st$sd, apply(M, 1, sd))))
ok("aggregate_cell_stats: media y SD por celda correctas")

cat("\nTodos los chequeos del ensemble bootstrap pasaron.\n")
