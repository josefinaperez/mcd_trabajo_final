# ============================================================
# File: r/src/ensemble_models.R
# Purpose: Funciones puras del ensemble bootstrap (#58). Sin I/O de
#          raster: se testean sobre datos sintéticos en
#          r/checks/check_ensemble_models.R. El orquestador con I/O
#          es r/src/ensemble_pipeline.R.
#
# La incertidumbre del ensemble sale del BOOTSTRAP DE LOS DATOS, no
# de la semilla del modelo: maxnet (3/5 ganadores) es determinista
# dada la data, así que variar solo la semilla daría SD=0. Remuestrear
# las filas de entrenamiento da variación para los tres algoritmos.
# El bootstrap es estratificado por clase para preservar la prevalencia
# (n_pres/n_bg) y mantener comparable la escala de scores entre réplicas.
# ============================================================

# Índices de un bootstrap estratificado por clase: remuestrea con
# reemplazo DENTRO de cada clase de `y` (0/1), preservando el número
# de filas por clase. Devuelve un vector de índices sobre las filas
# originales, en orden de clase (primero los de la clase 0 tal como
# aparecen, luego los de la clase 1) — el orden no importa para el fit.
stratified_bootstrap_indices <- function(y, seed) {
  set.seed(seed)
  classes <- sort(unique(y))
  idx <- lapply(classes, function(cl) {
    pool <- which(y == cl)
    sample(pool, size = length(pool), replace = TRUE)
  })
  unlist(idx, use.names = FALSE)
}

# Ajusta un miembro del ensemble: toma un bootstrap estratificado
# (seedeado) de (X, y) y reajusta vía fit_model(). Reusa el dispatcher
# agnóstico al algoritmo de train_models.R. La semilla del modelo se
# fija también a `seed` para que ranger/xgboost no agreguen una segunda
# fuente de variación no controlada por encima del bootstrap.
fit_ensemble_member <- function(algo, X, y, hp = list(), seed = 42) {
  idx <- stratified_bootstrap_indices(y, seed = seed)
  Xb  <- X[idx, , drop = FALSE]
  yb  <- y[idx]
  fit_model(algo, Xb, yb, hp = hp, seed = seed)
}

# Media y SD por celda sobre una matriz de predicciones donde cada
# COLUMNA es una réplica y cada FILA una celda. Devuelve list(mean, sd).
# (SD muestral, na.rm para tolerar celdas con algún NA.)
aggregate_cell_stats <- function(pred_matrix) {
  list(
    mean = rowMeans(pred_matrix, na.rm = TRUE),
    sd   = apply(pred_matrix, 1L, stats::sd, na.rm = TRUE)
  )
}
