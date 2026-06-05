# ============================================================
# File: residual_autocorr_pipeline.R
# Purpose: Diagnóstico de autocorrelación espacial de los
#          residuos del/los modelo(s) ganador(es) (issue #34).
#          Para cada ganador de winner_summary.csv:
#            - lee predictions_test.csv (pool spatial-block),
#            - residuo crudo = class - score en TODOS los puntos
#              (presencias + background),
#            - Moran's I (pesos knn k=8) + p-valor de permutación,
#            - correlograma por bandas de distancia,
#            - mapa de residuos sobre Argentina.
#          Persiste artefactos por ganador + un summary.csv.
#
# Ejecutar desde repo root CON Rscript (guarda en sys.nframe()):
#   Rscript r/src/residual_autocorr_pipeline.R
#
# Idempotente (skip-if-exists). SDM_FORCE=1 fuerza recálculo.
# ============================================================

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(tibble)
  library(purrr)
  library(ggplot2)
  library(sf)
})

source("r/src/residual_autocorrelation.R")

# Configuración ----------------------------------------------
K_NEIGHBORS    <- 8L
N_SIM          <- 999L           # permutaciones para el test global
N_SIM_CORR     <- 499L           # permutaciones por banda del correlograma
CORR_BREAKS    <- seq(0, 500e3, by = 50e3)   # bandas de distancia (m)

MODELS_ROOT    <- "data/outputs/sdm_models"
MAPS_ROOT      <- "data/outputs/sdm_maps"
RESIDUALS_ROOT <- "data/outputs/sdm_residuals"
ARGENTINA_SHP  <- "data/shp/argentina/argentina.shp"

force_recompute <- function() {
  v <- tolower(Sys.getenv("SDM_FORCE", ""))
  v %in% c("1", "true", "yes")
}

# ------------------------------------------------------------
# 1) GRÁFICOS
# ------------------------------------------------------------

# Mapa de residuos: shp de Argentina + puntos coloreados por
# residuo con signo (escala divergente centrada en 0).
plot_residual_map <- function(preds, residuals, shp, tag) {
  df <- preds |>
    dplyr::mutate(residual = residuals)
  pts <- sf::st_as_sf(df,
                      coords = c("decimalLongitude", "decimalLatitude"),
                      crs = 4326)
  lim <- max(abs(df$residual), na.rm = TRUE)

  ggplot2::ggplot() +
    ggplot2::geom_sf(data = shp, fill = "grey95", color = "grey60",
                     linewidth = 0.2) +
    ggplot2::geom_sf(data = pts, ggplot2::aes(color = residual),
                     size = 1.1, alpha = 0.8) +
    ggplot2::scale_color_gradient2(
      low = "#2166AC", mid = "grey85", high = "#B2182B",
      midpoint = 0, limits = c(-lim, lim),
      name = "Residuo\n(class - score)"
    ) +
    ggplot2::coord_sf() +
    ggplot2::labs(
      title    = "Residuos espaciales del modelo ganador",
      subtitle = tag
    ) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(legend.position = "right")
}

# Correlograma: Moran's I por banda de distancia, puntos
# significativos resaltados.
plot_correlogram <- function(corr, tag) {
  df <- corr |>
    dplyr::filter(!is.na(moran_i)) |>
    dplyr::mutate(
      dist_km = dist_mid / 1000,
      signif  = !is.na(p_value) & p_value <= 0.05
    )

  ggplot2::ggplot(df, ggplot2::aes(x = dist_km, y = moran_i)) +
    ggplot2::geom_hline(yintercept = 0, linetype = "dashed",
                        color = "grey50") +
    ggplot2::geom_line(color = "grey40") +
    ggplot2::geom_point(ggplot2::aes(color = signif), size = 2.6) +
    ggplot2::scale_color_manual(
      values = c(`TRUE` = "#B2182B", `FALSE` = "grey60"),
      labels = c(`TRUE` = "p <= 0.05", `FALSE` = "n.s."),
      name   = NULL
    ) +
    ggplot2::labs(
      title    = "Correlograma espacial de residuos",
      subtitle = tag,
      x = "Distancia (km)", y = "Moran's I"
    ) +
    ggplot2::theme_minimal(base_size = 10) +
    ggplot2::theme(legend.position = "bottom")
}

# ------------------------------------------------------------
# 2) DIAGNÓSTICO DE UN GANADOR
# ------------------------------------------------------------
#
# Lee predictions_test.csv del trío (run_id, cv_scheme, algorithm),
# computa Moran's I + correlograma + mapa, persiste artefactos y
# devuelve una fila de summary.
# ------------------------------------------------------------

diagnose_one_winner <- function(run_id, cv_scheme, algorithm, shp,
                                models_root, residuals_root) {
  tag       <- paste(run_id, cv_scheme, algorithm, sep = " / ")
  preds_path <- file.path(models_root, run_id, cv_scheme, algorithm,
                          "predictions_test.csv")
  if (!file.exists(preds_path)) {
    stop("falta predictions_test.csv para ", tag)
  }

  out_dir   <- file.path(residuals_root, run_id, cv_scheme, algorithm)
  moran_csv <- file.path(out_dir, "morans_i.csv")
  corr_csv  <- file.path(out_dir, "correlogram.csv")

  # Skip-if-exists: reconstruye la fila de summary desde el CSV.
  if (!force_recompute() && file.exists(moran_csv)) {
    message("  [skip] ", tag, " (ya existe morans_i.csv)")
    mt <- readr::read_csv(moran_csv, show_col_types = FALSE)
    return(dplyr::mutate(mt, run_id = run_id, cv_scheme = cv_scheme,
                         algorithm = algorithm, .before = 1))
  }

  preds <- readr::read_csv(preds_path, show_col_types = FALSE)
  required <- c("decimalLongitude", "decimalLatitude", "class", "score")
  miss <- setdiff(required, names(preds))
  if (length(miss) > 0) {
    stop("predictions_test.csv de ", tag, " sin columnas: ",
         paste(miss, collapse = ", "))
  }

  residuals <- compute_residuals(preds$class, preds$score)
  coords_m  <- project_coords_aea(preds$decimalLongitude,
                                  preds$decimalLatitude)

  listw <- build_knn_listw(coords_m, k = K_NEIGHBORS)
  mt    <- moran_test_residuals(residuals, listw, nsim = N_SIM,
                                k = K_NEIGHBORS)
  corr  <- residual_distance_correlogram(coords_m, residuals,
                                         breaks = CORR_BREAKS,
                                         nsim   = N_SIM_CORR)

  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(mt, moran_csv)
  readr::write_csv(corr, corr_csv)

  ggplot2::ggsave(file.path(out_dir, "residual_map.png"),
                  plot_residual_map(preds, residuals, shp, tag),
                  width = 6, height = 8, dpi = 150)
  ggplot2::ggsave(file.path(out_dir, "correlogram.png"),
                  plot_correlogram(corr, tag),
                  width = 7, height = 5, dpi = 150)

  message("  [ok]   ", tag,
          "  I=", round(mt$moran_i, 4),
          "  p=", signif(mt$p_value, 3))

  dplyr::mutate(mt, run_id = run_id, cv_scheme = cv_scheme,
                algorithm = algorithm, .before = 1)
}

# ------------------------------------------------------------
# 3) MAIN
# ------------------------------------------------------------

main <- function() {
  winner_path <- file.path(MAPS_ROOT, "winner_summary.csv")
  if (!file.exists(winner_path)) {
    stop("Falta ", winner_path,
         " — corré primero Rscript r/src/predict_pipeline.R")
  }
  if (!file.exists(ARGENTINA_SHP)) {
    stop("Falta el shapefile de Argentina en ", ARGENTINA_SHP)
  }

  winners <- readr::read_csv(winner_path, show_col_types = FALSE)
  stopifnot(all(c("run_id", "cv_scheme", "algorithm") %in% names(winners)))

  shp <- sf::st_read(ARGENTINA_SHP, quiet = TRUE) |> sf::st_union() |> sf::st_sf()

  dir.create(RESIDUALS_ROOT, recursive = TRUE, showWarnings = FALSE)
  message("Diagnóstico de residuos para ", nrow(winners), " ganador(es).")

  rows <- purrr::pmap(
    winners |> dplyr::select(run_id, cv_scheme, algorithm),
    function(run_id, cv_scheme, algorithm) {
      tryCatch(
        diagnose_one_winner(run_id, cv_scheme, algorithm, shp,
                            MODELS_ROOT, RESIDUALS_ROOT),
        error = function(e) {
          message("ERROR en ", run_id, " / ", cv_scheme, " / ", algorithm,
                  ": ", conditionMessage(e))
          NULL
        }
      )
    }
  ) |> purrr::compact() |> dplyr::bind_rows()

  if (nrow(rows) == 0L) {
    stop("Ningún ganador pudo diagnosticarse. Revisar logs.")
  }

  summary_tbl <- winners |>
    dplyr::select(run_id, cv_scheme, algorithm, winner_role) |>
    dplyr::left_join(rows, by = c("run_id", "cv_scheme", "algorithm"))

  readr::write_csv(summary_tbl, file.path(RESIDUALS_ROOT, "summary.csv"))
  message("Listo. Artefactos en ", RESIDUALS_ROOT)
  invisible(summary_tbl)
}

if (sys.nframe() == 0L) {
  main()
}
