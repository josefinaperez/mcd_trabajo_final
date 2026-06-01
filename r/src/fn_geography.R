# ============================================================
# File: fn_geography.R
# Purpose: Geografía de los falsos negativos del modelo ganador.
#          Toma las presencias del test (out-of-fold) del ganador
#          spatial-block — identificado dinámicamente con la misma
#          lógica de selección que predict_pipeline.R — las clasifica
#          en TP / FN según el umbral τ* (max TSS) y las mapea sobre
#          Argentina con los límites provinciales como referencia.
#          La lectura por ecorregión se hace a posteriori en el
#          informe (este script no superpone ecorregiones).
#          Ejecutar desde repo root con `Rscript r/src/fn_geography.R`.
# ============================================================

# predict_pipeline.R aporta select_runs(), TAU_FNR_MAIN, MODELS_ROOT,
# MAPS_ROOT; y transitivamente (vía predict_distribution_map.R) las
# libs y ARGENTINA_SHP. Sourcearlo NO ejecuta su main() (guard sys.nframe).
source("r/src/predict_pipeline.R")

suppressPackageStartupMessages({
  library(readr)
  library(dplyr)
  library(ggplot2)
  library(sf)
})

PROVINCES_SHP <- "data/shp/argentina/gadm41_ARG_1.shp"

# ------------------------------------------------------------
# 1) CLASIFICACIÓN TP / FN DE LAS PRESENCIAS DEL TEST
# ------------------------------------------------------------
#
# Lee predictions_test.csv del run ganador (predicciones out-of-fold
# de la CV espacial), se queda con las presencias (class == 1) y las
# etiqueta según el umbral: score >= threshold -> TP, si no -> FN.
# Devuelve el tibble de presencias con la columna `outcome`.
# ------------------------------------------------------------

classify_presence_outcomes <- function(model_dir, threshold) {
  pred_path <- file.path(model_dir, "predictions_test.csv")
  if (!file.exists(pred_path)) {
    stop("falta predictions_test.csv en ", model_dir)
  }
  readr::read_csv(pred_path, show_col_types = FALSE) |>
    filter(class == 1) |>
    mutate(
      outcome = if_else(score >= threshold, "TP", "FN"),
      outcome = factor(outcome, levels = c("TP", "FN"))
    )
}

# ------------------------------------------------------------
# 2) MAPA TP / FN
# ------------------------------------------------------------
#
# Mapa de las presencias del test sobre Argentina, con los límites
# provinciales (GADM nivel 1) como referencia geográfica. Los TP en
# gris tenue (contexto) y los FN en rojo prominente (el objeto de
# análisis). El subtítulo reporta n_FN / n_presencias y el FNR.
# ------------------------------------------------------------

plot_fn_geography <- function(presences, provinces, run_id, threshold, fnr) {
  n_pres <- nrow(presences)
  n_fn   <- sum(presences$outcome == "FN")

  # FN al frente: orden de dibujado TP primero, FN después.
  presences <- presences |> arrange(outcome == "FN")

  ggplot() +
    geom_sf(data = provinces, fill = "grey97", colour = "grey75", linewidth = 0.2) +
    geom_point(
      data = presences,
      aes(x = decimalLongitude, y = decimalLatitude,
          colour = outcome, size = outcome, alpha = outcome)
    ) +
    scale_colour_manual(
      name   = "Presencia del test",
      values = c(TP = "grey45", FN = "#e31a1c"),
      labels = c(TP = "TP (acierto)", FN = "FN (falso negativo)")
    ) +
    scale_size_manual(values = c(TP = 0.8, FN = 1.8), guide = "none") +
    scale_alpha_manual(values = c(TP = 0.5, FN = 0.95), guide = "none") +
    coord_sf(crs = 4326) +
    labs(
      title    = "Geografía de los falsos negativos del ganador",
      subtitle = paste0(
        run_id, "\n",
        "FN = ", n_fn, " / ", n_pres, " presencias del test  ·  FNR = ",
        sprintf("%.3f", fnr), "  ·  τ* = ", sprintf("%.3f", threshold)
      ),
      x = NULL, y = NULL
    ) +
    theme_minimal(base_size = 10) +
    theme(
      legend.position = "right",
      plot.subtitle   = element_text(size = 8)
    )
}

# ------------------------------------------------------------
# 3) MAIN
# ------------------------------------------------------------

main <- function() {
  summary_path <- file.path(MODELS_ROOT, "summary_table.csv")
  if (!file.exists(summary_path)) {
    stop("Falta ", summary_path, " — corré primero r/src/train_pipeline.R")
  }
  summary_df <- readr::read_csv(summary_path, show_col_types = FALSE)

  # Ganador dinámico: mismo criterio dual (filtro FNR + argmax TSS por
  # cv_scheme) que predict_pipeline.R. Cuando cambie el ganador, este
  # script regenera el mapa para el nuevo sin tocar código.
  winners <- select_runs(summary_df, TAU_FNR_MAIN) |>
    filter(is_winner)
  if (nrow(winners) == 0) {
    stop("Ningún modelo pasa el filtro τ_FNR = ", TAU_FNR_MAIN,
         "; no hay ganador que diagnosticar.")
  }

  provinces <- sf::st_read(PROVINCES_SHP, quiet = TRUE)

  summaries <- purrr::pmap_dfr(
    winners |> select(run_id, cv_scheme, algorithm, threshold_max_tss, fnr),
    function(run_id, cv_scheme, algorithm, threshold_max_tss, fnr) {
      model_dir <- file.path(MODELS_ROOT, run_id, cv_scheme, algorithm)
      pres <- classify_presence_outcomes(model_dir, threshold_max_tss)

      fig <- plot_fn_geography(pres, provinces, run_id, threshold_max_tss, fnr)
      out_dir <- file.path(MAPS_ROOT, run_id, cv_scheme, algorithm)
      dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
      out_png <- file.path(out_dir, "fn_geography.png")
      ggsave(out_png, fig, width = 7, height = 9, dpi = 130)

      # Persistir las presencias clasificadas para la lectura del informe.
      readr::write_csv(pres, file.path(out_dir, "fn_presences_test.csv"))
      message("FN map -> ", out_png)

      tibble(
        run_id, cv_scheme, algorithm,
        threshold_max_tss, fnr,
        n_presences_test = nrow(pres),
        n_fn = sum(pres$outcome == "FN"),
        n_tp = sum(pres$outcome == "TP")
      )
    }
  )

  readr::write_csv(summaries, file.path(MAPS_ROOT, "fn_geography_summary.csv"))
  message("Listo. Resumen en ", file.path(MAPS_ROOT, "fn_geography_summary.csv"))
  invisible(summaries)
}

if (sys.nframe() == 0L) {
  main()
}
