# ============================================================
# File: r/checks/plot_bg_strategies_map.R
# Purpose: Mapa diagnóstico comparando presencias vs background para las
#          distintas estrategias de background (bp_method), a igual env_set y
#          bp_n_strategy. Lee los datasets ya construidos por dataset_pipeline.R.
# Ejecutar desde repo root:
#   Rscript r/checks/plot_bg_strategies_map.R
# Salida: data/outputs/sdm_parallel/diagnostics/bg_strategies_map.png
# ============================================================

suppressPackageStartupMessages({ library(sf); library(readr); library(dplyr) })

ENV_SET   <- "bioclim_veg_reduced"   # env_set común a todas las estrategias
BP_N      <- "fixed"                  # fixed (10000) para ver bien el patrón espacial
OUT_DIR   <- "data/outputs/sdm_parallel/diagnostics"
OUT_PNG   <- file.path(OUT_DIR, "bg_strategies_map.png")
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)

# Orden y etiquetas legibles de las estrategias.
strategies <- c(
  random                     = "(i) Random",
  bias_weighted              = "Target-group (bias_weighted)",
  spatially_constrained      = "(ii) Spatially constrained",
  environmentally_dissimilar = "(iii) Environmentally dissimilar",
  three_step                 = "(iv) Three-step"
)

manifest <- read_csv("data/outputs/sdm_parallel/manifest.csv", show_col_types = FALSE)

# Polígono de Argentina para el fondo.
arg <- st_read("data/shp/argentina/argentina.shp", quiet = TRUE)
arg_geom <- st_geometry(arg)
bb <- st_bbox(arg)

# Para cada estrategia, localizar el run (bias=none, env_set, bp_n) y leer puntos.
pick_run <- function(bp) {
  row <- manifest |>
    filter(bp_method == bp, bias_method == "none",
           env_set == ENV_SET, bp_n_strategy == BP_N)
  if (nrow(row) == 0) return(NULL)
  row$output_dir[1]
}

col_bg  <- grDevices::adjustcolor("#2c7fb8", alpha.f = 0.30)
col_occ <- grDevices::adjustcolor("#d7301f", alpha.f = 0.90)

png(OUT_PNG, width = 2000, height = 1500, res = 150)
op <- par(mfrow = c(2, 3), mar = c(2, 2, 3, 1), oma = c(0, 0, 3, 0))

for (bp in names(strategies)) {
  dir <- pick_run(bp)
  plot(arg_geom, col = "grey95", border = "grey60",
       main = strategies[[bp]], xlim = c(bb["xmin"], bb["xmax"]),
       ylim = c(bb["ymin"], bb["ymax"]))
  if (is.null(dir) || !dir.exists(dir)) {
    text(mean(c(bb["xmin"], bb["xmax"])), mean(c(bb["ymin"], bb["ymax"])),
         "sin dataset", col = "grey50")
    next
  }
  bg  <- read_csv(file.path(dir, "background_points.csv"), show_col_types = FALSE)
  occ <- read_csv(file.path(dir, "occ_processed.csv"),     show_col_types = FALSE)
  points(bg$decimalLongitude,  bg$decimalLatitude,  pch = 16, cex = 0.38, col = col_bg)
  points(occ$decimalLongitude, occ$decimalLatitude, pch = 21, cex = 0.55,
         bg = col_occ, col = "white", lwd = 0.25)
  legend("bottomleft", bty = "n", cex = 0.7,
         legend = c(sprintf("background (n=%d)", nrow(bg)),
                    sprintf("presencias (n=%d)", nrow(occ))),
         pch = c(16, 21), pt.bg = c(NA, col_occ),
         col = c(col_bg, "grey30"))
}

# Panel 6: leyenda / nota.
plot.new()
legend("center", bty = "n", cex = 0.95, title = "Presencia vs background",
       legend = c("Presencias (rojo)", "Background (azul)",
                  paste0("env_set = ", ENV_SET), paste0("bp_n = ", BP_N)),
       pch = c(21, 16, NA, NA), pt.bg = c(col_occ, NA, NA, NA),
       col = c("grey30", col_bg, NA, NA))

mtext("Estrategias de background — Polyporaceae (Argentina)",
      outer = TRUE, cex = 1.1, font = 2)
par(op)
dev.off()
cat("Mapa guardado en:", OUT_PNG, "\n")
