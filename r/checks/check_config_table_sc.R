# ============================================================
# File: r/checks/check_config_table_sc.R
# Purpose: make_config_table registra el bloque spatially_constrained (#52)
#          cruzado con ambas bp_n_strategy, y nada cuando la lista está vacía.
# Ejecutar: Rscript r/checks/check_config_table_sc.R
# ============================================================
suppressPackageStartupMessages({ library(dplyr); library(tidyr) })
source("r/src/build_parallel_sdm_datasets.R")

ok <- function(msg) cat("OK:", msg, "\n")

sp <- tibble::tibble(species = "Fungus testus", occ_file = "df_x.csv")

# Con env_sets indicados: aparecen filas spatially_constrained, ambas cantidades.
ct <- make_config_table(
  species_table = sp,
  bp_methods = c("random"),
  bp_n_strategies = c("fixed", "match_presence"),
  env_sets = c("bioclim", "bioclim_veg"),
  grid_sizes_km = c(15),
  spatially_constrained_env_sets = c("bioclim", "bioclim_veg")
)
sc <- dplyr::filter(ct, bp_method == "spatially_constrained")
stopifnot(nrow(sc) == 4)                                  # 2 env_sets x 2 cantidades
stopifnot(setequal(unique(sc$bp_n_strategy), c("fixed", "match_presence")))
stopifnot(all(sc$bias_method == "none"))
stopifnot(all(grepl("bp-spatially_constrained", sc$run_id)))
ok("make_config_table registra spatially_constrained con ambas cantidades")

# Sin env_sets: ninguna fila spatially_constrained.
ct0 <- make_config_table(
  species_table = sp,
  env_sets = c("bioclim"),
  grid_sizes_km = c(15),
  spatially_constrained_env_sets = character(0)
)
stopifnot(sum(ct0$bp_method == "spatially_constrained") == 0)
ok("make_config_table no registra spatially_constrained cuando la lista está vacía")

cat("\nChequeos de make_config_table (#52) pasaron.\n")
