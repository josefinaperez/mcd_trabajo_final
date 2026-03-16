source("r/src/grid_thinning.R")
source("r/src/build_parallel_sdm_datasets.R")

species_table <- tibble::tribble(
  ~species,               ~occ_file,
  "Trametes sanguinea",   "df_trametes_sanguinea.csv",
  #"Ganoderma applanatum", "ganoderma_applanatum_clean.csv"
)

env_sets <- list(
  bioclim_30s = list(
    files = list.files("data/features/worldclim/wc2.1_30s_bio", full.names = TRUE)
  )
)

fixed_bp_n <- 2000L

config_table <- make_config_table(
  species_table = species_table,
  bp_methods = c("random"),
  bp_n_strategies = c("fixed", "match_presence"),
  fixed_bp_n = fixed_bp_n,
  env_sets = c("bioclim_30s"),
  grid_sizes_km = c(50)
)

manifest <- build_parallel_sdm_datasets(
  config_table = config_table,
  env_sets = env_sets,
  occ_dir = "data/ocurrences/processed",
  out_dir = "data/outputs/sdm_parallel",
  fixed_bp_n = fixed_bp_n,
  cols_to_keep = NA
)

print(manifest)