source("r/src/download_gbif.R")
source("r/src/preprocessing.R")
source("r/src/grid_thinning.R")
source("r/src/build_parallel_sdm_datasets.R")


# ------------------------------------------------------------
# 0) DOWNLOAD SPECIES DATA FROM GBIF
# ------------------------------------------------------------


species_list <- c(
  "Polyporaceae",
  "Pycnoporus sanguineus"
)

res <- download_gbif_fungi_species_batch(
  species_list = species_list,
  country_code = "AR",
  max_records = 10000,
  out_dir = "data/ocurrences/raw"
)

# ------------------------------------------------------------
# 0) PREPROCESS RAW OCCURRENCE DATASETS
# ------------------------------------------------------------

shp_path <- "data/shp/argentina/argentina.shp"

min_year_default <- 1945
max_uncertainty_km_default <- 10

tests_default <- c(
  "equal",
  "duplicates",
  "outliers",
  "zeros"
)

raw_occ_dir <- "../data/ocurrences/raw"
processed_occ_dir <- "../data/ocurrences/processed"

species_preproc_table <- tibble::tribble(
  ~species,               ~scientific_name,       ~raw_file,                     ~processed_file,               ~min_year, ~max_uncertainty_km,
  "Pycnoporus Sanguineus",   "Pycnoporus Sanguineus",   "df_pycnoporus_sanguineus.csv",   "df_trametes_sanguinea.csv",   1945,      10
  # "Ganoderma applanatum", "Ganoderma applanatum", "df_ganoderma_applanatum.csv", "df_ganoderma_applanatum.csv", 1945, 10
)

for (i in seq_len(nrow(species_preproc_table))) {
  raw_path <- file.path(raw_occ_dir, species_preproc_table$raw_file[i])
  processed_path <- file.path(processed_occ_dir, species_preproc_table$processed_file[i])
  
  message("Preprocessing: ", species_preproc_table$species[i])
  
  preprocess_dataset(
    df_path = raw_path,
    preproc_path = processed_path,
    min_year = species_preproc_table$min_year[i] %||% min_year_default,
    scientific_name = species_preproc_table$scientific_name[i],
    max_uncertainty_km = species_preproc_table$max_uncertainty_km[i] %||% max_uncertainty_km_default,
    shp_path = shp_path,
    tests = tests_default
  )
}

# ------------------------------------------------------------
# 1) BUILD DATASET WITH BIAS CORRECTION AND BACKGROUND POINTS
# ------------------------------------------------------------

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

fixed_bp_n <- 10000L

config_table <- make_config_table(
  species_table = species_table,
  bp_methods = c("random"),
  bp_n_strategies = c("fixed", "match_presence"),
  fixed_bp_n = fixed_bp_n,
  env_sets = c("bioclim_30s"),
  grid_sizes_km = c(10, 50)
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

