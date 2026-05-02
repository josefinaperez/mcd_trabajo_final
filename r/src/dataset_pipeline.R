source("r/src/download_gbif.R")
source("r/src/preprocessing.R")
source("r/src/grid_thinning.R")
source("r/src/build_parallel_sdm_datasets.R")

for (d in c("data/ocurrences/raw",
            "data/ocurrences/processed",
            "data/outputs/sdm_parallel",
            "data/outputs/sdm_models")) {
  dir.create(d, recursive = TRUE, showWarnings = FALSE)
}

# ------------------------------------------------------------
# Path helpers: el archivo de cada especie es df_<slug>.csv
# (mismo slug que usa download_gbif y safe_slug en build_*)
# ------------------------------------------------------------

slug <- function(x) {
  x <- gsub("[^a-z0-9]+", "_", tolower(x))
  gsub("^_|_$", "", x)
}

species_csv <- function(scientific_name, dir) {
  file.path(dir, paste0("df_", slug(scientific_name), ".csv"))
}

# ------------------------------------------------------------
# Única fuente de verdad: una fila por especie a procesar.
# Agregar una especie aquí propaga a download, preprocess y build.
# ------------------------------------------------------------

species_config <- tibble::tribble(
  ~scientific_name,         ~min_year, ~max_uncertainty_km,
  "Polyporaceae",                 1945,                  10
  # "Pycnoporus sanguineus",      1945,                  10,
  # "Ganoderma applanatum",       1945,                  10
)

raw_occ_dir       <- "data/ocurrences/raw"
processed_occ_dir <- "data/ocurrences/processed"
shp_path          <- "data/shp/argentina/argentina.shp"
tests_default     <- c("equal", "duplicates", "outliers", "zeros")

# ------------------------------------------------------------
# 0) DOWNLOAD SPECIES DATA FROM GBIF
# ------------------------------------------------------------

res <- download_gbif_fungi_species_batch(
  species_list = species_config$scientific_name,
  country_code = "AR",
  max_records  = 10000,
  out_dir      = raw_occ_dir
)

# ------------------------------------------------------------
# 1) PREPROCESS RAW OCCURRENCE DATASETS
# ------------------------------------------------------------

for (i in seq_len(nrow(species_config))) {
  sp <- species_config$scientific_name[i]
  message("Preprocessing: ", sp)

  preprocess_dataset(
    df_path            = species_csv(sp, raw_occ_dir),
    preproc_path       = species_csv(sp, processed_occ_dir),
    min_year           = species_config$min_year[i],
    scientific_name    = sp,
    max_uncertainty_km = species_config$max_uncertainty_km[i],
    shp_path           = shp_path,
    tests              = tests_default
  )
}

# ------------------------------------------------------------
# 2) BUILD DATASET WITH BIAS CORRECTION AND BACKGROUND POINTS
# ------------------------------------------------------------

species_table <- tibble::tibble(
  species  = species_config$scientific_name,
  occ_file = paste0("df_", slug(species_config$scientific_name), ".csv")
)

env_sets <- list(
  bioclim_30s = list(
    files = list.files("data/features/worldclim/wc2.1_30s_bio", full.names = TRUE)
  )
)

fixed_bp_n <- 10000L

config_table <- make_config_table(
  species_table   = species_table,
  bp_methods      = c("random"),
  bp_n_strategies = c("fixed", "match_presence"),
  fixed_bp_n      = fixed_bp_n,
  env_sets        = c("bioclim_30s"),
  grid_sizes_km   = c(10, 50)
)

manifest <- build_parallel_sdm_datasets(
  config_table = config_table,
  env_sets     = env_sets,
  occ_dir      = processed_occ_dir,
  out_dir      = "data/outputs/sdm_parallel",
  fixed_bp_n   = fixed_bp_n,
  cols_to_keep = NA
)

print(manifest)
