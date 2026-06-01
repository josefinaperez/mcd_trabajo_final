source("r/src/download_gbif.R")
source("r/src/preprocessing.R")
source("r/src/grid_thinning.R")
source("r/src/build_parallel_sdm_datasets.R")

# Idempotencia: por defecto se saltea cualquier etapa cuyo artefacto ya esté
# en disco (download, preprocess, build). Forzar recómputo completo con
# SDM_FORCE=1 (o true/yes) en el entorno: `SDM_FORCE=1 Rscript r/src/dataset_pipeline.R`.
FORCE <- tolower(Sys.getenv("SDM_FORCE", "")) %in% c("1", "true", "t", "yes", "y")

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
  "Polyporaceae",                 2010,                   5
  # "Pycnoporus sanguineus",      2010,                   5,
  # "Ganoderma applanatum",       2010,                   5
)

raw_occ_dir       <- "data/ocurrences/raw"
processed_occ_dir <- "data/ocurrences/processed"
shp_path          <- "data/shp/argentina/argentina.shp"
tests_default     <- c("equal", "duplicates", "outliers", "zeros",
                       "institutions", "centroids", "capitals")

# ------------------------------------------------------------
# 0) DOWNLOAD SPECIES DATA FROM GBIF
# ------------------------------------------------------------

res <- download_gbif_fungi_species_batch(
  species_list = species_config$scientific_name,
  country_code = "AR",
  max_records  = 10000,
  out_dir      = raw_occ_dir,
  force        = FORCE
)

# ------------------------------------------------------------
# 1) PREPROCESS RAW OCCURRENCE DATASETS
# ------------------------------------------------------------

for (i in seq_len(nrow(species_config))) {
  sp <- species_config$scientific_name[i]
  pp_path <- species_csv(sp, processed_occ_dir)

  if (!FORCE && file.exists(pp_path)) {
    message("Skip preprocess (ya existe): ", pp_path)
    next
  }

  message("Preprocessing: ", sp)

  preprocess_dataset(
    df_path            = species_csv(sp, raw_occ_dir),
    preproc_path       = pp_path,
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

bioclim_files    <- list.files("data/features/env_2.5m_ar/bioclim", pattern = "tif$", full.names = TRUE)
vegetation_files <- list.files("data/features/env_2.5m_ar/vegetation", pattern = "tif$", full.names = TRUE)
topo_files       <- list.files("data/features/env_2.5m_ar/topography", pattern = "tif$", full.names = TRUE)
soil_files       <- list.files("data/features/env_2.5m_ar/soil", pattern = "tif$", full.names = TRUE)

env_sets <- list(
  bioclim     = list(files = bioclim_files),
  bioclim_veg = list(files = c(bioclim_files, vegetation_files))
)
# #25: topografía (solo si las capas ya fueron preparadas).
if (length(topo_files) > 0) {
  env_sets$bioclim_topo     <- list(files = c(bioclim_files, topo_files))
  env_sets$bioclim_veg_topo <- list(files = c(bioclim_files, vegetation_files, topo_files))
}
# #26: suelo (solo si las capas ya fueron preparadas).
if (length(soil_files) > 0) {
  env_sets$bioclim_soil     <- list(files = c(bioclim_files, soil_files))
  env_sets$bioclim_veg_soil <- list(files = c(bioclim_files, vegetation_files, soil_files))
}

# Identificador de variable, agnóstico a la resolución y robusto a nombres de
# vegetación: para bioclim "wc2.1_30s_bio_11" y "wc2.1_2.5m_bio_11" -> "bio_11";
# para vegetación "tree_cover_pct" queda igual. Permite casar selecciones
# guardadas con nombres viejos contra los archivos actuales.
bio_id <- function(x) sub("^.*?(bio_\\d+)$", "\\1", tools::file_path_sans_ext(basename(x)))

# Registra un env_set reducido (subset no colineal) si existe su selección.
register_reduced_env_set <- function(name, selected_path, candidate_files) {
  if (!file.exists(selected_path)) return(invisible())
  selected <- readr::read_csv(selected_path, show_col_types = FALSE) |>
    dplyr::filter(status == "kept") |>
    dplyr::pull(variable)
  reduced <- candidate_files[bio_id(candidate_files) %in% bio_id(selected)]
  if (length(reduced) == length(selected)) {
    env_sets[[name]] <<- list(files = reduced)
    message(sprintf("env_sets: %s registrado con %d variables.", name, length(reduced)))
  } else {
    warning(basename(selected_path), " presente pero no coincide con los archivos. ",
            "Se omite ", name, ".")
  }
}

# bioclim_reduced: subset no colineal solo de bioclim.
register_reduced_env_set(
  "bioclim_reduced",
  "data/outputs/env_selection/selected_vars.csv",
  bioclim_files
)
# bioclim_veg_reduced: subset no colineal de bioclim + vegetación juntas.
register_reduced_env_set(
  "bioclim_veg_reduced",
  "data/outputs/env_selection/selected_vars_veg.csv",
  c(bioclim_files, vegetation_files)
)
# bioclim_topo_reduced: subset no colineal de bioclim + topografía.
register_reduced_env_set(
  "bioclim_topo_reduced",
  "data/outputs/env_selection/selected_vars_topo.csv",
  c(bioclim_files, topo_files)
)
# bioclim_veg_topo_reduced: subset no colineal de bioclim + vegetación + topografía.
register_reduced_env_set(
  "bioclim_veg_topo_reduced",
  "data/outputs/env_selection/selected_vars_veg_topo.csv",
  c(bioclim_files, vegetation_files, topo_files)
)
# bioclim_soil_reduced: subset no colineal de bioclim + suelo.
register_reduced_env_set(
  "bioclim_soil_reduced",
  "data/outputs/env_selection/selected_vars_soil.csv",
  c(bioclim_files, soil_files)
)
# bioclim_veg_soil_reduced: subset no colineal de bioclim + vegetación + suelo.
register_reduced_env_set(
  "bioclim_veg_soil_reduced",
  "data/outputs/env_selection/selected_vars_veg_soil.csv",
  c(bioclim_files, vegetation_files, soil_files)
)

# 10 000 puntos de background para la estrategia "fixed": valor recomendado para
# técnicas de regresión en Barbet-Massin et al. (2012). La alternativa "match_presence"
# (bp_n = n_presencias) es la recomendada para clasificadores tipo RF/BRT en ese mismo paper.
fixed_bp_n <- 10000L

config_table <- make_config_table(
  species_table   = species_table,
  bp_methods      = c("random"),
  bp_n_strategies = c("fixed", "match_presence"),
  fixed_bp_n      = fixed_bp_n,
  env_sets        = names(env_sets),
  grid_sizes_km   = c(10, 50)
)

manifest <- build_parallel_sdm_datasets(
  config_table = config_table,
  env_sets     = env_sets,
  occ_dir      = processed_occ_dir,
  out_dir      = "data/outputs/sdm_parallel",
  fixed_bp_n   = fixed_bp_n,
  cols_to_keep = NA,
  force        = FORCE
)

print(manifest)
