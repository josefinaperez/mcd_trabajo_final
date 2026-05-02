library(readr)
library(CoordinateCleaner)
library(dplyr)
source("r/src/utils.R")


preprocess_dataset <- function(df_path,
                               preproc_path,
                               min_year,
                               scientific_name,
                               max_uncertainty_km,
                               shp_path,
                               tests)
{
  df_trametes_sanguinea <- read_csv(df_path)

  # Remover registros sin coordenadas
  df_clean <- df_trametes_sanguinea %>%
    filter(!is.na(decimalLongitude)) %>%
    filter(!is.na(decimalLatitude))
  
  # Remover por fecha
  df_clean <- df_clean %>%
    filter(year >= min_year)
  
  # Remover diferentes especies
  df_clean <- df_clean %>%
    filter(scientificName == scientific_name)
  
  # Remover registros fuera del área de estudio
  shp_area <- create_shp_from(shp_path)
  df_clean <- filter_points(shp_area, 
                            df_clean,
                            lon_col = 'decimalLongitude',
                            lat_col = 'decimalLatitude')
  
  # Remover registros con baja precisión
  df_clean <- df_clean %>%
    filter(coordinateUncertaintyInMeters / 1000 <= max_uncertainty_km | is.na(coordinateUncertaintyInMeters))
  
  # Remover registros que no correspondan a una observación humana
  df_clean <- filter(df_clean, basisOfRecord == "HUMAN_OBSERVATION" | 
                       basisOfRecord == "OBSERVATION" |
                       basisOfRecord == "PRESERVED_SPECIMEN" |
                       basisOfRecord == "LIVING_SPECIMEN")
  
  
  # Remover registros según los tests.
  # `clean_coordinates()` (wrapper) tiene un bug de longitud de nombres en
  # cbind interno con datasets chicos / 1 sola especie. Llamamos las
  # funciones cc_* individuales: cada una devuelve un vector lógico
  # (TRUE = válido) que combinamos con AND. Sin cbind, sin el bug.
  cc_input <- as.data.frame(df_clean)

  test_fns <- list(
    equal      = function(d) cc_equ(d,  lon = "decimalLongitude", lat = "decimalLatitude", value = "flagged"),
    zeros      = function(d) cc_zero(d, lon = "decimalLongitude", lat = "decimalLatitude", value = "flagged"),
    duplicates = function(d) cc_dupl(d, lon = "decimalLongitude", lat = "decimalLatitude",
                                     species = "species", value = "flagged"),
    outliers   = function(d) cc_outl(d, lon = "decimalLongitude", lat = "decimalLatitude",
                                     species = "species", value = "flagged")
  )

  flag_mat <- vapply(
    tests,
    function(t) {
      fn <- test_fns[[t]]
      if (is.null(fn)) {
        warning("Unknown CoordinateCleaner test: ", t, " (skipped)")
        return(rep(TRUE, nrow(cc_input)))
      }
      tryCatch(
        as.logical(fn(cc_input)),
        error = function(e) {
          warning("cc_", t, " failed (", conditionMessage(e),
                  "); skipping for ", scientific_name)
          rep(TRUE, nrow(cc_input))
        }
      )
    },
    logical(nrow(cc_input))
  )

  # Si solo hay un test, vapply devuelve vector → reconvertir a matriz
  if (is.null(dim(flag_mat))) flag_mat <- matrix(flag_mat, ncol = length(tests))
  colnames(flag_mat) <- tests

  ok_rows <- apply(flag_mat, 1, all)

  df_flagged <- df_clean[!ok_rows, , drop = FALSE]
  df_flagged$FLAG <- apply(
    flag_mat[!ok_rows, , drop = FALSE],
    1,
    function(x) paste(tests[!x], collapse = ", ")
  )

  df_clean <- df_clean[ok_rows, , drop = FALSE]
  
  dir.create(dirname(preproc_path), recursive = TRUE, showWarnings = FALSE)
  readr::write_csv(df_clean, preproc_path)
  
  return(list(df_clean = df_clean,
       df_flagged = df_flagged))
}
  
  
  
