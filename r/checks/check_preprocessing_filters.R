# r/checks/check_preprocessing_filters.R
# Verifica la coherencia de los filtros de calidad (issue 22): corte de año,
# umbral de incertidumbre (+NA conservado), whitelist de basisOfRecord (incl.
# MATERIAL_SAMPLE) y los tests cc_inst/cc_cen/cc_cap. Correr desde la raíz:
#   Rscript r/checks/check_preprocessing_filters.R
# Guardado: se omite si falta el shapefile de Argentina.

suppressMessages({
  library(readr)
  library(dplyr)
})
source("r/src/preprocessing.R")  # trae utils.R y CoordinateCleaner

shp_path <- "data/shp/argentina/argentina.shp"
if (!file.exists(shp_path)) {
  message("check_preprocessing_filters: SKIP (falta ", shp_path, ")")
  quit(save = "no", status = 0)
}

# Centroide de país que usa cc_cen (misma referencia countryref) -> garantiza flag.
cref  <- CoordinateCleaner::countryref
arg_c <- cref[cref$iso3 == "ARG" & cref$type == "country", ][1, ]
cen_lon <- arg_c$centroid.lon
cen_lat <- arg_c$centroid.lat

# Dataset sintético: una fila por regla. Los puntos "buenos" caen en un cluster
# del interior de Buenos Aires (dentro de Argentina, lejos de capitales y
# centroides de provincia) para sobrevivir el clip sin falsos flags.
synth <- tibble::tibble(
  id                            = 1:8,
  scientificName                = "Polyporaceae",
  species                       = "Polyporaceae sp",
  decimalLongitude              = c(-60.0, -60.1, -60.2, -60.3, -60.4, -60.5, -60.6, cen_lon),
  decimalLatitude               = c(-34.0, -34.1, -34.2, -34.3, -34.4, -34.5, -34.6, cen_lat),
  year                          = c(2015, 2018, 2015, 1990, NA, 2015, 2015, 2015),
  coordinateUncertaintyInMeters = c(100, 200, 100, 100, 100, 8000, NA, 100),
  basisOfRecord                 = c("HUMAN_OBSERVATION", "MATERIAL_SAMPLE", "FOSSIL_SPECIMEN",
                                    "HUMAN_OBSERVATION", "HUMAN_OBSERVATION", "HUMAN_OBSERVATION",
                                    "HUMAN_OBSERVATION", "HUMAN_OBSERVATION")
)

in_csv  <- tempfile(fileext = ".csv")
out_csv <- tempfile(fileext = ".csv")
readr::write_csv(synth, in_csv)

res <- preprocess_dataset(
  df_path            = in_csv,
  preproc_path       = out_csv,
  min_year           = 2010,
  scientific_name    = "Polyporaceae",
  max_uncertainty_km = 5,
  shp_path           = shp_path,
  tests              = c("equal", "duplicates", "zeros",
                         "institutions", "centroids", "capitals")
)

clean   <- res$df_clean
flagged <- res$df_flagged

# Sobreviven exactamente las filas 1 (básica), 2 (MATERIAL_SAMPLE) y 7 (unc NA).
stopifnot(setequal(clean$id, c(1, 2, 7)))
# MATERIAL_SAMPLE entra; FOSSIL_SPECIMEN no.
stopifnot("MATERIAL_SAMPLE" %in% clean$basisOfRecord)
stopifnot(!"FOSSIL_SPECIMEN" %in% clean$basisOfRecord)
# Año < 2010 (fila 4) y año NA (fila 5) descartados; unc > 5 km (fila 6) descartada.
stopifnot(!any(c(4, 5, 6) %in% clean$id))
# El punto en el centroide del país (fila 8) lo marca cc_cen ("centroids").
stopifnot(8 %in% flagged$id)
stopifnot(any(grepl("centroids", flagged$FLAG[flagged$id == 8])))

message("check_preprocessing_filters: OK")
