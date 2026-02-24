library(readr)
library(CoordinateCleaner)
library(dplyr)
source("../src/utils.R")


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
  
  
  # Remover registros según los tests
  flags <- clean_coordinates(x = df_clean, 
                             lon = "decimalLongitude", 
                             lat = "decimalLatitude",
                             species = "species",
                             tests = tests)
  
  flag_cols <- names(flags)[grepl("^\\.", names(flags)) & names(flags) != ".summary"]
  
  df_flagged <- df_clean[!flags$.summary,]
  
  df_flagged$FLAG <- apply(
    flags[!flags$.summary, flag_cols],
    1,
    function(x) {
      paste(flag_cols[!x], collapse = ", ")
    }
  )
  
  df_clean <- df_clean[flags$.summary,]
  
  write.csv(df_clean, preproc_path)
  
  return(list(df_clean = df_clean,
       df_flagged = df_flagged))
}
  
  
  
