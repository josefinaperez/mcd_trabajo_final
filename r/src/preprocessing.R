library(readr)
library(CoordinateCleaner)
library(dplyr)
library(ggplot2)
source("r/src/utils.R")

df_path <- "data/ocurrences/raw/df_trametes_sanguinea.csv"
min_year <- 1945
scientific_name <- 'Polyporaceae'
max_uncertainty_km <- 10

preprocess_dataset <- function(df_path,
                               min_year,
                               scientific_name,
                               shp_path="data/shp/argentina/argentina.shp")
  
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
                             tests = c("capitals",
                                       "centroids",
                                       "equal",
                                       "gbif",
                                       "institutions",
                                       "outliers",
                                       "seas",
                                       "zeros"))
  
  df_clean <- df_clean[flags$.summary,]
  df_flagged <- df_clean[!flags$.summary,]
  
  return(df_clean, df_flagged)



