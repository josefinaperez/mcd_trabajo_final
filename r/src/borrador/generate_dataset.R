library(terra)
library(readr)
library(sp)
library(dplyr)


generate_background_points <- function(shp_path,
                                       bg_points_size,
                                       bg_points_method="random",
                                       seed = 99) {
  
  # Crear raster en la geometría del shp
  shp <- vect(shp_path)
  r <- rast(
    ext(shp),
    resolution = 0.01,
    crs = crs(shp)
  )
  values(r) <- 1
  r <- mask(r, shp)
  
  # Generar background points
  set.seed(seed)
  bg <- spatSample(
    r,
    size = bg_points_size,
    method = bg_points_method,
    na.rm = TRUE,
    xy = TRUE) |>
    dplyr::select(x, y)
  
  return(bg)
}

extract_values <- function(path_rasters,
                           df_points) {
  
  files <- list.files(path_rasters, full.names=TRUE)
  predictors <- rast(files)
  #plot(predictors)
  
  # Extraer valores del raster
  values <- extract(predictors, df_points) |>
            dplyr::select(-ID)
          
  return(values)
}


generate_sdm_data <- function(shp_path,
                              df_ocurrences_path,
                              save_path=NA) {
  
 # mejorar
  path_rasters <- "data/features/worldclim/wc2.1_30s_bio"
  
  # Presence values
    # Filtrar puntos dentro del shp
  shp <- st_as_sf(vect(shp_path))
  df_presence <- read_csv(df_ocurrences_path) |>
                  mutate(
                    x = decimalLongitude,
                    y = decimalLatitude
                  ) |>
                 st_as_sf(coords = c(lon_col, lat_col), crs = 4326) |>
                 st_transform(st_crs(shp)) |>
                 filter(st_within(geometry, shp, sparse = FALSE)) |>
                 st_drop_geometry() |>
                 dplyr::select(x, y)
  
  
  # Background points
  bg_points_size <- nrow(df_presence) # ver como calularlo
  bg_points_method <- "random" # parametrizar
  
  bg_points <- generate_background_points(shp_path,
                                          bg_points_size,
                                          bg_points_method)
  
  # Extraer valores de los rasters
  presence_values <- extract_values(path_rasters,
                                    df_presence)
  
  bg_values <- extract_values(path_rasters,
                              bg_points)
  
  pb <- c(rep(1, nrow(presence_values)), rep(0, nrow(bg_values)))
  sdm_data <- data.frame(cbind(pb, rbind(presence_values, bg_values)))
  
  if(!is.na(save_path)) {
    write.csv(sdm_data, save_path, row.names = FALSE)
  } 
  
  return(sdm_data)
}



