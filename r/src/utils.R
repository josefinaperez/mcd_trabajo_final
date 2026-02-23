library(sf)
library(terra)

create_shp_from <- function(shp_path) {
  return(st_as_sf(vect(shp_path)))
}

filter_points <- function(shp,
                          df,
                          lon_col,
                          lat_col) {
  
  df_filtered <- df |>
    mutate(
      x = df[[lon_col]],
      y = df[[lat_col]]
    ) |>
    st_as_sf(coords = c(lon_col, lat_col), crs = 4326) |>
    st_transform(st_crs(shp)) |>
    filter(st_within(geometry, shp, sparse = FALSE)) |>
    st_drop_geometry() |>
    #dplyr::select(x, y) |>
    rename(
      !!lon_col := x,
      !!lat_col := y
    )
  
  return(df_filtered)
}