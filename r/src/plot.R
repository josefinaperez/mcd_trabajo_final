library(ggplot2)
library(sf)

layer_shp <- function(
    shp,
    shp_fill = "grey90",
    shp_color = "black"
) {
  
 geom_sf(
      data = shp,
      fill = shp_fill,
      color = shp_color
    )
}

layer_points <- function(df,
                         crs,
                         x = 'x',
                         y = 'y',
                         color = "red",
                         size = 2,
                         shape = 16) {
  
  points_sf <- st_as_sf(
    df,
    coords = c(x, y),
    crs = 4326, 
    remove = FALSE
  )
  
  points_sf <- st_transform(points_sf, crs)
  
  geom_sf(
    data = points_sf,
    color = color,
    size = size,
    shape = shape
  )
}


plot_shp_points <- function(
    shp,
    df,
    x = "x",
    y = "y",
    crs_points = NULL,
    point_color = "red",
    point_size = 2,
    shp_fill = "grey90",
    shp_color = "black"
) {
 stopifnot(inherits(shp, "sf"))
 
 # Convertir puntos a sf
 pts <- sf::st_as_sf(
   df,
   coords = c(x, y),
   crs = crs_points
 )

 # Reproyectar puntos si hace falta
 if (!is.null(crs_points)) {
   pts <- sf::st_transform(pts, sf::st_crs(shp))
 }
  
  ggplot2::ggplot() +
    ggplot2::geom_sf(
      data = shp,
      fill = shp_fill,
      color = shp_color
    ) +
    ggplot2::geom_sf(
      data = pts,
      color = point_color,
      size = point_size
    ) +
    ggplot2::coord_sf() +
    ggplot2::theme_minimal()
}


plot_points_in_map <- function(df, shp_path) {
  
  shp <- create_shp_from(shp_path)
  
  ggplot() +
    layer_shp(shp) +
    layer_points(df,
                 crs = st_crs(shp),
                 x = 'decimalLongitude',
                 y = 'decimalLatitude',
                 color = "blue",
                 size = 1) +
    coord_sf() +
    theme_minimal()
}
