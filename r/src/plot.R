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


plot_points_in_map <- function(df) {
  
  #shp <- create_shp_from(shp_path)
  
  ggplot() +
    #layer_shp(shp) +
    layer_points(df,
                 crs = 4326,
                 #crs = st_crs(shp),
                 x = 'decimalLongitude',
                 y = 'decimalLatitude',
                 color = "blue",
                 size = 1) +
    coord_sf() +
    theme_minimal()
}

plot_points_in_map <- function(df, color_by_class = FALSE) {
  
  p <- ggplot()
  
  if (color_by_class) {
    p <- p +
      geom_point(
        data = df,
        aes(x = decimalLongitude, y = decimalLatitude, color = factor(class)),
        size = 1
      ) +
      scale_color_manual(
        values = c("1" = "red", "0" = "blue"),
        name = "Class"
      )
  } else {
    p <- p +
      layer_points(df,
                   crs = 4326,
                   x = 'decimalLongitude',
                   y = 'decimalLatitude',
                   color = "blue",
                   size = 0.5)
  }
  
  p + coord_sf() + theme_minimal()
}

plot_points_in_map_fast <- function(df, shp_path,
                                    x = "decimalLongitude",
                                    y = "decimalLatitude",
                                    point_color = "blue",
                                    point_size = 0.8,
                                    shp_fill = "grey90",
                                    shp_color = "black") {
  
  shp <- create_shp_from(shp_path)
  
  # llevar shapefile a lon/lat para usar geom_point directamente
  shp <- st_transform(shp, 4326)
  
  ggplot() +
    geom_sf(
      data = shp,
      fill = shp_fill,
      color = shp_color
    ) +
    geom_point(
      data = df,
      aes(x = .data[[x]], y = .data[[y]]),
      color = point_color,
      size = point_size
    ) +
    coord_sf() +
    theme_minimal()
}

plot_points_in_map_quick <- function(df, shp,
                                     x = "decimalLongitude",
                                     y = "decimalLatitude",
                                     point_col = "blue",
                                     point_cex = 0.4,
                                     border_col = "black",
                                     fill_col = "grey90") {
  stopifnot(inherits(shp, "sf"))
  
  # transformar una sola vez si hace falta
  shp_ll <- sf::st_transform(shp, 4326)
  
  plot(sf::st_geometry(shp_ll), col = fill_col, border = border_col)
  points(df[[x]], df[[y]], col = point_col, cex = point_cex, pch = 16)
}
