library(readr)
library(dismo)
library(terra)

sdm_data <- read_csv("data/train/df_trametes_sanguinea_santa_fe.csv")
m1 <- glm(pb ~ wc2.1_30s_bio_1 + wc2.1_30s_bio_5 + wc2.1_30s_bio_12,
          data=sdm_data)

m2 <- glm(pb ~ ., data=sdm_data)
bc <- bioclim(presvals[,c('wc2.1_30s_bio_1', 'wc2.1_30s_bio_5', 'wc2.1_30s_bio_12')])

# train test
df_ocurrences_path <- 'data/ocurrences/processed/df_trametes_sanguinea.csv'
shp <- st_as_sf(vect('data/shp/santa_fe/santa_fe.shp'))
lon_col <- 'decimalLongitude'
lat_col <-'decimalLatitude'
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

presence_values <- extract_values(path_rasters,
                                  df_presence)
bg_points <- generate_background_points(shp_path,
                                        bg_points_size,
                                        bg_points_method)


set.seed(0)

#pres <- smd_data[smd_data[,1] == 1,]
#back <- smd_data[smd_data[,1] == 0,]
group <- kfold(df_presence, 5)
pres_train <- df_presence[group != 1, ]
pres_test <- df_presence[group == 1, ]

#otra forma para los bg points, ver en modelling methods del tutorial
#ext es la extension. 1.25 es 12.5% de la superficie
#set.seed(10)
#backg <- randomPoints(pred_nf, n=1000, ext=ext, extf = 1.25)
#colnames(backg) = c('lon', 'lat')
group <- kfold(bg_points, 5)
backg_train <- bg_points[group != 1, ]
backg_test <- bg_points[group == 1, ]


#plot
shp <- vect('data/shp/santa_fe/santa_fe.shp')
r <- rast(
  ext(shp),
  resolution = 0.01,
  crs = crs(shp)
)
values(r) <- 1
r <- mask(r, shp)

plot(!is.na(r), col=c('white', 'light grey'), legend=FALSE)
points(backg_train, pch='-', cex=0.5, col='yellow')
points(backg_test, pch='-',  cex=0.5, col='black')
points(pres_train, pch= '+', col='green')
points(pres_test, pch='+', col='blue')


predict_raster <- function(shp_path,
               model) {
  
  # mejorar
  path_rasters <- "data/features/worldclim/wc2.1_30s_bio"
  files <- list.files(path_rasters, full.names=TRUE)
  predictors <- rast(files)
  
  # Recortar rasters a shp
  roi <- vect(shp_path)
  predictors_sf <- crop(predictors, roi) |> mask(roi)
  p <- predict(predictors_sf, model)
  
  return(p)
}

#maxent 
library(maxnet)
m <- maxnet(
  p = sdm_data$pb,
  data = sdm_data[, -1],  # variables ambientales
  f = maxnet.formula(sdm_data$pb, sdm_data[, -1])
)   #falta train test

path_rasters <- "data/features/worldclim/wc2.1_30s_bio"
files <- list.files(path_rasters, full.names=TRUE)
predictors <- rast(files)

# Recortar rasters a shp
roi <- vect(shp_path)
predictors_sf <- crop(predictors, roi) |> mask(roi)
pred <- raster::predict(
  predictors_r,
  m,
  type = "cloglog",
  na.rm = TRUE
)

plot(pred)
e <- evaluate(pres_test, backg_test, xm, predictors)

