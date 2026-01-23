#Does the model seem sensible, ecologically?
#Do the fitted functions (the shapes of the modeled relationships) make sense?
#Do the predictions seem reasonable? (map them, and think about them)?
#Are there any spatial patterns in model residuals? (see Leathwick and Whitehead 2001 for an interesting example)

pres <- sdm_data[sdm_data[,1] == 1, 2:9]
back <- sdm_data[sdm_data[,1] == 0, 2:9]
k <- 5
group <- kfold(pres, k)
e <- list()
for (i in 1:k) {
  train <- pres[group != i,]
  test <- pres[group == i,]
  bc <- bioclim(train)
  e[[i]] <- evaluate(p=test, a=back, bc)
}

#We can extract several things from the objects in ‘e’,
#but let’s restrict ourselves to the AUC values and the
#“maximum of the sum of the sensitivity (true positive rate)
#and specificity (true negative rate)” 
#threshold “spec_sens” (this is sometimes uses as a threshold
#                       for setting cells to presence or absence).

auc <- sapply(e, function(x){x@auc})


#The use of AUC in evaluating SDMs has been criticized
#(Lobo et al. 2008, Jiménez-Valverde 2011). 
#A particularly sticky problem is that the values of
#AUC vary with the spatial extent used to select background points.
#Generally, the larger that extent, the higher the AUC value. 
#Therefore, AUC values are generally biased and cannot be directly
#compared. Hijmans (2012) suggests that one could remove “spatial
#sorting bias” (the difference between the distance from
#               testing-presence to training-presence and the distance
#               from testing-absence to training-presence points)
#through “point-wise distance sampling”.

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

nr <- nrow(df_presence)
s <- sample(nr, 0.25 * nr)
pres_train <- df_presence[-s, ]
pres_test <- df_presence[s, ]
nr <- nrow(bg_points)
set.seed(9)
s <- sample(nr, 0.25 * nr)
back_train <- bg_points[-s, ]
back_test <- bg_points[s, ]
sb <- ssb(pres_test, back_test, pres_train)
sb[,1] / sb

#sb[,1] / sb[,2] is an indicator of spatial sorting bias (SSB).
#If there is no SSB this value should be 1, in these data
#it is close to zero, indicating that SSB is very strong.
#Let’s create a subsample in which SSB is removed.

pres_test_pwd <- pres_test[!is.na(i[,1]), ]
back_test_pwd <- back_test[na.omit(as.vector(i)), ]
sb2 <- ssb(pres_test_pwd, back_test_pwd, pres_train)
sb2[1]/ sb2[2]

predictors_r <- stack(files) 
pres_train_df <- as.data.frame(pres_train)[, c("x", "y")]
bc <- bioclim(predictors_r, pres_train_df)
evaluate(bc, p=pres_test, a=back_test, x=predictors_r)
#Spatial sorting bias is much reduced now; notice how the AUC dropped!

