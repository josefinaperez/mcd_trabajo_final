library(maxnet)

set.seed(0)

# split train test
group <- kfold(df_presence, 5)
pres_train <- df_presence[group != 1, ]
pres_test <- df_presence[group == 1, ]


pres <- sdm_data[sdm_data[,1] == 1, 2:9]
back <- sdm_data[sdm_data[,1] == 0, 2:9]
k <- 5
group <- kfold(pres, k)
e <- list()
for (i in 1:k) {
  train <- pres[group != i,]
  test <- pres[group == i,]
  #train modelo
  e[[i]] <- evaluate(p=test, a=back, bc)
}


train_maxent <- function(data) {
  m <- maxnet(
    p = sdm_data$pb,
    data = sdm_data[, -1],  # variables ambientales
    f = maxnet.formula(sdm_data$pb, sdm_data[, -1])
  )
  
  return(m)
}

