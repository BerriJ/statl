print("Loading Packages")
source("02_analysis/00_packages.R")
print("Packages loaded")

################################################################################
############################ Random Forest #####################################
################################################################################

print("Now doing the Random Forest-Loop")

m <- seq(10,100,10)
trees <- c(1,3,5,10,15,20,25)
grid <- expand.grid(m,trees)
rmse_RF <- c()
d <- c()
tic()
for(i in 1:nrow(grid)){
  x <- Sys.time()
  rf_wine <- randomForest(x = x.train, y = y.train, importance = T,
                          mtry = grid[i,1], ntree = grid[i,2])
  pred_rf <- predict(rf_wine, newdata = x.test, n.trees = grid[i,2])
  rmse_RF[i] <- mean((y.test - pred_rf)^2) %>% sqrt()
  d[i] <- Sys.time() - x
  print(paste("Iteration No. ",i, " ~ ",round(mean(d)*(nrow(grid)-i)), " minutes remaining.", sep = ""))
}
toc()

dir.create("02_analysis/cv/rf/", recursive = T, showWarnings = F)
save(file = paste("02_analysis/cv/rf/rf_",unique_identifier,".rda", sep = ""),
     grid, rmse_RF)
