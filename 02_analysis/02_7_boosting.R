print("Loading Packages")
source("02_analysis/00_packages.R")
print("Packages loaded")

#####################################################################################
############################# BOOSTING ##############################################
#####################################################################################

print("Now doing the Boosting-Loop")

lam <- seq(0.1,1,0.1)
dep <- seq(1,15,2)
trees <- seq(25,125,25)
grid <- expand.grid(lam, dep,trees)
rmse_BO <- c()
d <- c()
tic()
cl <- parallel::makeCluster(2)
doParallel::registerDoParallel(cl)
for(i in 1:nrow(grid)){
  x <- Sys.time()
  boost_wine <- gbm.fit(y.train, x = x.train, distribution = "gaussian",
                        n.trees = grid[i,3], interaction.depth = grid[i,2], 
                        shrinkage = grid[i,1], verbose = FALSE)    # try different depths and shrinkage
  pred_boost <- predict(boost_wine, newdata = x.test, n.trees = 25)
  rmse_BO[i] <- mean((y.test - pred_boost)^2) %>% sqrt()
  d[i] <- Sys.time() - x
  print(paste("~",(round(mean(d)*(nrow(grid)-i)/60)), " minutes remaining.", sep = ""))
}
parallel::stopCluster(cl)
toc()

grid$RMSE <- rmse_BO %>% round()
min(rmse_BO)
grid[rmse_BO %>% which.min(),]

#scatter3D(x = grid$Var1, y = grid$Var2, z = grid$RMSE, xlab = "Shrinkage", ylab = "depth",
#          zlab = "RMSE", ticktype = "detailed", pch = 16, cex = 1.25, type = "h", bty = "b2")



#for n.tree = 100, depth= 10, shrinkage = 0.05: RMSE = 5425
#for n.tree = 100, depth= 10, shrinkage = 0.2: RMSE = 4847
#for n.tree = 100, depth= 10, shrinkage = 0.4: RMSE = 4961
#for n.tree = 100, depth= 10, shrinkage = 0.5: RMSE = 4864
#for n.tree = 100, depth= 10, shrinkage = 0.2: RMSE = 4519
#for n.tree = 100, depth= 25, shrinkage = 0.05: RMSE = 4315
#for n.tree = 100, depth= 35, shrinkage = 0.05: RMSE = 4225 # took about 840 secs


# One cannot set the depth higher than 50:
# Error in checkID(interaction.depth) : 
#   interaction.depth must be less than 50. 
# You should also ask yourself why you want such large interaction terms. 
# A value between 1 and 5 should be sufficient for most applications.

# Maybe use this for variable importance plots:
# https://www.r-bloggers.com/in-search-of-the-perfect-partial-plot/

dir.create("02_analysis/cv/boosting/", recursive = T, showWarnings = F)
save(file = paste("02_analysis/cv/boosting/boost_grid_",unique_identifier,".rda", sep = ""),
     grid)
