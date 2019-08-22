print("Loading Packages")
source("02_analysis/00_packages.R")
print("Packages loaded")

################################################################################
################################ Bagging #######################################
################################################################################

models <- list()

print("Now doing the Bagging-Loop")

trees <- c(15,20) # We should think about what sizes we want to try
rmse_BA <- c()
d <- c()
tic()
for(i in 1:length(trees)){
  x <- Sys.time()
  models[[i]] <- randomForest(x = x.train, y = y.train, mtry = ncol(x.train)-1, importance = T, ntree = trees[i])
  pred_bag <- predict(models[[i]], newdata = x.test, n.trees = trees[i])
  rmse_BA[i] <- mean((y.test - pred_bag)^2) %>% sqrt()
  d[i] <- Sys.time() - x
  print(paste("~",round(mean(d)*(length(trees)-i)), " minutes remaining.", sep = ""))
}
toc()

dir.create("02_analysis/cv/bagging/", recursive = T, showWarnings = F)
save(file = paste("02_analysis/cv/bagging/bagging_data",unique_identifier,".rda", sep = ""),
     trees, rmse_BA)