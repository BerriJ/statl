print("Loading Packages")
source("02_analysis/00_packages.R")
print("Packages loaded")

################################################################################
############################ Random Forest #####################################
################################################################################

print("Now doing the Random Forest-Loop")

m <- seq(10,100,10)
trees <- c(1,3,5,10,15,20,25)
grid <- expand.grid(m,trees) %>% arrange(Var1, Var2)
grid <- data.frame(grid, fold = rep(NA, nrow(grid)))
colnames(grid) <- c("mtry","trees", paste("rmse_fold_",i,sep = ""))
z <- 0
d <- c()
tic()
for(k in 1:length(m)){
  x <- Sys.time()
  rf_wine <- randomForest(x = x.train, y = y.train, importance = T,
                          mtry = m[k], ntree = max(grid$trees))
  for(t in 1:length(trees)){
    z <- z+1
    pred_rf <- predict(rf_wine, newdata = x.test, n.trees = trees[t])
    grid[z,3] <- mean((y.test - pred_rf)^2) %>% sqrt()
  }
  d[i] <- Sys.time() - x
  print(paste("Iteration No. ",k, " ~ ",round(mean(d)*(nrow(grid)-k)), " minutes remaining.", sep = ""))
}
toc()

dir.create("02_analysis/cv/rf/", recursive = T, showWarnings = F)
save(file = paste("02_analysis/cv/rf/rf_",unique_identifier,".rda", sep = ""),
     grid)
