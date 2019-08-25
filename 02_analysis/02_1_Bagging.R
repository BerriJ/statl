print("Loading Packages")
source("02_analysis/00_packages.R")
print("Packages loaded")

################################################################################
################################ Bagging #######################################
################################################################################

# Create a list for the models
models <- list()
# Range of the trees
trees <- c(1,5,10,15,20,25,50,100,150) # We should think about what sizes we want to try
rmse_BA <- c()
d <- c()
tic()
for(i in 1:length(trees)){
  x <- Sys.time() # save start time of iteration i
  
  # Train the bagging model with trees[i] trees
  models[[i]] <- randomForest(x = x.train, y = y.train, mtry = ncol(x.train), 
                              importance = T, ntree = trees[i])
  
  # Predict the test data
  pred_bag <- predict(models[[i]], newdata = x.test)
  
  # Calculate RMSE
  rmse_BA[i] <- mean((y.test - pred_bag)^2) %>% sqrt()
  d[i] <- Sys.time() - x # save end time of iteration i
  
  # Print an estimated duration
  print(paste("~",round(mean(d)*(length(trees)-i)), " minutes remaining.", sep = ""))
}
toc()

# Create folder if not existent
dir.create("02_analysis/cv/bagging/", recursive = T, showWarnings = F)

# Save the results
save(file = paste("02_analysis/cv/bagging/bagging_data",unique_identifier,".rda", sep = ""),
     trees, rmse_BA)