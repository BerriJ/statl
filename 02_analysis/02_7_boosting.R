print("Loading Packages")
source("02_analysis/00_packages.R")
print("Packages loaded")

################################################################################
############################# BOOSTING Loop ####################################
################################################################################

print("Now doing the Boosting-Loop")

lam <- seq(0.1,1,0.1)
dep <- seq(1,15,2)
bag_frac <- c(0.5,1)
grid <- expand.grid(lam, dep, bag_frac)
n.trees <- 1:25
results <- data.frame(grid, matrix(ncol = 25, nrow = nrow(grid))) 
colnames(results) <- c("lambda", "int.depth", "bag.frac", paste("n.tree_",1:25, sep = ""))
results <- results %>% arrange(desc(int.depth))
d <- c()
tic()
for(i in 1:nrow(grid)){
  x <- Sys.time()
  boost_wine <- gbm.fit(y.train, x = x.train, distribution = "gaussian",
                        n.trees = 25, interaction.depth = grid[i,2],
                        shrinkage = grid[i,1], verbose = FALSE, bag.fraction = grid[i,3])
  #Generating a Prediction matrix for each Tree
  predmatrix<-predict(boost_wine,x.test,n.trees = n.trees)
  #Calculating The Mean squared Test Error
  results[i,4:28] <- apply((predmatrix-y.test)^2,2,mean) %>% sqrt()
  d[i] <- Sys.time() - x
  print(paste("~",(round(mean(d)*(nrow(grid)-i)/60)), " minutes remaining.", sep = ""))
}
toc()

# Maybe use this for variable importance plots:
# https://www.r-bloggers.com/in-search-of-the-perfect-partial-plot/

dir.create("02_analysis/cv/boosting/", recursive = T, showWarnings = F)
save(file = paste("02_analysis/cv/boosting/boost_grid_",unique_identifier,".rda", sep = ""),
     results)

