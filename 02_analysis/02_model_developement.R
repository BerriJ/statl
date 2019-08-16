# rm(list = ls())
# load("00_data/wine_preprocessed.rda")
# 
# # Remove variables with average na >= 50%
# wine <- wine %>% dplyr::select_if(.predicate = function(x) mean(is.na(x)) < 0.50) %>%
#   # Only keep complete cases
#   drop_na() %>%
#   # Drop llitre because we are using litre
#   dplyr::select(-llitre) %>%
#   # Remove unused levels from factor variables
#   droplevels()
# 
# # Split intro Training (3/4) and Test (1/4)
# 
# train <- sample(nrow(wine), floor(0.75*nrow(wine)))
# wine_train <- wine[train,]
# wine_test <- wine[-(train),]
# rm(train)


############# Above Code that will be run in 00_job_setup.R ####################
################################################################################
##################### Below Code will be run here ##############################

print("Loading Packages")
source("02_analysis/00_packages.R")
print("Packages loaded")

# Set up a data frame for model comparison
models <- data.frame(mod = rep(NA, 30), rmse = rep(NA, 30))

################################################################################
############################# Mean Regression ##################################
################################################################################
print("Start Mean Regression")
residuals <- wine_train$litre-mean(wine_train$litre, na.rm = T)
models[min(which(is.na(models$mod))),1] <- "mean_regression"
models[min(which(is.na(models$rmse))),2] <- residuals^2 %>% mean() %>% sqrt()

################################################################################
######################## Linear Model without selection ########################
################################################################################
print("Start Linear Model")
# WICHTIG Hier wird die Model Matrix so gebaut, dass Training und Test das exakt
# gleiche Feature Set aufweisen.

x.train <- model.matrix(litre~., data = wine_train)
x.test <- model.matrix(litre~., data = wine_test)
y.train <- wine_train$litre
y.test <- wine_test$litre
intsct <- intersect(colnames(x.train),colnames(x.test))
x.train <- x.train[,intsct]
x.test <- x.test[, intsct]

train_df <- cbind(y.train, x.train) %>% as.data.frame()
test_df <- cbind(y.test, x.test) %>% as.data.frame()

lmod <- lm(y.train~. -1,train_df)

preds <- predict(lmod, newdata = test_df)

models[min(which(is.na(models$mod))),1] <- "linear_model"
models[min(which(is.na(models$rmse))), "rmse"] <- (y.test-preds)^2 %>% 
  mean() %>%
  sqrt()

models

################################################################################
################################## LASSO #######################################
################################################################################
print("Start Lasso")

cv.out <- cv.glmnet(x = x.train, y = y.train, alpha = 1)
out <- glmnet(x = x.train, y = y.train, alpha = 1)
plot(cv.out)
bestlam <- cv.out$lambda.min
lasso.mod <- glmnet(x = x.train, y = y.train, alpha = 1, lambda = bestlam)
plotmo::plot_glmnet(out)
# prediction
pred <- predict(lasso.mod, x.test, s = bestlam)

models[min(which(is.na(models$mod))),1] <- "lasso"
models[min(which(is.na(models$rmse))), "rmse"] <- mean((y.test - pred)^2) %>% sqrt()

############################## Transform Y #####################################

# Areasinus Hyperbolicus
y.train_asinh <- asinh(wine_train$litre)
cv.out <- cv.glmnet(x = x.train, y = y.train_asinh, alpha = 1)
bestlam <- cv.out$lambda.min
lasso.mod_asinh <- glmnet(x = x.train, y = y.train_asinh, alpha = 1, lambda = bestlam)
pred <- predict(lasso.mod_asinh, x.test, s = bestlam)
pred <- sinh(pred)
models[min(which(is.na(models$mod))),1] <- "lasso_asinh"
models[min(which(is.na(models$rmse))), "rmse"] <- mean((y.test - pred)^2) %>% sqrt()

# Log
y.train_log<- log(wine_train$litre)
cv.out <- cv.glmnet(x = x.train, y = y.train_log, alpha = 1)
bestlam <- cv.out$lambda.min
lasso.mod_log <- glmnet(x = x.train, y = y.train_log, alpha = 1, lambda = bestlam)
pred <- predict(lasso.mod_log, x.test, s = bestlam) %>% exp()
models[min(which(is.na(models$mod))),1] <- "lasso_log"
models[min(which(is.na(models$rmse))), "rmse"] <- mean((y.test - pred)^2) %>% sqrt()

sum(x.train[,1] != 0 & x.train[,1] != 1)
notdummy <- which(apply(x.train, MARGIN = 2, FUN = function(x) sum(x != 0 & x != 1)) != 0)

for(i in seq_along(notdummy)){
  plot(x.train[,notdummy[i]],
       main = names(notdummy)[i])
  Sys.sleep(0.1)
}

test <- lasso.mod$beta[,1]
df <- data.frame(coef = names(test), beta = round(test))
df$coef %in% names(notdummy)

# The following shows the notdummys that are included in lasso with their coefficients

df[df$coef %in% names(notdummy),] %>% arrange(beta)

plot(wine$year)

################################################################################
################################# PCA ##########################################
################################################################################
print("Start PCA")
# https://www.datacamp.com/community/tutorials/pca-analysis-r

MM <- model.matrix(litre ~. -1, data = wine)
wine.pc <- prcomp(MM, center = T, scale. = T)
wine.pc.sm <- summary(wine.pc)
wine.pc.sm$importance

#remotes::install_github("vqv/ggbiplot")
ggbiplot::ggbiplot(wine.pc)
ggbiplot::ggbiplot(wine.pc,ellipse = T, groups = wine$taste_segment)
ggbiplot::ggbiplot(wine.pc, ellipse = T, groups = wine$segm)
ggbiplot::ggbiplot(wine.pc, ellipse = T, groups = wine$dist)
ggbiplot::ggbiplot(wine.pc, ellipse = T, groups = wine$dist,
                   choices = c(100,50))

wine.pcr.fit <- pcr(y.train ~ ., 
                    data = train_df, 
                    validation = "CV")

wine.pcr.fit$validation$PRESS %>% which.min()

pred <- predict(wine.pcr.fit, test_df, ncomp = 640)

models[min(which(is.na(models$mod))),1] <- "pcr"
models[min(which(is.na(models$rmse))), "rmse"] <- mean((y.test - pred)^2) %>% sqrt()

################################################################################
################################ Splines?? #####################################
################################################################################


# Natural Spline

wine_train$year <- as.numeric(wine_train$year)
wine_test$year <- as.numeric(wine_test$year)

x.train <- model.matrix(litre~. -1, data = wine_train)
x.test <- model.matrix(litre~. -1, data = wine_test)
y.train <- wine_train$litre
y.test <- wine_test$litre
intsct <- intersect(colnames(x.train),colnames(x.test))
x.train <- x.train[,intsct]
x.test <- x.test[, intsct]

train_df <- cbind(y.train, x.train) %>% as.data.frame()
test_df <- cbind(y.test, x.test) %>% as.data.frame()

lm.fit <- glm(y.train ~ ., data = train_df)
sp.fit <- glm(y.train ~ . + ns(year,df = 2), data = train_df)

sp.pred <- predict(sp.fit, newdata = test_df)

RMSE <- sqrt(mean((sp.pred - test_df$y.test)^2)) # Bullshit

################################################################################
######################### Simple Regression Tree ###############################
################################################################################


## Trees
tree_wine <- tree(litre ~ .-region, data = wine_train)
# we have to cancel region, because factor predictors must have at most 32 levels
# and there are 96 levels of regions
summary(tree_wine) # vars used for construction: "price_segm", "dist", "price",
                   # "ms_segm", "lp", "country", "rprice_litre", "taste_segment", "period"
plot(tree_wine)
text(tree_wine)

# check whether pruning improves performance
tree_cv_wine <- cv.tree(tree_wine) # takes about 30 sec
plot(tree_cv_wine$size,tree_cv_wine$dev,type='b')
# we would choose the largest tree, but could alternatively prune it to a size of 6

tree_prune_wine <- prune.tree(tree_wine, best = 6)
plot(tree_prune_wine)
text(tree_prune_wine)


pred_tree <- predict(tree_wine, newdata = wine_test)
pred_pruned <- predict(tree_prune_wine, newdata = wine_test)

models[min(which(is.na(models$mod))),1] <- "rmse_tree"
models[min(which(is.na(models$rmse))), "rmse"] <- mean((wine_test$litre - pred_tree)^2) %>% sqrt()  # RMSE: 7005.4693

models[min(which(is.na(models$mod))),1] <- "rmse_tree_pruned"
models[min(which(is.na(models$rmse))), "rmse"] <- mean((wine_test$litre - pred_pruned)^2) %>% sqrt() # RMSE: 7661.7498

################################################################################
############################### Bagging ########################################
################################################################################


############################# BAGGING-LOOP #####################################


print("Now doing the Bagging-Loop")

trees <- c(50, 100, 150) # We should think about what sizes we want to try
rmse_BA <- c()
d <- c()
tic()
for(i in 1:length(trees)){
  x <- Sys.time()
  bag_wine <- randomForest(x = x.train, y = y.train, mtry = ncol(x.train)-1, importance = T, ntree = trees[i])
  pred_bag <- predict(bag_wine, newdata = x.test, n.trees = trees[i])
  rmse_BA[i] <- mean((y.test - pred_bag)^2) %>% sqrt()
  d[i] <- Sys.time() - x
  print(paste("~",round(mean(d)*(length(trees)-i)), " minutes remaining.", sep = ""))
}
toc()

# rmse_BA [1] 4410.918 4373.491 4384.591 4360.431
# => 250 Trees perform best but 50 trees seem okay

models[min(which(is.na(models$mod))),1] <- "rmse_bag"
models[min(which(is.na(models$rmse))), "rmse"] <- min(rmse_BA)




################################################################################
############################ Random Forest #####################################
################################################################################

## Trying different values for mtry and ntree
print("Now doing a Random Forest")
tic()
rf_wine <- randomForest(x = x.train, y = y.train, importance = T, ntree = 30) 
toc()
rf_wine
varImpPlot(rf_wine)
# % Var explained: 88.35 for mtry not set (236) & ntree = 25
# % Var explained: 86.62 for mtry = 50 & ntree = 25
# % Var explained: 83.31 for mtry = 25 & ntree = 25
# % Var explained: 68.66 for mtry = 12
# % Var explained: 64.07 for mtry = 10
# % Var explained: 56.45 for mtry = 8
pred_rf <- predict(rf_wine, newdata = x.test)
#plot(pred_rf, y.test)
rmse_rf <- mean((y.test - pred_rf)^2) %>% sqrt()

models[min(which(is.na(models$mod))),1] <- "rmse_rf"
models[min(which(is.na(models$rmse))), "rmse"] <- mean((y.test - pred_rf)^2) %>% sqrt()

# 4206.9744 for mtry not set (236) & ntree = 25
# 4627.1517 for mtry = 50 & ntree = 25 
# 5220.2826 for mtry = 25 & ntree = 25
# 7583.9097 for mtry = 12
# 8119.9565 for mtry = 10
# 8959.972 for mtry = 8

# Parallelization following https://stackoverflow.com/questions/14106010/parallel-execution-of-random-forest-in-r/15771458#15771458
# Implementation of Answer (combined with https://privefl.github.io/blog/a-guide-to-parallelism-in-r/)

#### trying and comparing different values for mtry ###



# m <- c(1,seq(25,225,50), 236)
# rmse_rf_m <- numeric(length(m))
# cl <- parallel::makeCluster(7)
# for( i in 1:length(m)){
#   doParallel::registerDoParallel(cl)
#   set.seed(123)
#   rf_wine <- randomForest(x = x.train, y = y.train, importance = T, ntree = 25) # mtry = 12
#   parallel::stopCluster(cl)
#   rf_wine
#   pred_rf <- predict(rf_wine, newdata = x.test)
#   #plot(pred_rf, y.test)
#   rmse_rf_m[i] <- mean((y.test - pred_rf)^2) %>% sqrt()
#   rmse_rf_m[i]
# }

############################# RANDOM FOREST-LOOP #####################################


print("Now doing the Random Forest-Loop")

m <- seq(10,100,10)
trees <- seq(25,125,25)
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

models[min(which(is.na(models$mod))),1] <- "rmse_RF"
models[min(which(is.na(models$rmse))), "rmse"] <- min(rmse_RF)







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

save(file = "02_analysis/boost_grid.rda", grid)

scatter3D(x = grid$Var1, y = grid$Var2, z = grid$RMSE, xlab = "Shrinkage", ylab = "depth",
          zlab = "RMSE", ticktype = "detailed", pch = 16, cex = 1.25, type = "h", bty = "b2")

print("This is just a single Boosting. May be deleted?")
tic()
boost_wine <- gbm.fit(y.train, x = x.train, distribution = "gaussian",
                  n.trees = 100, interaction.depth = 10, shrinkage = 0.05)    # try different depths and shrinkage
toc()
pred_boost <- predict(boost_wine, newdata = x.test, n.trees = 100)
rmse_boost <- mean((y.test - pred_boost)^2) %>% sqrt() 

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

models[min(which(is.na(models$mod))),1] <- "rmse_boost"
models[min(which(is.na(models$rmse))), "rmse"] <- mean((y.test - pred_boost)^2) %>% sqrt()
dir.create("02_analysis/cv/", recursive = T, showWarnings = F)
save(file = paste("02_analysis/cv/models_df_",unique_identifier,".rda", sep = ""), models)
