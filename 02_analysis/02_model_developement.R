rm(list = ls())
# What we tried in the Tutorial

# Non_linear Transformation
# Subset Selection
# Leave one out CV
# Lasso
# PLS PCR
# Natural Splines and Bsplines
# GAMS
# Random Forests

load("00_data/wine_preprocessed.rda")

# Remove variables with average na >= 50%
wine <- wine %>% dplyr::select_if(.predicate = function(x) mean(is.na(x)) < 0.50) %>% 
  # Only keep complete cases
  drop_na() %>% 
  # Drop llitre because we are using litre
  dplyr::select(-llitre) %>%
  # Remove unused levels from factor variables
  droplevels()

# Split intro Training (3/4) and Test (1/4)

set.seed(123)
train <- sample(nrow(wine), floor(0.75*nrow(wine)))
wine_train <- wine[train,]
wine_test <- wine[-(train),]
rm(train)

# Set up a data frame for model comparison
models <- data.frame(mod = rep(NA, 30), rmse = rep(NA, 30))

################################################################################
############################# Mean Regression ##################################
################################################################################

residuals <- wine_train$litre-mean(wine_train$litre, na.rm = T)
models[min(which(is.na(models$mod))),1] <- "mean_regression"
models[min(which(is.na(models$rmse))),2] <- residuals^2 %>% mean() %>% sqrt()

################################################################################
######################## Linear Model without selection ########################
################################################################################

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
################## Stepwise Forward / Backward Selection #######################
################################################################################

# => Hier noch auf die Matrix anpassen

# Übersicht über die Levels
wine_train[,sapply(wine_train, is.factor)] %>% drop_na() %>% sapply(levels)

form <- formula(litre ~ region + dist + taste_segment + price)

regfit_forward <- regsubsets(form, data = wine_train,
                     method = "forward", nvmax = 1000)
regfit_backward <- regsubsets(form, data = wine_train,
                     method = "backward", nvmax = 1000)

test.rmse_fwd <- c()
test.rmse_bwd <- c()
for (i in 1:(min(regfit_backward$nvmax, regfit_backward$nvmax)-1)) {
  coefi = coef(regfit_forward, id = i)
  pred = x.test[,names(coefi)] %*% coefi
  test.rmse_fwd[i] = sqrt(mean((wine_test$litre - pred)^2))
  
  coefi = coef(regfit_backward, id = i)
  pred = x.test[,names(coefi)] %*% coefi
  test.rmse_bwd[i] = sqrt(mean((wine_test$litre - pred)^2))
}

plot(test.rmse_bwd, ylim = c(5000, 13000), col = "black", pch = 19)
points(sqrt(regfit_forward$rss[1:regfit_forward$nvmax]/dim(wine_train)[1]), col = "red", pch = 19, type = "b")
points(sqrt(regfit_backward$rss[1:regfit_backward$nvmax]/dim(wine_train)[1]), col = "darkgreen", pch = 19, type = "b")
points(test.rmse_fwd, col = "deeppink", pch = 19, type = "b")

models[min(which(is.na(models$mod))),1] <- "regsubsets_bwd"
models[min(which(is.na(models$rmse))), "rmse"] <- test.rmse_bwd %>% min()

models[min(which(is.na(models$mod))),1] <- "regsubsets_fwd"
models[min(which(is.na(models$rmse))), "rmse"] <- test.rmse_fwd %>% min()

# legend("topright", legend = c("Training", "Validation"), col = c("grey", "black"),
#        pch = 19)

################################################################################
################################## LASSO #######################################
################################################################################

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
############################# ANFANG TODESSTREIFEN #############################
################################################################################

















































################################################################################
############################ ENDE TODESSTREIFEN ################################
################################################################################


















################################################################################
############################ TREES and FORESTS #################################
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


## Bagging
set.seed(123)
bag_wine <- randomForest(x = x.train, y = y.train, mtry = ncol(x.train)-1, importance = T, ntree = 25)
bag_wine
pred_bag <- predict(bag_wine, newdata = x.test)
plot(pred_bag, y.test)
# Bagging took about 2900 secs (~48 min)
# % Var explained: 88.47
# RMSE: 4144.7812

models[min(which(is.na(models$mod))),1] <- "rmse_bagging"
models[min(which(is.na(models$rmse))), "rmse"] <- mean((y.test - pred_bag)^2) %>% sqrt()


## Trying different values for mtry and ntree
cl <- parallel::makeCluster(2)
doParallel::registerDoParallel(cl)
set.seed(123)
tic()
rf_wine <- randomForest(x = x.train, y = y.train, importance = T, ntree = 30) 
toc()
parallel::stopCluster(cl)
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
# cl <- parallel::makeCluster(2)
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



#####################################################################################
############################# BOOSTING ##############################################
#####################################################################################


lam <- seq(0.1,1,0.1)
dep <- 1:10
rmse_BO <- matrix(NA, ncol = length(lam), nrow = length(dep))
tic()
for(i in 1:length(dep)){
  for(j in 1:length(lam)){
    set.seed(123)
     boost_wine <- gbm.fit(y.train, x = x.train, distribution = "gaussian",
                           n.trees = 25, interaction.depth = dep[i], shrinkage = lam[j])    # try different depths and shrinkage
    pred_boost <- predict(boost_wine, newdata = x.test, n.trees = 25)
    rmse_BO[i,j] <- mean((y.test - pred_boost)^2) %>% sqrt()

  }
}
toc()

which.min(rmse_BO)

# For ntree=15, depth = 1:10, lam =  0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1   MIN: [10,4]
#            [,1]     [,2]     [,3]     [,4]     [,5]     [,6]     [,7]     [,8]     [,9]    [,10]
#  [1,] 11032.223 9801.343 8950.790 8337.324 7920.161 7461.948 7278.442 7259.353 7142.722 7220.552
#  [2,]  8484.334 7358.563 6734.363 6463.314 6526.351 6390.371 6574.784 6484.854 6001.804 6180.264
#  [3,]  7699.387 6334.363 6123.238 5818.283 6001.830 5605.619 5867.651 5955.819 5936.540 5885.352
#  [4,]  7279.176 6126.020 5759.820 5749.023 5739.443 5603.377 5547.761 5574.719 5734.030 5750.826
#  [5,]  6969.465 5861.720 5502.507 5462.842 5360.748 5337.990 5495.928 5270.215 5478.045 5609.208
#  [6,]  6764.522 5616.277 5371.014 5218.554 5460.735 5195.884 5297.494 5230.387 5299.484 5426.557
#  [7,]  6640.888 5469.999 5106.896 5002.053 5082.263 5148.594 5254.421 5204.652 5274.297 5546.529
#  [8,]  6490.647 5271.098 5026.871 5000.594 4953.673 5047.807 5001.039 5030.516 5190.884 5219.936
#  [9,]  6422.084 5195.272 4897.252 4880.130 4848.327 4925.192 4856.342 4937.839 5243.325 5179.363
# [10,]  6263.339 5182.743 4861.993 4780.954 4870.337 4900.469 4885.299 5058.065 5104.825 5114.935

# For ntree=25, depth 1:5, lam: 0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1   MIN: [9,4]     7542.05 sec elapsed
#           [,1]     [,2]     [,3]     [,4]     [,5]     [,6]     [,7]     [,8]     [,9]    [,10]
# [1,] 10234.818 8804.907 8045.142 7546.062 7204.978 6951.729 6893.330 6811.662 6703.004 6906.592
# [2,]  7545.157 6789.241 6241.280 6058.543 6141.968 6082.078 6052.900 6182.192 5752.780 5883.212
# [3,]  6727.324 5876.307 5753.054 5558.193 5619.261 5451.181 5496.406 5635.080 5595.419 5766.356
# [4,]  6349.552 5731.793 5450.980 5456.640 5464.688 5284.496 5298.936 5424.647 5427.750 5402.750
# [5,]  6051.101 5464.025 5193.618 5274.992 5090.233 5036.871 5337.495 5093.611 5323.185 5566.715
# [6,]  5874.151 5224.869 5084.415 5026.881 5264.415 5029.280 5133.017 5141.318 5291.418 5518.447
# [7,]  5706.636 5079.472 4881.400 4818.261 4839.299 4993.756 5013.185 5109.016 5188.260 5315.370
# [8,]  5548.640 4909.541 4810.983 4745.685 4795.435 4926.473 4871.174 4874.713 5144.879 5140.846
# [9,]  5473.184 4837.015 4789.351 4673.868 4754.270 4742.169 4772.024 4878.421 5232.132 5141.139
# [10,]  5328.417 4812.176 4713.108 4701.066 4796.006 4787.254 4818.337 4913.354 5020.855 5065.716


# For ntree = 50 depth 1:5, lam: 0.1,0.2,0.3,0.4,0.5
         # [,1]     [,2]     [,3]     [,4]     [,5]
# [1,] 8856.711 7560.260 7002.461 6754.915 6532.789
# [2,] 6632.525 6063.778 5760.698 5564.106 5580.637
# [3,] 5997.924 5455.387 5311.677 5252.932 5239.971
# [4,] 5678.047 5274.558 5074.982 5123.897 5128.888
# [5,] 5431.540 5082.205 4945.562 5099.090 4996.868


# For ntree=100, depth = 1:5, lam =  0.1,0.2,0.3,0.4,0.5,0.6,0.7,0.8,0.9,1
# [,1]     [,2]     [,3]     [,4]     [,5]     [,6]     [,7]     [,8]     [,9]    [,10]
# [1,] 7646.386 6761.501 6395.823 6196.974 6038.908 6023.518 5981.763 6037.364 5890.068 5974.164
# [2,] 5989.766 5556.112 5423.310 5235.674 5308.945 5302.661 5348.472 5291.075 5360.873 5379.743
# [3,] 5499.057 5134.587 5071.350 5112.052 5019.168 5110.127 5125.755 5087.250 5224.853 5374.684
# [4,] 5251.322 5031.972 4860.348 4967.344 4981.307 4924.928 5048.075 5305.361 5249.443 5245.277
# [5,] 5059.465 4847.223 4853.835 4960.823 4863.758 4878.694 5026.886 5029.816 5453.761 5578.128


set.seed(123)
tic()
boost_wine <- gbm.fit(y.train, x = x.train, distribution = "gaussian",
                  n.trees = 100, interaction.depth = 35, shrinkage = 0.05)    # try different depths and shrinkage
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