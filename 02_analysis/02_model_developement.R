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

# Set up a data frame for model comparison
models <- data.frame(mod = rep(NA, 10), rmse = rep(NA, 10))

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

x.train <- model.matrix(litre~. -1, data = wine_train)
x.test <- model.matrix(litre~. -1, data = wine_test)
y.train <- wine_train$litre
y.test <- wine_test$litre
intsct <- intersect(colnames(x.train),colnames(x.test))
x.train <- x.train[,intsct]
x.test <- x.test[, intsct]

train_df <- cbind(y.train, x.train) %>% as.data.frame()
test_df <- cbind(y.test, x.test) %>% as.data.frame()

lmod <- lm(y.train~.,train_df)

preds <- predict(lmod, newdata = test_df)

models[min(which(is.na(models$mod))),1] <- "linear_model"
models[min(which(is.na(models$rmse))), "rmse"] <- (y.test-preds)^2 %>% 
  mean() %>%
  sqrt()

models

################################################################################
################## Stepwise Forward / Backward Selection #######################
################################################################################

regfit_backward <- regsubsets(litre ~.-region -taste_segment -segm, data = wine_train,
                     method = "backward", nvmax = 50)

reg_sum_backward <- summary(regfit_backward)
id <- reg_sum_backward$rss %>% which.min()
coefficients_backward <- coef(regfit_backward, id = id)


regfit_forward <- regsubsets(litre ~. -region -taste_segment -segm, data = wine_train,
                     method = "forward", nvmax = 50)

reg_sum_forward <- summary(regfit_forward)
id <- reg_sum_forward$rss %>% which.min()
coefficients_forward <- coef(regfit_forward, id = id)

intersect(names(coefficients_backward), names(coefficients_forward))

reg_sum_forward$rss %>% min()
reg_sum_backward$rss %>% min()

val.errors = rep(NA, regfit_forward$nvmax)

x.test = model.matrix(litre ~ . -region -taste_segment -segm, data = wine_test)

for (i in 1:(regfit_forward$nvmax-1)) {
  coefi = coef(regfit_forward, id = i)
  pred = x.test[, names(coefi)] %*% coefi
  val.errors[i] = mean((wine_test$litre - pred)^2)
}
plot(sqrt(val.errors))
points(sqrt(regfit_forward$rss[1:regfit_forward$nvmax]/dim(wine_train)[1]), col = "blue", pch = 19, type = "b")
legend("topright", legend = c("Training", "Validation"), col = c("blue", "black"),
       pch = 19)

################################################################################
################################## LASSO #######################################
################################################################################

# library(glmnet) # Lasso / Ridge
# library(reshape2)

x.train <- model.matrix(litre ~ ., data = wine_train)
y.train <- wine_train$litre
x.test <- model.matrix(litre ~ ., data = wine_test)
y.test <- wine_test$litre
cv.out <- cv.glmnet(x = x.train, y = y.train, alpha = 1)
out <- glmnet(x = x.train, y = y.train, alpha = 1)
plot(cv.out)
bestlam <- cv.out$lambda.min
lasso.mod <- glmnet(x = x.train, y = y.train, alpha = 1, lambda = bestlam)
plotmo::plot_glmnet(out)
# prediction
pred <- predict(lasso.mod, x.test, s = bestlam)
rmse_lasso <- mean((y.test - pred)^2) %>% sqrt()

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

RMSE <- sqrt(mean((pred - test_df$y.test)^2))

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
rmse_tree <- mean((wine_test$litre - pred_tree)^2) %>% sqrt()  # RMSE: 7005.4693
rmse_tree_pruned <- mean((wine_test$litre - pred_pruned)^2) %>% sqrt() # RMSE: 7661.7498


## Bagging
set.seed(123)
bag_wine <- randomForest(x = x.train, y = y.train, mtry = ncol(x.train)-1, importance = T, ntree = 25)
bag_wine
pred_bag <- predict(bag_wine, newdata = x.test)
plot(pred_bag, y.test)
rmse_bag <- mean((y.test - pred_bag)^2) %>% sqrt()

# Bagging took about 2900 secs (~48 min)
# % Var explained: 88.47
# RMSE: 4144.7812




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
# 4206.9744 for mtry not set (236) & ntree = 25
# 4627.1517 for mtry = 50 & ntree = 25 
# 5220.2826 for mtry = 25 & ntree = 25
# 7583.9097 for mtry = 12
# 8119.9565 for mtry = 10
# 8959.972 for mtry = 8



# Parallelization following https://stackoverflow.com/questions/14106010/parallel-execution-of-random-forest-in-r/15771458#15771458
# Implementation of Answer (combined with https://privefl.github.io/blog/a-guide-to-parallelism-in-r/)



#### trying and comparing different values for mtry ###

m <- c(1,seq(25,225,50), 236)
rmse_rf_m <- numeric(length(m))
cl <- parallel::makeCluster(2)
for( i in 1:length(m)){
  doParallel::registerDoParallel(cl)
  set.seed(123)
  rf_wine <- randomForest(x = x.train, y = y.train, importance = T, ntree = 25) # mtry = 12
  parallel::stopCluster(cl)
  rf_wine
  pred_rf <- predict(rf_wine, newdata = x.test)
  #plot(pred_rf, y.test)
  rmse_rf_m[i] <- mean((y.test - pred_rf)^2) %>% sqrt()
  rmse_rf_m[i]
}



