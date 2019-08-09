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

id_bwd <- reg_sum_backward$rss %>% which.min()
id_fwd <- reg_sum_forward$rss %>% which.min()

intersect(names(coefficients_backward), names(coefficients_forward))

x.test = model.matrix(form, data = wine_test)

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

test.rmse_bckwd %>% min()
test.rmse_fwd %>% min()

# legend("topright", legend = c("Training", "Validation"), col = c("grey", "black"),
#        pch = 19)

################################################################################
################################## LASSO #######################################
################################################################################

x.train <- model.matrix(litre~., data = wine_train)
x.test <- model.matrix(litre~., data = wine_test)
y.train <- wine_train$litre
y.test <- wine_test$litre
intsct <- intersect(colnames(x.train),colnames(x.test))
x.train <- x.train[,intsct]
x.test <- x.test[, intsct]

# library(glmnet) # Lasso / Ridge
# library(reshape2)

cv.out <- cv.glmnet(x = x.train, y = y.train, alpha = 1)
out <- glmnet(x = x.train, y = y.train, alpha = 1)
plot(cv.out)
bestlam <- cv.out$lambda.min
lasso.mod <- glmnet(x = x.train, y = y.train, alpha = 1, lambda = bestlam)
plotmo::plot_glmnet(out)
# prediction
pred <- predict(lasso.mod, x.test, s = bestlam)
coef(lasso.mod)
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
rmse_tree <- mean((wine_test$litre - pred_tree)^2) %>% sqrt()  # RMSE: 7380.601
rmse_tree_pruned <- mean((wine_test$litre - pred_pruned)^2) %>% sqrt() # RMSE: 7972.512


## Bagging
set.seed(123)
bag_wine <- randomForest(x = x.train, mtry = 10, y = y.train, importance = T)
#took about 6288.55 sec elapsed for mtry = 10
bag_wine
pred_bag <- predict(bag_wine, newdata = x.test)
plot(pred_bag, y.test)
rmse_bag <- mean((y.test - pred_bag)^2) %>% sqrt() # 5605.3265 for mtry = 10


# set.seed(123)
# rf_wine <- randomForest(x = x.train, y = y.train, importance = T) # now whithout spec of mtry
# rf_wine
# pred_rf <- predict(rf_wine, newdata = x.test)
# plot(pred_rf, y.test)
# rmse_rf <- mean((y.test - pred_rf)^2) %>% sqrt()


# Parallelization following https://stackoverflow.com/questions/14106010/parallel-execution-of-random-forest-in-r/15771458#15771458
# Implementation of Answer (combined with https://privefl.github.io/blog/a-guide-to-parallelism-in-r/)


cl <- parallel::makeCluster(2)
doParallel::registerDoParallel(cl)
set.seed(123)
rf_par <- foreach(mtry = 1:3, .combine = randomForest::combine,          #took about 998.27 sec
              .multicombine = TRUE, .packages = 'randomForest') %dopar% {
                randomForest(x.train, y.train, mtry = mtry)
              }
parallel::stopCluster(cl)

pred_rf_par <- predict(rf_par, newdata = x.test)
plot(pred_rf_par, y.test)
rmse_rf_par <- mean((y.test - pred_rf_par)^2) %>% sqrt() # RMSE : 10113.45
