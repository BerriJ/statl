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

# Regsubsets (Forward and Backward stepwise):

load("00_data/wine_preprocessed.rda")

wine <- wine %>% dplyr::select(-name, - llitre, -artikelid, -artikelnr)

wine <- wine[,which(colSums(is.na(wine)) < 1000)] %>% 
  drop_na()

glimpse(wine)

corrmat <- dplyr::select_if(wine, is.numeric) %>% cor()
set.seed(123)
train <- sample(nrow(wine), floor(0.75*nrow(wine)))

wine_train <- wine[train,]
wine_test <- wine[-(train),]

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

library(glmnet) # Lasso / Ridge
library(reshape2)

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
############################ TREES and FORESTS #################################
################################################################################


## Trees

library(tree)
library(randomForest)

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
rmse_tree <- mean((wine_test$litre - pred_tree)^2) %>% sqrt()
rmse_tree_pruned <- mean((wine_test$litre - pred_pruned)^2) %>% sqrt()


## Bagging
set.seed(123)
tic()
bag_wine <- randomForest(x = x.train, mtry = 10, y = y.train, importance = T) 
#took about 6288.55 sec elapsed for mtry = 10
toc()
bag_wine
pred_bag <- predict(bag_wine, newdata = x.test)
plot(pred_bag, y.test)
rmse_bag <- mean((y.test - pred_bag)^2) %>% sqrt() # 5605.3265 for mtry = 10


set.seed(123)
tic()
rf_wine <- randomForest(x = x.train, y = y.train, importance = T) # now whithout spec of mtry
toc()
rf_wine
pred_rf <- predict(rf_wine, newdata = x.test)
plot(pred_rf, y.test)
rmse_rf <- mean((y.test - pred_rf)^2) %>% sqrt() 
