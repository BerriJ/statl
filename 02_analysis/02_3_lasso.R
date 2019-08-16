print("Loading Packages")
source("02_analysis/00_packages.R")
print("Packages loaded")

################################################################################
################################## LASSO #######################################
################################################################################
print("Start Lasso")

cv.mod <- cv.glmnet(x = x.train, y = y.train, alpha = 1)
mod <- glmnet(x = x.train, y = y.train, alpha = 1)
plot(cv.mod)
bestlam <- cv.mod$lambda.min
lasso.mod <- glmnet(x = x.train, y = y.train, alpha = 1, lambda = bestlam)
plotmo::plot_glmnet(mod)
# prediction
pred <- predict(lasso.mod, x.test, s = bestlam)
rmse_lasso <- mean((y.test - pred)^2) %>% sqrt()

############################## Transform Y #####################################

# Log Transformation
y.train_log <- log(wine_train$litre)
cv.out <- cv.glmnet(x = x.train, y = y.train_log, alpha = 1)
bestlam <- cv.out$lambda.min
lasso.mod_log <- glmnet(x = x.train, y = y.train_log, alpha = 1, lambda = bestlam)
pred <- predict(lasso.mod_log, x.test, s = bestlam) %>% exp()
rmse_lasso_log <- mean((y.test - pred)^2) %>% sqrt()

dir.create("02_analysis/cv/lasso/", recursive = T, showWarnings = F)
save(file = paste("02_analysis/cv/lasso/models_df_",unique_identifier,".rda", sep = ""),
     lasso.mod, mod, rmse_lasso, rmse_lasso_log)



# test <- lasso.mod$beta[,1]
# df <- data.frame(coef = names(test), beta = round(test))
# df$coef %in% names(notdummy)
# 
# # The following shows the notdummys that are included in lasso with their coefficients
# 
# df[df$coef %in% names(notdummy),] %>% arrange(beta)
