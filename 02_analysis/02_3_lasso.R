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

# Open a pdf file
pdf(paste("02_analysis/cv/lasso/lasso_var_imp_",unique_identifier,".pdf", sep = ""), width = 7, height = 4) 
# 2. Create a plot
plotmo::plot_glmnet(mod, xlim = c(9,-10))
# Close the pdf file
dev.off() 

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
     lasso.mod, mod, rmse_lasso, rmse_lasso_log, bestlam)


