print("Loading Packages")
source("02_analysis/00_packages.R")
print("Packages loaded")

################################################################################
################################## LASSO #######################################
################################################################################
print("Start Lasso")

colnames(x.train) <- colnames(x.train) %>% iconv(to='ASCII', sub='') %>% abbreviate(minlength=15)
colnames(x.test) <- colnames(x.test) %>% iconv(to='ASCII', sub='') %>% abbreviate(minlength=15)

# Estimation to get the lambda
cv.mod <- cv.glmnet(x = x.train, y = y.train, alpha = 1)
mod <- glmnet(x = x.train, y = y.train, alpha = 1)
plot(cv.mod)
bestlam <- cv.mod$lambda.min

# Estimate best model according to the cv
lasso.mod <- glmnet(x = x.train, y = y.train, alpha = 1, lambda = bestlam)
# plotmo::plot_glmnet(mod)

# predict and compute RMSE
pred <- predict(lasso.mod, x.test, s = bestlam)
rmse_lasso <- mean((y.test - pred)^2) %>% sqrt()

# Save coefficients plot
dir.create("02_analysis/cv/lasso_plots/", recursive = T, showWarnings = F)
# Open a pdf file
pdf(paste("02_analysis/cv/lasso_plots/lasso_var_imp_",
          unique_identifier,".pdf", sep = ""), width = 7, height = 4) 
# 2. Create a plot
plotmo::plot_glmnet(mod)
# Close the pdf file
dev.off() 

################################################################################
########################## Log Transformation ##################################
################################################################################

# Transform
y.train_log <- log(wine_train$litre)

# Estimate
cv.out <- cv.glmnet(x = x.train, y = y.train_log, alpha = 1)
bestlam_log <- cv.out$lambda.min

lasso.mod_log <- glmnet(x = x.train, y = y.train_log, alpha = 1, lambda = bestlam_log)

# Predict
pred <- predict(lasso.mod_log, x.test, s = bestlam_log) %>% exp()
rmse_lasso_log <- mean((y.test - pred)^2) %>% sqrt()

# Save the results
dir.create("02_analysis/cv/lasso/", recursive = T, showWarnings = F)
save(file = paste("02_analysis/cv/lasso/models_df_",unique_identifier,".rda", sep = ""),
     lasso.mod, mod, rmse_lasso, rmse_lasso_log, bestlam, bestlam_log)


