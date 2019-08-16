print("Loading Packages")
source("02_analysis/00_packages.R")
print("Packages loaded")

################################################################################
############################# Mean Regression ##################################
################################################################################
print("Start Mean Regression")
residuals <- wine_train$litre-mean(wine_train$litre, na.rm = T)
rmse_mean_reg <- residuals^2 %>% mean() %>% sqrt()

################################################################################
######################## Linear Model without selection ########################
################################################################################
print("Start Linear Model")

# We fit a full linear model which will most likely overfit
lmod <- lm(y.train~. -1,train_df)
preds <- predict(lmod, newdata = test_df)

rmse_lin_reg <- (y.test-preds)^2 %>% mean() %>% sqrt()

dir.create("02_analysis/cv/baseline/", recursive = T, showWarnings = F)
save(file = paste("02_analysis/cv/baseline/baseline",unique_identifier,".rda", sep = ""),
     rmse_mean_reg, rmse_lin_reg, lmod)