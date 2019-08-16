print("Loading Packages")
source("02_analysis/00_packages.R")
print("Packages loaded")

################################################################################
################################# Splines ######################################
################################################################################

# Natural Spline

rmse_splines <- c()

for(i in 1:20){
  sp.fit <- glm(y.train ~ . -year - price -rprice_litre + ns(year,df = i) + ns(price,df = i) +
                ns(rprice_litre,df = i),
                data = train_df)
  sp.pred <- predict(sp.fit, newdata = test_df)
  rmse_splines[i] <- sqrt(mean((sp.pred - test_df$y.test)^2)) # Bullshit
  progress(i, max.value = 10, progress.bar = T)
}

dir.create("02_analysis/cv/splines/", recursive = T, showWarnings = F)
save(file = paste("02_analysis/cv/splines/splines_",unique_identifier,".rda", sep = ""),
     rmse_splines)
