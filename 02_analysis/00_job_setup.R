# rm(list = ls())
# load("00_data/wine_preprocessed.rda")
# # Remove variables with average na >= 50%
# wine <- wine %>% dplyr::select_if(.predicate = function(x) mean(is.na(x)) < 0.50) %>% 
#   # Only keep complete cases
#   drop_na() %>% 
#   # Drop llitre because we are using litre
#   dplyr::select(-llitre) %>%
#   # Remove unused levels from factor variables
#   droplevels()
# 
# # Create Training and Test Datasets
# index <- data.frame(index = 1:nrow(wine), obs = 1:nrow(wine))
# sets <- list()
# 
# for(i in 1:5){
#   sets[[i]] <- sample(na.omit(index$obs), size = (floor(nrow(wine)/5)), replace = F)
#   index[sets[[i]],2] <- NA 
# }
# 
# train_list <- list()
# test_list <- list()
# 
# for(i in 1:5){
#   train_list[[i]] <- wine[sets[setdiff(c(1,2,3,4,5), c(i))] %>% unlist(),]
#   test_list[[i]] <- wine[sets[i] %>% unlist(),]
# }
# 
# rm(i, index, sets)

load("02_analysis/cv_env.rda")

for(i in 1:5){
  # Create an Identifier for every iteration
  unique_identifier <- Sys.time() %>% as.character(format = "%Y%m%d_%H%M")
  unique_identifier <- paste(unique_identifier,i, sep = "_")
  
  # Assign training and test data
  wine_train <- train_list[[i]]
  wine_test <- test_list[[i]]
  x.train <- model.matrix(litre~., data = wine_train)
  x.test <- model.matrix(litre~., data = wine_test)
  y.train <- wine_train$litre
  y.test <- wine_test$litre
  intsct <- intersect(colnames(x.train),colnames(x.test))
  x.train <- x.train[,intsct]
  x.test <- x.test[, intsct]
  train_df <- cbind(y.train, x.train) %>% as.data.frame()
  test_df <- cbind(y.test, x.test) %>% as.data.frame()
  
  # Run the models
  
  # # Mean Regression and Linear Regression 
  # rstudioapi::jobRunScript("02_analysis/02_2_mean_and_lin_reg.R",
  #                          workingDir = "../statl",
  #                          importEnv = T)
  # 
  # Lasso
  # rstudioapi::jobRunScript("02_analysis/02_3_lasso.R",
  #                          workingDir = "../statl",
  #                          importEnv = T)
  #
  # # PCR and PLS 
  rstudioapi::jobRunScript("02_analysis/02_4_pcr_pls.R",
                           workingDir = "../statl",
                           importEnv = T)
  # 
  # Splines
  # rstudioapi::jobRunScript("02_analysis/02_5_splines.R",
  #                          workingDir = "../statl",
  #                          importEnv = T)
  # 
  # Single Tree
  # rstudioapi::jobRunScript("02_analysis/02_6_single_tree.R",
  #                          workingDir = "../statl",
  #                          importEnv = T)
  # 
  # Bagging
  # rstudioapi::jobRunScript("02_analysis/02_1_Bagging.R",
  #                          workingDir = "../statl",
  #                          importEnv = T)
  # 
  # # Random Forest
  # rstudioapi::jobRunScript("02_analysis/02_7_random_forest.R",
  #                          workingDir = "../statl",
  #                          importEnv = T)
  # 
  # Boosting
  # rstudioapi::jobRunScript("02_analysis/02_7_boosting.R",
  #                          workingDir = "../statl",
  #                          importEnv = T)
}


### Summary Baseline Models

files <- dir(recursive = T, path = "02_analysis/cv/baseline")
df <- data.frame(RMSE_Lin = rep(NA,5), RMSE_mean = rep(NA,5))
for(i in seq_along(files)){
  load(file = paste("02_analysis/cv/baseline/", files[i], sep = ""))
  df$RMSE_Lin[i] <- rmse_lin_reg
  df$RMSE_mean[i] <- rmse_mean_reg
}

save(file = "00_data/output_paper/03_baseline.rda", df)

### Summary Lasso Model

files <- dir(path = "02_analysis/cv/lasso")

bestlam_mod <- list()
lasso_flexlam <- list()
df <- data.frame(RMSE_Lasso = rep(NA,5), RMSE_lasso_log = rep(NA,5),
                 loglam = rep(NA,5), ncoef = rep(NA, 5))

for(i in 1:(length(files)-1)){
  load(file = paste("02_analysis/cv/lasso/", files[i], sep = ""))
  bestlam_mod[[i]] <- lasso.mod
  lasso_flexlam[[i]] <- mod
  df$RMSE_Lasso[i] <- rmse_lasso
  df$RMSE_lasso_log[i] <- rmse_lasso_log
  df$loglam[i] <- bestlam %>% log()
  df$ncoef[i] <- ((bestlam_mod[[i]]$beta %>% as.numeric()) != 0) %>% sum()
}

names <- rownames(bestlam_mod[[5]]$beta)
beta <- bestlam_mod[[5]]$beta %>% as.numeric()
coef <- data.frame(beta, names)
coef <- arrange(coef, desc(abs(beta)))
tail(coef)

df <- colMeans(df) %>% round(2)

save(file = "00_data/output_paper/05_lasso.rda", df)

### Summary PCR/PLS

files <- dir(path = "02_analysis/cv/pcr_pls")

bestlam_mod <- list()
lasso_flexlam <- list()
df <- data.frame(RMSE_pcr = rep(NA,5), RMSE_pls = rep(NA,5),
                 ncomp_pls = rep(NA,5), ncomp_pcr = rep(NA,5))

for(i in 1:(length(files))){
  load(file = paste("02_analysis/cv/pcr_pls/", files[i], sep = ""))
  df$RMSE_pcr[i] <- rmse_pcr
  df$RMSE_pls[i] <- rmse_pls
  df$ncomp_pls[i] <- n_comp_pls
  df$ncomp_pcr[i] <- n_comp_pcr
}

colMeans(df)
