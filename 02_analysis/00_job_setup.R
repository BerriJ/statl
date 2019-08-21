# Crossvalidation  ####
#
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
  # rstudioapi::jobRunScript("02_analysis/02_4_pcr_pls.R",
  #                          workingDir = "../statl",
  #                          importEnv = T)
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


### Summary Baseline Models  ####

files <- dir(recursive = T, path = "02_analysis/cv/baseline")
df <- data.frame(RMSE_Lin = rep(NA,5), RMSE_mean = rep(NA,5))
for(i in seq_along(files)){
  load(file = paste("02_analysis/cv/baseline/", files[i], sep = ""))
  df$RMSE_Lin[i] <- rmse_lin_reg
  df$RMSE_mean[i] <- rmse_mean_reg
}

save(file = "00_data/output_paper/03_baseline.rda", df)

### Summary Lasso Model  ####

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

### Summary PCR/PLS  ####

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

save(file = "00_data/output_paper/07_pca_pls.rda", df)

### Summary Splines  ####

files <- dir(path = "02_analysis/cv/splines")

df <- data.frame(matrix(ncol = 20, nrow = 5))

for(i in 1:(length(files))){
  load(file = paste("02_analysis/cv/splines/", files[i], sep = ""))
  df[i,] <- rmse_splines
}

df_splines <- data.frame(knots = 1:20, t(df), average = colMeans(df)) %>%
  gather(key = var, value = RMSE, -knots)

splines_plot <- ggplot(df_try, aes(x = knots, y = RMSE)) +
  geom_line(aes(color = var), size = 1) +
  theme_minimal() +
  xlab("Knots") + ylab("RMSE in Litre") +
  scale_color_manual(values = c("grey20", "grey", "grey", "grey", "grey", "grey"),
                     labels = c("Average", paste("Fold", 1:5))) +
  labs(col = "Legend:")

ggsave(filename = "00_data/output_paper/08_splines.pdf", plot =  splines_plot, width = 7, height = 3)

### Random Forest  ####

files <- dir(path = "02_analysis/cv/rf")

df_rf_list <- list()

for(i in 1:(length(files))){
  load(file = paste("02_analysis/cv/rf/", files[i], sep = ""))
  df_rf_list[[i]] <- cbind(grid, rmse_RF)
  colnames(df_rf_list[[i]]) <- c("mtry", "trees", paste("rmse_fold_",i, sep = ""))
}

rf_df <- purrr::reduce(df_rf_list, .f = full_join) %>%
  mutate(mean = rowMeans(.[3:7])) %>%
  arrange(desc(mean))

library(plotly)

rf_plot <- plot_ly(x = rf_df$mtry, y = rf_df$trees, z = rf_df$mean,
        type="scatter3d",
        mode = "markers",
        marker = list(color = rf_df$mean,
                      colorscale = list(c(0, 1), c('25c900', 'bf0000')),
                      showscale = TRUE,
                      line = list(width = 0), size = 6)) %>% layout(
          title = "",
          scene = list(
            xaxis = list(title = "Vars per Split"),
            yaxis = list(title = "Trees"),
            zaxis = list(title = "RMSE")
          ))

# orca(rf_plot, file = "00_data/output_paper/10_rf_plot.png")

### Boosting ####

files <- dir(path = "02_analysis/cv/boosting")

df_boosting_list <- list()

for(i in 1:(length(files))){
  load(file = paste("02_analysis/cv/boosting/", files[i], sep = ""))
  df_boosting_list[[i]] <- grid
  colnames(df_boosting_list[[i]]) <- c("Lambda", "Depth", "Trees",paste("RMSE_fold_",i, sep = ""))
}

boosting_df <- purrr::reduce(df_boosting_list, .f = full_join) %>%
  mutate(mean = rowMeans(.[4:8])) %>%
  arrange(desc(mean))


colorScale <- data.frame(z=c(0,0.2,0.2,0.4,0.4,0.6,0.6,0.8,0.8,1),
                         col=c("#00A600FF","#00A600FF","#63C600FF","#63C600FF", "#E6E600FF",
                               "#E6E600FF", "#EAB64EFF","#EAB64EFF","#EEB99FFF","#EEB99FFF"))

colorScale$col <- as.character(colorScale$col)

plot_ly(x = boosting_df$Lambda, y = boosting_df$Depth, z = boosting_df$mean,
                   type="scatter3d",
                   mode = "markers",
                   symbol= 25,
                   marker = list(color = boosting_df$Trees,
                                 colorscale = colorScale,
                                 showscale = TRUE,
                                 symbol = 'circle',
                                 opacity = 1,
                                 colorbar = list(title = "Number of Trees",
                                                 tickvals = c(35,55,75,95,115),
                                                 ticktext = c("25","50","75","100","125")),
                                 line = list(width = 0), size = 6)) %>% layout(
                                   title = "",
                                   scene = list(
                                     xaxis = list(title = "Lambda"),
                                     yaxis = list(title = "Depth"),
                                     zaxis = list(title = "RMSE",
                                                  range=c(4200,10000),
                                                  tick0 = 4500,
                                                  dtick = 1000)
                                   ))

orca(rf_plot, file = "00_data/output_paper/11_boosting_plot.pdf")