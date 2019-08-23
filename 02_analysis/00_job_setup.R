# Load and prepare Data  ####
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
# Crossvalidation  ####
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

# load("00_data/cv_env.rda") # For reproducability

for(i in 1:5){
  
  # Create an identifier for every iteration
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

splines_plot <- ggplot(df_splines, aes(x = knots, y = RMSE, color = var)) +
  geom_smooth(se = F, span = 0.5) +
  theme_minimal() +
  xlab("Number of Knots") + ylab("RMSE in Litre") +
  scale_color_manual(values = c("grey20", terrain.colors(n = 6, rev = T)[2:6]),
                     labels = c("Average", paste("Fold", 1:5))) +
  labs(col = "Legend:") + geom_point()

ggsave(filename = "00_data/output_paper/08_splines.pdf", plot =  splines_plot, width = 7, height = 3)


### Single Tree ###

files <- dir(recursive = T, path = "02_analysis/cv/rpart")
df <- data.frame(RMSE_Tree = rep(NA,5), RMSE_Tree_Pruned = rep(NA,5))
for(i in seq_along(files)){
  load(file = paste("02_analysis/cv/rpart/", files[i], sep = ""))
  df$RMSE_Tree[i] <- rmse_tree
  df$RMSE_Tree_Pruned[i] <- rmse_tree_pruned
}

save(file = "00_data/output_paper/09_tree.rda", df)

### Summary Random Forest  ####

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
                      colorbar = list(title = "RMSE"),
                      colorscale = list(c(0, 1), c('25c900', 'bf0000')),
                      showscale = TRUE,
                      line = list(width = 0), size = 2)) %>% layout(
          title = "",
          scene = list(
            xaxis = list(title = "Vars per Split"),
            yaxis = list(title = "Trees"),
            zaxis = list(title = "RMSE",
                         range=c(4200,10000),
                         tick0 = 4500,
                         dtick = 1000),
            annotations = list(list(
              showarrow = T,
              z = min(rf_df$mean),
              y = rf_df$trees[which.min(rf_df$mean)],
              x = rf_df$mtry[which.min(rf_df$mean)],
              ay = -150,
              ax = 150,
              text = "Minimum",
              arrowcolor = "black",
              arrowsize = 1,
              arrowwidth = 1,
              arrowhead = 1,
              font = list(
                size = 14
              ))
          )))

rf_df[which.min(rf_df$mean),]

# orca(rf_plot, file = "00_data/output_paper/10_rf_plot.pdf")

### Summary Boosting ####

files <- dir(path = "02_analysis/cv/boosting")

df_boosting_list <- list()

for(i in 1:(length(files))){
  load(file = paste("02_analysis/cv/boosting/", files[i], sep = ""))
  df_boosting_list[[i]] <- results
  colnames(df_boosting_list[[i]]) <- c("lambda", "int.depth", "bag.frac",paste("fold_",i,"_nrees",1:25, sep = ""))
}

boosting_df <- purrr::reduce(df_boosting_list, .f = full_join)

for(i in 1:25){
  boosting_df <- boosting_df %>% dplyr::mutate(!!paste(i) := rowMeans(.[,seq(3+i,103+i, 25)]))
}

boosting_df <- boosting_df[, c(1:3, 129:153)] %>% gather(key = Trees, value = RMSE, "1":"25")
boosting_df$Trees <- as.numeric(boosting_df$Trees)

colorScale <- data.frame(z=c(0,0.16,0.16,0.32,0.32,0.49,0.49,0.67,0.67,0.84,0.84,1),
                         col=c("#b00000","#b00000","#ff0000","#ff0000","#ff9900","#ff9900", "#E6E600FF",
                               "#E6E600FF", "#63C600FF","#63C600FF","#00A600FF","#00A600FF"))
colorScale$col <- as.character(colorScale$col)
colorScale$col <- as.character(colorScale$col)
boosting_df_full <- boosting_df
boosting_df <- boosting_df[boosting_df$bag.frac == 1 & 
                             (boosting_df$Trees == 1 |
                                boosting_df$Trees == 5 | 
                                boosting_df$Trees == 10 | 
                                boosting_df$Trees == 15 | 
                                boosting_df$Trees == 20 | 
                                boosting_df$Trees == 25),]

plot_ly(x = boosting_df$lambda, y = boosting_df$int.depth, z = boosting_df$RMSE,
                   type="scatter3d",
                   mode = "markers",
                   symbol= 25,
                   marker = list(color = boosting_df$Trees,
                                 colorscale = colorScale,
                                 showscale = TRUE,
                                 symbol = 'circle',
                                 opacity = 0.75,
                                 colorbar = list(title = "Number of Trees",
                                                 tickvals = seq(1, 25, length = 13)[c(2,4,6,8,10,12)],
                                                 ticktext = c("1","5","10","15","20","25")),
                                 line = list(width = 0), size = 2)) %>% 
  layout(
                                   title = "",
                                   scene = list(
                                     xaxis = list(title = "Lambda",
                                                  dtick = 0.1),
                                     yaxis = list(title = "Depth"),
                                     zaxis = list(title = "RMSE",
                                                  range=c(4200,10000),
                                                  tick0 = 4500,
                                                  dtick = 1000),
                                     annotations = list(list(
                                       showarrow = T,
                                       z = min(boosting_df$RMSE),
                                       y = boosting_df$int.depth[which.min(boosting_df$RMSE)],
                                       x = boosting_df$lambda[which.min(boosting_df$RMSE)],
                                       ay = -100,
                                       ax = 100,
                                       text = "Minimum",
                                       arrowcolor = "black",
                                       arrowsize = 1,
                                       arrowwidth = 1,
                                       arrowhead = 1,
                                       font = list(
                                         size = 14
                                       )))
                                   ))

best.boosting <- boosting_df_full$RMSE %>% which.min()
boosting_df_full[best.boosting,]
boosting_df_full$RMSE[boosting_df_full$bag.frac == 0.5] %>% mean()
boosting_df_full$RMSE[boosting_df_full$bag.frac == 1] %>% mean()

# orca(boosting_plot, file = "00_data/output_paper/11_boosting_plot.pdf")

save(file = "00_data/output_paper/12_boosting.rda", boosting_df)

# Summary Bagging ####

files <- dir(path = "02_analysis/cv/bagging")

df_bagging_list <- list()

for(i in 1:(length(files))){
  load(file = paste("02_analysis/cv/bagging/", files[i], sep = ""))
  df_bagging_list[[i]] <- data.frame(trees,rmse_BA)
  colnames(df_bagging_list[[i]])[2] <- paste("RMSE_fold_",c(1:5,1:5)[i], sep = "")
}

bagging_df <- df_bagging_list %>% purrr::reduce(.f = full_join) %>%
  mutate(mean = rowMeans(.[,2:6])) %>%
  arrange(desc(mean))

save(file = "00_data/output_paper/13_bagging.rda", bagging_df)

bagging_df <- bagging_df %>% gather(key = var, value = RMSE, -trees)

bagging_df <- bagging_df %>% arrange(var)

bagging_smoothed <- ggplot(bagging_df, aes(x = trees, y = RMSE,color = var)) +
  geom_smooth(se = F, span = 0.5) +
  theme_minimal() +
  xlab("Number of Trees") + ylab("RMSE in Litre") +
  scale_color_manual(values = c("grey20", terrain.colors(n = 6, rev = T)[2:6]),
                     labels = c("Average", paste("Fold", 1:5))) +
  labs(col = "Legend:") + 
  geom_point()

ggsave(filename = "00_data/output_paper/14_bagging.pdf", plot =  bagging_smoothed, width = 7, height = 3)







# Package Citation

knitr::write_bib(x = c("base",
                "stargazer",
                "haven",
                "stringr",
                "tidyr",
                "dplyr",
                "glmnet",
                "class",
                "MASS",
                "plm",
                "leaps",
                "caret",
                "reshape2",
                "tree",
                "gbm",
                "plotmo",
                "pls",
                "splines",
                "tictoc",
                "plotly",
                "inspectdf",
                "rpart",
                "rpart.plot",
                "stargazer",
                "knitr",
                "rstudioapi"), file = "03_paper/R_packages_draft.bib")

# Dont forget to add Rstudio
RStudio.Version()$citation
