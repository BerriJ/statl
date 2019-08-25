# Aggregation of the results ####

# Summary Baseline Models  ####

# Get the filenames
files <- dir(recursive = T, path = "02_analysis/cv/baseline")
#Create a df to gather results
df <- data.frame(RMSE_Lin = rep(NA,5), RMSE_mean = rep(NA,5))
for(i in seq_along(files)){
  # Load every fold
  load(file = paste("02_analysis/cv/baseline/", files[i], sep = ""))
  # save RMSE of linear model
  df$RMSE_Lin[i] <- rmse_lin_reg
  # Save RMSE of mean model
  df$RMSE_mean[i] <- rmse_mean_reg
}
df_base <- df
# Save data to use it in the paper
# save(file = "00_data/output_paper/03_baseline.rda", df)

# Summary Lasso Model  ####

# Get filenames
files <- dir(path = "02_analysis/cv/lasso")

# Set up model lists
bestlam_mod <- list()
lasso_flexlam <- list()

# Create df to save results
df <- data.frame(RMSE_Lasso = rep(NA,5), RMSE_lasso_log = rep(NA,5),
                 loglam = rep(NA,5), ncoef = rep(NA, 5), loglamlog = rep(NA,5))

# Read and aggregates the data
for(i in 1:(length(files))){
  load(file = paste("02_analysis/cv/lasso/", files[i], sep = ""))
  bestlam_mod[[i]] <- lasso.mod
  lasso_flexlam[[i]] <- mod
  df$RMSE_Lasso[i] <- rmse_lasso
  df$RMSE_lasso_log[i] <- rmse_lasso_log
  df$loglam[i] <- bestlam %>% log()
  df$loglamlog[i] <- bestlam_log
  df$ncoef[i] <- ((bestlam_mod[[i]]$beta %>% as.numeric()) != 0) %>% sum()
}

df_lasso <- df
df <- colMeans(df) %>% round(2)

# Save the results
save(file = "00_data/output_paper/05_lasso.rda", df)

### Summary PCR/PLS  ####

# load files
files <- dir(path = "02_analysis/cv/pcr_pls")

# setup df for results
df <- data.frame(RMSE_pcr = rep(NA,5), RMSE_pls = rep(NA,5),
                 ncomp_pls = rep(NA,5), ncomp_pcr = rep(NA,5))

for(i in 1:(length(files))){
  load(file = paste("02_analysis/cv/pcr_pls/", files[i], sep = ""))
  df$RMSE_pcr[i] <- rmse_pcr
  df$RMSE_pls[i] <- rmse_pls
  df$ncomp_pls[i] <- n_comp_pls
  df$ncomp_pcr[i] <- n_comp_pcr
}

df_pcr_pls <- df

# Save results
save(file = "00_data/output_paper/07_pca_pls.rda", df)

### Summary Splines  ####

# Get filenames
files <- dir(path = "02_analysis/cv/splines")

df <- data.frame(matrix(ncol = 20, nrow = 5))

# Load the data
for(i in 1:(length(files))){
  load(file = paste("02_analysis/cv/splines/", files[i], sep = ""))
  df[i,] <- rmse_splines
}

df_splines <- df

# Create a plot that depicts the dependency of RMSE and Knots
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

# Summary Single Tree ####

# Get filenames
files <- dir(recursive = T, path = "02_analysis/cv/tree")

# Create df for results
df <- data.frame(RMSE_Tree = rep(NA,5), RMSE_Tree_Pruned = rep(NA,5))

# Load the data
for(i in seq_along(files)){
  load(file = paste("02_analysis/cv/tree/", files[i], sep = ""))
  df$RMSE_Tree[i] <- rmse_tree
  df$RMSE_Tree_Pruned[i] <- rmse_tree_pruned
}

df_tree <- df

# Save the results
save(file = "00_data/output_paper/09_tree.rda", df)

# Summary Bagging ####

# Get filenames
files <- dir(path = "02_analysis/cv/bagging")

df_bagging_list <- list()

for(i in 1:(length(files))){
  load(file = paste("02_analysis/cv/bagging/", files[i], sep = ""))
  df_bagging_list[[i]] <- data.frame(trees,rmse_BA)
  colnames(df_bagging_list[[i]])[2] <- paste("RMSE_fold_",c(1:5,1:5)[i], sep = "")
}

# Reduce list into df, calculate mean and arrange descending
df_bagging <- df_bagging_list %>% purrr::reduce(.f = full_join) %>%
  mutate(mean = rowMeans(.[,2:6])) %>%
  arrange(desc(mean))

# Save the baggging results
save(file = "00_data/output_paper/13_bagging.rda", df_bagging)

# Create a plot that shows bagging RMSE's vs. number of trees
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

### Summary Random Forest  ####

# Get filenames
files <- dir(path = "02_analysis/cv/rf")
df_rf_list <- list()

# Load the data
for(i in 1:(length(files))){
  load(file = paste("02_analysis/cv/rf/", files[i], sep = ""))
  df_rf_list[[i]] <- cbind(grid, rmse_RF)
  colnames(df_rf_list[[i]]) <- c("mtry", "trees", paste("rmse_fold_",i, sep = ""))
}

df_rand_frst <- rf_df

# Produce a plot that shows RMSE vs. Hyperparameters
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

# Save the plot (this needs orca)
# orca(rf_plot, file = "00_data/output_paper/10_rf_plot.pdf")

### Summary Boosting ####

# Get filenames
files <- dir(path = "02_analysis/cv/boosting")

df_boosting_list <- list()
for(i in 1:(length(files))){
  load(file = paste("02_analysis/cv/boosting/", files[i], sep = ""))
  df_boosting_list[[i]] <- results
  colnames(df_boosting_list[[i]]) <- c("lambda", "int.depth", "bag.frac",paste("fold_",i,"_ntrees",1:25, sep = ""))
}

# Create a plot that depicts RMSE vs. Hyperparameters of Boosting
boosting_df <- purrr::reduce(df_boosting_list, .f = full_join)
df_boosting <- boosting_df %>% gather(key = "Fold_Trees", "RMSE", -lambda, -int.depth, -bag.frac)

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

# The plot should only present some number of trees, the whole seq would be too much
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

# Best model in terms of RMSE
best.boosting <- boosting_df_full$RMSE %>% which.min()

# Parameter values of the best model
boosting_df_full[best.boosting,]

# Bag.fraction 0.5 vs. bag.fraction 1
boosting_df_full$RMSE[boosting_df_full$bag.frac == 0.5] %>% mean()
boosting_df_full$RMSE[boosting_df_full$bag.frac == 1] %>% mean()

# Save the plot (this needs orca)
# orca(boosting_plot, file = "00_data/output_paper/11_boosting_plot.pdf")

# Save the boosting results
save(file = "00_data/output_paper/12_boosting.rda", boosting_df)

# Gather RMSEs from all Models into a single df:

Summary <- data.frame(matrix(ncol = 6, nrow = 50)) 
colnames(Summary) <- c("Model", paste("Fold", 1:5))

# Mean Regression
Summary$Model[1] <- "Mean Regression"
Summary[1,2:6] <- df_baseline$RMSE_mean
# Linear Regression
Summary$Model[2] <- "Linear Regression"
Summary[2,2:6] <- df_baseline$RMSE_Lin
# Lasso 
Summary$Model[3] <- "Lasso"
Summary[3,2:6] <- df_lasso$RMSE_Lasso
# Lasso Loglitre
Summary$Model[4] <- "Lasso Loglitre"
Summary[4,2:6] <- df_lasso$RMSE_lasso_log
# PCR
Summary$Model[5] <- "PCR"
Summary[5,2:6] <- df_pcr_pls$RMSE_pcr
# PLS
Summary$Model[6] <- "PLS"
Summary[6,2:6] <- df_pcr_pls$RMSE_pls
# Splines
Summary$Model[7] <- "Splines (1 Knot)"
Summary[7,2:6] <- df_splines$RMSE[df_splines$knots == 1][1:5]
Summary$Model[8] <- "Splines (20 Knots)"
Summary[8,2:6] <- df_splines$RMSE[df_splines$knots == 20][1:5]
# Trees
Summary$Model[9] <- "Single Tree"
Summary[9,2:6] <- df_tree$RMSE_Tree
Summary$Model[10] <- "Single Tree (Pruned)"
Summary[10,2:6] <- df_tree$RMSE_Tree_Pruned
# Bagging
Summary$Model[11] <- "Bagging (5 Trees)"
Summary[11,2:6] <- df_bagging[2,2:6]
Summary$Model[12] <- "Bagging (25 Trees)"
Summary[12,2:6] <- df_bagging[5,2:6]
# Random Forest
Summary$Model[13] <- "Random Forest (Best)"
Summary[13,2:6] <- df_rand_frst[which.min(df_rand_frst$mean),3:8]
# Boosting
Summary$Model[14] <- "Boosting (Best)"
Summary[14,2:6] <- df_boosting[
  (df_boosting$lambda == 0.5 & 
     df_boosting$int.depth == 15 & 
     df_boosting$bag.frac == 1 &
     (df_boosting$Fold_Trees == "fold_1_ntrees25" |
        df_boosting$Fold_Trees == "fold_2_ntrees25" |
        df_boosting$Fold_Trees == "fold_3_ntrees25" |
        df_boosting$Fold_Trees == "fold_4_ntrees25" |
        df_boosting$Fold_Trees == "fold_5_ntrees25")),
  "RMSE"
  ]

Summary <- Summary %>% drop_na()
Summary$Mean <- rowMeans(Summary[,2:6])

# Save the summary table
save(file = "00_data/output_paper/16_summary.rda", Summary)