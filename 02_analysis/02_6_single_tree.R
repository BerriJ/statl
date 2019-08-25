print("Loading Packages")
source("02_analysis/00_packages.R")
print("Packages loaded")

################################################################################
######################### Simple Regression Tree ###############################
################################################################################

tree_wine <- rpart(y.train ~., data = data.frame(train_df)) 
pred_tree <- predict(tree_wine, newdata = data.frame(test_df))
rmse_tree <- mean((wine_test$litre - pred_tree)^2) %>% sqrt()


dir.create("02_analysis/cv/tree_plots/", recursive = T, showWarnings = F)

# Open a pdf file
pdf(paste("02_analysis/cv/tree_plots/tree_plot_",unique_identifier,".pdf", sep = ""), width = 7, height = 4)
# 2. Create a plot
rpart.plot(tree_wine, roundint = F)
# Close the pdf file
dev.off()

# Open a pdf file
pdf(paste("02_analysis/cv/tree_plots/cv_prune_plot_",unique_identifier,".pdf", sep = ""), width = 7, height = 4)
# 2. Create a plot
plotcp(tree_wine)
# Close the pdf file
dev.off()

tree_pruned <- prune(tree_wine, cp = 0.088) # cp-curves clearly flatten out for values <0.088
pred_pruned <- predict(tree_pruned, newdata = data.frame(test_df))
rmse_tree_pruned <- mean((wine_test$litre - pred_pruned)^2) %>% sqrt()

# Open a pdf file
pdf(paste("02_analysis/cv/tree_plots/prune_plot_",unique_identifier,".pdf", sep = ""), width = 7, height = 4)
# 2. Create a plot
rpart.plot(tree_pruned, roundint = F)
# Close the pdf file
dev.off()

dir.create("02_analysis/cv/tree/", recursive = T, showWarnings = F)
save(file = paste("02_analysis/cv/tree/rpart_",unique_identifier,".rda", sep = ""),
     tree_wine, tree_pruned, rmse_tree, rmse_tree_pruned)
