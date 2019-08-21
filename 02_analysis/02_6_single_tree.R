print("Loading Packages")
source("02_analysis/00_packages.R")
print("Packages loaded")

################################################################################
######################### Simple Regression Tree ###############################
################################################################################

# tree_wine <- tree(y.train ~., data = data.frame(train_df))
# # we have to cancel region, because factor predictors must have at most 32 levels
# # and there are 96 levels of regions
# summary(tree_wine) # vars used for construction: "price_segm", "dist", "price",
# # "ms_segm", "lp", "country", "rprice_litre", "taste_segment", "period"
# plot(tree_wine)
# text(tree_wine)
# 
# # check whether pruning improves performance
# tree_cv_wine <- cv.tree(tree_wine) # takes about 30 sec
# plot(tree_cv_wine$size,tree_cv_wine$dev,type='b')
# # we would choose the largest tree, but could alternatively prune it to a size of 6
# 
# tree_prune_wine <- prune.tree(tree_wine, best = 6)
# plot(tree_prune_wine)
# text(tree_prune_wine)
# 
# pred_tree <- predict(tree_wine, newdata = data.frame(test_df))
# pred_pruned <- predict(tree_prune_wine, newdata = data.frame(test_df))
# 
# rmse_tree <- mean((wine_test$litre - pred_tree)^2) %>% sqrt()
# rmse_tree_pruned <- mean((wine_test$litre - pred_pruned)^2) %>% sqrt()

tree_wine <- rpart(y.train ~., data = data.frame(train_df)) 
pred_tree <- predict(tree_wine, newdata = data.frame(test_df))
rmse_tree <- mean((wine_test$litre - pred_tree)^2) %>% sqrt()

# Open a pdf file
pdf(paste("02_analysis/cv/rpart_plots/tree_plot_",unique_identifier,".pdf", sep = ""), width = 7, height = 4)
# 2. Create a plot
rpart.plot(tree_wine, roundint = F)
# Close the pdf file
dev.off()

# Open a pdf file
pdf(paste("02_analysis/cv/rpart_plots/cv_prune_plot_",unique_identifier,".pdf", sep = ""), width = 7, height = 4)
# 2. Create a plot
plotcp(tree_wine)
# Close the pdf file
dev.off()


tree_pruned <- prune(tree_wine, cp = 0.088) # cp-curves clearly flatten out for values <0.088
pred_pruned <- predict(tree_pruned, newdata = data.frame(test_df))
rmse_tree_pruned <- mean((wine_test$litre - pred_pruned)^2) %>% sqrt()


# Open a pdf file
pdf(paste("02_analysis/cv/rpart_plots/prune_plot_",unique_identifier,".pdf", sep = ""), width = 7, height = 4)
# 2. Create a plot
rpart.plot(tree_pruned, roundint = F)
# Close the pdf file
dev.off()

dir.create("02_analysis/cv/rpart/", recursive = T, showWarnings = F)
save(file = paste("02_analysis/cv/rpart/rpart_",unique_identifier,".rda", sep = ""),
     tree_wine, tree_pruned, rmse_tree, rmse_tree_pruned)
