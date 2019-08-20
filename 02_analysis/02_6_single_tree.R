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

rpa <- rpart(y.train ~., data = data.frame(train_df)) 

# Open a pdf file
pdf(paste("02_analysis/cv/rpart/tree_plot_",unique_identifier,".pdf", sep = ""), width = 7, height = 4) 
# 2. Create a plot
rpart.plot(rpa, roundint = F)
# Close the pdf file
dev.off() 

dir.create("02_analysis/cv/rpart/", recursive = T, showWarnings = F)
save(file = paste("02_analysis/cv/rpart/rpart_",unique_identifier,".rda", sep = ""),
     rpa)
