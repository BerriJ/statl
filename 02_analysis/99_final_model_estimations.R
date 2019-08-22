source("02_analysis/00_packages.R")
load("02_analysis/cv_env.rda")

# We should estimate all models, only load the data necessary (create new .rda 
# file only for that). At the bottom: delete everything uneccesary and save 
# final models AND data as .rda for submission


### Random Forest ###

rf <- randomForest(x = x.train, y = y.train, mtry = 100, ntree = 25, importance = T, keep.forest = F)

# Save importance
imp_df <- data.frame(importance(rf, scale = FALSE, type = 1))

# Tidy up and sort the data frame
imp_df <- imp_df %>% 
  mutate(names = rownames(imp_df)) %>% 
  arrange(desc(imp_df$X.IncMSE))

# Plot mean decreased accuracy
imp_df  <- top_n(imp_df, 20, imp_df$X.IncMSE)  # only use top 20 importances
var_imp_random_forest_bp <- ggplot(imp_df, aes(x = reorder(imp_df$names, imp_df$X.IncMSE),y = imp_df$X.IncMSE)) +
  geom_bar(stat = 'identity', fill = "chocolate1") + theme_minimal() +
  labs(title = "Variable Importance, Random Forest",
       subtitle = "Random Forests (N = 25, m = 100)",
       x= "",
       y= "IncMSE")

ggsave("00_data/output_paper/11_var_imp_random_forest_bp.pdf", var_imp_random_forest_bp,
       width = 7, height = 3)



# Boosting ####

boosting_model_final <- gbm.fit(y.train, x = x.train, distribution = "gaussian",
                      n.trees = 25, interaction.depth = 1,
                      shrinkage = 0.5, verbose = FALSE, bag.fraction = 1)

var_imp_boost <- summary(boosting_model_final)
var_imp_boost <- var_imp_boost %>% arrange(desc(rel.inf))

var_imp_booting_bp <- ggplot(var_imp_boost[1:15,], 
       aes(x = reorder(var, -rel.inf), rel.inf)) + 
  geom_bar(stat = 'identity', fill = "chocolate1") + theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,
                                   vjust=1), legend.position="top") + 
  ylab("") + 
  xlab("Variable") +
  scale_x_discrete(label=function(x) abbreviate(x, minlength=15))

ggsave("00_data/output_paper/12_var_imp_boosting_bp.pdf", var_imp_booting_bp,
       width = 7, height = 3)
  
