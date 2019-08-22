source("02_analysis/00_packages.R")
load("02_analysis/cv_env.rda")

# We should estimate all models, only load the data necessary (create new .rda 
# file only for that). At the bottom: delete everything uneccesary and save 
# final models AND data as .rda for submission

# Bagging ####
tic()
bagging_model <- randomForest(x = x.train, y = y.train, mtry = ncol(x.train), 
                              importance = T, ntree = 25, keep.forest = F)
toc()
# Save importance
imp_df_bagging <- data.frame(importance(bagging_model, scale = FALSE, type = 1)) %>% 
  mutate(names = rownames(.)) %>% 
  arrange(desc(X.IncMSE)) %>%
  top_n(20, X.IncMSE) %>%
  mutate(X.IncMSE = sqrt(X.IncMSE))

# Plot mean decreased accuracy
var_imp_random_forest_bp <- ggplot(imp_df_bagging, aes(x = reorder(names, -X.IncMSE),y = X.IncMSE)) +
  geom_bar(stat = 'identity', fill = "chocolate1") + theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,
                                   vjust=1)) +
  labs(x= "Variable",y= "Mean Increase of RMSE") +
  scale_x_discrete(label=function(x) abbreviate(x, minlength=15))

ggsave("00_data/output_paper/15_var_imp_bagging.pdf", var_imp_random_forest_bp,
       width = 7, height = 3)


# Random Forest ####

rf <- randomForest(x = x.train, y = y.train, mtry = 100, ntree = 25, 
                   importance = T, keep.forest = F)

# Save importance
imp_df <- data.frame(importance(rf, scale = FALSE, type = 1)) %>% 
  mutate(names = rownames(.)) %>% 
  arrange(desc(X.IncMSE)) %>%
  top_n(20, X.IncMSE) %>%
  mutate(X.IncMSE = sqrt(X.IncMSE))

# Plot mean decreased accuracy
var_imp_random_forest_bp <- ggplot(imp_df, aes(x = reorder(imp_df$names, -imp_df$X.IncMSE),y = imp_df$X.IncMSE)) +
  geom_bar(stat = 'identity', fill = "chocolate1") + theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,
                                   vjust=1)) +
  labs(x= "Variable",y= "Mean Increase of RMSE") +
  scale_x_discrete(label=function(x) abbreviate(x, minlength=15))

ggsave("00_data/output_paper/11_var_imp_random_forest_bp.pdf", var_imp_random_forest_bp,
       width = 7, height = 3)

# Boosting ####

boosting_model_final <- gbm.fit(y.train, x = x.train, distribution = "gaussian",
                      n.trees = 25, interaction.depth = 1,
                      shrinkage = 0.5, verbose = FALSE, bag.fraction = 1)

var_imp_boost <- summary(boosting_model_final) #%>% arrange(desc(rel.inf))

var_imp_boosting_bp <- ggplot(var_imp_boost[1:20,], 
       aes(x = reorder(var, -rel.inf), rel.inf)) + 
  geom_bar(stat = 'identity', fill = "chocolate1") + theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,
                                   vjust=1), legend.position="top") + 
  ylab("Relative Importance (Percent)") + 
  xlab("Variable") +
  scale_x_discrete(label=function(x) abbreviate(x, minlength=15))

ggsave("00_data/output_paper/12_var_imp_boosting_bp.pdf", var_imp_boosting_bp,
       width = 7, height = 3)



