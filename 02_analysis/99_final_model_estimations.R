load("02_analysis/cv_env.rda")

# We should estimate all models, only load the data necessary (create new .rda 
# file only for that). At the bottom: delete everything uneccesary and save 
# final models AND data as .rda for submission

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
  
