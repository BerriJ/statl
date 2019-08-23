rm(list = ls())
source("02_analysis/00_packages.R")
load("00_data/wine_preprocessed.rda")
# Remove variables with average na >= 50%
wine <- wine %>% dplyr::select_if(.predicate = function(x) mean(is.na(x)) < 0.50) %>%
  # Only keep complete cases
  drop_na() %>%
  # Drop llitre because we are using litre
  dplyr::select(-llitre) %>%
  # Remove unused levels from factor variables
  droplevels()
x.train <- model.matrix(litre~., data = wine)
y.train <- wine$litre

colnames(x.train) <- colnames(x.train) %>% iconv(to='ASCII', sub='') %>% abbreviate(minlength=15)

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
tic()
rf <- randomForest(x = x.train, y = y.train, mtry = 100, ntree = 25, 
                   importance = T, keep.forest = T)
toc()
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

# ggsave("00_data/output_paper/11_var_imp_random_forest_bp.pdf", var_imp_random_forest_bp,
#        width = 7, height = 3)

# Partial Dependence Plots:

imp_df

par(mfrow = c(2,5))

test <- partialPlot(rf, pred.data = x.train, x.var = imp_df$names[1], plot = FALSE)
names(test)


pd_list <- list()

for(i in 1:20){
  pd_list[[i]] <- partialPlot(rf, pred.data = x.train, x.var = imp_df$names[i]) %>% 
    as.data.frame() %>%
    round(2)
  colnames(pd_list[[i]]) <- c(LETTERS[i],"y")
  cat(i)
}

pd_df <- purrr::reduce(pd_list[1:10], .f = full_join) %>% round(2)

pd_df <- apply(pd_df[,-2], 2, function(x) x/max(x, na.rm = T)) %>% data.frame(y = pd_df[,2], .) %>% 
  gather(key = var, value = x, -y) %>% drop_na()

# Facet Labels
pd_df.labs <- imp_df$names[1:10]
names(pd_df.labs) <- LETTERS[1:10]

cols <- replicate("#0080db", n = 363)
cols[pd_df$var == "F" | pd_df$var == "I" |pd_df$var == "J"] <- "#ff6633"

ggplot(pd_df, aes(y = y/1000, x = x)) + 
  geom_line(col = cols, size = 1) +
  facet_wrap("var", scales = "free", nrow = 2, 
             labeller = labeller(var = pd_df.labs), strip.position = "bottom") + 
  scale_x_continuous(breaks = NULL) + 
  ylab("Litre (Thousands)") + 
  xlab(label = NULL)


# Boosting ####

boosting_model_final <- gbm.fit(y.train, x = x.train, distribution = "gaussian",
                      n.trees = 25, interaction.depth = 15,
                      shrinkage = 0.5, verbose = FALSE, bag.fraction = 1)

var_imp_boost <- summary(boosting_model_final) %>% arrange(desc(rel.inf))

var_imp_boosting_bp <- ggplot(var_imp_boost[1:20,], 
       aes(x = reorder(var, -rel.inf), rel.inf)) + 
  geom_bar(stat = 'identity', fill = "chocolate1") + theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1,
                                   vjust=1), legend.position="top") + 
  ylab("Relative Importance (Percent)") + 
  xlab("Variable") +
  scale_x_discrete(label=function(x) abbreviate(x, minlength=15))

# ggsave("00_data/output_paper/12_var_imp_boosting_bp.pdf", var_imp_boosting_bp,
#       width = 7, height = 3)

var_imp_vars <- var_imp_boost$var %>% as.character()

plot(boosting_model_final, i = var_imp_vars[1])
plot(boosting_model_final, i = var_imp_vars[2])
plot(boosting_model_final, i = var_imp_vars[3])
plot(boosting_model_final, i = var_imp_vars[4])
plot(boosting_model_final, i = var_imp_vars[5])
plot(boosting_model_final, i = var_imp_vars[6])
plot(boosting_model_final, i = var_imp_vars[7])
plot(boosting_model_final, i = var_imp_vars[8])
plot(boosting_model_final, i = var_imp_vars[9])
plot(boosting_model_final, i = var_imp_vars[10])
