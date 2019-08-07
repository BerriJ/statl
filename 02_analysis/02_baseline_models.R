load("00_data/wine_preprocessed.rda")

set.seed(123)
train <- sample(nrow(wine), floor(0.75*nrow(wine)))

wine_train <- wine[train,]
wine_test <- wine[-(train),]


# Baseline Model: Mean

models <- data.frame(mod = c("Mean", "Basic_lm"), rmse = c(NA, NA))

# RMSE:

residuals <- na.omit(wine_train$litre)-mean(wine_train$litre, na.rm = T)
plot(residuals)

models[models$mod == "Mean","rmse"] <- residuals^2 %>% mean() %>% sqrt()
print(models)

# # Baseline Model: Linear Model with all* variables

colSums(is.na(wine_train)) %>% sort(decreasing = T) # We've got some problems

wine_subset <- wine_train[,which(colSums(is.na(wine)) < 1000)] %>% 
  drop_na() %>% dplyr::select(-name, - llitre)

lmod <- lm(litre ~., data = wine_subset)

preds <- predict(lmod, newdata = wine_test)

models[models$mod == "Basic_lm", "rmse"] <- (na.omit(wine_test$litre-preds))^2 %>% 
  mean() %>%
  sqrt()

models

