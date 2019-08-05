load("00_data/wine_preprocessed.rda")

train <- sample(nrow(wine), floor(0.75*nrow(wine)))

wine_train <- wine[train,]
wine_test <- wine[-(train),]


# Baseline Model: Mean

models <- data.frame(mod = c("Mean", "Basic_lm"), rmse = c(NA, NA))

residuals <- wine_train$litre-mean(wine_train$litre, na.rm = T) %>% na.omit()
plot(residuals)

models[models$mod == "Mean","rmse"] <- (wine_test$litre-mean(wine$litre, na.rm = T))^2 %>% 
  na.omit() %>%
  sqrt() %>%
  mean()
print(models)

# # Baseline Model: Linear Model with all* variables

colSums(is.na(wine_train)) %>% sort(decreasing = T) # We've got some problems

wine_subset <- wine_train[,which(colSums(is.na(wine)) < 1000)] %>% 
  drop_na() %>% dplyr::select(-name, - llitre)

lmod <- lm(litre ~., data = wine_subset)

preds <- predict(lmod, newdata = wine_test)

models[models$mod == "Basic_lm", "rmse"] <- (na.omit(wine_test$litre-preds))^2 %>% 
  sqrt() %>%
  mean()

models

