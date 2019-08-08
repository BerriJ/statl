load("00_data/wine_preprocessed.rda")

# Remove variables with average na >= 50%
wine <- wine %>% dplyr::select_if(.predicate = function(x) mean(is.na(x)) < 0.50) %>% 
  # Only keep complete cases
  drop_na() %>% 
  # Drop llitre because we are using litre
  dplyr::select(-llitre) %>%
  # Remove unused levels from factor variables
  droplevels()

# Split intro Training (3/4) and Test (1/4)

set.seed(123)
train <- sample(nrow(wine), floor(0.75*nrow(wine)))
wine_train <- wine[train,]
wine_test <- wine[-(train),]

# Set up a data frame for model comparison
models <- data.frame(mod = rep(NA, 10), rmse = rep(NA, 10))

# Mean Regression:
residuals <- wine_train$litre-mean(wine_train$litre, na.rm = T)
models[min(which(is.na(models$mod))),1] <- "mean_regression"
models[min(which(is.na(models$rmse))),2] <- residuals^2 %>% mean() %>% sqrt()

# # Baseline Model: Linear Model with all variables

x.train <- model.matrix(litre~. -1, data = wine_train)
x.test <- model.matrix(litre~. -1, data = wine_test)
y.train <- wine_train$litre
y.test <- wine_test$litre
intsct <- intersect(colnames(x.train),colnames(x.test))
x.train <- x.train[,intsct]
x.test <- x.test[, intsct]

train_df <- cbind(y.train, x.train) %>% as.data.frame()
test_df <- cbind(y.test, x.test) %>% as.data.frame()

lmod <- lm(y.train~.,train_df)

preds <- predict(lmod, newdata = test_df)

models[min(which(is.na(models$mod))),1] <- "linear_model"
models[min(which(is.na(models$rmse))), "rmse"] <- (y.test-preds)^2 %>% 
  mean() %>%
  sqrt()

models

