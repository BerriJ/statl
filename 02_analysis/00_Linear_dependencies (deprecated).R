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

x.train <- model.matrix(litre~. -1, data = wine_train)
x.test <- model.matrix(litre~. -1, data = wine_test)
y.train <- wine_train$litre
y.test <- wine_test$litre
intsct <- intersect(colnames(x.train),colnames(x.test))
x.train <- x.train[,intsct]
x.test <- x.test[, intsct]

train_df <- cbind(y.train, x.train) %>% as.data.frame()
test_df <- cbind(y.test, x.test) %>% as.data.frame()

################################################################################
################################# PCA ##########################################
################################################################################

# https://www.datacamp.com/community/tutorials/pca-analysis-r

MM <- model.matrix(litre ~. -1, data = wine)
wine.pc <- prcomp(MM, center = T, scale. = T)
wine.pc.sm <- summary(wine.pc)
wine.pc.sm$importance

#remotes::install_github("vqv/ggbiplot")
ggbiplot::ggbiplot(wine.pc)
ggbiplot::ggbiplot(wine.pc,ellipse = T, groups = wine$taste_segment)
ggbiplot::ggbiplot(wine.pc, ellipse = T, groups = wine$segm)
ggbiplot::ggbiplot(wine.pc, ellipse = T, groups = wine$dist)
ggbiplot::ggbiplot(wine.pc, ellipse = T, groups = wine$dist,
                   choices = c(100,50))

wine.pcr.fit <- pcr(y.train ~ ., 
                    data = train_df, 
                    validation = "CV")

wine.pcr.fit$validation$PRESS %>% which.min()

pred <- predict(wine.pcr.fit, test_df, ncomp = 640)

RMSE <- sqrt(mean((pred - test_df$y.test)^2))

################################################################################
############################## 


# Natural Spline

wine_train$year <- as.numeric(wine_train$year)
wine_test$year <- as.numeric(wine_test$year)

x.train <- model.matrix(litre~. -1, data = wine_train)
x.test <- model.matrix(litre~. -1, data = wine_test)
y.train <- wine_train$litre
y.test <- wine_test$litre
intsct <- intersect(colnames(x.train),colnames(x.test))
x.train <- x.train[,intsct]
x.test <- x.test[, intsct]

train_df <- cbind(y.train, x.train) %>% as.data.frame()
test_df <- cbind(y.test, x.test) %>% as.data.frame()

lm.fit <- glm(y.train ~ ., data = train_df)
sp.fit <- glm(y.train ~ . + ns(year,df = 2), data = train_df)

sp.pred <- predict(sp.fit, newdata = test_df)

RMSE <- sqrt(mean((sp.pred - test_df$y.test)^2)) # Bullshit
