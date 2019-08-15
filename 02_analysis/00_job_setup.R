rm(list = ls())
load("00_data/wine_preprocessed.rda")
# Remove variables with average na >= 50%
wine <- wine %>% dplyr::select_if(.predicate = function(x) mean(is.na(x)) < 0.50) %>% 
  # Only keep complete cases
  drop_na() %>% 
  # Drop llitre because we are using litre
  dplyr::select(-llitre) %>%
  # Remove unused levels from factor variables
  droplevels()

# Create Training and Test Datasets
index <- data.frame(index = 1:nrow(wine), obs = 1:nrow(wine))
sets <- list()

for(i in 1:5){
  sets[[i]] <- sample(na.omit(index$obs), size = (floor(nrow(wine)/5)), replace = F)
  index[sets[[i]],2] <- NA 
}

train_list <- list()
test_list <- list()

for(i in 1:5){
  train_list[[i]] <- wine[sets[setdiff(c(1,2,3,4,5), c(i))] %>% unlist(),]
  test_list[[i]] <- wine[sets[i] %>% unlist(),]
}

rm(i, index, sets)



for(i in 1:5){
  unique_identifier <- Sys.time() %>% as.character(format = "%Y%m%d_%H%M")
  unique_identifier <- paste(unique_identifier,i, sep = "_")
  wine_train <- train_list[[i]]
  wine_test <- test_list[[i]]
  x.train <- model.matrix(litre~., data = wine_train)
  x.test <- model.matrix(litre~., data = wine_test)
  y.train <- wine_train$litre
  y.test <- wine_test$litre
  intsct <- intersect(colnames(x.train),colnames(x.test))
  x.train <- x.train[,intsct]
  x.test <- x.test[, intsct]
  
  train_df <- cbind(y.train, x.train) %>% as.data.frame()
  test_df <- cbind(y.test, x.test) %>% as.data.frame()
  rstudioapi::jobRunScript("02_analysis/02_1_Bagging.R",
                           workingDir = "../statl", 
                           importEnv = T)
  #Sys.sleep(30*60)
}

files <- dir(recursive = T, path = "02_analysis/cv")

models_ <- list()

for(i in files){
  load(file = paste("02_analysis/cv/", i, sep = ""))
  models_[[i]] <- models
}

models <- models_[[1]]

for(i in 1:(length(files)-1)){
  models <- models %>% full_join(models_[[i+1]], by = "mod") %>% drop_na()
}

models$Mean <- rowMeans(models[,-1])
