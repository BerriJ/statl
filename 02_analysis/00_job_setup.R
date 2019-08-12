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

for(i in 1:10){
  sets[[i]] <- sample(na.omit(index$obs), size = (floor(nrow(wine)/10)), replace = F)
  index[sets[[i]],2] <- NA 
}

train_list <- list()
test_list <- list()

for(i in 1:10){
  train_list[[i]] <- wine[sets[setdiff(c(1,2,3,4,5,6,7,8,9,10), c(i))] %>% unlist(),]
  test_list[[i]] <- wine[sets[i] %>% unlist(),]
}

rm(i, wine, index, sets)

for(i in 1:10){
  unique_identifier <- Sys.time() %>% as.character(format = "%Y%m%d_%H%M")
  unique_identifier <- paste(unique_identifier,i, sep = "_")
  wine_train <- train_list[[i]]
  wine_test <- test_list[[i]]
  rstudioapi::jobRunScript("02_analysis/02_model_developement.R",
                           workingDir = "../statl", 
                           importEnv = T)
}


