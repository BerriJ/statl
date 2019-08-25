# Load and prepare Data  ####

# Cleanup the Workspace
rm(list = ls())

# Load the dataset
load("00_data/wine_preprocessed.rda")

# Remove variables with average na >= 50%
wine <- wine %>% dplyr::select_if(.predicate = function(x) mean(is.na(x)) < 0.50) %>%
  # Only keep complete cases
  drop_na() %>%
  # Drop llitre because we are using litre
  dplyr::select(-llitre) %>%
  # Remove unused levels from factor variables
  droplevels()

# Crossvalidation Setup  ####

# Create 5 training and  5 test datasets
index <- data.frame(index = 1:nrow(wine), obs = 1:nrow(wine))
sets <- list()
for(i in 1:5){
  sets[[i]] <- sample(na.omit(index$obs), size = (floor(nrow(wine)/5)), replace = F)
  index[sets[[i]],2] <- NA
}
train_list <- list()
test_list <- list()

# Save every set in the respective list
for(i in 1:5){
  train_list[[i]] <- wine[sets[setdiff(c(1,2,3,4,5), c(i))] %>% unlist(),]
  test_list[[i]] <- wine[sets[i] %>% unlist(),]
}

# remov stuff
rm(i, index, sets)

# Run the actual CV ####

for(i in 1:5){
  
  # Create an identifier for every iteration 
  # (this is necessary to save the results)
  unique_identifier <- Sys.time() %>% as.character(format = "%Y%m%d_%H%M")
  unique_identifier <- paste(unique_identifier,i, sep = "_")

  # Assign training and test data
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

  # Train the models

  # # Mean Regression and Linear Regression
  # rstudioapi::jobRunScript("02_analysis/02_2_mean_and_lin_reg.R",
  #                          workingDir = "../statl",
  #                          importEnv = T)
  # 
  # # Lasso
  rstudioapi::jobRunScript("02_analysis/02_3_lasso.R",
                           workingDir = "../statl",
                           importEnv = T)
  # 
  # # PCR and PLS
  # rstudioapi::jobRunScript("02_analysis/02_4_pcr_pls.R",
  #                          workingDir = "../statl",
  #                          importEnv = T)
  # 
  # # Splines
  # rstudioapi::jobRunScript("02_analysis/02_5_splines.R",
  #                          workingDir = "../statl",
  #                          importEnv = T)
  # 
  # # Single Tree
  # rstudioapi::jobRunScript("02_analysis/02_6_single_tree.R",
  #                          workingDir = "../statl",
  #                          importEnv = T)
  # 
  # # Bagging
  # rstudioapi::jobRunScript("02_analysis/02_1_Bagging.R",
  #                          workingDir = "../statl",
  #                          importEnv = T)
  # 
  # # Random Forest
  # rstudioapi::jobRunScript("02_analysis/02_7_random_forest.R",
  #                          workingDir = "../statl",
  #                          importEnv = T)
  # 
  # # Boosting
  # rstudioapi::jobRunScript("02_analysis/02_7_boosting.R",
  #                          workingDir = "../statl",
  #                          importEnv = T)
}



# Package Citation

knitr::write_bib(x = c("base",
                "stargazer",
                "haven",
                "stringr",
                "tidyr",
                "dplyr",
                "glmnet",
                "class",
                "MASS",
                "plm",
                "leaps",
                "caret",
                "reshape2",
                "tree",
                "gbm",
                "plotmo",
                "pls",
                "splines",
                "tictoc",
                "plotly",
                "inspectdf",
                "rpart",
                "rpart.plot",
                "stargazer",
                "knitr",
                "purrr",
                "randomForest",
                "rstudioapi"), file = "03_paper/R_packages_draft.bib")

# Dont forget to add Rstudio
RStudio.Version()$citation
