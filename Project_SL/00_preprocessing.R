# Load the dataset

rm(list = ls())

load("Wein_train.rda")

head(Wein_train$name)

Wein_train$name <- Wein_train$name %>% 
  str_replace_all(pattern = "\"", replacement =  "")

Wein_train$vintage <- Wein_train$vintage %>% 
  str_replace_all(pattern = "\"", replacement =  "")

Wein_train$country <- Wein_train$country %>% 
  str_replace_all(pattern = "\"", replacement =  "")

Wein_train$region <- Wein_train$region %>% 
  str_replace_all(pattern = "\"", replacement =  "")

Wein_train$date <- Wein_train$date %>% as.Date()

Wein_train$litre <- Wein_train$litre %>% as.numeric()

Wein_train$taste_segment <- Wein_train$taste_segment %>% 
  str_replace_all(pattern = "\"", replacement =  "")
