# Load the dataset

rm(list = ls())

load("Wein_train.rda")

# Remove Backslashes

remove_backslashes <- function(x){
  if(is.character(x)){
    str_replace_all(x, pattern = "\"", replacement =  "")
  }
}

Wein_train <- apply(Wein_train, 2, FUN = remove_backslashes) %>%
  as_tibble() %>% 
  mutate_at(
    vars(-name, -vintage, -country, -region, -date, -taste_segment, -segm,
         -price_segm, -time_segm_price, -v10_dn, -v10_exp, -v10_svd, -v10_aom), 
    .funs = as.numeric) %>%
  mutate(date = as.Date(date))

# We have to give special attention to: -v10_dn, -v10_exp, -v10_svd, -v10_aom
# Those couldn't be transformed into numeric(?)

