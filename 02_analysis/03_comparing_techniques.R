rm(list = ls())

library(leaps)
library(plm)

library(caret)

load("00_data/wine_preprocessed.rda")

wine <- wine %>% dplyr::select(-name, - litre, -artikelid, -artikelnr)

wine <- wine[,which(colSums(is.na(wine)) < 1000)] %>% 
  drop_na()

glimpse(wine)

corrmat <- dplyr::select_if(wine, is.numeric) %>% cor()

train <- sample(nrow(wine), floor(0.75*nrow(wine)))

wine_train <- wine[train,]
wine_test <- wine[-(train),]

regfit <- regsubsets(llitre ~.-region -taste_segment -segm, data = wine_train, 
                     method = "backward", nvmax = 10,really.big = F)

reg.sum <- summary(regfit)
