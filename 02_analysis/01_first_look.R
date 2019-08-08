rm(list = ls())
load("00_data/wine_preprocessed.rda")

head(wine)
hist(wine$litre)


# First problem: 0 complete_cases
wine %>% drop_na() %>% dim()

# model.matix  hence complains having factors with <= 1 level
# Overview about the missings:
av_na <- apply(wine, MARGIN = 2, FUN = function(x) is.na(x) %>% sum()) %>% 
  sort(dec = T) / nrow(wine)

av_na[av_na >= 0.5]

# We have identified some variables which have massive amounts of missing values
# Proposal: Remove all variables with average missing above a cutoff

wine %>% dplyr::select(names(av_na[av_na <=0.25])) %>% drop_na() %>% dim()

wine %>% dplyr::select(names(av_na[av_na <=0.5])) %>% drop_na() %>% dim()

wine %>% dplyr::select(names(av_na[av_na <=0.75])) %>% drop_na() %>% dim()

rm <- av_na[av_na >= 0.6] %>% names()

wine <- wine %>% dplyr::select(-rm)

# Levels of factors after keeping complete cases:
wine[,sapply(wine, is.factor)] %>% drop_na() %>% sapply(levels) # looks fine

# Now renoving na's and checking the length again: ok. 
wine %>% drop_na() %>% dim()
wine_cc <- wine %>% drop_na()

MM <- model.matrix(litre ~. -1, data = wine)

# From STACKOVERFLOW

# Here's a straightforward approach: compute the rank of the matrix that results 
# from removing each of the columns. The columns which, when removed, result in 
# the highest rank are the linearly dependent ones (since removing those does 
# not decrease rank, while removing a linearly independent column does).

# rankifremoved <- c()
# d <- c()
# for(i in 1:ncol(MM)){
#   x <- Sys.time()
#   rankifremoved[i] <- qr(MM[,-i])$rank
#   d[i] <- Sys.time() - x
#   print(paste(round(mean(d)*(ncol(MM)-i)), "Seconds Remaining"))
# }
# See https://stats.stackexchange.com/a/39321/212827
# which(rankifremoved == max(rankifremoved))
# lin_dependend <- colnames(MM)[which(rankifremoved == max(rankifremoved))]
# save(file = "00_data/lin_dependend.rda", lin_dependend)
load("00_data/lin_dependend.rda")

apply(MM, MARGIN = 2, function(x) sum(x==0)==length(x)) %>% which()

