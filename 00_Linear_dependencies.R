# Check for linear dependencies:

rm(list = ls())

load("00_data/wine_preprocessed.rda")

# First we have the problem that if removing all incomplete observations we 
# are left with 0 observations!!!
wine %>% drop_na() %>% dim()

# model.matix does this and hence complains having factors with <= 1 level
# Overview about the missings:
av_na <- apply(wine, MARGIN = 2, FUN = function(x) is.na(x) %>% sum()) %>% 
  sort(dec = T) / nrow(wine)

av_na[av_na >= 0.5]

# We have identified some variables which have massive amounts of missing values
# Proposal: Remove all variables with average missing >= 0.95
rm <- av_na[av_na >= 0.6] %>% names()

wine <- wine %>% dplyr::select(-rm)

# Levels of factors after keeping complete cases:
wine[,sapply(wine, is.factor)] %>% drop_na() %>% sapply(levels) # looks fine

# Now renoving na's and checking the length again: ok. 
wine %>% drop_na() %>% dim()

MM <- model.matrix(litre ~., data = wine)

# From STACKOVERFLOW

# Here's a straightforward approach: compute the rank of the matrix that results 
# from removing each of the columns. The columns which, when removed, result in 
# the highest rank are the linearly dependent ones (since removing those does 
# not decrease rank, while removing a linearly independent column does).


rankifremoved <- sapply(1:ncol(MM), function (x) qr(MM[,-x])$rank)
which(rankifremoved == max(rankifremoved))
# See https://stats.stackexchange.com/a/39321/212827
