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
# Proposal: Remove all variables with average missing >= 0.6
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

################################################################################
################################# PCA ##########################################
################################################################################

# https://www.datacamp.com/community/tutorials/pca-analysis-r

# Remove columns without variation:

MM.sd <- apply(MM, MARGIN = 2, FUN = sd)
MM <- MM[,-which(MM.sd == 0)]
wine.pca <- prcomp(MM, center = T, scale. = T)
wine.pca.sm <- summary(wine.pca)
wine.pca.sm$importance

#remotes::install_github("vqv/ggbiplot")
ggbiplot::ggbiplot(wine.pca)
ggbiplot::ggbiplot(wine.pca,ellipse = T, groups = wine_cc$taste_segment)
ggbiplot::ggbiplot(wine.pca, ellipse = T, groups = wine_cc$segm)
ggbiplot::ggbiplot(wine.pca, ellipse = T, groups = wine_cc$dist)

ggbiplot::ggbiplot(wine.pca, ellipse = T, groups = wine_cc$dist,
                   choices = c(100,50))