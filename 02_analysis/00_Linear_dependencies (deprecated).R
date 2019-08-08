load("00_data/wine_preprocessed.rda")

################################################################################
################################# PCA ##########################################
################################################################################

# https://www.datacamp.com/community/tutorials/pca-analysis-r

# Proposal: Remove all variables with average missing >= 0.6
rm <- av_na[av_na >= 0.6] %>% names()

wine <- wine %>% dplyr::select(-rm)

# Remove columns without variation:

MM <- model.matrix(litre ~. -1, data = wine)

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