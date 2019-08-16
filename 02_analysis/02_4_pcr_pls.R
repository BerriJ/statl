print("Loading Packages")
source("02_analysis/00_packages.R")
print("Packages loaded")

################################################################################
################################# PCA ##########################################
################################################################################
print("Start PCA")
# https://www.datacamp.com/community/tutorials/pca-analysis-r

# MM <- model.matrix(litre ~. -1, data = wine)
# wine.pc <- prcomp(MM, center = T, scale. = T)
# wine.pc.sm <- summary(wine.pc)
# wine.pc.sm$importance

# #remotes::install_github("vqv/ggbiplot")
# ggbiplot::ggbiplot(wine.pc)
# ggbiplot::ggbiplot(wine.pc,ellipse = T, groups = wine$taste_segment)
# ggbiplot::ggbiplot(wine.pc, ellipse = T, groups = wine$segm)
# ggbiplot::ggbiplot(wine.pc, ellipse = T, groups = wine$dist)
# ggbiplot::ggbiplot(wine.pc, ellipse = T, groups = wine$dist,
#                    choices = c(100,50))

wine.pcr.fit <- pcr(y.train ~ ., 
                    data = train_df, 
                    validation = "CV")

n_comp <- wine.pcr.fit$validation$PRESS %>% which.min()

pred <- predict(wine.pcr.fit, test_df, ncomp = n_comp)

rmse_pcr <- mean((y.test - pred)^2) %>% sqrt()

################################################################################
################################# PLS ##########################################
################################################################################

pls.fit <- plsr(y.train ~ ., data = train_df, validation = "CV")
# pls.fit %>% summary()

# Prediction Error Sum of Squares minimum
n_comp <- which.min(pls.fit$validation$PRESS)

pred <- predict(pls.fit, test_df, ncomp = n_comp)

rmse_pls <- mean((y.test - pred)^2) %>% sqrt()

dir.create("02_analysis/cv/pcr_pls/", recursive = T, showWarnings = F)
save(file = paste("02_analysis/cv/pcr_pls/pcr_pls_",unique_identifier,".rda", 
                  sep = ""), 
     rmse_pcr, 
     rmse_pls)

