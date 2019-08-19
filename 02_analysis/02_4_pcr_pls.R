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
# 
# #remotes::install_github("vqv/ggbiplot")
# ggbiplot::ggbiplot(wine.pc)
# 
# pca <- ggbiplot::ggbiplot(wine.pc,ellipse = F, 
#                    groups = wine$taste_segment, varname.abbrev =  TRUE) +
#   theme_minimal() + theme(legend.position = "right") + labs(col = "Teste Segment")
# 
# ggsave("00_data/output_paper/07_pca.png",plot =  pca, width = 7, height = 4)
# 
# # Open a pdf file
# pdf("00_data/output_paper/07_pca.pdf", width = 7, height = 4) 
# # 2. Create a plot
# pca
# # Close the pdf file
# dev.off() 


wine.pcr.fit <- pcr(y.train ~ ., 
                    data = train_df,
                    validation = "CV")

n_comp_pcr <- wine.pcr.fit$validation$PRESS %>% which.min()

pred <- predict(wine.pcr.fit, test_df, ncomp = n_comp_pcr)

rmse_pcr <- mean((y.test - pred)^2) %>% sqrt()

################################################################################
################################# PLS ##########################################
################################################################################

print("Start PLS")

pls.fit <- plsr(y.train ~ ., data = train_df, validation = "CV")
# pls.fit %>% summary()

# Prediction Error Sum of Squares minimum
n_comp_pls <- which.min(pls.fit$validation$PRESS)

pred <- predict(pls.fit, test_df, ncomp = n_comp_pls)

rmse_pls <- mean((y.test - pred)^2) %>% sqrt()

dir.create("02_analysis/cv/pcr_pls/", recursive = T, showWarnings = F)
save(file = paste("02_analysis/cv/pcr_pls/pcr_pls_",unique_identifier,".rda", 
                  sep = ""), 
     rmse_pcr, 
     rmse_pls,
     n_comp_pls,
     n_comp_pcr)

