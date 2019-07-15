load("00_data/wine_preprocessed.rda")

# Baseline Model: Mean

models <- data.frame(mod = c("Mean", "Basic_lm"), rmse = c(NA, NA))

residuals <- wine$llitre-mean(wine$llitre, na.rm = T) %>% na.omit()
plot(residuals)
models[models$mod == "Mean","rmse"] <- (wine$llitre-mean(wine$llitre, na.rm = T))^2 %>% 
  na.omit() %>%
  sqrt() %>%
  mean()
print(models)

# # Baseline Model: Linear Model with all* variables

colSums(is.na(wine)) %>% sort(decreasing = T) # We've got some problems

wine_subset <- wine[,which(colSums(is.na(wine)) < 1000)] %>% 
  drop_na()

lmod <- lm(llitre ~. -litre -name, data = wine_subset)

models[models$mod == "Basic_lm", "rmse"] <- (wine_subset$llitre-lmod$fitted.values)^2 %>% 
  sqrt() %>%
  mean()

models
