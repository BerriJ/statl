rm(list = ls())

# library(leaps)
# library(plm)
# 
# library(caret)

load("00_data/wine_preprocessed.rda")

# What we tried in the Tutorial

# Non_linear Transformation
# Subset Selection
# Leave one out CV
# Lasso
# PLS PCR
# Natural Splines and Bsplines
# GAMS
# Random Forests

# Visual inspection:

# Name: ####

# Boxplot der Logliter nach Name

names_boxplot <- wine %>% dplyr::group_by(name) %>% 
  summarise(mean = mean(llitre, na.rm = T)) %>% 
  drop_na() %>% ggplot(aes(x = factor(name), y = mean)) + 
  geom_point() + theme(axis.text.x = element_blank())

dir.create("02_analysis/plots", showWarnings = F)
ggsave(file = "02_analysis/plots/01_names_boxplot.pdf",names_boxplot, width = 16, 
       height = 9, limitsize = F)

# Verteilung der Mittleren Logliter je Name

names_density <- wine %>% dplyr::group_by(name) %>% 
  summarise(mean = mean(llitre)) %>% 
  drop_na() %>%
  ggplot() + 
  geom_histogram(aes(x=mean,y=..density..), position="identity") + 
  geom_density(aes(x=mean,y=..density..), col = "gray13") + 
  ylab("Density")

dir.create("02_analysis/plots", showWarnings = F)
ggsave(file = "02_analysis/plots/01_names_density.pdf", names_density, width = 16, 
       height = 9)

# Artikelnr: ####

unique(wine$name) %>% length() %>% paste("Names")
unique(wine$artikelid) %>% length() %>% paste("Artikel ID's")
unique(wine$artikelnr) %>% length() %>% paste("Artikel Numbers")

# => Omit at least 2 out of those 3!

# Atikelid => siehe Artikelnr

# Vintage: ####

unique(wine$vintage)

# Boxplot der Logliter nach Vintage:

vintage_boxplot <- wine %>% dplyr::group_by(vintage) %>% 
  ggplot(aes(x = factor(vintage), y = llitre)) + 
  geom_boxplot()

dir.create("02_analysis/plots", showWarnings = F)
ggsave(file = "02_analysis/plots/02_vintage_boxplot.pdf",vintage_boxplot, width = 16, 
       height = 9, limitsize = F)

# Verändert Vintage die Verteilung?

vintage_density <- ggplot(wine) + 
  geom_histogram(aes(x=llitre,y=..density..), position="identity") + 
  geom_density(aes(x=llitre,y=..density..), col = "gray13") + 
  ylab("Density") + 
  facet_wrap(~ vintage, ncol =7)

dir.create("02_analysis/plots", showWarnings = F)
ggsave(file = "02_analysis/plots/03_vintage_density.pdf",vintage_density , width = 16, 
       height = 9, limitsize = F)



# Country: ####

unique(wine$vintage)

# Boxplot der Logliter nach Country:

county_boxplot <- wine %>% dplyr::group_by(country) %>% 
  ggplot(aes(x = factor(country), y = llitre)) + 
  geom_boxplot() + theme(axis.text.x = element_text(angle = 45))


dir.create("02_analysis/plots", showWarnings = F)
ggsave(file = "02_analysis/plots/03_county_boxplot.pdf",county_boxplot, width = 16, 
       height = 9, limitsize = F) 


# Verändert Country die Verteilung?

country_density <- ggplot(wine) + 
  geom_histogram(aes(x=llitre,y=..density..), position="identity") + 
  geom_density(aes(x=llitre,y=..density..), col = "gray13") + 
  ylab("Density") + 
  facet_wrap(~ country, ncol =7)

dir.create("02_analysis/plots", showWarnings = F)
ggsave(file = "02_analysis/plots/03_country_density.pdf",country_density , width = 16, 
       height = 9, limitsize = F)

# Region ####

# Boxplot der Logliter nach Region:

region_boxplot <- wine %>%
  ggplot(aes(x = factor(region), y = llitre)) + 
  geom_boxplot() + theme(axis.text.x = element_text(angle = 45, size = 9))


dir.create("02_analysis/plots", showWarnings = F)
ggsave(file = "02_analysis/plots/04_region_boxplot.pdf",region_boxplot, width = 16, 
       height = 9, limitsize = F) 


# Year ####

wine %>% group_by(year) %>% count()

year_scatter <- ggplot(wine, aes(x = year, y = llitre)) + geom_point(position = "jitter", alpha = 0.5)

dir.create("02_analysis/plots", showWarnings = F)
ggsave(file = "02_analysis/plots/05_year_scatter.pdf",year_scatter , width = 16, 
       height = 9, limitsize = F) 


# Week ####

week_boxplot <- wine %>% 
  ggplot(aes(x = factor(week), y = llitre)) + 
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90))

dir.create("02_analysis/plots", showWarnings = F)
ggsave(file = "02_analysis/plots/06_week_boxplot.pdf", week_boxplot , width = 16, 
       height = 9, limitsize = F) 

# period

period_boxplot <- wine %>% 
  ggplot(aes(x = factor(period), y = llitre)) + 
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90, size =  5))

dir.create("02_analysis/plots", showWarnings = F)
ggsave(file = "02_analysis/plots/07_period_boxplot.pdf", period_boxplot , width = 16, 
       height = 9, limitsize = F) 

# date

date_boxplot <- wine %>% 
  ggplot(aes(x = factor(date), y = llitre)) + 
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90))

dir.create("02_analysis/plots", showWarnings = F)
ggsave(file = "02_analysis/plots/08_date_boxplot.pdf", date_boxplot , width = 16, 
       height = 9, limitsize = F) 

# price

price_scatter <- ggplot(wine, aes(x = price, y = llitre)) + geom_point(alpha = 0.5)

dir.create("02_analysis/plots", showWarnings = F)
ggsave(file = "02_analysis/plots/08_price_scatter.pdf", price_scatter , width = 16, 
       height = 9, limitsize = F) 

# log price

price_scatter <- ggplot(wine, aes(x = lp, y = llitre)) + geom_point(alpha = 0.5)

dir.create("02_analysis/plots", showWarnings = F)
ggsave(file = "02_analysis/plots/08_price_scatter.pdf", price_scatter , width = 16, 
       height = 9, limitsize = F) 


# dist: level of distribution ####

dist_scatter <- ggplot(wine, aes(x = dist, y = llitre)) + geom_point(position = "jitter", alpha = 0.5)

dir.create("02_analysis/plots", showWarnings = F)
ggsave(file = "02_analysis/plots/09_dist_scatter.pdf", dist_scatter , width = 16, 
       height = 9, limitsize = F)

dist_density <- ggplot(wine) + 
  geom_histogram(aes(x=llitre,y=..density..), position="identity") + 
  geom_density(aes(x=llitre,y=..density..), col = "gray13") + 
  ylab("Density") + 
  facet_wrap(~ dist, ncol =3)

dir.create("02_analysis/plots", showWarnings = F)
ggsave(file = "02_analysis/plots/09_dist_density.pdf",dist_density , width = 16, 
       height = 9, limitsize = F)

# Taste Segment ####

taste_segment_boxplot <- wine %>% 
  ggplot(aes(x = factor(taste_segment), y = llitre)) + 
  geom_boxplot() + theme(axis.text.x = element_text(angle = 45))


dir.create("02_analysis/plots", showWarnings = F)
ggsave(file = "02_analysis/plots/10_taste_segment_boxplot.pdf", taste_segment_boxplot , width = 16, 
       height = 9) 

# Color Segment ####

segm_boxplot <- wine %>% 
  ggplot(aes(x = factor(segm), y = llitre)) + 
  geom_boxplot() + theme(axis.text.x = element_text())


dir.create("02_analysis/plots", showWarnings = F)
ggsave(file = "02_analysis/plots/11_segm_boxplot.pdf", segm_boxplot , width = 16, 
       height = 9) 

segm_density <- ggplot(wine) + 
  geom_histogram(aes(x=llitre,y=..density..), position="identity") + 
  geom_density(aes(x=llitre,y=..density..), col = "gray13") + 
  ylab("Density") + 
  facet_wrap(~ segm)

dir.create("02_analysis/plots", showWarnings = F)
ggsave(file = "02_analysis/plots/11_segm_density.pdf",dist_density , width = 16, 
       height = 9, limitsize = F)

# price_segm ####

price_segm_boxplot <- wine %>% 
  ggplot(aes(x = factor(price_segm), y = llitre)) + 
  geom_boxplot() + theme(axis.text.x = element_text())


dir.create("02_analysis/plots", showWarnings = F)
ggsave(file = "02_analysis/plots/12_price_segm_boxplot.pdf", price_segm_boxplot , width = 16, 
       height = 9) 

price_segm_density <- ggplot(wine) + 
  geom_histogram(aes(x=llitre,y=..density..), position="identity") + 
  geom_density(aes(x=llitre,y=..density..), col = "gray13") + 
  ylab("Density") + 
  facet_wrap(~ price_segm)

dir.create("02_analysis/plots", showWarnings = F)
ggsave(file = "02_analysis/plots/12_price_segm_density.pdf",dist_density , width = 16, 
       height = 9)

# Old ####

old_scatter <- ggplot(wine, aes(x = old, y = llitre)) + geom_point(position = "jitter", alpha = 0.5)

dir.create("02_analysis/plots", showWarnings = F)
ggsave(file = "02_analysis/plots/13_old_scatter.pdf", old_scatter , width = 16, 
       height = 9) 

old_density <- ggplot(wine) + 
  geom_histogram(aes(x=old,y=..density..), position="identity") + 
  geom_density(aes(x=old,y=..density..), col = "gray13") + 
  ylab("Density")

dir.create("02_analysis/plots", showWarnings = F)
ggsave(file = "02_analysis/plots/13_old_density.pdf", old_density , width = 16, 
       height = 9) 

# ma_split

ma_split_scatter <- ggplot(wine, aes(x = ma_split, y = litre)) + 
  geom_point(position = "jitter", alpha = 0.25)

dir.create("02_analysis/plots", showWarnings = F)
ggsave(file = "02_analysis/plots/13_ma_split_scatter.pdf", old_scatter , width = 16, 
       height = 9) 

# Reviews

reviews <- colnames(wine)[22:57]

for(i in 1:length(reviews)){
  scatter <- ggplot(wine, aes_string(x = reviews[i], y = "litre")) + 
  geom_point(position = "jitter", alpha = 0.25)
  ggsave(file = paste("02_analysis/plots/",13+ i, "_", reviews[i],
                      "_scatter.pdf", sep = ""), scatter , width = 16, 
         height = 9) 
  
}

# Regsubsets (Forward and Backward stepwise):

load("00_data/wine_preprocessed.rda")

wine <- wine %>% dplyr::select(-name, - llitre, -artikelid, -artikelnr)

wine <- wine[,which(colSums(is.na(wine)) < 1000)] %>% 
  drop_na()

glimpse(wine)

corrmat <- dplyr::select_if(wine, is.numeric) %>% cor()
set.seed(123)
train <- sample(nrow(wine), floor(0.75*nrow(wine)))

wine_train <- wine[train,]
wine_test <- wine[-(train),]

regfit_backward <- regsubsets(litre ~.-region -taste_segment -segm, data = wine_train, 
                     method = "backward", nvmax = 50)

reg_sum_backward <- summary(regfit_backward)
id <- reg_sum_backward$rss %>% which.min()
coefficients_backward <- coef(regfit_backward, id = id)


regfit_forward <- regsubsets(litre ~. -region -taste_segment -segm, data = wine_train, 
                     method = "forward", nvmax = 50)

reg_sum_forward <- summary(regfit_forward)
id <- reg_sum_forward$rss %>% which.min()
coefficients_forward <- coef(regfit_forward, id = id)

intersect(names(coefficients_backward), names(coefficients_forward))

reg_sum_forward$rss %>% min()
reg_sum_backward$rss %>% min()

val.errors = rep(NA, regfit_forward$nvmax)

x.test = model.matrix(litre ~ . -region -taste_segment -segm, data = wine_test)

for (i in 1:(regfit_forward$nvmax-1)) {
  coefi = coef(regfit_forward, id = i)
  pred = x.test[, names(coefi)] %*% coefi
  val.errors[i] = mean((wine_test$litre - pred)^2)
}
plot(sqrt(val.errors))
points(sqrt(regfit_forward$rss[1:regfit_forward$nvmax]/dim(wine_train)[1]), col = "blue", pch = 19, type = "b")
legend("topright", legend = c("Training", "Validation"), col = c("blue", "black"), 
       pch = 19)

################################################################################
################################## LASSO #######################################
################################################################################

# library(glmnet) # Lasso / Ridge
# library(reshape2)

x.train <- model.matrix(litre ~ ., data = wine_train)
y.train <- wine_train$litre
x.test <- model.matrix(litre ~ ., data = wine_test)
y.test <- wine_test$litre
cv.out <- cv.glmnet(x = x.train, y = y.train, alpha = 1)
out <- glmnet(x = x.train, y = y.train, alpha = 1)
plot(cv.out)
bestlam <- cv.out$lambda.min
lasso.mod <- glmnet(x = x.train, y = y.train, alpha = 1, lambda = bestlam)
plotmo::plot_glmnet(out)
# prediction
pred <- predict(lasso.mod, x.test, s = bestlam)
rmse_lasso <- mean((y.test - pred)^2) %>% sqrt()


################################################################################
############################ TREES and FORESTS #################################
################################################################################


## Trees


tree_wine <- tree(litre ~ .-region, data = wine_train) 
# we have to cancel region, because factor predictors must have at most 32 levels
# and there are 96 levels of regions
summary(tree_wine) # vars used for construction: "price_segm", "dist", "price",
                   # "ms_segm", "lp", "country", "rprice_litre", "taste_segment", "period"
plot(tree_wine)
text(tree_wine)

# check whether pruning improves performance

tree_cv_wine <- cv.tree(tree_wine) # takes about 30 sec 
plot(tree_cv_wine$size,tree_cv_wine$dev,type='b') 
# we would choose the largest tree, but could alternatively prune it to a size of 6

tree_prune_wine <- prune.tree(tree_wine, best = 6)
plot(tree_prune_wine)
text(tree_prune_wine)


pred_tree <- predict(tree_wine, newdata = wine_test) 
pred_pruned <- predict(tree_prune_wine, newdata = wine_test)
rmse_tree <- mean((wine_test$litre - pred_tree)^2) %>% sqrt()  # RMSE: 7380.601
rmse_tree_pruned <- mean((wine_test$litre - pred_pruned)^2) %>% sqrt() # RMSE: 7972.512


## Bagging
set.seed(123)
bag_wine <- randomForest(x = x.train, mtry = 10, y = y.train, importance = T) 
#took about 6288.55 sec elapsed for mtry = 10
bag_wine
pred_bag <- predict(bag_wine, newdata = x.test)
plot(pred_bag, y.test)
rmse_bag <- mean((y.test - pred_bag)^2) %>% sqrt() # 5605.3265 for mtry = 10


# set.seed(123)
# rf_wine <- randomForest(x = x.train, y = y.train, importance = T) # now whithout spec of mtry
# rf_wine
# pred_rf <- predict(rf_wine, newdata = x.test)
# plot(pred_rf, y.test)
# rmse_rf <- mean((y.test - pred_rf)^2) %>% sqrt() 


# Parallelization following https://stackoverflow.com/questions/14106010/parallel-execution-of-random-forest-in-r/15771458#15771458
# Implementation of Answer (combined with https://privefl.github.io/blog/a-guide-to-parallelism-in-r/)


cl <- parallel::makeCluster(2)
doParallel::registerDoParallel(cl)
set.seed(123)
rf_par <- foreach(mtry = 1:3, .combine = randomForest::combine,          #took about 998.27 sec
              .multicombine = TRUE, .packages = 'randomForest') %dopar% {
                randomForest(x.train, y.train, mtry = mtry)
              }
parallel::stopCluster(cl)

pred_rf_par <- predict(rf_par, newdata = x.test)
plot(pred_rf_par, y.test)
rmse_rf_par <- mean((y.test - pred_rf_par)^2) %>% sqrt() # RMSE : 10113.45
