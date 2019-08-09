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

wine %>% dplyr::select_if(.predicate = function(x) mean(is.na(x)) < 0.25) %>% 
  drop_na() %>% dim()

wine %>% dplyr::select_if(.predicate = function(x) mean(is.na(x)) < 0.40) %>% 
  drop_na() %>% dim()
  
wine %>% dplyr::select_if(.predicate = function(x) mean(is.na(x)) < 0.50) %>% 
  drop_na() %>% dim()

wine %>% dplyr::select_if(.predicate = function(x) mean(is.na(x)) < 0.60) %>% 
  drop_na() %>% dim()

wine %>% dplyr::select_if(.predicate = function(x) mean(is.na(x)) < 0.75) %>% 
  drop_na() %>% dim()

wine <- wine %>% dplyr::select_if(.predicate = function(x) mean(is.na(x)) < 0.50) %>% 
  drop_na()

# Levels of factors after keeping complete cases:
wine[,sapply(wine, is.factor)] %>% drop_na() %>% sapply(levels) # looks fine

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

# Visual inspection:

# Name: ####

# Boxplot der Logliter nach Name

names_boxplot <- wine %>% dplyr::group_by(name) %>% 
  summarise(mean = mean(litre, na.rm = T)) %>% 
  drop_na() %>% ggplot(aes(x = factor(name), y = mean)) + 
  geom_point() + theme(axis.text.x = element_blank())

dir.create("02_analysis/plots", showWarnings = F)
ggsave(file = "02_analysis/plots/01_names_boxplot.pdf",names_boxplot, width = 16, 
       height = 9, limitsize = F)

# Verteilung der Mittleren Logliter je Name

names_density <- wine %>% dplyr::group_by(name) %>% 
  summarise(mean = mean(litre)) %>% 
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
  ggplot(aes(x = factor(vintage), y = litre)) + 
  geom_boxplot()

dir.create("02_analysis/plots", showWarnings = F)
ggsave(file = "02_analysis/plots/02_vintage_boxplot.pdf",vintage_boxplot, width = 16, 
       height = 9, limitsize = F)

# Verändert Vintage die Verteilung?

vintage_density <- ggplot(wine) + 
  geom_histogram(aes(x=litre,y=..density..), position="identity") + 
  geom_density(aes(x=litre,y=..density..), col = "gray13") + 
  ylab("Density") + 
  facet_wrap(~ vintage, ncol =7)

dir.create("02_analysis/plots", showWarnings = F)
ggsave(file = "02_analysis/plots/03_vintage_density.pdf",vintage_density , width = 16, 
       height = 9, limitsize = F)



# Country: ####

unique(wine$vintage)

# Boxplot der Logliter nach Country:

county_boxplot <- wine %>% dplyr::group_by(country) %>% 
  ggplot(aes(x = factor(country), y = litre)) + 
  geom_boxplot() + theme(axis.text.x = element_text(angle = 45))


dir.create("02_analysis/plots", showWarnings = F)
ggsave(file = "02_analysis/plots/03_county_boxplot.pdf",county_boxplot, width = 16, 
       height = 9, limitsize = F) 


# Verändert Country die Verteilung?

country_density <- ggplot(wine) + 
  geom_histogram(aes(x=litre,y=..density..), position="identity") + 
  geom_density(aes(x=litre,y=..density..), col = "gray13") + 
  ylab("Density") + 
  facet_wrap(~ country, ncol =7)

dir.create("02_analysis/plots", showWarnings = F)
ggsave(file = "02_analysis/plots/03_country_density.pdf",country_density , width = 16, 
       height = 9, limitsize = F)

# Region ####

# Boxplot der Logliter nach Region:

region_boxplot <- wine %>%
  ggplot(aes(x = factor(region), y = litre)) + 
  geom_boxplot() + theme(axis.text.x = element_text(angle = 45, size = 9))


dir.create("02_analysis/plots", showWarnings = F)
ggsave(file = "02_analysis/plots/04_region_boxplot.pdf",region_boxplot, width = 16, 
       height = 9, limitsize = F) 


# Year ####

wine %>% group_by(year) %>% count()

year_scatter <- ggplot(wine, aes(x = year, y = litre)) + geom_point(position = "jitter", alpha = 0.5)

dir.create("02_analysis/plots", showWarnings = F)
ggsave(file = "02_analysis/plots/05_year_scatter.pdf",year_scatter , width = 16, 
       height = 9, limitsize = F) 


# Week ####

week_boxplot <- wine %>% 
  ggplot(aes(x = factor(week), y = litre)) + 
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90))

dir.create("02_analysis/plots", showWarnings = F)
ggsave(file = "02_analysis/plots/06_week_boxplot.pdf", week_boxplot , width = 16, 
       height = 9, limitsize = F) 

# period

period_boxplot <- wine %>% 
  ggplot(aes(x = factor(period), y = litre)) + 
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90, size =  5))

dir.create("02_analysis/plots", showWarnings = F)
ggsave(file = "02_analysis/plots/07_period_boxplot.pdf", period_boxplot , width = 16, 
       height = 9, limitsize = F) 

# date

date_boxplot <- wine %>% 
  ggplot(aes(x = factor(date), y = litre)) + 
  geom_boxplot() + theme(axis.text.x = element_text(angle = 90))

dir.create("02_analysis/plots", showWarnings = F)
ggsave(file = "02_analysis/plots/08_date_boxplot.pdf", date_boxplot , width = 16, 
       height = 9, limitsize = F) 

# price

price_scatter <- ggplot(wine, aes(x = price, y = litre)) + geom_point(alpha = 0.5)

dir.create("02_analysis/plots", showWarnings = F)
ggsave(file = "02_analysis/plots/08_price_scatter.pdf", price_scatter , width = 16, 
       height = 9, limitsize = F) 

# log price

price_scatter <- ggplot(wine, aes(x = lp, y = litre)) + geom_point(alpha = 0.5)

dir.create("02_analysis/plots", showWarnings = F)
ggsave(file = "02_analysis/plots/08_price_scatter.pdf", price_scatter , width = 16, 
       height = 9, limitsize = F) 


# dist: level of distribution ####

dist_scatter <- ggplot(wine, aes(x = dist, y = litre)) + geom_point(position = "jitter", alpha = 0.5)

dir.create("02_analysis/plots", showWarnings = F)
ggsave(file = "02_analysis/plots/09_dist_scatter.pdf", dist_scatter , width = 16, 
       height = 9, limitsize = F)

dist_density <- ggplot(wine) + 
  geom_histogram(aes(x=litre,y=..density..), position="identity") + 
  geom_density(aes(x=litre,y=..density..), col = "gray13") + 
  ylab("Density") + 
  facet_wrap(~ dist, ncol =3)

dir.create("02_analysis/plots", showWarnings = F)
ggsave(file = "02_analysis/plots/09_dist_density.pdf",dist_density , width = 16, 
       height = 9, limitsize = F)

# Taste Segment ####

taste_segment_boxplot <- wine %>% 
  ggplot(aes(x = factor(taste_segment), y = litre)) + 
  geom_boxplot() + theme(axis.text.x = element_text(angle = 45))


dir.create("02_analysis/plots", showWarnings = F)
ggsave(file = "02_analysis/plots/10_taste_segment_boxplot.pdf", taste_segment_boxplot , width = 16, 
       height = 9) 

# Color Segment ####

segm_boxplot <- wine %>% 
  ggplot(aes(x = factor(segm), y = litre)) + 
  geom_boxplot() + theme(axis.text.x = element_text())


dir.create("02_analysis/plots", showWarnings = F)
ggsave(file = "02_analysis/plots/11_segm_boxplot.pdf", segm_boxplot , width = 16, 
       height = 9) 

segm_density <- ggplot(wine) + 
  geom_histogram(aes(x=litre,y=..density..), position="identity") + 
  geom_density(aes(x=litre,y=..density..), col = "gray13") + 
  ylab("Density") + 
  facet_wrap(~ segm)

dir.create("02_analysis/plots", showWarnings = F)
ggsave(file = "02_analysis/plots/11_segm_density.pdf",dist_density , width = 16, 
       height = 9, limitsize = F)

# price_segm ####

price_segm_boxplot <- wine %>% 
  ggplot(aes(x = factor(price_segm), y = litre)) + 
  geom_boxplot() + theme(axis.text.x = element_text())


dir.create("02_analysis/plots", showWarnings = F)
ggsave(file = "02_analysis/plots/12_price_segm_boxplot.pdf", price_segm_boxplot , width = 16, 
       height = 9) 

price_segm_density <- ggplot(wine) + 
  geom_histogram(aes(x=litre,y=..density..), position="identity") + 
  geom_density(aes(x=litre,y=..density..), col = "gray13") + 
  ylab("Density") + 
  facet_wrap(~ price_segm)

dir.create("02_analysis/plots", showWarnings = F)
ggsave(file = "02_analysis/plots/12_price_segm_density.pdf",dist_density , width = 16, 
       height = 9)

# Old ####

old_scatter <- ggplot(wine, aes(x = old, y = litre)) + geom_point(position = "jitter", alpha = 0.5)

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