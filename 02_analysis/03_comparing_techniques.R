rm(list = ls())

library(leaps)
library(plm)

library(caret)

load("00_data/wine_preprocessed.rda")

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


































# Regsubsets (Forward and Backward stepwise):


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
