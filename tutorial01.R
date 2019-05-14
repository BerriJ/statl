# Exercise 1 Simple linear regression model ####

# Part1 Get an overview about the dataset

library(MASS)

data(Boston)

glimpse(Boston)

summary(Boston)

?Boston

# Part 2 linear regression model

mod1 <- lm(medv ~ lstat, data = Boston)

summary(mod1)

# Part 3 confidence interval

confint(mod1)

# Part 4

predict(mod1, 
        newdata = data.frame(lstat = c(5, 10, 15)))

# Mittelwertvorhersage daher kleines Intervall
predict(mod1, 
        newdata = data.frame(lstat = c(5, 10, 15)), interval = "conf")

# Vorhersage des "wirklichen" Wertes, daher ist das Intervall deutlich größer
predict(mod1, 
        newdata = data.frame(lstat = c(5, 10, 15)), interval = "pred")

# Part 5

plot(Boston$lstat, Boston$medv, col = "grey")

abline(mod1, col = "red", lwd = 2)

# Nichtlineares Modell für die Ränder vermutlich besser geeignet. 
# Im Mittleren Wertebereich ist unser Modell einigermaßen okay

# Part 6

par(mfrow = c(2,2))

plot(mod1)

# Exercise 2 Multivariate Regression Model ####

# Part 1 

mod2 <- lm(medv ~ lstat + age, data = Boston)

summary(mod2)

# Part 2

mod3 <- lm(medv~., data = Boston)

summary(mod3)

# Part 3

library(car)
vif(mod3)

# Part 4

mod4 <- lm(medv~. -age, data = Boston)

summary(mod4)

# Update kann alternativ zu einer Neuschätzung genutzt werden
mod4 <- update(mod3, ~.-age) 

# Exercise 3 Non linear Model ####

# Part 1 estimate the model

mod5 <- lm(medv~ lstat + I(lstat^2), data = Boston)

summary(mod5)

# Part 2 analysis of variance

anova(mod1, mod5) 
# Null Hyphothese wird verworfen => Zweites Modell ist besser

# Part 3 

mod6 <- lm(medv~ poly(lstat, 5), data = Boston)

summary(mod6)






rm(list=ls()) 

# Exercise 4 Qualitative Predictors => Dummys

library(ISLR)
data("Carseats")

# Part a Overview

?Carseats

glimpse(Carseats)

# Part b Linear Model

mod1 <- lm(Sales ~. + Income:Advertising + Price:Age, data = Carseats)

# Der Effekt von Advertising ist abhängig vom Einkommen => Das sagt uns die 
# Interaktion. Je höher das Einkommen desto größer der Effekt vom Advertising

summary(mod1)

# Part c

contrasts(Carseats$ShelveLoc)














# Exercise 4 Logistic Regression ####

rm(list=ls())

data("Smarket")

# Part a Overview

?Smarket

glimpse(Smarket)

summary(Smarket)

# Part b Corrplot ####

cormat <- Smarket[,-9] %>% cor() %>% round(4)

library(reshape2)
melted_cormat <- melt(cormat)
head(melted_cormat)

library(ggplot2)
library(reshape2)

get_upper_tri <- function(cormat){
  cormat[lower.tri(cormat)]<- NA
  return(cormat)
}

reorder_cormat <- function(cormat){
  # Use correlation between variables as distance
  dd <- as.dist((1-cormat)/2)
  hc <- hclust(dd)
  cormat <-cormat[hc$order, hc$order]
}

# Reorder the correlation matrix
cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
# Melt the correlation matrix
melted_cormat <- melt(upper_tri, na.rm = TRUE)
upper_tri <- get_upper_tri(cormat)



cormat <- reorder_cormat(cormat)
upper_tri <- get_upper_tri(cormat)
melted_cormat <- melt(upper_tri, na.rm = TRUE)

ggheatmap <- ggplot(melted_cormat, aes(Var2, Var1, fill = value))+
  geom_tile(color = "white")+
  theme_minimal()+ # minimal theme
  theme(axis.text.x = element_text(angle = 90, vjust = 1, 
                                   size = 12, hjust = 1))+
  coord_fixed()

ggheatmap  +
  theme(
    axis.title.x = element_blank(),
    axis.title.y = element_blank(),
    panel.grid.major = element_blank(),
    panel.border = element_blank(),
    panel.background = element_blank(),
    axis.ticks = element_blank(),
    legend.justification = c(1, 0),
    legend.position = c(0.5, 0.7),
    legend.direction = "horizontal")+
  guides(fill = guide_colorbar(barwidth = 7, barheight = 1,
                               title.position = "top", title.hjust = 0.5))

# Part c Logistic Regression Model ####

glm.fit <- glm(Direction ~. -Year -Today, data = Smarket, family = binomial)

summary(glm.fit)

# Part d Predictions
contrasts(Smarket$Direction)

# Get the probability that Direction == 1
glm.probs <- predict(glm.fit, type = "response")

glm.pred <- rep("Down", length(Smarket$Direction))

glm.pred[glm.probs > 0.5] <- "Up"

# Exercise 6 ####

rm(list=ls())

data("Weekly")

# Part a Overview

glimpse(Weekly)

?Weekly

pairs(Weekly)

cor(Weekly[,-9])

# Part b Logistic Regression

mod.glm <- glm(Direction ~ . -Year -Today, data = Weekly, family = binomial)

contrasts(Weekly$Direction)
summary(mod.glm)

# Part c

glm.probs <- predict(mod.glm, Weekly, type = "response")

glm.pred <- rep("Down", Weekly$Direction %>% length())

glm.pred[glm.probs > 0.5] <- "Up"

table(glm.pred, Weekly$Direction)

# Rows are predictions columns are the real values
# Obacht! This is in sample...

mean(glm.pred == Weekly$Direction)

# Part d Now the fun starts ;)

train <- Weekly[Weekly$Year <= 2008,]
test  <- Weekly[Weekly$Year > 2008,]


logit.fit <- glm(Direction ~ Lag2, data = train, family = binomial)

glm.probs <- predict(logit.fit, test, type = "response")

glm.pred <- rep("Down", length(test$Direction))

glm.pred[glm.probs > 0.5] <- "Up"

table(glm.pred, test$Direction)

mean(glm.pred == test$Direction)

# Linear discriminant analysis as an alternative way

library(Matrix)

lda.fit <- lda(Direction ~ Lag2, data = train)
lda.fit

lda.pred <- predict(lda.fit, test)$class

table(lda.pred, test$Direction)

mean(lda.pred == test$Direction)

# Wir bekommen hier den gleichen wert weil LDA (Komplette Likelihood)  
# und Logistic Regression (Bedingte Likelihood) die gleiche Methode anwenden.