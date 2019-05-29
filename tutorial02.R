# Exerciese 11 ####

library(ISLR)

data(Default)

# a) einfaches logistisches regressionsmodell

glm.fit <- glm(default ~ income + balance, data = Default, family = "binomial")

# b) validation (test) set approach 

set.seed(1)

# index for training set observations

train <- sample(nrow(Default), floor(0.75*nrow(Default)))

Default.train <- Default[train,]
Default.test <- Default[-train,]

# Fit logistic model
glm.fit <- glm(default ~ income + balance, data = Default.train, family = "binomial")
glm.fit

# obtain estimates
glm.probs <- predict(glm.fit, Default.test, type = "response")

# classify
glm.pred <- ifelse(glm.probs > 0.5, "Yes", "No")

# compute validation set error
mean(glm.pred != Default.test$default)

# c) now with a loop

test.error <- c()
m <- 3

set.seed(1)

for(i in 1:m){
  # seed
  
  # split the data
  train <- sample(nrow(Default), floor(0.75*nrow(Default)))
  Default.train <- Default[train,]
  Default.test <- Default[-train,]
  
  # Fit logistic model
  glm.fit <- glm(default ~ income + balance, data = Default.train, family = "binomial")
  
  # obtain estimates
  glm.probs <- predict(glm.fit, Default.test, type = "response")
  
  # classify
  glm.pred <- ifelse(glm.probs > 0.5, "Yes", "No")
  
  # compute validation set error
  test.error[i] <- mean(glm.pred != Default.test$default)
  print(test.error[i])
}

#evaluation
mean(test.error)

# d) 

set.seed(1)

# index for training set observations

train <- sample(nrow(Default), floor(0.75*nrow(Default)))

Default.train <- Default[train,]
Default.test <- Default[-train,]

# Fit logistic model
glm.fit <- glm(default ~ income + balance + student, data = Default.train, family = "binomial")
glm.fit

# obtain estimates
glm.probs <- predict(glm.fit, Default.test, type = "response")

# classify
glm.pred <- ifelse(glm.probs > 0.5, "Yes", "No")

# compute validation set error
mean(glm.pred != Default.test$default)

# c) now with a loop

test.error.student <- c()
m <- 3

set.seed(1)

for(i in 1:m){
  # seed
  
  # split the data
  train <- sample(nrow(Default), floor(0.75*nrow(Default)))
  Default.train <- Default[train,]
  Default.test <- Default[-train,]
  
  # Fit logistic model
  glm.fit <- glm(default ~ income + balance + student, data = Default.train, family = "binomial")
  
  # obtain estimates
  glm.probs <- predict(glm.fit, Default.test, type = "response")
  
  # classify
  glm.pred <- ifelse(glm.probs > 0.5, "Yes", "No")
  
  # compute validation set error
  test.error.student[i] <- mean(glm.pred != Default.test$default)
  print(test.error[i])
}

#evaluation
mean(test.error.student)

rm(list = ls())

# Exercise 12 ####
# Leave one out cross validation (loocv)

data("Weekly")

# a)

glm.fit <- glm(Direction ~ Lag1 + Lag2, data = Weekly, family = "binomial")

# b)

# Update model such that the first observation os omitted
glm.fit2 <- update(glm.fit, subset = -1)


# c)

# test observation: "the one to be left out"
Weekly.test <- Weekly[1,]

# predict response using the model from exercise b

glm.prob <- predict(glm.fit2, Weekly.test, type = "response")

glm.pred <- ifelse(glm.prob > 0.5, "Up", "Down")

glm.pred == Weekly.test$Direction

# d)

# initilize vector for class membership

class <- c()

# init loop

for(i in 1:nrow(Weekly)){
  # Update model such that the first observation os omittedglm.fit <- glm(Direction ~ Lag1 + Lag2, data = Weekly, family = "binomial")
  glm.fitti <- update(glm.fit, subset = -i)
  Weekly.test <- Weekly[i,]
  glm.prob <- predict(glm.fitti, Weekly.test, type = "response")
  glm.pred <- ifelse(glm.prob > 0.5, "Up", "Down")
  cat(glm.pred != Weekly.test$Direction)
  class[i] <- glm.pred != Weekly.test$Direction
}

# e)

mean(class)

# f) 

# 5 Fold Cross-Validation

# seed 
set.seed(1)

# sample indices
K <- 5

Folds <- matrix(ncol = 2,
                nrow = nrow(Weekly))

Folds[,1] <- sample(1:nrow(Weekly), nrow(Weekly), replace = F)

Folds[,2] <- c(
  rep(1:K, floor(nrow(Weekly)/K)),
  1:(nrow(Weekly)-floor(nrow(Weekly)/K)*K)
)

class <- c()

for(i in 1:K){
  # Update model such that the first observation os omittedglm.fit <- glm(Direction ~ Lag1 + Lag2, data = Weekly, family = "binomial")
  index <- Folds[,1][which(Folds[,2] == i)]
  glm.fitti <- update(glm.fit, subset = -index)
  Weekly.test <- Weekly[index,]
  glm.prob <- predict(glm.fitti, Weekly.test, type = "response")
  glm.pred <- ifelse(glm.prob > 0.5, "Up", "Down")
  cat(glm.pred != Weekly.test$Direction)
  class[i] <- mean(glm.pred != Weekly.test$Direction)
}

mean(class)

# 10 Fold Cross-Validation

# seed 
set.seed(1)

# sample indices
K <- 10

Folds <- matrix(ncol = 2,
                nrow = nrow(Weekly))

Folds[,1] <- sample(1:nrow(Weekly), nrow(Weekly), replace = F)

Folds[,2] <- c(
  rep(1:K, floor(nrow(Weekly)/K)),
  1:(nrow(Weekly)-floor(nrow(Weekly)/K)*K)
)

class <- c()

for(i in 1:K){
  # Update model such that the first observation os omittedglm.fit <- glm(Direction ~ Lag1 + Lag2, data = Weekly, family = "binomial")
  index <- Folds[,1][which(Folds[,2] == i)]
  glm.fitti <- update(glm.fit, subset = -index)
  Weekly.test <- Weekly[index,]
  glm.prob <- predict(glm.fitti, Weekly.test, type = "response")
  glm.pred <- ifelse(glm.prob > 0.5, "Up", "Down")
  cat(glm.pred != Weekly.test$Direction)
  class[i] <- mean(glm.pred != Weekly.test$Direction)
}

mean(class)

# Useful package for CV:

# "boot"

# Useful package for different ML Algos

# "SuperLearner"

# https://topepo.github.io/caret/

# Exerciese 13 ####

rm(list = ls())
library(boot)

# a)

set.seed(1)
n <- 10000
u <- rnorm (n)
x <- rnorm (n)
y <- x-2*x^2+u


# b), c)

# maximum degree of polynomial

pol <- 10
folds <- c(n, 5, 10)

# plot

plot(x,y, col = "darkgrey", pch = 16)

# gather (sorted) data in a data.frame

Dat <- data.frame(y,x)[order(x), ]

# colors
color <- c("cyan3", "dodgerblue1", "deeppink")

# vector for saving the degree of polynomial with the smallest mse

Degree.min <- c()

# loop over the three different K for K-fold-CV
for(k in 1:length(folds)){
  #K should be n, 5 or 10
  K <- folds[k]
  
  # kfold, vector to save the rsults of k-fold-cv for each polynomial
  kfold <- c()
  
  for(j in 1:pol){
    #fit polynomial model
    glm.fit <- glm(y ~ poly(x, degree = j), data = Dat)
    
    # save
    kfold[j] <- cv.glm(Dat, glm.fit, K = K)$delta[1]
    
  }
  
  #plot the results for k-fold-cv
  
  plot(1:pol, kfold, type = "l", col = color[k],
       lwd = 2, ylim = c(0,10))
  # graphical par new = T such that new plots arent drawn in new device
  par(new = TRUE)
  
  Degree.min[k] <- which.min(kfold)
}

# add a legend

legend("topleft", 
       legend = c("LOOCV", "5-Fold-CV", "10-Fold-CV"),
       col = color, lty = 1, lwd = 2)

names(Degree.min) <- c("LOOCV", "5-Fold-CV", "10-Fold-CV")

Degree.min

# d)

# initialize matrix of significaces 
Signif <- matrix(NA, ncol = pol + 1, nrow = pol)

# set column names
colnames(Signif) <- c("Intercept", 
                      paste("X^",1:10, sep = ""))

for(j in 1:pol){
  
  #fit polynomial model
  glm.fit <- glm(y ~ poly(x, degree = j), data = Dat)
  
  # write "TRUE" if coef is significantly different from 0
  Signif[j, 1:(j+1)] <- summary(glm.fit)$coef[,4] < 0.05 
  
}

Signif

# Keine geeignete Lösung da die P-Werte lediglich den in Sample fit betrachten
# CV ist deutlich besser geeignet!