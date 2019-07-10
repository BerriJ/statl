# Exercise 20

rm(list=ls())
library(leaps)
library(ISLR)
library(glmnet)
library(boot) # cv.glm

set.seed(1)

# We want to calculate threw polynomials with degree 1 to 10
# We then evaluate which degree performs best with CV

k <- 10
d <- 10

all.deltas <- rep(NA,d)
for(i in 1:d){
  glm.fit <- glm(wage ~ poly(age,i), data = Wage)
  
  all.deltas[i] <- cv.glm(Wage, glm.fit, K = k)$delta[2] # K = folds
}

which.min(all.deltas) # Polynomial degree which fits best

plot(1:d, all.deltas, xlab = "Degree", ylab = "CV R-or", type = "l")

# Anova:

for(i in 1:d){
  
  assign(sprintf("fit.%d", i), lm(wage~poly(age,i), data = Wage))
  
}

anova(fit.1, fit.2, fit.3, fit.4, fit.5,fit.6,fit.7,fit.8,fit.9,fit.10)

agelims <- range(Wage$age)

age.grid <- seq(from = agelims[1], to = agelims[2])

lm.fit <- lm(wage~poly(age,3), data = Wage)

lm.pred <- predict(lm.fit, data.frame(age = age.grid))

plot(wage~age, data = Wage, col = "darkgrey")
lines(age.grid, lm.pred, col = "deeppink", lwd = 2)

# b) step function

c <- 10
all.cvs <- rep(NA,c)

for(i in 2:(c-1)){
  
  Wage$age.cut <- cut(Wage$age, i)
  
  lm.fit <- lm(wage~ age.cut, data = Wage)
  all.cvs[i] <- cv.glm(Wage, lm.fit, K = k)$delta[2]
}

plot(1:10, all.cvs)
