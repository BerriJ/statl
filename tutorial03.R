# Exercicse 14 ####

# a)

library(ISLR)
library(boot)

# Set seed

set.seed(1)

# Fit model

glm.fit <- glm(default ~ income + balance, data = Default, family = "binomial")

model <- glm.fit %>% summary()

# Obtain standard errord

model$coefficients[,2]

# b)

boot.fn <- function(data, index){
  glm(default ~ income + balance, data = data, 
      family = "binomial", subset = index) %>%
    coefficients()
}

# Funktion testen 
boot.fn(Default, 1:nrow(Default))

# c)

bootstrap <- boot(data = Default, statistic = boot.fn, R = 1000)
bootstrap

# Sometimes we can't compute standard errors. In that case bootstrap is nice 
# because it allows us to estimate them and use them for example to construct
# test statistics

# Digression: Parallelization
install.packages("tictoc")
library(tictoc)
tic()
bootstrap <- boot(data = Default, statistic = boot.fn, R = 1000)
toc()

ncores <- parallel::detectCores()

tic()
bootstrap_par <- boot(data = Default, statistic = boot.fn, R = 1000,
                      parallel = "multicore", ncpus = ncores)
toc()

# d)

model$coefficients[,2]
bootstrap

library(microbenchmark)

microbenchmark(
  replicate(1000, rnorm(1000)),
  replicate(1000, rnorm(100))
)

# Exercise 15

# Boston Housing Bootstrap

rm(list = ls())

library(MASS)
library(boot)

# a)

mu <- mean(Boston$medv)
mu

# b)

# Compute the standard error of \hat{mu}

n <- length(Boston$medv)
sd(Boston$medv)/sqrt(n)

# c)

# estimate standard error of \hat{mu} using the bootstrap

boot.fn <- function(data, index) {
  mean(data[index])
}

bootstrap <- boot(data = Boston$medv, statistic = boot.fn, R = 1000)

# d)

# using t.test()
t.test(Boston$medv)$conf

# estimate
mu <- bootstrap$t0

# Standard error using bootstrap

SE <- sd(bootstrap$t)

# confidence interval (based on bootsrap)

c("2.5%" = mu - 2* SE, "97.5%" = mu + 2 * SE)

# e) 

med <- median(Boston$medv)
med

hist(Boston$medv)

# f)

boot.fn <- function(data, index) {
  median(data[index])
}

set.seed(2)
bootstrap <- boot(data = Boston$medv, statistic = boot.fn, R = 1000)

# g), # h)

quantile(Boston$medv, p = 0.1)

boot.fn <- function(data, index) {
  quantile(data[index], p = 0.1)
}

bootstrap <- boot(data = Boston$medv, statistic = boot.fn, R = 1000)

hist(bootstrap$t)

# Exercise 16

library(leaps)
library(ISLR)
rm(list = ls())
set.seed(123)
# a)

# gernerate sample data

n <- 100
x <- rnorm(n)
u <- rnorm(n, 0, 4)

# b)

# generate a response vector

beta0 <- 100
beta1 <- 5
beta2 <- 15
beta3 <- -5

y <- beta0 + beta1*x + beta2*x^2 + beta3*x^3 + u

dat <- data.frame(y, x,  x^2, x^3, x^4, x^5, x^6, x^7, x^8, x^9, x^10)

# c)

# perform best subsert selection

regfit <- regsubsets(y ~ ., data = dat, nvmax = 10)

reg.sum <- summary(regfit)

# adj. R^2, C_p, BIC for the best model with k predictors

R2 <- reg.sum$adjr2
CP <- reg.sum$cp
BIC <- reg.sum$bic

# choose the best model ... 

which.min(R2)
which.max(CP)
which.max(BIC)

# visualize
plot(regfit)

par(mfrow = c(2,2))

plot(reg.sum$rss,
     xlab = "Number of Variables",
     ylab = "RSS",
     type = "l", lwd = 2, col = "Deeppink")
points(which.min(reg.sum$rss), min(reg.sum$rss), pch = 20)

plot(reg.sum$adjr2,
     xlab = "Number of Variables",
     ylab = "adjr2",
     type = "l", lwd = 2, col = "Deeppink")
points(which.max(reg.sum$adjr2), max(reg.sum$adjr2), pch = 20)


plot(reg.sum$cp,
     xlab = "Number of Variables",
     ylab = "cp",
     type = "l", lwd = 2, col = "Deeppink")
points(which.min(reg.sum$cp), min(reg.sum$cp), pch = 20)

plot(reg.sum$bic,
     xlab = "Number of Variables",
     ylab = "bic",
     type = "l", lwd = 2, col = "Deeppink")
points(which.min(reg.sum$bic), min(reg.sum$bic), pch = 20)

# Write Function for prediction using "regsubsets()"

predict.regsubsets <- function(object, newdata, id) {
  from  <- as.formula(object$call[[2]])
  mat   <- model.matrix(from, newdata)
  coefi <- coef(object, id = id)
  mat[, names(coefi)] %*% coefi
}

# prediction on the data set

pred <- predict.regsubsets(regfit, newdata = dat, id = 3)

data.pred <- cbind(dat, pred)
data.pred <- data.pred[order(data.pred$x),]

par(mfrow = c(1,1))

plot(dat$x,
     dat$y,
     col = "darkgrey",
     pch = 20,
     ylim = c(50,250),
     lwd = 2)

lines(data.pred$x, data.pred$pred, col = "deeppink", lwd = 2)

################################################################################
# We conitniue with forward and backward stepwise selection...
################################################################################

# perform backward stepwise selection ####

regfit <- regsubsets(y ~ ., data = dat, nvmax = 10, method = "backward")

reg.sum <- summary(regfit)

# adj. R^2, C_p, BIC for the best model with k predictors

R2 <- reg.sum$adjr2
CP <- reg.sum$cp
BIC <- reg.sum$bic

# choose the best model ... 

which.min(R2)
which.max(CP)
which.max(BIC)

# visualize
plot(regfit)

par(mfrow = c(2,2))

plot(reg.sum$rss,
     xlab = "Number of Variables",
     ylab = "RSS",
     type = "l", lwd = 2, col = "Deeppink")
points(which.min(reg.sum$rss), min(reg.sum$rss), pch = 20)

plot(reg.sum$adjr2,
     xlab = "Number of Variables",
     ylab = "adjr2",
     type = "l", lwd = 2, col = "Deeppink")
points(which.max(reg.sum$adjr2), max(reg.sum$adjr2), pch = 20)


plot(reg.sum$cp,
     xlab = "Number of Variables",
     ylab = "cp",
     type = "l", lwd = 2, col = "Deeppink")
points(which.min(reg.sum$cp), min(reg.sum$cp), pch = 20)

plot(reg.sum$bic,
     xlab = "Number of Variables",
     ylab = "bic",
     type = "l", lwd = 2, col = "Deeppink")
points(which.min(reg.sum$bic), min(reg.sum$bic), pch = 20)

# Write Function for prediction using "regsubsets()"

predict.regsubsets <- function(object, newdata, id) {
  from  <- as.formula(object$call[[2]])
  mat   <- model.matrix(from, newdata)
  coefi <- coef(object, id = id)
  mat[, names(coefi)] %*% coefi
}

# prediction on the data set

pred <- predict.regsubsets(regfit, newdata = dat, id = 3)

data.pred <- cbind(dat, pred)
data.pred <- data.pred[order(data.pred$x),]

par(mfrow = c(1,1))

plot(dat$x,
     dat$y,
     col = "darkgrey",
     pch = 20,
     ylim = c(50,250),
     lwd = 2)

lines(data.pred$x, data.pred$pred, col = "deeppink", lwd = 2)

# Use forward stepwise selection ####

regfit <- regsubsets(y ~ ., data = dat, nvmax = 10, method = "forward")

reg.sum <- summary(regfit)

# adj. R^2, C_p, BIC for the best model with k predictors

R2 <- reg.sum$adjr2
CP <- reg.sum$cp
BIC <- reg.sum$bic

# choose the best model ... 

which.min(R2)
which.max(CP)
which.max(BIC)

# visualize
plot(regfit)

par(mfrow = c(2,2))

plot(reg.sum$rss,
     xlab = "Number of Variables",
     ylab = "RSS",
     type = "l", lwd = 2, col = "Deeppink")
points(which.min(reg.sum$rss), min(reg.sum$rss), pch = 20)

plot(reg.sum$adjr2,
     xlab = "Number of Variables",
     ylab = "adjr2",
     type = "l", lwd = 2, col = "Deeppink")
points(which.max(reg.sum$adjr2), max(reg.sum$adjr2), pch = 20)


plot(reg.sum$cp,
     xlab = "Number of Variables",
     ylab = "cp",
     type = "l", lwd = 2, col = "Deeppink")
points(which.min(reg.sum$cp), min(reg.sum$cp), pch = 20)

plot(reg.sum$bic,
     xlab = "Number of Variables",
     ylab = "bic",
     type = "l", lwd = 2, col = "Deeppink")
points(which.min(reg.sum$bic), min(reg.sum$bic), pch = 20)

# Write Function for prediction using "regsubsets()"

predict.regsubsets <- function(object, newdata, id) {
  from  <- as.formula(object$call[[2]])
  mat   <- model.matrix(from, newdata)
  coefi <- coef(object, id = id)
  mat[, names(coefi)] %*% coefi
}

# prediction on the data set

pred <- predict.regsubsets(regfit, newdata = dat, id = 3)

data.pred <- cbind(dat, pred)
data.pred <- data.pred[order(data.pred$x),]

par(mfrow = c(1,1))

plot(dat$x,
     dat$y,
     col = "darkgrey",
     pch = 20,
     ylim = c(50,250),
     lwd = 2)

lines(data.pred$x, data.pred$pred, col = "deeppink", lwd = 2)

# e)

library(glmnet)

dat <- matrix(
  c(x,x^2,x^3,x^4,x^5,x^6,x^7,x^8,x^9,x^10),
  ncol = 10,
  byrow = FALSE
)

colnames(dat) <- paste("x^", 1:10, sep = "")

head(dat)

# cv

cv.out <- cv.glmnet(dat, y, alpha = 1) # 1 == Lasso...

par(mfrow = c(1,1))

plot(cv.out)


bestlam <- cv.out$lambda.min

lasso.mod <- glmnet(dat, y, alpha = 1, lambda = bestlam)
coef(lasso.mod)

# f)

set.seed(555)

n <- 100

beta0 <- 100
beta7 <- 7

u <- rnorm(n, 0, 7)

y2 <- beta0 + beta7*x^7 + u

dat2 <- data.frame(y, x,  x^2, x^3, x^4, x^5, x^6, x^7, x^8, x^9, x^10)

### best subset selection ####

regfit <- regsubsets(y2 ~., data = dat2, nvmax = 10)

reg.sum <- summary(regfit)

R2 <- reg.sum$adjr2
CP <- reg.sum$cp
BIC <- reg.sum$bic

coef(regfit, which.max(R2))
coef(regfit, which.min(CP))
coef(regfit, which.min(BIC))

### Lasso ###

dat2 <- matrix(
  c(x,x^2,x^3,x^4,x^5,x^6,x^7,x^8,x^9,x^10),
  ncol = 10,
  byrow = FALSE
)

colnames(dat2) <- paste("x^", 1:10, sep = "")
cv.out <- cv.glmnet(dat2, y2, alpha = 1) # 1 == Lasso...
par(mfrow = c(1,1))
plot(cv.out)
bestlam <- cv.out$lambda.min
lasso.mod <- glmnet(dat2, y2, alpha = 1, lambda = bestlam)
coef(lasso.mod)

### Exercise 17 ####

rm(list = ls())
library(leaps)
library(ISLR)

state.x77 <- state.x77 %>% as.data.frame()

# Best subset selection

regfit <- regsubsets(`Life Exp` ~., data = state.x77, nvmax = 10)

reg.sum <- summary(regfit)

R2  <- reg.sum$adjr2
CP  <- reg.sum$cp
BIC <- reg.sum$bic

which.max(R2)
which.min(CP)
which.min(BIC)

coef(regfit, which.max(R2))
coef(regfit, which.min(CP))
coef(regfit, which.min(BIC))

par(mfrow = c(2,2))

plot(reg.sum$rss,
     xlab = "Number of Variables",
     ylab = "RSS",
     type = "l", lwd = 2, col = "Deeppink")
points(which.min(reg.sum$rss), min(reg.sum$rss), pch = 20)

plot(reg.sum$adjr2,
     xlab = "Number of Variables",
     ylab = "adjr2",
     type = "l", lwd = 2, col = "Deeppink")
points(which.max(reg.sum$adjr2), max(reg.sum$adjr2), pch = 20)


plot(reg.sum$cp,
     xlab = "Number of Variables",
     ylab = "cp",
     type = "l", lwd = 2, col = "Deeppink")
points(which.min(reg.sum$cp), min(reg.sum$cp), pch = 20)

plot(reg.sum$bic,
     xlab = "Number of Variables",
     ylab = "bic",
     type = "l", lwd = 2, col = "Deeppink")
points(which.min(reg.sum$bic), min(reg.sum$bic), pch = 20)

# Hier noch mit forward und backward stepwise selection probieren

### Exercise 18 ####

rm(list = ls())

library(leaps)
library(ISLR)
library(glmnet)

# a)

# ?College

# split the data into training and test set

train <- sample(c(T,F), nrow(College), replace = T)
test  <- !train

# besser: subset() !

college.train <- College[train,]
college.test  <- College[test, ] 

# rsq for all approaches:

rsq <- numeric(5)
names(rsq) <- c("linear reg", "ridge", "lasso", "pcr", "pls")

# b)

### fit linear model
lm.fit <- lm(Apps ~ ., data = college.train)

summary(lm.fit)

# prediction
pred <- predict(lm.fit, college.test)

rss <- sum((pred- college.test$Apps)^2)
tss <- sum((college.test$Apps - mean(college.test$Apps))^2)

rsq[1] <- 1 - rss/tss

# c)

college.train.x <- scale(
  model.matrix(
    Apps ~., data = college.train
  )[,-1],
  scale = T, center = T
)

college.test.x <- scale(
  model.matrix(
    Apps ~., data = college.test
  )[,-1],
  scale = T, center = T
)

college.train.Y <- college.train$Apps
college.test.Y  <- college.test$Apps

### fit a ridge regression

cv.out <- cv.glmnet(x = college.train.x, y = college.train.Y, 
                    alpha = 0)

bestlam <- cv.out$lambda.min

ridge.mod <- glmnet(x = college.train.x, y = college.train.Y, 
                    alpha = 0, lambda = bestlam)

# prediction
pred <- predict(ridge.mod, college.test.x, s = bestlam)

rss <- sum((pred- college.test$Apps)^2)
tss <- sum((college.test$Apps - mean(college.test$Apps))^2)

rsq[2] <- 1 - rss/tss


# Lasso

cv.out <- cv.glmnet(x = college.train.x, y = college.train.Y, 
                    alpha = 1)

bestlam <- cv.out$lambda.min

lasso.mod <- glmnet(x = college.train.x, y = college.train.Y, 
                    alpha = 1, lambda = bestlam)

# prediction
pred <- predict(lasso.mod, college.test.x, s = bestlam)

rss <- sum((pred- college.test$Apps)^2)
tss <- sum((college.test$Apps - mean(college.test$Apps))^2)

rsq[3] <- 1 - rss/tss




