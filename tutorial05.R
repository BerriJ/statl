# Exercise 21

library(ISLR) # data
library(boot)
library(MASS) # data
library(splines)

attach(Boston)

# a)

lm.fit <- lm(nox~poly(dis,3), data = Boston)

lm.fit %>% summary()

dislim <- range(dis)
dis.grid <- seq(dislim[1], dislim[2], by = 0.1)

lm.pred <- predict(lm.fit, newdata = list(dis = dis.grid))
plot(nox~dis, data = Boston, col = "grey13")
lines(dis.grid, lm.pred, col = "deeppink", lwd = 2)

# b) 

d <- 10
all.rss <- rep(NA,d)

for(i in 1:d){
  
  lm.fit <- lm(nox~poly(dis,i), data = Boston)
  lm.pred <- predict(lm.fit, newdata = list(dis = dis.grid))
  all.rss[i] <- sum(lm.fit$residuals^2)
  plot(nox~dis, data = Boston, col = "grey13")
  lines(dis.grid, lm.pred, col = terrain.colors(n = 10)[i], lwd = 2)
  locator(1)
}

which.min(all.rss)

# c)

all.deltas <- rep(NA,d)
k <- 10
set.seed(1234)

for(i in 1:d){
  glm.fit <- glm(nox~poly(dis,i), data = Boston)
  all.deltas[i] <- cv.glm(Boston, glm.fit, K = k)$delta[2]
}

plot(all.deltas, type = "l")


# d) fit a bspline

sp.fit <- glm(nox~bs(dis, df = 4), data = Boston)
summary(sp.fit)

attributes(bs(dis, df = 4))$knots

sp.pred <- predict(sp.fit, list(dis = dis.grid))

plot(nox~dis, data = Boston, col = "grey13")
lines(dis.grid, sp.pred, col = "deeppink", lwd = 2)

abline(v = median(Boston$dis), lty = 2)

# Natural Spline

sp.fit <- glm(nox~ns(dis, df = 4), data = Boston)
summary(sp.fit)

attributes(bs(dis, df = 4))$knots

sp.pred <- predict(sp.fit, list(dis = dis.grid))

plot(nox~dis, data = Boston, col = "grey13")
lines(dis.grid, sp.pred, col = "deeppink", lwd = 2)

abline(v = median(Boston$dis), lty = 2)

# e) How many knots should we use???

dfs <- 20

all.rss <- rep(NA, dfs)

for(i in 3:dfs){
  glm.fit <- glm(nox~bs(dis, df = i), data = Boston)
  all.rss[i] <- sum(glm.fit$residuals^2)
  
}

which.min(all.rss[-c(1,2)])

# f)

set.seed(123)
k <- 10 
all.cv <- rep(NA, dfs)

for(i in 3:dfs){
  glm.fit <- glm(nox~bs(dis, df = i), data = Boston)
  all.cv[i] <- cv.glm(Boston, glm.fit, K = k)$delta[2]
  lm.pred <- predict(glm.fit, list(dis = dis.grid))
  plot(nox~dis, data = Boston, col = "grey13")
  lines(dis.grid, lm.pred, col = "deeppink", lwd = 2)
  abline(v = attributes(bs(dis, df =i))$knots, lty = 3)
}

which.min(all.cv)

plot(nox~dis, data = Boston, col = "grey13")
lines(dis.grid, lm.pred, col = "deeppink", lwd = 2)
abline(v = attributes(bs(dis, df =which.min[all.cv]))$knots, lty = 3)

# Exercise 22

library(leaps) # regsubsets

set.seed(1)

test <- sample(1:nrow(College), nrow(College)/4)
train <- -test

College.train <- College[train,]
College.test <- College[test,]

reg.fit <- regsubsets(Outstate~., College.train,
                      nvmax = 17, method = "forward")

reg.summary <- summary(reg.fit)

plot(reg.summary$bic, type = "l") # Ellbow is at 5 or 6
plot(reg.summary$cp, type = "l")
plot(reg.summary$adjr2, type = "l")

coefi <- coef(reg.fit, id = 12)
names(coefi)

good.features <- c("Outstate", "Private", names(coefi)[-c(1,2)])

# b)

library(gam)

pairs(College.train[,good.features])

gam.fit <- gam(Outstate ~ ns(Accept,5) + ns(Apps,3) +
               ns(F.Undergrad,5) + ns(PhD, 3) + ns(Terminal, 3) + Private + 
                 Room.Board + Personal + S.F.Ratio + perc.alumni +
                 Expend + Grad.Rate, data = College.train)

par(mfrow = c(4,3))

plot.Gam(gam.fit, se = T, residuals = T, col = "grey12")

gam.pred <- predict(gam.fit, newdata = College.test)

sqrt(mean(College.test$Outstate - gam.pred)^2)

# Not compare this to a linear model

lm.fit <- lm(Outstate~., data = College.train[,good.features])

lm.pred <- predict(lm.fit, newdata = College.test)

sqrt(mean(College.test$Outstate - lm.pred)^2)

?install.packages
