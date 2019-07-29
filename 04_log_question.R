rm(list = ls())

load("00_data/Wein_train.rda")

plot(Wein_train$litre)

# Warum sind die folgenden beiden nicht identisch?

plot(Wein_train$llitre)

plot(log(Wein_train$llitre))
