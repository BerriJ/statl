rm(list = ls())

load("00_data/Wein_train.rda")

plot(Wein_train$litre)

# Warum sind die folgenden beiden nicht identisch?

plot(Wein_train$llitre)

plot(log(Wein_train$llitre))


# Der Einfachalthalber habe ich nur die ersten n Beobachtungen betrachtet

plot(Wein_train$litre[1:100])

# Warum sind die folgenden beiden nicht identisch?
par(mfrow= c(2,1))

plot(Wein_train$llitre[1:100],ylim = c(1000000,10000000))

plot(log(as.numeric(Wein_train$litre[1:100]))*1000000, ylim = c(1000000,10000000))

diff <- (Wein_train$llitre-log(as.numeric(Wein_train$litre))*1000000)[1:100]

diff # Abgesehen von einigen Ausreißern (besonders niedrige Werte) (siehe Grafiken) gute Näherung

par(mfrow= c(1,1))
hist(diff, freq = F)

