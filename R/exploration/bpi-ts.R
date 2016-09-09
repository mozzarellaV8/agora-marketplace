# Bitcoin Price Index
# Times Series exploration

# load data -------------------------------------------------------------------

library(data.table)

p14 <- fread("~/GitHub/agora-data/ag07-2014.csv", stringsAsFactors = T)
p14$date <- as.Date(p14$date)

bpi <- read.csv("data/bpi/bpi-coindesk.csv")
bpi$Date <- as.Date(bpi$Date)
colnames(bpi) <- c("date", "price")

bpiAg <- read.csv("data/bpi/bpi-Agora.csv")
bpiAg$btcAge <- NULL
colnames(bpiAg) <- c("date", "price")
bpiAg$date <- as.Date(bpiAg$date)

# time series BPI all ---------------------------------------------------------

bpi.ts <- ts(1:2210, frequency = 365, start = c(2010, 7, 18))     # natural numbers
bpi.ts <- ts(bpi$price, frequency = 365, start = c(2010, 7, 18))  # julian dates

# additive decomposition
f <- decompose(bpi.ts, type = "additive")

par(mar = c(8, 8, 8, 8), family = "FranklinGothicSSK")
plot(f$figure, type = "b")
plot(f)

# multiplicative decomposition
fm <- decompose(bpi.ts, type = "multiplicative")
plot(fm$figure, type = "b")
plot(fm)

# monthly frequency ---------------------------------------

bpi.ts12 <- ts(bpi$price, frequency = 12, start = c(2010, 7, 18))
f12 <- decompose(bpi.ts12, type = "additive")
f12m <- decompose(bpi.ts12, type = "multiplicative")

par(mfrow = c(2, 2), las = 1, family = "FranklinGothicSSK")
plot(f12$figure, type = "b", xlab = "",
     main = "Bitcoin Price Index: Additive Decomposition, Monthly")
plot(f12m$figure, type = "b", xlab = "", ylab = "",
     main = "Multiplicative Decomposition, Monthly")
plot(f$figure, type = "b", main = "Additive Decomposition, Daily")
plot(fm$figure, type = "b", ylab = "",
     main = "Multiplicative Decomposition, Daily")




# time series BPI agora -------------------------------------------------------

bpiAg <- subset(bpiAg, bpiAg$date <= "2015-07-07")

# weekly by necessity
ag <- ts(bpiAg$price, frequency = 7, start = c(2014, 01, 01),
             end = c(2015, 7, 7))

# additive decomposition
g <- decompose(ag, type = "additive")
# time series has no or less than 2 periods
# weekly intervals

plot(g$figure, type = "b")
plot(g)

# multiplicative decomposition
gm <- decompose(ag, type = "multiplicative")
plot(gm$figure, type = "b")
plot(gm)

par(mar = c(8, 8, 8, 8), las = 1, family = "FranklinGothicSSK")
plot(bpiAg$date, bpiAg$price, main = "Bitcoin Price Index 2014-01-01 -- 2015-07-07",
     xlab = "", ylab = "Bitcoin Price (USD)")

# monthly - from 2014 subset ------------------------------
ag14 <- subset(bpiAg, bpiAg$date <=  "2014-12-31")
ag2 <- ts(ag14$price, frequency = 12, start = c(2014, 1, 1))

g2 <- decompose(ag2, type = "additive")
g2m <- decompose(ag2, type = "multiplicative")

par(mfrow = c(1, 1), mar = c(0, 0, 0, 0), las = 0)
plot(g2$figure, type = "b")
plot(g2)
plot(g2m)

par(mfrow = c(2, 2), mar = c(8, 8, 8, 8), las = 1, family = "FranklinGothicSSK")
plot(g$figure, type = "b", xlab = "", 
     main = "***Agora BPI*** - Additive Decomposition, Weekly")
plot(gm$figure, type = "b", xlab = "", ylab = "",
     main = "Multiplicative Decomposition, Weekly")
plot(g2$figure, type = "b", main = "Additive Decomposition, Monthly (2014)")
plot(g2m$figure, type = "b", ylab = "",
     main = "Multiplicative Decomposition, Monthly (2014)")

# fit ARIMA -------------------------------------------------------------------

ag2 <- ts(ag14$price, frequency = 12, start = c(2014, 1, 1), 
          end = c(2014, 12, 31))

fitAg <- arima(ag2, order = c(1, 0, 12), 
               list(order = c(2, 1, 0), period = 12))

fc <- predict(fitAg, n.ahead = 12)
upper <- fc$pred + 2 * fc$se
lower <- fc$pred - 2 * fc$se

ts.plot(ag2, fc$pred, upper, lower, col = c(1, 2, 4, 4),
        lty = c(1, 2, 4, 4))

par(mar = c(8, 8, 8, 8))
plot(ag14$date, ag14$price - fitAg$residuals)
points(ag14$date, ag14$price, col = "red3", pch = 19)

