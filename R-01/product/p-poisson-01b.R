# Agora Marketplace Analysis
# Poisson Regression Prep
# "How much is there?" --> "How much could there be?"

# load data -------------------------------------------------------------------

library(data.table)
library(sandwich)
library(ggplot2)
library(dplyr)
library(zoo)


p15 <- fread("~/GitHub/agora-data/ag15-02.csv", stringsAsFactors = T)
p15$date <- as.Date(p15$date)
p14 <- fread("~/GitHub/agora-data/ag14-01.csv", stringsAsFactors = T)
p14$date <- as.Date(p14$date)
mo <- read.csv("data/MonthlyCounts.csv")
mo$month <- as.Date(mo$month)

bpi <- read.csv("data/bpi/bpi-coindesk.csv")
colnames(bpi) <- c("date", "price")
bpi$date <- as.Date(bpi$date)
bpi365 <- subset(bpi, bpi$date >= "2014-01-01" & bpi$date <= "2014-12-31")

# poisson distributions ------------------------------------------------------
# remember as lambda get large, poisson approximates to normal

par(mfrow = c(2, 2), mar = c(8, 8, 8, 8), bty = "l", las = 1)
plot(0:10, dpois(0:10, lambda = 2), type = "h")
plot(0:20, dpois(0:20, lambda = 10), type = "h")
plot(0:200, dpois(0:200, lambda = 100), type = "h")
plot(0:1000, dpois(0:01000, lambda = 500), type = "h")

# crawl counts ----------------------------------------------------------------
# "How much is there?"

# current counts and dates
daily <- as.data.frame(table(p14$date))
colnames(daily) <- c("date", "count")
daily$date <- as.Date(daily$date)

# 365 day table
p14.365 <- data.frame(date = seq(as.Date("2014-01-01"), by = "day", length.out = 365))
p14.365 <- dplyr::left_join(p14.365, daily, by = "date")
# write.csv(p14.365, file = "p14-daily-00.csv", row.names = F)
p14.365 <- fread("p14-daily-01.csv")
p14.365$date <- as.Date(p14.365$date)

summary(p14.365$count)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max.    NA's 
#       7     929    3534    4870    7636   18140     149

p14.365$count <- na.locf(p14.365$count, na.rm = F)
p14.365$rmean <- rollmean(p14.365$count, k = 3)
p14.365$rmean <- round(p14.365$rmean, digits = 0)

# remainder from rollmean went back to january
p14.365$rmean[364] <- 17280 # actual count
p14.365$rmean[365] <- 5404  # actual count

# 2015 - daily counts
daily15 <- as.data.frame(table(p15$date))
colnames(daily15) <- c("date", "count")
daily15$date <- as.Date(daily15$date)

p15.365 <- data.frame(date = seq(as.Date("2015-01-01"), by = "day", length.out = 185))
p15.365 <- dplyr::left_join(p15.365, daily15, by = "date")
write.csv(p15.365, file = "p15-daily-00.csv", row.names = F)
p15.365 <- fread("p15-daily-01.csv")
p15.365$date <- as.Date(p15.365$date)

summary(p15.365)

# poisson model - daily counts ------------------------------------------------

dpm01 <- glm(count ~ rmean, family = "poisson", data = p14.365)
summary(dpm01)
#     Null deviance: 1440478  on 364  degrees of freedom
# Residual deviance:  507717  on 363  degrees of freedom
# AIC: 511076

dpm02 <- glm(count ~ rmean, family = "quasipoisson", data = p14.365)
summary(dpm02)

plot(p14.365$date, dpm01$fitted.values)

dpmd01 <- glm(rmean ~ date, family = "poisson", data = p14.365)
summary(dpmd01)
# Null deviance: 1122538  on 364  degrees of freedom
# Residual deviance:  475966  on 363  degrees of freedom
# AIC: 479419

dpmd02 <- glm(rmean ~ date, family = "quasipoisson", data = p14.365)
summary(dpmd02)
# same values as above. 

lmd01 <- lm(rmean ~ date, p14.365)
summary(lmd01)
# Multiple R-squared:  0.4859,	Adjusted R-squared:  0.4845 
# F-statistic: 343.1 on 1 and 363 DF,  p-value: < 0.00000000000000022

par(mar = c(8, 8, 8, 8), las = 1, family = "HersheySans")
plot(p14.365$date, p14.365$rmean, type = "p", pch = 19, xlab = "2014", ylab = "",
     main = "Agora Listings: Poisson/Quasipoisson Fitted and Observed Values")
points(p14.365$date, dpmd01$fitted.values, pch = 1, col = "red3")
points(p14.365$date, lmd01$fitted.values, pch = 3, col = "deepskyblue3")

points(p14.365$date, log(bpi365$price), pch = 3, col = "deepskyblue2")


