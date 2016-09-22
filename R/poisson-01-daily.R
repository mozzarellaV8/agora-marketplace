# Agora Marketplace Analysis
# Poisson Regression - Monthly, Weekly, Daily counts
# "How much is there? --> How much could there be?"

# load data -------------------------------------------------------------------

library(data.table)
library(sandwich)
library(ggplot2)
library(dplyr)
library(vcd)
library(zoo)

agora <- fread("~/GitHub/agora-data/agora-01b.csv", stringsAsFactors = T)
agora$date <- as.Date(agora$date)

mo <- read.csv("data/MonthlyAll.csv")
mo$month <- as.Date(mo$month)

mo14 <- subset(mo, mo$month <= "2014-12-31")
sum(mo14$count)

length(levels(as.factor(agora$date))) # 307
summary(agora$date)

# poisson distributions ------------------------------------------------------
# remember as lambda get large, poisson approximates to normal

par(mfrow = c(2, 2), mar = c(8, 8, 8, 8), bty = "l", las = 1)
plot(0:10, dpois(0:10, lambda = 2), type = "h")
plot(0:20, dpois(0:20, lambda = 10), type = "h")
plot(0:200, dpois(0:200, lambda = 100), type = "h")
plot(0:1000, dpois(0:01000, lambda = 500), type = "h")

# Daily counts ----------------------------------------------------------------

# daily counts of product listings
daily <- as.data.frame(table(agora$date))
colnames(daily) <- c("date", "count")
daily$date <- as.Date(daily$date)

par(mar = c(6, 6, 6, 6), bty = "l", las = 1, family = "GillSans")
plot(daily$date, daily$count, xlab = "", ylab = "")
# Quite scattered

# create df - all days
p365 <- data.frame(date = seq(as.Date("2014-01-01"), 
                               as.Date("2015-07-07"), 
                               by = "day"))

p365 <- left_join(p365, daily, by = "date")
# 553 observations of 2 variables; many NAs.

# impute missing count values with 5-day moving average.
p365$count <- na.locf(p365$count, na.rm = F)
p365$rmean <- rollmean(p365$count, k = 5, fill = "")
p365$rmean <- round(p365$rmean, digits = 0)

summary(p365$count)
#     Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#       7    1495    5170    6166   10180   20440 

# date breaks for x-axis
dates <- seq(as.Date("2014-01-01"), as.Date("2015-07-01"), by = "month")

p365.01 <- ggplot(p365, aes(date, count)) + 
  geom_point(size = 3, shape = 20, aes(color = count)) +
  theme_minimal(base_size = 12, base_family = "GillSans") +
  theme(axis.text.x = element_text(size = 10, angle = 45, hjust = 1, vjust = 1)) +
  theme(axis.text.y = element_text(size = 12)) +
  theme(axis.title.y = element_text(family = "Times", face = "italic", size = 14, 
                                    margin = margin(0, 20, 0, 0))) +
  theme(plot.margin = unit(c(0.5, 0.5, 0.5, 0.5), "cm")) +
  labs(title = "Agora: Number of Products by Date (5 day moving average)", 
       x = "", y = "number of product pages", color = "page count")

p365.01 + scale_x_date(breaks = dates, labels = c("Jan 2014", "Feb 2014", "Mar 2014",
                                                  "Apr 2014", "May 2014", "June 2014",
                                                  "July 2014", "Aug 2014", "Sept 2014",
                                                  "Oct 2014", "Nov 2014", "Dec 2014",
                                                  "Jan 2015", "Feb 2015", "Mar 2015",
                                                  "Apr 2015", "May 2015", "June 2015",
                                                  "July 2015"))

# Poisson Model - Daily -------------------------------------------------------

# count ~ date
pm01 <- glm(count ~ date, family = poisson(link = "log"), data = p365)
summary(pm01)
# Coefficients:
#                   Estimate    Std. Error z value            Pr(>|z|)    
#  (Intercept) -16.365191595   0.056587869  -289.2 <0.0000000000000002 ***
#  date          0.001533134   0.000003453   444.0 <0.0000000000000002 ***
#     Null deviance: 2321366  on 552  degrees of freedom
# Residual deviance: 2120736  on 551  degrees of freedom
# AIC: 2126272

# Overdispersed, and residual and null deviance are ~200k apart. Stronger
# effects have been observed in the past. Is the 5 day moving average
# flattening things out too much? 

qm01 <- glm(count ~ date, family = quasi(link = "log", variance = "mu^2"), data = p365)
summary(qm01) # same values as Poisson

#                  Estimate  Std. Error t value             Pr(>|t|)    
#   (Intercept) -22.2917276   3.5256951  -6.323       0.000000000532 ***
#   date          0.0018952   0.0002157   8.788 < 0.0000000000000002 ***
#
# (Dispersion parameter for quasi family taken to be 0.6554879)
#
#     Null deviance: 621.99  on 552  degrees of freedom
# Residual deviance: 581.21  on 551  degrees of freedom
# AIC: NA

# rolling mean ~ date
qm02 <- glm(rmean ~ date, family = quasi(link = "log", variance = "mu^2"), data = p365)
summary(qm02)
#                 Estimate Std. Error t value             Pr(>|t|)    
#   (Intercept) -21.978405   2.811012  -7.819   0.0000000000000276 ***
#   date          0.001876   0.000172  10.912 < 0.0000000000000002 ***

# (Dispersion parameter for quasi family taken to be 0.4077019)

#     Null deviance: 355.53  on 548  degrees of freedom
# Residual deviance: 316.25  on 547  degrees of freedom
# (4 observations deleted due to missingness)
# AIC: NA

# linear model ------------------------
lm.daily01 <- lm(count ~ date, data = p365)
summary(lm.daily01)
# Adjusted R-squared:  0.09145 
# whoooo pretty low.

sqrt(0.09145 ) # 0.3051229
# sse.lm.daily01 <- sqrt(sum(lm.daily01$residuals^2)) # 109448.5
# mse.lm.daily01 <- mean(residuals(lm.daily01)^2)     # 
# rmse.lm.daily01 <- sqrt(mse.lm.daily01)             # 
# rss.lm.daily01 <- sum(residuals(lm.daily01)^2)      # 
# rse.lm.daily01 <- sqrt(sum(residuals(lm.daily01)^2) / lm.daily01$df.residual) # 

# fortify models --------------------------------------------------------------
qm01f <- fortify(qm01)
qm01f$fitted.values <- exp(qm01f$.fitted)

qm02f <- fortify(qm02)
qm02f$fitted.values <- exp(qm02f$.fitted)

lm.daily01.f <- fortify(lm.da)

# for ggplot2
p365$lm.fitted <- lm.daily01$fitted.values
p365$qm01.fitted <- qm01$fitted.values

# first two missing values
# p365$qm02.fitted <- qm02$fitted.values

# plot fitted vs. observed ----------------------------------------------------

# quick look: fitted
qm01.p01 <- ggplot(qm01f, aes(date, fitted.values)) + 
  geom_point(aes(date, count)) +
  geom_line(aes(date, fitted.values))

qm01.p01

# quick look: base
plot(p365$date, qm01$fitted.values, ylim = c(0, 22500))
points(p365$date, p365$count, pch = 20, col = "firebrick3")


qm01p <- ggplot(p365, aes(date, count)) + 
  stat_smooth(colour = "gold4", se = F, size = 1, linetype = "dotdash", alpha = 0.75) +
  geom_point(size = 2, colour = "firebrick3", shape = 19, alpha = 0.75) +  
  geom_line(colour = "gold2", linetype = "dashed", size = 1,  aes(date, lm.fitted)) +  
  geom_point(size = 2.75, colour = "deepskyblue3", shape = 2, aes(date, qm01.fitted)) +
  geom_line(colour = "deepskyblue4", linetype = "solid", size = 1.25,  aes(date, qm01.fitted)) +
  geom_rug(aes(date), sides = "bl", size = 0.15, linetype = 1) +
  scale_x_date(breaks = dates, labels = c("2014 - Jan", "Feb", "Mar", "Apr", "May", "June", 
                                          "July", "Aug", "Sept", "Oct", "Nov", "Dec",
                                          "2015 - Jan", "Feb", "Mar", "Apr", 
                                          "May", "June", "July"))

qm01p + theme_minimal(base_size = 12, base_family = "GillSans") +
  theme(axis.text.x = element_text(size = 11, angle = 45, hjust = 1, vjust=1)) +
  theme(axis.text.y = element_text(size = 11)) +
  theme(plot.margin = unit(c(0.25, 0.25, 0.10, 0.25), "cm")) +
  labs(title = substitute(
    paste("QuasiPoisson (", sigma, "=", mu^2, "), Linear, and Loess Regressions on Listing Count ~ Date")), 
       x = "", y = "", fill = "")
