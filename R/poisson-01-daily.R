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

# daily counts of 
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
  geom_point(size = 3.75, shape = 17, aes(color = count)) +
  scale_x_date(breaks = dates) +
  theme_minimal(base_size = 14, base_family = "GillSans") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1)) +
  theme(axis.title.y = element_text(family = "Times", face = "italic", size = 14, 
                                    margin = margin(0, 20, 0, 0))) +
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm")) +
  labs(title = "Agora: Number of Products by Date (5 day moving average)", 
       x = "", y = "number of product pages", color = "product\ncount")

p365.01

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

pm01q <- glm(count ~ date, family = quasipoisson(link = "log"), data = p365)
summary(pm01q) # same values as Poisson

# rolling mean ~ date
pm02 <- glm(rmean ~ date, family = poisson(link = "log"), data = p365)
summary(pm02)

pm02q <- glm(rmean ~ date, family = "quasipoisson", data = p365)
summary(pm02q)
#     Null deviance: 1423407  on 548  degrees of freedom
# Residual deviance: 1228777  on 547  degrees of freedom
# (4 observations deleted due to missingness)
# AIC: 1234404


# linear model ------------------------
lm01 <- lm(count ~ date, data = p365)
summary(lm01)


sqrt(0.0931) # 0.3051229
sse.lm01 <- sqrt(sum(lm01$residuals^2)) # 109448.5
mse.lm01 <- mean(residuals(lm01)^2)     # 
rmse.lm01 <- sqrt(mse.lm01)             # 
rss.lm01 <- sum(residuals(lm01)^2)      # 
rse.lm01 <- sqrt(sum(residuals(lm01)^2) / lm01$df.residual) # 

# add fitted values to dataframe
p365$pm01.fitted <- pm01$fitted.values
p365$pm02.fitted <- pm02$fitted.values
p365$lm.fitted <- lm01$fitted.values

# fortify on pm02
head(fortify(pm01))
pm01f <- fortify(pm01)
pm02f <- fortify(pm02)



pm01.p01 <- ggplot(pm01f, aes(date, .fitted)) + geom_point()
pm01.p01
# linear - interesting. .fitted values already exponentiated? 
# add fortify into the existing model

plot(p365$date, pm01$fitted.values, ylim = c(0, 25000))

# plot fitted vs. observed ----------------------------------------------------

pm01p <- ggplot(p365, aes(date, count)) + 
  stat_smooth(colour = "gold3", se = F, size = 1, linetype = "dotdash") +
  geom_point(size = 1.5, colour = "firebrick3", shape = 20) + 
  geom_point(size = 0, colour = "firebrick4", shape = 1) +  
  geom_line(colour = "gray23", linetype = "dashed", size = 1,
            aes(date, lm.fitted)) +  
  geom_point(size = 0, colour = "deepskyblue3", shape = 17, 
             aes(date, pm01.fitted)) +
  geom_line(colour = "deepskyblue4", linetype = "solid", size = 2,
            aes(date, pm01.fitted)) +     # poisson
  scale_x_date(breaks = dates, labels = c("2014 - Jan", "February", "March",
                                          "April", "May", "June", "July", "August",
                                          "September", "October", "November", "December",
                                          "2015 - Jan", "February", "March", "April",
                                          "May", "June", "July"))

pm01p + theme_minimal(base_size = 14, base_family = "GillSans") +
  theme(axis.text.x = element_text(angle = 35, size = 12, hjust = 1)) +
  theme(plot.margin = unit(c(1.25, 1.25, 0.10, 1.25), "cm")) +
  labs(title = "Poisson, Linear, and Loess Regressions on Listing Count ~ Date", 
       x = "", y = "", fill = "")

