# Agora Marketplace Analysis
# Poisson Regression
# "How much is there? --> How much could there be?"

# load data -------------------------------------------------------------------

library(data.table)
library(sandwich)
library(ggplot2)

agora <- fread("~/GitHub/agora-data/agora-01b.csv", stringsAsFactors = T)
agora$date <- as.Date(agora$date)

length(levels(as.factor(agora$date))) # 307
summary(agora$date)

# daily countes of 
daily <- as.data.frame(table(agora$date))
colnames(daily) <- c("date", "count")
daily$date <- as.Date(daily$date)

# poisson distributions ------------------------------------------------------
# remember as lambda get large, poisson approximates to normal

par(mfrow = c(2, 2), mar = c(8, 8, 8, 8), bty = "l", las = 1)
plot(0:10, dpois(0:10, lambda = 2), type = "h")
plot(0:20, dpois(0:20, lambda = 10), type = "h")
plot(0:200, dpois(0:200, lambda = 100), type = "h")
plot(0:1000, dpois(0:01000, lambda = 500), type = "h")

# plot daily counts -----------------------------------------------------------






# poisson fit 01 --------------------------------------------------------------

# poisson 01
pm01 <- glm(count ~ month, family = "poisson", data = mo)
summary(pm01)

# poisson 02 - as.Date
mo$month <- as.Date(mo$month)
pm02 <- glm(count ~ month, family = poisson(link = "log"), data = mo)
summary(pm02)

# quasipoisson
qp01 <- glm(count ~ month, quasipoisson(link = "log"), data = mo)
summary(qp01)

# linear model
lm01 <- lm(count ~ month, data = mo)
summary(lm01)
sqrt(0.773) # 0.8792042
sse.lm01 <- sqrt(sum(lm01$residuals^2)) # 194570.3
mse.lm01 <- mean(residuals(lm01)^2)     # 2523840022
rmse.lm01 <- sqrt(mse.lm01)             # 50237.83
rss.lm01 <- sum(residuals(lm01)^2)      # 37857600331
rse.lm01 <- sqrt(sum(residuals(lm01)^2) / lm01$df.residual) # 53964.09

# add fitted values to dataframe
mo$pm01.fitted <- pm01$fitted.values
mo$pm02.fitted <- pm02$fitted.values
mo$qp01.fitted <- qp01$fitted.values
mo$lm.fitted <- lm01$fitted.values

# fortify on pm02
head(fortify(pm02))
pm02f <- fortify(pm02)
pm02f

pm02fp <- ggplot(pm02f, aes(month, .fitted)) + geom_point()
pm02fp

# linear - interesting. .fitted values already exponentiated? 
# add fortify into the existing model

# plot fitted vs. observed ----------------------------------------------------

pm01p <- ggplot(mo, aes(month, count)) + 
  stat_smooth(colour = "gold3", se = F, size = 0.65, linetype = "dotdash") +            # loess
  geom_point(size = 2.25, colour = "firebrick4", shape = 19) +                          # observed
  geom_point(size = 4, colour = "firebrick4", shape = 1) +                              # observed
  geom_point(size = 3.5, colour = "lightblue3", shape = 15, aes(month, lm.fitted)) +    # linear
  geom_line(colour = "lightblue2", linetype = "dotted", aes(month, lm.fitted)) +        # linear
  geom_point(size = 4.75, colour = "deepskyblue4", shape = 17,aes(month, pm02.fitted))+ # poisson
  geom_line(colour = "deepskyblue4", linetype = "solid", aes(month, pm02.fitted)) +     # poisson
  scale_x_date(breaks = mo$month, labels = c("2014 - Jan", "February", "March",
                                             "April", "May", "June", "July", "August",
                                             "September", "October", "November", "December",
                                             "2015 - Jan", "February", "March"))

pm01p + theme_minimal(base_size = 14, base_family = "GillSans") +
  theme(axis.text.x = element_text(angle = 45, size = 14)) +
  theme(plot.margin = unit(c(1.25, 1.25, 0.10, 1.25), "cm")) +
  labs(title = "Poisson, Linear, and Loess Regressions on listing count ~ month", 
       x = "", y = "", fill = "")

# poisson model on daily values -----------------------------------------------

pmd01 <- glm()