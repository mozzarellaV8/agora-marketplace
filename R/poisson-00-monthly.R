# Agora Marketplace Analysis
# Poisson Regression
# "How much is there? --> How much could there be?"

# load data -------------------------------------------------------------------

library(data.table)
library(sandwich)
library(ggplot2)

agora <- fread("~/GitHub/agora-data/agora-01b.csv", stringsAsFactors = T)
# mo <- fread("data/MonthlyCounts-15.csv", stringsAsFactors = T)
# mo$month.1 <- NULL
# mo14 <- fread("data/MonthlyCounts.csv")

# poisson distributions ------------------------------------------------------
# remember as lambda get large, poisson approximates to normal

par(mfrow = c(2, 2), mar = c(8, 8, 8, 8), bty = "l", las = 1)
plot(0:10, dpois(0:10, lambda = 2), type = "h")
plot(0:20, dpois(0:20, lambda = 10), type = "h")
plot(0:200, dpois(0:200, lambda = 100), type = "h")
plot(0:1000, dpois(0:01000, lambda = 500), type = "h")

# full monthly subsets -------------------------------------------------------

jan14 <- subset(agora, agora$month == "01" & agora$year == "2014")     # 7986
feb14 <- subset(agora, agora$month == "02" & agora$year == "2014")     # 27300
mar14 <- subset(agora, agora$month == "03" & agora$year == "2014")     # 18807
apr14 <- subset(agora, agora$month == "04" & agora$year == "2014")     # 20000
may14 <- subset(agora, agora$month == "05" & agora$year == "2014")     # 42535
jun14 <- subset(agora, agora$month == "06" & agora$year == "2014")     # 25422
jul14 <- subset(agora, agora$month == "07" & agora$year == "2014")     # 39989
aug14 <- subset(agora, agora$month == "08" & agora$year == "2014")     # 40465
sep14 <- subset(agora, agora$month == "09" & agora$year == "2014")     # 72666
oct14 <- subset(agora, agora$month == "10" & agora$year == "2014")     # 187091
nov14 <- subset(agora, agora$month == "11" & agora$year == "2014")     # 276028
dec14 <- subset(agora, agora$month == "12" & agora$year == "2014")     # 276978
jan15 <- subset(agora, agora$month == "01" & agora$year == "2015")     # 259154
feb15 <- subset(agora, agora$month == "02" & agora$year == "2015")     # 236056
mar15 <- subset(agora, agora$month == "03" & agora$year == "2015")     # 216631

mo <- data.frame(month = seq(as.Date("2014-01-01"), by = "month", length.out = 15), 
                 count = c(nrow(jan14), nrow(feb14), nrow(mar14), nrow(apr14), 
                           nrow(may14), nrow(jun14), nrow(jul14), nrow(aug14),
                           nrow(sep14), nrow(oct14), nrow(nov14), nrow(dec14),
                           nrow(jan15), nrow(feb15), nrow(mar15)))

write.csv(mo, file = "data/MonthlyCounts-15.csv", row.names = F)

par(mfrow = c(1, 1), mar = c(8, 8, 8, 8), las = 1, family = "FranklinGothicSSK")
plot(mo$month, mo$count, type = "b", pch = 2, cex = 3.0, ylab = "", xlab = "",
     main = "Listing Count by Month - AgMarket, Jan 2014 - Mar 2015")

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