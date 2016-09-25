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

# total monthly counts -------------------------------------------------------

# by month new
jan <- subset(agora, agora$month == "01" & agora$year == "2014")     # 7986
feb <- subset(agora, agora$month == "02" & agora$year == "2014")     # 27300
mar <- subset(agora, agora$month == "03" & agora$year == "2014")     # 18807
apr <- subset(agora, agora$month == "04" & agora$year == "2014")     # 20000
may <- subset(agora, agora$month == "05" & agora$year == "2014")     # 42535
jun <- subset(agora, agora$month == "06" & agora$year == "2014")     # 25422
jul <- subset(agora, agora$month == "07" & agora$year == "2014")     # 39989
aug <- subset(agora, agora$month == "08" & agora$year == "2014")     # 40465
sep <- subset(agora, agora$month == "09")     # 72666
oct <- subset(agora, agora$month == "10")     # 187091
nov <- subset(agora, agora$month == "11")     # 276028
dec <- subset(agora, agora$month == "12")     # 259820

jan15 <- subset(agora, agora$month == "01" & agora$year == "2015")     # 259154
feb15 <- subset(agora, agora$month == "02" & agora$year == "2015")     # 236056
mar15 <- subset(agora, agora$month == "03" & agora$year == "2015")     # 216631
apr15 <- subset(agora, agora$month == "04" & agora$year == "2015")     # 204373
may15 <- subset(agora, agora$month == "05" & agora$year == "2015")     # 118613
jun15 <- subset(agora, agora$month == "06" & agora$year == "2015")     # 219594

mo <- data.frame(month = seq(as.Date("2014-01-01"), by = "month", length.out = 18), 
                 count = c(nrow(jan), nrow(feb), nrow(mar), nrow(apr), 
                           nrow(may), nrow(jun), nrow(jul), nrow(aug),
                           nrow(sep), nrow(oct), nrow(nov), nrow(dec),
                           nrow(jan15), nrow(feb15), nrow(mar15), 
                           nrow(apr15), nrow(may15), nrow(jun15)))

write.csv(mo, file = "data/MonthlyAll.csv", row.names = F)
mo <- read.csv("data/MonthlyAll.csv")

# Poisson - Monthly Count -----------------------------------------------------

# observed mean and variance ------------------------------
mo.avg <- mean(mo$count)    # 127204.9
mo.var <- var(mo$count)     # 10768122019

# mean and variance differ greatly in observed.

mean((mo.avg - mo$count)^2) # 10169893018
sd(mo$count)                # 103769.6

# poisson test - chi-sq -----------------------------------
gf <- goodfit(mo$count, type = "poisson", method = "MinChisq")
summary(gf)
#         X^2     df   P(>X^2)
# Pearson NaN 276977      NaN
# Inconclusive.

chisq.test(mo$count, mo$month)
# data: mo$count and mo$month
# X-squared = 306, df = 289, p-value = 0.2354

# poisson model -------------------------------------------
pmm01 <- glm(count ~ month, family = poisson, data = mo)
summary(pmm01)
# Coefficients:
#                    Estimate    Std. Error z value            Pr(>|z|)    
#   (Intercept) -59.659989945   0.078617100  -758.9 <0.0000000000000002 ***
#   month         0.004359654   0.000004785   911.0 <0.0000000000000002 ***

# Null deviance:     1595676  on 17  degrees of freedom
# Residual deviance:  648429  on 16  degrees of freedom
# AIC: 648669

# quasipoisson model --------------------------------------
qpm01 <- glm(count ~ month, family = quasipoisson(link = "log"), data = mo)
summary(qpm01)
#                 Estimate  Std. Error t value Pr(>|t|)    
#  (Intercept) -59.6599899  16.2745354  -3.666 0.002088 ** 
#  month         0.0043597   0.0009906   4.401 0.000446 ***

#     Null deviance: 1595676  on 17  degrees of freedom
# Residual deviance:  648429  on 16  degrees of freedom
# AIC: NA

mean(qpm01$fitted.values) # 127204.9
var(qpm01$fitted.values)  # 7394493273

var(qpm01$fitted.values) / mean(qpm01$fitted.values)  # 58130.57
# 58130 is approx half the mean of all counts.


# quasipoisson model 02 -----------------------------------
qpm02 <- glm(count ~ month, data = mo,
             family = quasi(link = "log", variance = "mu^2"))
summary(qpm02)
#                  Estimate  Std. Error t value   Pr(>|t|)    
#   (Intercept) -95.8970709  14.0983766  -6.802 0.00000425 ***
#   month         0.0065712   0.0008634   7.611 0.00000105 ***

#     Null deviance: 17.7713  on 17  degrees of freedom
# Residual deviance:  5.1095  on 16  degrees of freedom
# AIC: NA

# quasipoisson model 03 -----------------------------------
qpm03 <- glm(count ~ month, data = mo,
             family = quasi(link = "log", variance = "mu^3"))
summary(qpm03)
#                 Estimate   Std. Error t value    Pr(>|t|)    
#   (Intercept) -118.2612550   16.1177687  -7.337 0.000001671 ***
#   month          0.0079463    0.0009965   7.974 0.000000578 ***
#     Null deviance: 0.000293011  on 17  degrees of freedom
# Residual deviance: 0.000068497  on 16  degrees of freedom

# A bit extreme.

# Tests ---------------------------------------------------

anova(pmm01)
anova(qpm01, test = "F")
#       Df Deviance Resid. Df Resid. Dev
# NULL                     17    1595676
# month  1   947247        16     648429

qt(c(0.025, .975), df = 16)
# -2.119905  2.119905

qt(c(0.025, .975), df = 1:16)

t.test(mo$count, qpm01$fitted.values)
t.test(pmm01$fitted.values, qpm01$fitted.values)
t.test(pmm01$fitted.values, mo$count)

# Test NULL -----------------------------------------------

beta <- coef(pmm01)
exp(beta[2])
#    month 
# 1.004369

round(exp(confint.default(pmm01)), 5)
#               2.5 %  97.5 %
# (Intercept) 0.00000 0.00000
# month       1.00436 1.00438

# 0% change in mean, with 0.43% - 0.44% increase at 95% confidence interval.

# compare to linear model ---------------------------------
lmm01 <- lm(count ~ month, data = mo)
summary(lmm01)
#                      Estimate  Std. Error t value  Pr(>|t|)    
# (Intercept)       -8162733.53  1586842.22  -5.144 0.0000980 ***
#       month            507.69       97.18   5.224 0.0000835 ***

# Residual standard error:     65020 on 16 degrees of freedom
# Multiple R-squared:     0.6304,	Adjusted R-squared:  0.6073 
# F-statistic:     27.29 on 1 and 16 DF,  p-value: 0.00008347

# Look at mean and variance differences -------------------
pmm01f <- fortify(pmm01)
qpm01f <- fortify(qpm01)
lmm01f <- fortify(lmm01)

daily.b <- subset(daily, daily$date < "2015-07-01")
summary(daily.b)

pmm.avg <- mean(exp(pmm01f$.fitted))  # 127204.9
pmm.var <- var(exp(pmm01f$.fitted))   # 7394493273

var.diff <- mo.var - pmm.var          # 3373628746
deviance.diff <- 1595676 - 648429     # 947247

# no difference in average between fitted and observed values,
# but a difference of 3373628746 between variances?
# difference of 947247 observed between NULL and Residual deviances.

# plot observed and fitted --------------------------------
par(mfrow = c(1, 1), mar = c(7, 7, 7, 7), family = "GillSans", bty = "l", las = 1)
plot(pmm01f$month, pmm01f$count, xlab = "", ylab = "",
     col = "deepskyblue4", cex = 1.4, pch = 1,
     main = "Quasi/Poisson Model • Observed v. Fitted values • Listing Count ~ Month")

points(pmm01f$month, pmm01f$count, pch = 20, col = "deepskyblue4", cex = 1)
lines(pmm01f$month, pmm01f$count, lty = 6, lwd = 0.25, col = "deepskyblue4")
# linear
points(lmm01f$month, lmm01f$.fitted, col = "gold3", pch = 20, cex = 0.75)
points(lmm01f$month, lmm01f$.fitted, col = "gold3", pch = 21, cex = 1.2)
lines(lmm01f$month, lmm01f$.fitted, col = "gold3", lty = 3, cex = 1.0)
# poisson
points(pmm01f$month, exp(pmm01f$.fitted), col = "Firebrick3", pch = 18, cex = 1.8)
points(pmm01f$month, exp(pmm01f$.fitted), col = "Firebrick3", pch = 5, cex = 1.8)
lines(pmm01f$month, exp(pmm01f$.fitted), col = "Firebrick3", lty = 2, cex = 1.0)
# quasipoisson
points(qmm01f$month, exp(qpm01f$.fitted), col = "Firebrick3", pch = 18, cex = 1.8)
points(qpm01f$month, exp(qpm01f$.fitted), col = "Firebrick3", pch = 5, cex = 1.8)
lines(qpm01f$month, exp(qpm01f$.fitted), col = "Firebrick3", lty = 2, cex = 1.0)
# add daily values for comparison
rug(daily.b$date, ticksize = -0.01, side = 1, lwd = 0.75)
points(daily.b$date, daily.b$count, pch = 20, col = "#00000050", cex = 0.8)

# plot residuals ------------------------------------------

par(mfrow = c(2, 2), mar = c(6, 6, 6, 6), family = "GillSans", bty = "l", las = 1)
plot(pmm01)

plot(pmm01f$month, pmm01f$.resid, xlab = "", ylab = "",
     col = "deepskyblue4", cex = 1.4, pch = 1, ylim = 
       main = "Poisson Model • Observed v. Residual values • Listing Count ~ Month")
points(pmm01f$month, pmm01f$count)

write.csv(qpm01f, file = "quasipoisson-qpm01f.csv", row.names = F)