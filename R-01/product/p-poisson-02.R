# Agora Marketplace Analysis
# Poisson Regression Prep
# "How much is there? --> How much could there be?"

# load data -------------------------------------------------------------------

library(data.table)

p14 <- fread("~/GitHub/agora-data/ag07-2014.csv", stringsAsFactors = T)
mo <- fread("data/MonthlyCounts.csv")

# models ----------------------------------------------------------------------

mean(mo$count) # 84842.42
summary(mo$count)
#   Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   7986   24070   40230   84840  101300  276000

# poisson model - monthly counts ----------------------------------------------

pm01 <- glm(count ~ month, family = "poisson", data = mo)
summary(pm01)
#     Null deviance: 1052014  on 11  degrees of freedom
# Residual deviance:  100674  on 10  degrees of freedom
# mean != variance here.......

pm02 <- glm(count ~ month, family = "quasipoisson", data = mo)
summary(pm02)

#     Null deviance: 1052014  on 11  degrees of freedom
# Residual deviance:  100674  on 10  degrees of freedom

# compare to linear model -----------------------------------------------------
lm01 <- lm(count ~ month, data = mo)
summary(lm01)
# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
#   (Intercept) -1.199e+07  2.511e+06  -4.776 0.000751 ***
#   month        7.438e+02  1.546e+02   4.810 0.000713 ***

# Multiple R-squared:  0.6982,	Adjusted R-squared:  0.668 
#   F-statistic: 23.13 on 1 and 10 DF,  p-value: 0.0007134

# correlation, if month were not time based.
sqrt(0.6982) # 0.8355836
cor(mo$count, as.numeric(mo$month)) # 0.8355671

# plots -----------------------------------------------------------------------

library(ggplot2)

# add fitted values to dataframe
mo$pm01.fitted <- pm01$fitted.values
mo$lm.fitted <- lm01$fitted.values

pm01p <- ggplot(mo, aes(month, count)) + 
  stat_smooth(colour = "gold3", se = F, size = 0.65, linetype = "dashed") +
  geom_point(size = 4.5, shape = 19, aes(colour = count)) +
  geom_point(size = 6, shape = 1, aes(colour = count)) +
  scale_colour_gradient(low = "firebrick3", high = "bisque1") +
  geom_point(size = 4.5, shape = 17, colour = "deepskyblue4", aes(month, pm01.fitted)) +
  geom_point(size = 4.5, colour = "lightblue2", shape = 15, aes(month, lm.fitted)) +
  geom_line(colour = "deepskyblue4", linetype = "solid", aes(month, pm01.fitted)) +
  geom_line(colour = "lightblue2", linetype = "dotted", aes(month, lm.fitted)) +
  scale_x_date(breaks = mo$month) +
  theme_minimal(base_size = 14, base_family = "FranklinGothicSSK") +
  theme(plot.title = element_text(face = "bold")) +
  theme(axis.text.x = element_text(angle = 45, size = 11,
                                   family = "Times", face = "italic")) +
  theme(axis.text.y = element_text(size = 12, family = "Times", face = "italic")) +
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm")) +
  labs(title = "Poisson, Linear, and Loess Regressions on listing count ~ month (2014, n = 1018109)", 
       x = "", y = "", fill = "observed\ncount")

pm01p


# subset for cannabis ---------------------------------------------------------

can <- subset(p14, p14$subcat == "Cannabis") # 177477 obs.

# number of cannabis listings by month
canmo <- as.data.frame(table(can$month))  # 12 obs
colnames(canmo) <- c("month", "count")
canmo$month <- seq(as.Date("2014-01-01"), by = "month", length.out = 12)

# number of cannabis listings by date
candate <- as.data.frame(table(can$date)) # 139 obs
colnames(candate) <- c("date", "count")
candate$date <- as.Date(candate$date)

summary(canmo$count) # lambda = 14790
#    Min. 1st Qu.  Median   Mean  3rd Qu.    Max. 
#   1500    5037    6422   14790   20510   44360

summary(candate$count) # lambda = 1277
#   Min. 1st Qu.  Median    Mean  3rd Qu.    Max. 
#      1     257     787    1277    2232    3963

# poisson model by date -------------------------------------------------------
pm03 <- glm(count ~ date, family = "poisson", data = candate)
summary(pm03)

plot(candate$date, pm03$fitted.values)
points(candate$date, candate$count, col = "red", pch = 19)
lines(candate$date, pm03$fitted.values, col = "deepskyblue4", lty = 2)
# visually and from the deviance:df relationship, 
# this looks overdispersed. 
# check if the low counts correspond to bad crawl dates?

par(mfrow = c(2, 2), mar = c(6, 6, 6, 6))
plot(pm03)
# 135, 70, 67 stand out in the Q-Q and residuals v. fitted plots
# 135 is very low with 18 count
# 67, 70 are very high - 3963, 3864 counts respectively

which.max(candate[, "count"]) # 67 - count of 3963
lambda.pm03 <- pm03$fitted.values[67] # 1360.862 predicted
qpois(0.95, lambda.pm03) # 1422
# 95% of the time, count of 1422 
qpois(0.90, lambda.pm03) # 1408
qpois(0.99, lambda.pm03) # 1447
qpois(0.75, lambda.pm03) #1386

# compare to linear model
lm03 <- lm(count ~ date, data = candate)
summary(lm03)
# Coefficients:
#                Estimate  Std. Error t value Pr(>|t|)   
# (Intercept) -48002.7492  15502.8181  -3.096  0.00238 **
# date             3.0194      0.9499   3.179  0.00183 **
# Residual standard error: 1139 on 137 degrees of freedom
# Multiple R-squared:  0.06869,	Adjusted R-squared:  0.06189 
# ...not the lowest t-values either.

# observed values first
par(mfrow = c(1, 1), mar = c(8, 8, 8, 8), family = "HersheySans")
plot(candate$date, candate$count, pch = 19, col = "red3", cex = 1.2,
     main = "Cannabis: listing count ~ date, family = Poisson",
     xlab = "dates (n = 139)", ylab = "listing count (n = 177447)")
points(candate$date, lm03$fitted.values, col = "#EED5B775", pch = 15, cex = 2)
lines(candate$date, lm03$fitted.values, col = "#EED5B7", lty = 3)
points(candate$date, pm03$fitted.values, col = "#00688B90", pch = 17, cex = 1.8)
lines(candate$date, pm03$fitted.values, col = "deepskyblue4", lty = 2)

# fitted poisson first - not plotted to max count in ylim
plot(candate$date, pm03$fitted.values, pch = 17, col = "#00688B90", cex = 2,
     main = "Cannabis: listing count ~ date, family = Poisson", 
     xlab = "date (n = 139)", ylab = "poisson fitted values")
points(candate$date, lm03$fitted.values, col = "#EED5B775", pch = 15, cex = 1.2)
lines(candate$date, lm03$fitted.values, col = "#EED5B7", lty = 3)
points(candate$date, candate$count, col = "red3", pch = 19, cex = 1.6)
lines(candate$date, pm03$fitted.values, col = "deepskyblue4", lty = 2)

candate$pm.fitted <- pm03$fitted.values
candate$lm.fitted <- lm03$fitted.values

# There's a lot of overdispersion. I wonder if some of this can be 
# attributed to bad crawls, or to downtimes in the market.
# I can compare with certain dates that I have downtime data for,
# or look at the counts from the grams dataset. 

# poisson model by month -------------------------------------------------------
pm04 <- glm(count ~ month, family = "poisson", data = canmo)
summary(pm04)
#     Null deviance: 170477  on 11  degrees of freedom
# Residual deviance:  22360  on 10  degrees of freedom
log(22360) # 10.01503 - 10 degrees of freedom - on.

# linear comparison
lm04 <- lm(count ~ month, data = canmo)
summary(lm04)
# Multiple R-squared:  0.7034,	Adjusted R-squared:  0.6738

sum(canmo$count)

par(mar = c(8, 8, 8, 8), family = "HersheySans")
plot(canmo$month, pm04$fitted.values, pch = 17, col = "#00688B75", cex = 1.6,
     main = "Cannabis: listing count ~ month, family = Poisson (n = 177447)",
     xlab = "month (n = 12)", ylab = "poisson fitted values", 
     ylim = c(0, max(canmo$count)))
points(canmo$month, lm04$fitted.values, col = "#EED5B7", pch = 15, cex = 1.4)
lines(canmo$month, lm04$fitted.values, col = "#EED5B7", lty = 2)
points(canmo$month, canmo$count, col = "red3", pch = 19, cex = 1.4)
lines(canmo$month, pm04$fitted.values, col = "deepskyblue4", lty = 2)
points(canmo$month, pm04$fitted.values, col = "#00688B90", pch = 17, cex = 1.8)

par(mfrow = c(2, 2), mar = c(6, 6, 6, 6), family = "FranklinGothicSSK")
plot(pm04)

which.max(canmo[, "count"])
qpois(0.975, lambda = pm04$fitted.values[12]) # 48298
qpois(0.950, lambda = pm04$fitted.values[12]) # 48229

canmo$pm.fitted <- pm04$fitted.values
canmo$lm.fitted <- lm04$fitted.values
