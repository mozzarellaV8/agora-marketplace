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

wk <- fread("data/WeeklyCountsPop.csv")
wk$mw <- as.Date(wk$mw) 

mo <- fread("data/MonthlyAll.csv")

w14 <- fread("data/WeeklyCounts14.csv")
w15 <- fread("data/WeeklyCounts15.csv")

# get weekly counts -----------------------------------------------------------

# convert to numeric
agora$day <- as.numeric(agora$day)
agora$month <- as.numeric(agora$month)

# loop over months and count rows - 2014 ------------------

w14 <- data.frame()

for (i in 1:12) {
  
  w1 <- nrow(subset(agora, agora$month == i & agora$year == "2014" & 
                      agora$day <= 7))
  w2 <- nrow(subset(agora, agora$month == i & agora$year == "2014" & 
                      agora$day <= 14 & agora$day > 7))
  w3 <- nrow(subset(agora, agora$month == i & agora$year == "2014" & 
                      agora$day <= 21 & agora$day > 14))
  w4 <- nrow(subset(agora, agora$month == i & agora$year == "2014" & 
                      agora$day <= 31 & agora$day > 21))
  
  wLog <- cbind(w = i, w1 = w1, w2 = w2, w3 = w3, w4 = w4)
  w14 <- rbind(w14, wLog)
}

write.csv(w14, file = "data/WeeklyCounts14.csv", row.names = F)
colnames(w14)[1] <- "month"

# stack counts + sort chronologically
w14 <- stack(w14, select = c(w1, w2, w3, w4))
w14$month <- c("01", "02", "03", "04", "05", "06", "07", "08", "09", "10", "11", "12")
w14$m.w <- paste("2014", w14$month, w14$ind, sep = "-")
colnames(w14) <- c("count", "week", "month", "mw")
w14 <- w14[c(4, 1, 2, 3)]
w14 <- w14[order(w2$mw, decreasing = F), ]
rownames(w2) <- NULL

# 2015 weekly counts --------------------------------------

w15 <- data.frame()

for (i in 1:7) {
  
  w1 <- nrow(subset(agora, agora$month == i & agora$year == "2015" & 
                      agora$day <= 7))
  w2 <- nrow(subset(agora, agora$month == i & agora$year == "2015" & 
                      agora$day <= 14 & agora$day > 7))
  w3 <- nrow(subset(agora, agora$month == i & agora$year == "2015" & 
                      agora$day <= 21 & agora$day > 14))
  w4 <- nrow(subset(agora, agora$month == i & agora$year == "2015" & 
                      agora$day <= 31 & agora$day > 21))
  
  wLog <- cbind(w = i, w1 = w1, w2 = w2, w3 = w3, w4 = w4)
  w15 <- rbind(w15, wLog)
}

write.csv(w15, file = "data/WeeklyCounts15.csv", row.names = F)

# clean up 2015
w15 <- stack(w15, select = c(w1, w2, w3, w4))
w15$month <- c("01", "02", "03", "04", "05", "06", "07")
w15$mw <- paste("2015", w15$month, w15$ind, sep = "-")
colnames(w15) <- c("count", "week", "month", "mw")
w15 <- w15[c(4, 1, 2, 3)]
w15 <- w15[order(w3$mw, decreasing = F), ]
rownames(w3) <- NULL

# bind 2014-2015 weekly counts ----------------------------

weekly <- rbind(w14, w15)
weekly$mw <- gsub("\\bw1\\b", "07", weekly$mw)
weekly$mw <- gsub("\\bw2\\b", "14", weekly$mw)
weekly$mw <- gsub("\\bw3\\b", "21", weekly$mw)
weekly$mw <- gsub("\\bw4\\b", "28", weekly$mw)

write.csv(weekly, file = "data/WeeklyCountsPop.csv", row.names = F)
wk <- weekly

# Pre Poisson Plots -----------------------------------------------------------

# remove last empty counts - end of crawl
wk <- wk[-c(74:76), ]
colnames(wk) <- c("week", "count", "wk", "month")
wk$week <- as.Date(wk$week)

# observed mean and variance ------------------------------
wk.avg <- mean(wk$count)    # 31821.38
wk.var <- var(wk$count)     # 830,845,394

wk.var/wk.avg               # 26109.66l; factor of 26.1k

# Again, mean and variance differ greatly in observed.

mean((wk.avg - wk$count)^2) # 819463950
sum(abs(wk.avg - wk$count)) # 1855712
wk.abs.dev <- abs(wk.avg - wk$count)
sd(wk$count)                # 28824.39; 28824.39^2 = 830845394

# plots ---------------------------------------------------

par(mar = c(6, 6, 6, 6), family = "GillSans", bty = "l", las = 1)

plot(wk$week, wk$count, xlab = "", ylab = "", col = "#000000", pch = 19, cex = 1.2,
     main  = "Agora • Weekly Observed Counts • Mean • Absolute Difference between Mean and Observed", 
     xlim = c(as.Date("2014-01-07"), as.Date("2015-06-28")))

points(wk$week, wk$count, col = "#000000", pch = 20, cex = 1.2)
points(wk$week, wk.abs.dev, col = "firebrick3", pch = 8, cex = 1.2)
abline(a = wk.avg, b = 0, lty = 6, col = "gold2", lwd = 2)

rug(wk$week, ticksize = 0.0085, side = 1, lwd = 1.5, 
    lty = 1, col = "#000000")

# Poisson Model - Weekly ------------------------------------------------------

# Poisson 01 ----------------------------------------------
pmw01 <- glm(count ~ week, family = poisson, data = wk)
summary(pmw01)
#                    Estimate    Std. Error z value            Pr(>|z|)    
#   (Intercept) -58.051732708   0.076378255  -760.1 <0.0000000000000002 ***
#   week          0.004171856   0.000004644   898.4 <0.0000000000000002 ***
# 
# (Dispersion parameter for poisson family taken to be 1)
#
#     Null deviance: 2106049  on 72  degrees of freedom
# Residual deviance: 1191952  on 71  degrees of freedom
# AIC: 1192752

# Quasipoisson 01 -----------------------------------------
qmw01 <- glm(count ~ week, family = quasipoisson, data = wk)
summary(qmw01)
#                  Estimate   Std. Error  t value     Pr(>|t|)  
#   (Intercept) -58.0517327   9.6458373  -6.018 0.000000069874 ***
#   week          0.0041719   0.0005864   7.114 0.000000000726 ***
# 
# (Dispersion parameter for quasipoisson family taken to be 15949.25)
#
#     Null deviance: 2106049  on 72  degrees of freedom
# Residual deviance: 1191952  on 71  degrees of freedom
# AIC: NA

# When I forgot to convert week to a date object, the dispersion 
# parameter was estimated at near exactly the same factor from 
# wk.var/wk.avg -- 26.1k times!

# Quasipoisson 02 -----------------------------------------
qmw02 <- glm(count ~ week, family = quasi(link = "log", variance = "mu^2"), data = wk)
summary(qmw02)
#                 Estimate  Std. Error  t value           Pr(>|t|)
#  (Intercept) -96.0231972  10.3609211  -9.268 0.00000000000007527 ***
#  week          0.0064864   0.0006337  10.236 0.00000000000000128 ***
#
# (Dispersion parameter for quasi family taken to be 0.8623852)
#
#     Null deviance: 175.35  on 72  degrees of freedom
# Residual deviance: 125.06  on 71  degrees of freedom
# AIC: NA

# witha mu^2 parameter added to the quasi poisson model, 
# it appears the data are just a touch underdispersed at 0.86. 
# That's not bad given the rather large differences observed before.

# Quasipoisson 03 -----------------------------------------
qmw03 <- glm(count ~ week + month + wk, data = wk,
             family = quasi(link = "log", variance = "mu^2"))

summary(qmw03)
#                  Estimate  Std. Error t value           Pr(>|t|)    
#   (Intercept) -97.2697515  10.8407891  -8.973 0.0000000000017118 ***
#   week          0.0065719   0.0006659   9.869 0.0000000000000608 ***
#   month02       0.1264772   0.4348387   0.291             0.7722    
#   month03      -0.2241067   0.4361217  -0.514             0.6093    
#   month04      -0.3801890   0.4384636  -0.867             0.3895    
#   month05      -0.3906327   0.4416377  -0.885             0.3801    
#   month06      -0.5909993   0.4458353  -1.326             0.1903    
#   month07      -0.5377981   0.4985914  -1.079             0.2853    
#   month08      -0.6126249   0.5323287  -1.151             0.2546    
#   month09      -0.1076163   0.5334894  -0.202             0.8409    
#   month10       0.5905783   0.5353687   1.103             0.2746    
#   month11       0.9254168   0.5380835   1.720             0.0909 .  
#   month12       0.6748475   0.5414475   1.246             0.2177    
#   wkw2         -0.6642139   0.2864937  -2.318             0.0240 *  
#   wkw3         -0.2085034   0.2864374  -0.728             0.4696    
#   wkw4          0.1500150   0.2864569   0.524             0.6025    
#
# (Dispersion parameter for quasi family taken to be 0.7546344)
#
#     Null deviance: 175.35  on 72  degrees of freedom
# Residual deviance: 104.19  on 57  degrees of freedom
# AIC: NA

# Quasipoisson 04 -----------------------------------------
qmw04 <- glm(count ~ week + wk, data = wk,
             family = quasi(link = "log", variance = "mu^2"))

summary(qmw04)
#                   Estimate   Std. Error t value             Pr(>|t|) 
#   (Intercept) -103.2768372   10.2482057 -10.078  0.00000000000000398 ***
#   week           0.0069363    0.0006267  11.068 < 0.0000000000000002 ***
#   wkw2          -0.5252828    0.2819037  -1.863               0.0667 .  
#   wkw3          -0.2534276    0.2818581  -0.899               0.3718    
#   wkw4           0.2231234    0.2818809   0.792               0.4314 
#
# (Dispersion parameter for quasi family taken to be 0.7343154)
#
#     Null deviance: 175.35  on 72  degrees of freedom
# Residual deviance: 119.80  on 68  degrees of freedom
# AIC: NA

# Quasipoisson 05 -----------------------------------------
qmw05 <- glm(count ~ week + month, data = wk,
             family = quasipoisson(link = "log"))

summary(qmw05)
#                  Estimate  Std. Error t value        Pr(>|t|)    
#   (Intercept) -82.5262770  11.1654955  -7.391 0.0000000005373 ***
#   week          0.0056840   0.0006802   8.356 0.0000000000121 ***
#   month02      -0.1904705   0.2693681  -0.707        0.482241    
#   month03      -0.4616820   0.2793409  -1.653        0.103605    
#   month04      -0.6860241   0.2866559  -2.393        0.019848 *  
#   month05      -1.1875311   0.3190765  -3.722        0.000438 ***
#   month06      -0.9447352   0.2922034  -3.233        0.001991 ** 
#   month07      -1.1850169   0.4121759  -2.875        0.005583 ** 
#   month08      -0.8993667   0.5272357  -1.706        0.093215 .  
#   month09      -0.4901348   0.4128782  -1.187        0.239860    
#   month10       0.2850663   0.2968744   0.960        0.340795    
#   month11       0.4977691   0.2657779   1.873        0.065960 .  
#   month12       0.3306845   0.2652787   1.247        0.217405  
#
# (Dispersion parameter for quasipoisson family taken to be 9563.747)
#
#     Null deviance: 2106049  on 72  degrees of freedom
# Residual deviance:  573847  on 60  degrees of freedom
# AIC: NA
# this goes back to overdispersion 

# Linear Model comparison ---------------------------------

lmw01 <- lm(count ~ week, data = wk)
summary(lmw01)

#                Estimate     Std. Error  t value       Pr(>|t|)    
#   (Intercept) -1965368.49   253298.41  -7.759 0.0000000000467 ***
#   week             122.16       15.49   7.885 0.0000000000273 ***

# Residual standard error: 21190 on 71 degrees of freedom
# Multiple R-squared:  0.4669,	Adjusted R-squared:  0.4594 
# F-statistic: 62.17 on 1 and 71 DF,  p-value: 0.00000000002731

# Fortify Model fits --------------------------------------

pwf01 <- fortify(pmw01)
qmf01 <- fortify(qmw01)
lmf01 <- fortify(lmw01)

qmf04 <- fortify(qmw04)
qmf04$fitted.values <- exp(qmf04$.fitted)
qmf04 <- qmf04[, c(1, 10, 7, 8, 2, 3, 4, 5, 6, 7, 9)]

# qmw03 added month and week index to the model.
# fitted values are close to observed - but doesn't model well.
qmf03 <- fortify(qmw03)
qmf03$fitted.values <- exp(qmf03$.fitted)
qmf03 <- qmf03[, c(1, 11, 8, 9, 2, 3, 4, 5, 6, 7, 10)]

# qmw02 may be the most interesting model - simple with mu^2 variance.
qmf02 <- fortify(qmw02)
qmf02$fitted.values <- exp(qmf02$.fitted)
qmf02 <- qmf02[, c(1, 9, 6, 7, 2, 3, 4, 5)]

# plot various fits and observed --------------------------
par(mar = c(6, 6, 6, 6), bty = "n", las = 1, family = "GillSans")

plot(qmf02$week, qmf02$count, xlab = "", ylab = "", col = "black",
     main  = "Quasi/Poisson Model • Count ~ Week • Observed, Poisson, and Linear Fitted Values", 
     xlim = c(as.Date("2014-01-07"), as.Date("2016-01-01")),
     pch = 19, cex = 1.2)

lines(lmf01$week, lmf01$.fitted, col = "gold2", lty = 2, lwd = 1.8)
points(qmf03$week, qmf03$fitted.values, pch = 10, cex = 1.4,
       col = "firebrick2")
points(qmf03$week, qmf03$fitted.values, pch = 10, cex = 1.2,
       col = "firebrick3")
points(qmf04$week, qmf04$fitted.values, pch = 5, cex = 1.4,
       col = "cadetblue4")
points(qmf04$week, qmf04$fitted.values, pch = 5, cex = 1.2,
       col = "cadetblue3")
points(qmf04$week, qmf04$fitted.values, pch = 5, cex = 1,
       col = "cadetblue2")
points(qmf04$week, qmf04$fitted.values, pch = 5, cex = 0.8,
       col = "cadetblue1")
lines(qmf02$week, qmf02$fitted.values, lty = 1, lwd = 3.4, col = "deepskyblue3")
rug(qmf02$week, ticksize = 0.01, side = 1, lwd = 1, col = "deepskyblue4")

# So at what rate does the market grow (measured in listing counts)
# if left unchecked?

# points(lmf01$week, lmf01$.fitted, col = "gold3", pch = 1)
# lines(qmf04$week, qmf04$fitted.values, lty = 1, lwd = 2, col = "deepskyblue3")
# points(qmf02$week, qmf02$fitted.values, pch = 19, cex = 0.6, col = "deepskyblue3")

# plot model ----------------------------------------------

par(mfrow = c(2, 2))
plot(qmf02)
plot(qmf03)
plot(qmf04)

plot(qmw02)
plot(qmw03)
plot(qmw04)

# Test NULL -----------------------------------------------

beta <- coef(qmw02)
exp(beta[2])
#     week 
# 1.006508

round(exp(confint.default(qmw02)), 5)
#               2.5 %  97.5 %
# (Intercept) 0.00000 0.00000
# week        1.00526 1.00776

# 0% change in mean, with 0.53% - 0.78% increase at 95% confidence interval.

# write out models ----------------------------------------

write.csv(qmf02, file = "data/poisson/qmf02.csv", row.names = F)
write.csv(qmf03, file = "data/poisson/qmf03.csv", row.names = F)
write.csv(qmf04, file = "data/poisson/qmf04.csv", row.names = F)
