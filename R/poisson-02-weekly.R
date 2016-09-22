# Agora Marketplace Analysis
# Poisson Regression - Monthly, Weekly, Daily counts
# "How much is there? --> How much could there be?"

# load data -------------------------------------------------------------------

library(data.table)
library(sandwich)
library(glmm)
library(lme4)
library(boot)
library(caret)
library(ggplot2)
library(vcd)

agora <- fread("~/GitHub/agora-data/agora-01b.csv", stringsAsFactors = T)
agora$date <- as.Date(agora$date)

wk <- fread("data/WeeklyCountsFitted.csv")
wk$week <- as.Date(wk$week)

w14 <- fread("data/WeeklyCounts14.csv")
w15 <- fread("data/WeeklyCounts15.csv")

mo <- fread("data/MonthlyAll.csv")

# Pre-Poisson Plots -----------------------------------------------------------

# remove last empty counts - end of crawl
wk <- wk[-c(74:76), ]

slice(wk, 36:40)
#          week count wk month
# 1: 2014-09-28 38379 w4    09
# 2: 2014-10-07 33618 w1    10
# 3: 2014-10-14 29122 w2    10
# 4: 2014-10-21 26111 w3    10
# 5: 2014-10-28 98240 w4    10

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

par(mfrow = c(1, 1), mar = c(6, 6, 6, 6), family = "GillSans", bty = "l", las = 1)

plot(wk$week, wk$count, xlab = "", ylab = "", col = "firebrick3", pch = 19,
     main  = "Agora • Weekly Observed Counts • Mean • Absolute Difference between Mean and Observed", 
     xlim = c(as.Date("2014-01-07"), as.Date("2015-06-28")),
     cex.main = 1.2, cex.axis = 1)

points(wk$week, wk$count, col = "firebrick3", pch = 20, cex = 1.8)
points(wk$week, wk.abs.dev, col = "#000000", pch = 8, cex = 1)
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
qmw02 <- glm(count ~ week, data = wk, 
             family = quasi(link = "log", variance = "mu^2"))

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
# Number of Fisher Scoring iterations: 13

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
# Number of Fisher Scoring iterations: 17

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

# this goes back to overdispersion, but with much stronger effect
# as observed in null vs. residual deviance

# Linear Model comparison ---------------------------------

lmw01 <- lm(count ~ week, data = wk)
summary(lmw01)

#                Estimate     Std. Error  t value       Pr(>|t|)    
#   (Intercept) -1965368.49   253298.41  -7.759 0.0000000000467 ***
#   week             122.16       15.49   7.885 0.0000000000273 ***

# Residual standard error: 21190 on 71 degrees of freedom
# Multiple R-squared:  0.4669,	Adjusted R-squared:  0.4594 
# F-statistic: 62.17 on 1 and 71 DF,  p-value: 0.00000000002731

lmw02 <- lm(count ~ week + wk, data = wk)
summary(lmw02)

# Coefficients:
#   Estimate Std. Error t value        Pr(>|t|)    
# (Intercept) -1953892.9   253395.6  -7.711 0.0000000000734 ***
#   week             121.5       15.5   7.844 0.0000000000420 ***
#   wkw2           -4062.7     6970.3  -0.583           0.562    
#   wkw3           -6825.9     6969.2  -0.979           0.331    
#   wkw4            4709.4     6969.7   0.676           0.502    

# Residual standard error: 21190 on 68 degrees of freedom
# Multiple R-squared:  0.4897,	Adjusted R-squared:  0.4597 
# F-statistic: 16.31 on 4 and 68 DF,  p-value: 0.000000002057


# glmm01 ----------------------------------------------------------------------
glmm.wk01 <- glmm(count ~ week, random = count ~ wk, varcomps.names = "wk", 
                  data = wk, family.glmm = poisson.glmm, m = 1000)

summary(glmm.wk01)
# Fixed Effects:
#                    Estimate    Std. Error z value            Pr(>|z|)    
#   (Intercept) -57.575270848   0.076240612  -755.2 <0.0000000000000002 ***
#   week          0.004144729   0.000004635   894.2 <0.0000000000000002 ***

# Variance Components for Random Effects (P-values are one-tailed):
#     Estimate Std. Error z value Pr(>|z|)/2  
# wk  0.01800    0.01273   1.414     0.0787 .

# glmm02 ----------------------------------------------------------------------
glmm.wk02 <- glmm(count ~ wk, random = count ~ as.factor(week), varcomps.names = "week", 
                  data = wk, family.glmm = poisson.glmm, m = 1000)

summary(glmm.wk02)
# Fixed Effects:
#               Estimate  Std. Error z value           Pr(>|z|)    
#   (Intercept) 7.010911   0.001289    5438 <0.0000000000000002 ***
#   wkw2        1.208501   0.001891     639 <0.0000000000000002 ***
#   wkw3        2.394269   0.001945    1231 <0.0000000000000002 ***
#   wkw4        2.700672   0.001736    1555 <0.0000000000000002 ***

# Variance Components for Random Effects (P-values are one-tailed):
#       Estimate Std. Error z value Pr(>|z|)/2  
# week    7.324      1.212   6.041 0.000000000765 ***

# interesting.

glmm.wk02$coefficients[-1, 3]


glmm.wk02.est <- exp(glmm.wk02$beta)
hist(glmm.wk02$x) # 0 or 1

par(mfrow = c(1, 2), family = "GillSans")
hist(glmm.wk02$y, breaks = 50) # looks like the counts.
hist(wk$count, breaks = 50)


# glmm03 ----------------------------------------------------------------------

# set variance to be equal
glmm.wk03 <- glmm(count ~ wk, random = count ~ as.factor(week), varcomps.names = "week", 
                  varcomps.equal = 1, data = wk, family.glmm = poisson.glmm, m = 1000)

summary(glmm.wk03)
#   Fixed Effects:
#                Estimate Std. Error z value            Pr(>|z|)    
#   (Intercept) 7.006841   0.001389  5044.8 <0.0000000000000002 ***
#   wkw2        1.210982   0.001927   628.5 <0.0000000000000002 ***
#   wkw3        2.398926   0.002065  1162.0 <0.0000000000000002 ***
#   wkw4        2.703032   0.001783  1516.3 <0.0000000000000002 ***

# Variance Components for Random Effects (P-values are one-tailed):
#      Estimate Std. Error z value    Pr(>|z|)/2    
# week    7.401      1.225    6.04 0.00000000077 ***

# glmm04 - lme4 ---------------------------------------------------------------

glmm.wk04 <- glmer(count ~ wk + (1 | week), data = wk,
                          family = poisson("log"))


sjp.setTheme(theme = "forest")
sjp.glmer(glmm.wk04, 
          facet.grid = FALSE, 
          sort = "sort.all")

library(sjPlot)
library(arm)
sjp.glmer(glmm.wk04, type = "re.qq")
sjp.glmer(glmm.wk04, type = "fe.cor")
sjp.glmer(glmm.wk04, type = "re")

# Fortify Model fits --------------------------------------

# pwf01 <- fortify(pmw01)   # poisson model 01
# qmf01 <- fortify(qmw01)   # quasi   model 01

# linear models
lmf01 <- fortify(lmw01)
lmf02 <- fortify(lmw02)   

# qmw02 may be the most interesting model - simple with mu^2 variance.
qmf02 <- fortify(qmw02)
qmf02$fitted.values <- exp(qmf02$.fitted)
qmf02 <- qmf02[, c(1, 9, 6, 7, 2, 3, 4, 5)]

# qmw04 might be second most interesting - appears adding the weekly
# index as an independent variable leads to interval'd predictions.
qmf04 <- fortify(qmw04)
qmf04$fitted.values <- exp(qmf04$.fitted)
qmf04 <- qmf04[, c(1, 10, 7, 8, 2, 3, 4, 5, 6, 7, 9)]

# qmw03 added month and week index to the model.
# fitted values are close to observed - but doesn't model well.
qmf03 <- fortify(qmw03)
qmf03$fitted.values <- exp(qmf03$.fitted)
qmf03 <- qmf03[, c(1, 11, 8, 9, 2, 3, 4, 5, 6, 7, 10)]

# qmf05
qmf05 <- fortify(qmw05)
qmf05$fitted.values <- exp(qmf05$.fitted)
qmf05 <- qmf05[, c(1, 10, 8, 9, 2, 3, 4, 5, 6, 7)]

# plot various fits and observed --------------------------

wk$lm.fitted <- lmf01$.fitted
wk$qm02.fitted <- qmf02$fitted.values
wk$qm03.fitted <- qmf03$fitted.values
wk$qm04.fitted <- qmf04$fitted.values

write.csv(wk, file = "data/WeeklyCountsFitted.csv", row.names = F)

# gg
qw01p <- ggplot(wk, aes(week, count)) + 
  stat_smooth(colour = "gold3", se = F, size = 1, linetype = "dotdash", alpha = 1) +
  geom_point(size = 3.5, colour = "firebrick3", shape = 19, alpha = 0.75) +  
  geom_line(colour = "gold1", linetype = "dashed", size = 1,  aes(week, lm.fitted)) +  
  geom_point(size = 2.75, colour = "deepskyblue3", shape = 2, aes(week, qm02.fitted)) +
  geom_line(colour = "deepskyblue4", linetype = "solid", size = 1.25,  aes(week, qm02.fitted)) +
  geom_point(size = 3.75, colour = "lightblue2", shape = 18, alpha = 0.75, aes(week, qm03.fitted)) +
  geom_point(size = 1.75, colour = "deepskyblue1", shape = 15, aes(week, qm04.fitted)) +
  geom_rug(aes(week), color = "firebrick3", sides = "l", size = 0.25, linetype = 1) +
  scale_x_date(breaks = dates, labels = c("2014 - Jan", "Feb", "Mar", "Apr", "May", "June", 
                                          "July", "Aug", "Sept", "Oct", "Nov", "Dec",
                                          "2015 - Jan", "Feb", "Mar", "Apr", 
                                          "May", "June", "July"))

qw01p + theme_minimal(base_size = 12, base_family = "GillSans") +
  theme(axis.text.x = element_text(size = 11, angle = 45, hjust = 1, vjust=1)) +
  theme(axis.text.y = element_text(size = 11)) +
  theme(plot.margin = unit(c(0.25, 0.25, 0.10, 0.25), "cm")) +
  labs(title = substitute(
    paste("QuasiPoisson (", sigma, "=", mu^2, "), Linear, and Loess Regressions on Listing Count ~ Date")), 
    x = "", y = "", fill = "")

# gg2 - plot different qp models individually
qw02p <- ggplot(wk, aes(week, count)) + 
  stat_smooth(colour = "gold3", se = F, size = 1, linetype = "dotdash", alpha = 1) +
  geom_point(size = 3.5, colour = "firebrick3", shape = 19, alpha = 0.75) +  
  geom_line(colour = "gold1", linetype = "dashed", size = 1,  aes(week, lm.fitted)) +  
  geom_point(size = 2.75, colour = "deepskyblue4", shape = 15, aes(week, qm04.fitted)) +
  geom_rug(aes(week), color = "firebrick3", sides = "l", size = 0.25, linetype = 1) +
  scale_x_date(breaks = dates, labels = c("2014 - Jan", "Feb", "Mar", "Apr", "May", "June", 
                                          "July", "Aug", "Sept", "Oct", "Nov", "Dec",
                                          "2015 - Jan", "Feb", "Mar", "Apr", 
                                          "May", "June", "July"))

qw02p + theme_minimal(base_size = 12, base_family = "GillSans") +
  theme(axis.text.x = element_text(size = 11, angle = 45, hjust = 1, vjust=1)) +
  theme(axis.text.y = element_text(size = 11)) +
  theme(plot.margin = unit(c(0.25, 0.25, 0.10, 0.25), "cm")) +
  labs(title = substitute(
    paste("QuasiPoisson (", sigma, "=", mu^2, "), Linear, and Loess Regressions on Listing Count ~ Date")), 
    x = "", y = "", fill = "")

# base:
par(mar = c(6, 6, 6, 6), bty = "l", las = 1, family = "GillSans")

# qmf02
plot(qmf02$week, qmf02$count, xlab = "", ylab = "", col = "firebrick3",
     main  = "Quasi/Poisson Model • Count ~ Week • Observed, Poisson, and Linear Fitted Values",
     xlim = c(as.Date("2014-01-07"), as.Date("2015-08-01")),
     pch = 19, cex.main = 1.1, cex.axis = 1)

lines(lmf01$week, lmf01$.fitted, col = "gold2", lty = 2, lwd = 1.8)
lines(qmf02$week, qmf02$fitted.values, lty = 1, lwd = 3.6, col = "deepskyblue4")
# points(qmf02$week, qmf02$fitted.values, pch = 2, cex = 0.75, col = "deepskyblue3")

#qmf03
plot(qmf02$week, qmf02$count, xlab = "", ylab = "", col = "firebrick3",
     main  = "Quasi/Poisson Model • Count ~ Week • Observed, Poisson, and Linear Fitted Values",
     xlim = c(as.Date("2014-01-07"), as.Date("2015-08-01")),
     pch = 19, cex.main = 1.1, cex.axis = 1)

lines(lmf01$week, lmf01$.fitted, col = "gold2", lty = 2, lwd = 1.8)
lines(qmf03$week, qmf03$fitted.values, lty = 1, lwd = 2, col = "dodgerblue4")
points(qmf03$week, qmf03$fitted.values, pch = 7, cex = 1.2, col = "dodgerblue2")

# qmf04
plot(qmf02$week, qmf02$count, xlab = "", ylab = "", col = "firebrick3",
     main  = "Quasi/Poisson Model • Count ~ Week • Observed, Poisson, and Linear Fitted Values",
     xlim = c(as.Date("2014-01-07"), as.Date("2015-08-01")),
     pch = 19, cex.main = 1.1, cex.axis = 1)

lines(lmf01$week, lmf01$.fitted, col = "gold2", lty = 2, lwd = 1.8)
lines(qmf04$week, qmf04$fitted.values, lty = 3, lwd = 1.4, col = "cadetblue4")
points(qmf04$week, qmf04$fitted.values, pch = 18, cex = 1.4, col = "cadetblue3")
points(qmf04$week, qmf04$fitted.values, pch = 18, cex = 0.8, col = "cadetblue4")


# qmf05
plot(qmf05$week, qmf05$count, xlab = "", ylab = "", col = "firebrick3",
     main  = "Quasi/Poisson Model • Count ~ Week • Observed, Poisson, and Linear Fitted Values",
     xlim = c(as.Date("2014-01-07"), as.Date("2015-08-01")),
     pch = 19, cex.main = 1.1, cex.axis = 1)

lines(lmf01$week, lmf01$.fitted, col = "gold2", lty = 2, lwd = 1.8)
lines(qmf05$week, qmf05$fitted.values, lty = 3, lwd = 1.2, col = "steelblue2")
points(qmf05$week, qmf05$fitted.values, pch = 18, cex = 1.4, col = "steelblue3")
points(qmf05$week, qmf05$fitted.values, pch = 18, cex = 0.8, col = "steelblue4")


# rug(qmf02$week, ticksize = -0.01, side = 1, lwd = 2, col = "#000000")

# So at what rate does the market grow (measured in listing counts)
# if left unchecked?

# ggplot fitted v. observed ---------------------------------------------------

dates <- seq(as.Date("2014-01-01"), as.Date("2015-07-01"), by = "month")

qw01p <- ggplot(wk, aes(date, count)) + 
  stat_smooth(colour = "gold4", se = F, size = 1, linetype = "dotdash", alpha = 0.75) +
  geom_point(size = 1.5, colour = "firebrick3", shape = 20) + 
  geom_point(size = 1.5, colour = "firebrick3", shape = 1) +  
  geom_line(colour = "gold2", linetype = "dashed", size = 1,  aes(date, lm.fitted)) +  
  geom_point(size = 1.5, colour = "deepskyblue3", shape = 5, aes(date, qm01.fitted)) +
  geom_line(colour = "deepskyblue3", linetype = "solid", size = 2,  aes(date, qm01.fitted)) +
  scale_x_date(breaks = dates, labels = c("2014 - Jan", "Feb", "Mar", "Apr", "May", "June", 
                                          "July", "Aug", "Sept", "Oct", "Nov", "Dec",
                                          "2015 - Jan", "Feb", "Mar", "Apr", 
                                          "May", "June", "July"))

qw01p + theme_minimal(base_size = 12, base_family = "GillSans") +
  theme(axis.text.x = element_text(size = 11, angle = 35, hjust = 1)) +
  theme(axis.text.y = element_text(size = 11)) +
  theme(plot.margin = unit(c(0.25, 0.25, 0.10, 0.25), "cm")) +
  labs(title = substitute(
    paste("QuasiPoisson (", sigma, "=", mu^2, "), Linear, and Loess Regressions on Listing Count ~ Date")), 
    x = "", y = "", fill = "")

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

# Bootstrap Validation --------------------------------------------------------

# via caret  ----------------------------------------------

# Caret may not have poisson GAM/GLM support yet?

t.control <- caret::trainControl(method = "boot", number = 100)
qmw02boot <- caret::train(count ~ week, data = wk, trControl=t.control, 
                      method="")

summary(qmw02boot)

# via plsRglm ---------------------------------------------

# qpw <- plsRglm(count ~ week, dataX)
# qmw02boot <- bootplsglm(qmw02, typeboot = "plsmodel", R = 1000, sim = "ordinary")
# rownames(qmw02boot3$t0)<-c("Intercept\n","peri\n","shape\n","perm\n","peri.\nshape",
#                              "peri.\nperm","shape.\nperm")


# Cross Validation ------------------------------------------------------------

# via caret

tmp <- createResample(counts,times = 10)
myCtrl <- trainControl(method = "cv", index = tmp, timingSamps = 10)

# Confidence Intervals --------------------------------------------------------

cov.qmw02 <- vcovHC(qmw02, type = "HC0")
summary(cov.qmw02)

# two tailed t-test
tt <- qt(c(0.025, 0.975), summary(qmw02)$df[2])

# standard errors
se <- sqrt(diag(cov.qmw02))

# confidence intervals
ci.qmw02 <- coef(qmw02) + se %o% tt
ci.qmw02
#                       [,1]          [,2]
# (Intercept) -115.830321481 -76.216072832
# week           0.005279001   0.007693837

# ucla example - how is 1.96 derived? 100/(degrees of freedom)?
r.est <- cbind(Estimate = round((coef(qmw02)), 4), "Robust SE" = round(se, 4),
               "Pr(>|z|)" = round((2 * pnorm(abs(coef(qmw02)/se), lower.tail = F)), 4),
               LL = round((coef(qmw02) - 1.96 * se), 4),
               UL = round((coef(qmw02) + 1.96 * se), 4))

r.est
#                  Estimate    Robust SE                           Pr(>|z|)             LL            UL
# (Intercept) -96.023197157 9.9336443771 0.00000000000000000000041859913356 -115.493140136 -76.553254177
# week          0.006486419 0.0006055429 0.00000000000000000000000000896598    0.005299555   0.007673283

#             Estimate Robust SE Pr(>|z|)        LL       UL
# (Intercept) -96.0232    9.9336        0 -115.4931 -76.5533
# week          0.0065    0.0006        0    0.0053   0.0077
