# Agora Marketplace Analysis
# Poisson Regression Prep
# "How much is there?" --> "How much could there be?"

# load data -------------------------------------------------------------------

library(data.table)
library(sandwich)
library(ggplot2)
agora <- fread("~/GitHub/agora-data/agora-01b.csv")
p14 <- fread("~/GitHub/agora-data/ag14-01.csv", stringsAsFactors = T)
mo <- read.csv("data/MonthlyCounts.csv")
mo$month <- as.Date(mo$month)

# poisson distributions ------------------------------------------------------
# remember as lambda get large, poisson approximates to normal

par(mfrow = c(2, 2), mar = c(8, 8, 8, 8), bty = "l", las = 1)
plot(0:10, dpois(0:10, lambda = 2), type = "h")
plot(0:20, dpois(0:20, lambda = 10), type = "h")
plot(0:200, dpois(0:200, lambda = 100), type = "h")
plot(0:1000, dpois(0:01000, lambda = 500), type = "h")

# crawl counts ----------------------------------------------------------------
# "How much is there?"

# generate new crawl counts - previous ones by hand are off.

nrow(p14)
# 1018109 - 2014 population

# by month
jan <- subset(p14, p14$month == "01")     # 7986
feb <- subset(p14, p14$month == "02")     # 27300
mar <- subset(p14, p14$month == "03")     # 18807
apr <- subset(p14, p14$month == "04")     # 20000
may <- subset(p14, p14$month == "05")     # 42535
jun <- subset(p14, p14$month == "06")     # 25422
jul <- subset(p14, p14$month == "07")     # 39989
aug <- subset(p14, p14$month == "08")     # 40465
sep <- subset(p14, p14$month == "09")     # 72666
oct <- subset(p14, p14$month == "10")     # 187091
nov <- subset(p14, p14$month == "11")     # 276028
dec <- subset(p14, p14$month == "12")     # 259820

# counts by day
# this needs to be fixed - jan thru sep at least.
# (sigh...date created values of html page vs. provided date of crawl)
# but vendor counts have correctly labelled html date created values.
summary(jan$day)
summary(feb$day)
summary(mar$day)
summary(apr$day)
summary(may$day)
summary(jun$day)
summary(jul$day)
summary(aug$day)
summary(sep$day)
summary(oct$day)
summary(nov$day)
summary(dec$day)

mo <- data.frame(month = seq(as.Date("2014-01-01"), by = "month", length.out = 12), 
                 count = c(nrow(jan), nrow(feb), nrow(mar), nrow(apr), 
                           nrow(may), nrow(jun), nrow(jul), nrow(aug),
                           nrow(sep), nrow(oct), nrow(nov), nrow(dec)))

# mo$month <- gsub("\\b-01\\b", "", mo$month)
mo$month <- as.Date(mo$month)

write.csv(mo, "data/MonthlyCounts.csv", row.names = F)
mo <- read.csv("data/MonthlyCounts.csv")

par(mfrow = c(1, 1), mar = c(8, 8, 8, 8), family = "HersheySans")
plot(mo$month, mo$count, pch = 19, cex = 3.0, ylab = "", xlab = "",
     main = "Listing Count by Month", type = "h")

mean(mo$count) # 84842.42

# poisson model - monthly counts ----------------------------------------------

pm01 <- glm(count ~ month, family = "poisson", data = mo)
summary(pm01)
# Coefficients:
#                 Estimate Std. Error z value Pr(>|z|)    
#   (Intercept) -1.647e+02  2.121e-01  -776.6   <2e-16 ***
#   month        1.081e-02  1.298e-05   832.3   <2e-16 ***

# (Dispersion parameter for poisson family taken to be 1)

#     Null deviance: 1052014  on 11  degrees of freedom
# Residual deviance:  100674  on 10  degrees of freedom
# mean != variance here.......

pm02 <- glm(count ~ month, family = "quasipoisson", data = mo)
summary(pm02)
# Coefficients:
#                 Estimate Std. Error t value Pr(>|t|)    
#   (Intercept) -1.647e+02  2.166e+01  -7.606 1.83e-05 ***
#   month        1.081e-02  1.326e-03   8.152 9.99e-06 ***

# (Dispersion parameter for quasipoisson family taken to be 10425.99)

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

# add fitted values to dataframe
mo$pm01.fitted <- pm01$fitted.values
mo$lm.fitted <- lm01$fitted.values

# plot comparison -------------------------------------------------------------

par(mfrow = c(1, 1), mar = c(8, 8, 8, 8), las = 1, bty = "n", family = "HersheySans")
plot(mo$month, pm01$fitted.values, cex = 3.0, col = "deepskyblue4", pch = 17,
     main = "listing count ~ month: Poisson, quasipoisson, & linear fitted values w/ observed",
     xlab = "", ylab = "", xaxt = "n")
points(mo$month, mo$count, cex = 2.0, col = "firebrick3", pch = 19)
points(mo$month, pm02$fitted.values, cex = 2.0, col = "lightblue2", pch = 2)
points(mo$month, lm01$fitted.values, cex = 2.0, col = "bisque2", pch = 15)
lines(mo$month, pm01$fitted.values, cex = 2.0, col = "deepskyblue4", lty = 1)
lines(mo$month, lm01$fitted.values, cex = 2.0, col = "bisque2", lty = 2)
text(seq(mo$month[1], mo$month[12], length.out = 12), 0, 
     labels = mo$month, srt = 0, cex = 0.85, font = 2)


# no observed difference between poisson and quasipoisson.
# the plot looks reasonable - but what about the huge discrepancy
# residual deviance and degrees of freedom? mean != variance, it would seem...
# of by factor of 10,000 - 10^4...taking the log yields:
# log(100674) #  11.51964
# now that is much closer to a mean = variance relationship.
# but is this actually a correct measure? No. 

pm01$residuals
pm01$df.residual

# plot comparison gg ----------------------------------------------------------

pm01p <- ggplot(mo, aes(month, count)) + 
  stat_smooth(colour = "gold3", se = F, size = 0.65, linetype = "dotdash") +
  geom_point(size = 4.5, colour = "firebrick4", shape = 19) +
  geom_point(size = 6, colour = "firebrick4", shape = 1) +
  geom_point(size = 4.5, colour = "deepskyblue4", shape = 17,
             aes(month, pm01.fitted)) +
  geom_point(size = 4.5, colour = "lightblue2", shape = 15, aes(month, lm.fitted)) +
  geom_line(colour = "deepskyblue4", linetype = "solid", aes(month, pm01.fitted)) +
  geom_line(colour = "lightblue2", linetype = "dotted", aes(month, lm.fitted)) +
  scale_x_date(breaks = mo$month) +
  theme_minimal(base_size = 14, base_family = "FranklinGothicSSK") +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm")) +
  labs(title = "Poisson, Linear, and Loess Regressions on listing count ~ month", 
       x = "", y = "", fill = "")

pm01p

# Mean-Variance comparison ----------------------------------------------------

# this doesn't make sense yet.
plot(pm01$fitted, pm01$residuals, pch=19, col="black", ylab="Residuals", xlab="fitted")
points(pm01$residuals, col = "red3", pch = 2)
points(pm01$fitted, col = "blue", pch = 6)

# confidence intervals --------------------------------------------------------

exp(confint(pm01, "month"))
#    2.5 %   97.5 % 
# 1.010840 1.010891
# 1.1% and 1.1%

exp(confint(pm02, "month"))
#    2.5 %   97.5 % 
# 1.008347 1.013614
# 0.8% and 1.36%

# model agnostic via lecture/stack overflow
confint.agnostic <- function (object, parm, level = 0.95, ...)
{
    cf <- coef(object); pnames <- names(cf)
    if (missing(parm))
        parm <- pnames
    else if (is.numeric(parm))
        parm <- pnames[parm]
    a <- (1 - level)/2; a <- c(a, 1 - a)
    pct <- stats:::format.perc(a, 3)
    fac <- qnorm(a)
    ci <- array(NA, dim = c(length(parm), 2L), dimnames = list(parm,
                                                               pct))
    ses <- sqrt(diag(sandwich::vcovHC(object)))[parm]
    ci[] <- cf[parm] + ses %o% fac
    ci
}

confint(pm01)
#                     2.5 %    97.5 %
# (Intercept)     8.9634324 9.0072975
# month2014-02-01 1.2043185 1.2541885 
# month2014-03-01 0.8304071 0.8827636
# month2014-04-01 0.8921460 0.9440350
# month2014-05-01 1.6487995 1.6966055
# month2014-06-01 1.1328374 1.1831226
# month2014-07-01 1.5869557 1.6350016
# month2014-08-01 1.5988124 1.6468113
# month2014-09-01 2.1851487 2.2313614
# month2014-10-01 3.1315862 3.1763778
# month2014-11-01 3.5206427 3.5651378   356%?
# month2014-12-01 3.4601098 3.5046440   350% increase?

confint.agnostic(pm01, "month")
#             2.5 %     97.5 %
# month 0.008025378 0.01358811
# result here is similar to CIs returned by quasipoisson model
# 0.8% and 1.3%

confint.agnostic(pm02, "month")
#             2.5 %     97.5 %
# month 0.008025378 0.01358811
# 0.8% and 1.3%
