# Agora Marketplace Analysis
# Poisson Regression - Monthly, Weekly, Daily counts
# "How much is there? --> How much could there be?"

# load data -------------------------------------------------------------------

library(data.table)
library(ggplot2)
library(vcd)
library(extrafont)
library(extrafontdb)

library(sandwich)
library(glmm)
library(lme4)
library(boot)
library(caret)

agora <- fread("~/GitHub/agora-data/agora-01b.csv", stringsAsFactors = T)
agora$date <- as.Date(agora$date)

wk <- fread("data/WeeklyCountsFitted.csv")
wk$week <- as.Date(wk$week)

# w14 <- fread("data/WeeklyCounts14.csv")
# w15 <- fread("data/WeeklyCounts15.csv")
# mo <- fread("data/MonthlyAll.csv")

# split B/A -------------------------------------------------------------------

# split is determined by Silk Road 2 (SR2) shut down date - 
# an interval loosely defined between 2014-09-25 - 2014-10-10
weekly01 <- wk[1:36, ]
weekly02 <- wk[37:73, ]
weekly03 <- wk[33:73, ]

# date sequences for plot labels
dates <- seq(as.Date("2014-01-01"), as.Date("2015-07-01"), by = "month")
dates01 <- seq(as.Date("2014-01-01"), as.Date("2014-10-01"), by = "month")
dates02 <- seq(as.Date("2014-10-01"), as.Date("2015-07-01"), by = "month")
dates03 <- seq(as.Date("2014-09-01"), as.Date("2015-07-01"), by = "month")

# observed values pre Oct2014/SR shutdown -------------------------------------
wk01.01 <- ggplot(weekly01, aes(week, count)) +
  geom_point(size = 2, color = "firebrick3", alpha = 1) +
  scale_x_date(breaks = dates01, labels = c("2014 - Jan", "Feb", "Mar", "Apr", "May", "June", 
                                            "July", "Aug", "Sept", "Oct"))

wk01.01 + theme_minimal(base_size = 12, base_family = "GillSans") +
  theme(axis.text.x = element_text(size = 12, angle = 35, hjust = 1, vjust = 1)) +
  theme(axis.text.y = element_text(size = 12)) +
  theme(plot.margin = unit(c(0.25, 0.25, 0.10, 0.25), "cm")) +
  labs(title = substitute(
    paste("pre-QuasiPoisson (", sigma, "=", mu^2, ") - Observed Listing Count ~ Week (Jan 2015 - Oct 2014)")), 
    x = "", y = "", fill = "")

# observed values post Oct2014/SR shutdown ------------------------------------
wk01.02 <- ggplot(weekly02, aes(week, count)) +
  geom_point(size = 2, color = "firebrick3", alpha = 1) +
  scale_y_continuous(limits = c(0, 100000)) +
  scale_x_date(breaks = dates02, labels = c("2014 - Oct", "Nov", "Dec",
                                            "2015 - Jan", "Feb", "Mar", "Apr", 
                                            "May", "June", "July"))

wk01.02 + theme_minimal(base_size = 12, base_family = "GillSans") +
  theme(axis.text.x = element_text(size = 12, angle = 35, hjust = 1, vjust = 1)) +
  theme(axis.text.y = element_text(size = 12)) +
  theme(plot.margin = unit(c(0.25, 0.25, 0.10, 0.25), "cm")) +
  labs(title = substitute(
    paste("pre-QuasiPoisson (", sigma, "=", mu^2, ") 2 - Observed Listing Count ~ Week (Oct 2014 - July 2015)")), 
    x = "", y = "", fill = "")

# observed values Sept 2014-July 2015 -----------------------------------------
wk01.03 <- ggplot(weekly03, aes(week, count)) +
  geom_point(size = 2, color = "firebrick3", alpha = 1) +
  scale_y_continuous(limits = c(0, 100000)) +
  scale_x_date(breaks = dates03, labels = c("2014 - Sept", "Oct", "Nov", "Dec",
                                            "2015 - Jan", "Feb", "Mar", "Apr", 
                                            "May", "June", "July"))

wk01.03 + theme_minimal(base_size = 12, base_family = "GillSans") +
  theme(axis.text.x = element_text(size = 12, angle = 35, hjust = 1, vjust = 1)) +
  theme(axis.text.y = element_text(size = 12)) +
  theme(plot.margin = unit(c(0.25, 0.25, 0.10, 0.25), "cm")) +
  labs(title = substitute(
    paste("pre-QuasiPoisson (", sigma, "=", mu^2, ") 2 - Observed Listing Count ~ Week (Sept 2014 - July 2015)")), 
    x = "", y = "", fill = "") +
  annotate("text", x = as.Date("2014-09-20"), y = 46000, label = "SR2 shut down",
           family = "GillSans", size = 4) +
  annotate("segment", x = as.Date("2014-09-21"), xend = as.Date("2014-09-27"), y = 44000, yend = 40000,
           arrow = arrow(length = unit(0.2, "cm")))


# entire date range with annotation of SR2 ------------------------------------
wk01.04 <- ggplot(wk, aes(week, count)) +
  geom_point(size = 2, color = "firebrick3", alpha = 1) +
  scale_y_continuous(limits = c(0, 100000)) +
  scale_x_date(breaks = dates, labels = c("2014 - Jan", "Feb", "Mar", "Apr", "May", "June", 
                                            "July", "Aug", "Sept", "Oct", "Nov", "Dec",
                                            "2015 - Jan", "Feb", "Mar", "Apr", 
                                            "May", "June", "July"))

# arrow annotation
wk01.04 + theme_minimal(base_size = 12, base_family = "GillSans") +
  theme(axis.text.x = element_text(size = 12, angle = 35, hjust = 1, vjust = 1)) +
  theme(axis.text.y = element_text(size = 12)) +
  theme(plot.margin = unit(c(0.25, 0.25, 0.10, 0.25), "cm")) +
  labs(title = substitute(
    paste("pre-QuasiPoisson (", sigma, "=", mu^2, ") 2 - Observed Listing Count ~ Week (Jan 2014 - July 2015)")), 
    x = "", y = "", fill = "") +
  annotate("text", x = as.Date("2014-09-20"), y = 48000, label = "SR2 shut down\nOct 6th, 2014",
           family = "GillSans", size = 4) +
  annotate("segment", x = as.Date("2014-09-21"), xend = as.Date("2014-09-27"), y = 44000, yend = 40000,
           arrow = arrow(length = unit(0.2, "cm")))

# other annotation
wk01.04 + theme_minimal(base_size = 12, base_family = "GillSans") +
  theme(axis.text.x = element_text(size = 12, angle = 35, hjust = 1, vjust = 1)) +
  theme(axis.text.y = element_text(size = 12)) +
  theme(plot.margin = unit(c(0.25, 0.25, 0.10, 0.25), "cm")) +
  labs(title = substitute(
    paste("pre-QuasiPoisson (", sigma, "=", mu^2, ") 2 - Observed Listing Count ~ Week (Jan 2014 - July 2015)")), 
    x = "", y = "", fill = "") +
  annotate("text", x = as.Date("2014-09-25"), y = 46600, label = "Sept-Oct 2014: SR2 shuts down\n market size transition",
           family = "GillSans", size = 4) +
  annotate("segment", x = as.Date("2014-09-10"), xend = as.Date("2014-10-15"), y = 42000, yend = 42000,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm")))

# Piecewise Poisson -----------------------------------------------------------

# pre-SR2 01 ----------------------------------------------
preSR2.01 <- glm(count ~ week, data = weekly01, 
                 family = quasi(link = "log", variance = "mu^2"))
summary(preSR2.01)
#                Estimate Std. Error t value Pr(>|t|)  
#   (Intercept) -91.379087  37.463816  -2.439   0.0201 *
#   week          0.006186   0.002311   2.676   0.0114 *

# (Dispersion parameter for quasi family taken to be 1.191639)

# Null deviance: 109.04  on 35  degrees of freedom
# Residual deviance: 100.35  on 34  degrees of freedom
# AIC: NA
# Number of Fisher Scoring iterations: 11

# slight overdispersion: 1.19


# pre-SR2 02 ----------------------------------------------
preSR2.02 <- glm(count ~ week + wk, data = weekly01, 
                 family = quasi(link = "log", variance = "mu^2"))
summary(preSR2.02)
# Estimate Std. Error t value             Pr(>|t|)    
# (Intercept) -87.950514  32.704284  -2.689              0.01142 *  
#   week          0.005986   0.002019   2.965              0.00578 ** 
#   wkw2          7.677865   0.447491  17.158 < 0.0000000000000002 ***
#   wkw3         -0.234265   0.448160  -0.523              0.60488    
#   wkw4          0.296537   0.449273   0.660              0.51410   

# (Dispersion parameter for quasi family taken to be 0.9002182)

# Null deviance: 109.04  on 35  degrees of freedom
# Residual deviance: 234.42  on 31  degrees of freedom
# AIC: NA
# Number of Fisher Scoring iterations: 25

# very slight underdispersion: 0.90

# pre-SR2 lm ----------------------------------------------

preSR2.lm01 <- lm(count ~ week, data = weekly01)
summary(preSR2.lm01)
#                 Estimate Std. Error t value Pr(>|t|)   
#   (Intercept) -820405.77  281775.43  -2.912  0.00631 **
#   week             51.12      17.38   2.941  0.00585 **

# Residual standard error: 8210 on 34 degrees of freedom
# Multiple R-squared:  0.2028,	Adjusted R-squared:  0.1793 
# F-statistic: 8.648 on 1 and 34 DF,  p-value: 0.005855

# plot fitted vs. linear vs. observed -----------------------------------------

weekly01$qSR201.fitted <- preSR2.01$fitted.values
weekly01$qSR202.fitted <- preSR2.02$fitted.values
weekly01$lm.fitted <- preSR2.lm01$fitted.values

tufte.breaks01 <- weekly01$count

preSR2.01p <- ggplot(weekly01, aes(week, count)) + 
  geom_point(size = 3.5, colour = "firebrick3", shape = 20) + 
  geom_rug(aes(week, count), sides = "l", colour = "firebrick3", size = 0.5) +
  # geom_line(colour = "gold2", linetype = "dashed", size = 1,  aes(week, lm.fitted)) +  
  geom_point(size = 3.5, colour = "deepskyblue3", shape = 1, aes(week, qSR201.fitted)) +
  geom_line(colour = "deepskyblue3", linetype = "solid", size = 1.75,  aes(week, qSR201.fitted)) +
  # compare with quasipoisson on population
  geom_line(size = 1.4, colour = "deepskyblue4", linetype = "dotdash", aes(week, qm02.fitted)) +
  scale_y_continuous(breaks = tufte.breaks01, labels = tufte.breaks01) +
  scale_x_date(breaks = dates01, labels = c("2014 - Jan", "Feb", "Mar", "Apr", "May", "June", 
                                          "July", "Aug", "Sept", "Oct"))

preSR2.01p + theme_minimal(base_size = 12, base_family = "GillSans") +
  theme(axis.text.x = element_text(size = 11, angle = 0, vjust = 0)) +
  theme(axis.text.y = element_text(size = 09)) +
  theme(plot.margin = unit(c(0.25, 0.25, 0.10, 0.25), "cm")) +
  labs(title = substitute(
    paste("QuasiPoisson (", sigma, "=", mu^2, ") Regressions on Listing Count ~ Week (pre-SR2 shutdown)")), 
    x = "", y = "", fill = "") +
  annotate("text", x = as.Date("2014-09-09"), y = 37600, label = "SR2 shuts down", 
           size = 4, family = "GillSans") +
  annotate("segment", x = as.Date("2014-09-24"), xend = as.Date("2014-10-16"), y = 37600, yend = 37600,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.35, "cm")))

