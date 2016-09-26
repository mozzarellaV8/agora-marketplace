# Agora Marketplace Analysis
# Poisson Regression - Monthly, Weekly, Daily counts
# "How much is there? --> How much could there be?"

# load data -------------------------------------------------------------------

library(data.table)
library(sandwich)
library(ggplot2)
library(dplyr)
library(broom)

library(vcd)
library(extrafont)
library(extrafontdb)

agora <- fread("~/GitHub/agora-data/agora-01b.csv", stringsAsFactors = T)
agora$date <- as.Date(agora$date)

wk <- fread("data/counts/WeeklyCountsFitted.csv")
wk$week <- as.Date(wk$week)

# w14 <- fread("data/WeeklyCounts14.csv")
# w15 <- fread("data/WeeklyCounts15.csv")
# mo <- fread("data/MonthlyAll.csv")

# filter dates ----------------------------------------------------------------

# split is determined by Silk Road 2 (SR2) shut down date - 
# an interval loosely defined between 2014-09-29 - 2014-11-06

weekly00 <- wk[1:36, ]    # pre-SR2 - before complaint 
weekly01 <- wk[1:41, ]    # through-SR2  (2014-11-06)
weekly02 <- wk[36:73, ]   # through-post-SR2
weekly03 <- wk[33:73, ]   # through-post-SR2
weekly04 <- wk[41:73, ]   # post-SR2    (2014-11-07 - 2015-07-07)
weekly05 <- wk[21:72, ]   # June 2014 to June 2015

# date sequences for plot labels
dates <- seq(as.Date("2014-01-01"), as.Date("2015-07-01"), by = "month") # population

dates00 <- seq(as.Date("2014-01-01"), as.Date("2014-10-01"), by = "month") # pre-SR2
dates01 <- seq(as.Date("2014-01-01"), as.Date("2014-11-07"), by = "month") # through-SR2
dates02 <- seq(as.Date("2014-09-28"), as.Date("2015-07-07"), by = "month") # through-post-SR2
dates03 <- seq(as.Date("2014-09-07"), as.Date("2015-07-07"), by = "month") # through-post-SR2

dates04 <- seq(as.Date("2014-11-01"), as.Date("2015-07-07"), by = "month") # post-SR2
dates05 <- seq(as.Date("2014-06-01"), as.Date("2015-06-30"), by = "month") # junes

# observed values pre-SR wk00 -------------------------------------------------
# for editing (consider - RUG as GRID):
# tufte.breaks01 <- weekly01$count
# tb02 <- weekly02$count
# tb03 <- weekly03$count
# tb04 <- wk$count
# tufte.sorted <- tufte.breaks01[order(tufte.breaks01, decreasing = T)]
# write(tufte.sorted, file = "~/GitHub/agora-local-market/info/tufte.txt")

wk01.00 <- ggplot(weekly00, aes(week, count)) +
  geom_point(size = 3.25, color = "firebrick3", alpha = 1) + 
#  geom_line(aes(week, qm02.fitted), size = 0.75, alpha = 0.5, 
#           colour = "steelblue3", linetype = "dashed") +
  
  scale_y_continuous(limits = c(0, 50000)) +
  scale_x_date(breaks = dates00, labels = c("2014 • January", "Feb", "Mar", 
                                            "Apr", "May", "June", "July", 
                                            "Aug", "Sept", "Oct")) + 
  
  theme_minimal(base_size = 14, base_family = "GillSans") +
  theme(axis.text.x = element_text(size = 14, angle = 0, vjust = 1),
        axis.text.y = element_text(size = 14),
        axis.title.y = element_text(family = "Times New Roman", face = "italic",
                                    margin = margin(0, 20, 0, 0))) +
  theme(panel.grid.major = element_line(colour = "gray80"),
        plot.margin = unit(c(0.25, 0.75, 0.10, 0.25), "cm")) +
  
  labs(title = substitute(
    paste("Observed Product Listings ~ Week (Jan 2014 - Oct 2014)")), x = "",
    y = "number of product listings")

wk01.00

# observed values pre-SR shutdown wk01-----------------------------------------
wk01.01 <- ggplot(weekly01, aes(week, count)) +
  geom_point(size = 3.25, color = "firebrick3", alpha = 1) +
#  geom_rug(aes(week, count), sides = "l", colour = "firebrick3", size = 0.5) +
#  geom_line(aes(week, qm02.fitted), size = 0.75, alpha = 0.5,
#            colour = "steelblue3", linetype = "dashed") +
  
  scale_y_continuous(limits = c(0, 100000)) +
  scale_x_date(breaks = dates01, labels = c("2014 • January", "Feb", "Mar", "Apr", "May", 
                                            "June", "July", "Aug", "Sept", "Oct", "Nov")) + 
  
  theme_minimal(base_size = 12, base_family = "GillSans") +
  theme(axis.text.x = element_text(size = 12, angle = 0, vjust = 1),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(family = "Times New Roman", face = "italic",
                                    size = 13, margin = margin(0, 20, 0, 0)) ) +
  theme(panel.grid.major = element_line(colour = "gray80"),
        plot.margin = unit(c(0.25, 0.75, 0.10, 0.25), "cm")) +
  labs(title = substitute(
    paste("Observed Product Listings ~ Week (Jan 2014 - Nov 2014)")), 
    x = "", y = "number of product listings")

wk01.01 + annotate("text", x = as.Date("2014-09-25"), y = 38379, label = "SR2 complaint filed",
                   family = "Times New Roman", fontface = "italic", size = 4, hjust = 1) + 
  annotate("segment", x = as.Date("2014-09-28"), xend = as.Date("2014-11-06"), y = 38379, yend = 38379,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.3, "cm"))) +
  annotate("rect", xmin = as.Date("2014-09-28"), xmax = as.Date("2014-11-06"), ymin = 0, ymax = 38379,
           fill = "bisque3", alpha = 0.125) +
  annotate("text", x = as.Date("2014-10-25"), y = 98240, label = "SR2 shut down",
           family = "Times New Roman", fontface = "italic", size = 4, hjust = 1) +
  annotate("segment", x = as.Date("2014-10-28"), xend = as.Date("2014-11-06"), y = 98240, yend = 98240,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.3, "cm"))) +
  annotate("rect", xmin = as.Date("2014-10-28"), xmax = as.Date("2014-11-06"), ymin = 0, ymax = 98240,
           fill = "bisque4", alpha = 0.125)

# observed values post-SR wk02 ------------------------------------------------

wk01.02 <- ggplot(weekly02, aes(week, count)) +
  geom_point(size = 3.25, color = "firebrick3", alpha = 1) +
  
#  previous poisson fit on population:  
#  geom_line(aes(week, qm02.fitted), size = 0.75, alpha = 0.5,
#            colour = "steelblue3", linetype = "dashed") +
  
  scale_y_continuous(limits = c(0, 100000)) +
  scale_x_date(breaks = dates02, labels = c("Oct", "Nov", "December\n2014",
                                            "January\n2015", "Feb", "Mar", "Apr", 
                                            "May", "June", "July")) +
  theme_minimal(base_size = 12, base_family = "GillSans") +
  theme(panel.grid.major = element_line(colour = "gray85"),
        plot.margin = unit(c(0.25, 0.25, 0.10, 0.25), "cm")) +
  theme(axis.text.x = element_text(size = 12, angle = 0, vjust = 1),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(family = "Times New Roman", face = "italic",
                                    size = 13, margin = margin(0, 30, 0, 0))) +
  labs(title = "Observed Product Listings ~ Week, post-SR2 shutdown (Oct 2014 - July 2015)", 
       x = "", y = "number of product listings") 

wk01.02 + annotate("text", x = as.Date("2014-09-28"), y = 40379, label = "SR2 complaint filed",
                   family = "Times New Roman", fontface = "italic", size = 4, hjust = 0) + 
  annotate("segment", x = as.Date("2014-09-28"), xend = as.Date("2014-11-06"), y = 38379, yend = 38379,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.3, "cm"))) +
  annotate("rect", xmin = as.Date("2014-09-28"), xmax = as.Date("2014-11-06"), ymin = 0, ymax = 38379,
           fill = "bisque3", alpha = 0.125) +
  annotate("text", x = as.Date("2014-10-25"), y = 98240, label = "SR2 shut down",
           family = "Times New Roman", fontface = "italic", size = 4, hjust = 1) +
  annotate("segment", x = as.Date("2014-10-28"), xend = as.Date("2014-11-06"), y = 98240, yend = 98240,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.3, "cm"))) +
  annotate("rect", xmin = as.Date("2014-10-28"), xmax = as.Date("2014-11-06"), ymin = 0, ymax = 98240,
           fill = "bisque4", alpha = 0.125)

# observed values - postSR2 - wk03 --------------------------------------------
# annotate with seperate script or below

wk01.03 <- ggplot(weekly03, aes(week, count)) +
  geom_point(size = 3.25, color = "firebrick3", alpha = 1) +
  scale_y_continuous(limits = c(0, 100000)) +
  scale_x_date(breaks = dates03, labels = c("Sept", "Oct", "Nov", "December\n2014",
                                            "January\n2015", "Feb", "Mar", "Apr", 
                                            "May", "June", "July")) + 
  theme_minimal(base_size = 12.5, base_family = "GillSans") +
  theme(plot.margin = unit(c(0.25, 0.25, 0.10, 0.25), "cm")) +
  theme(axis.text.x = element_text(size = 12, angle = 0),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(family = "Times New Roman", face = "italic",
                                    size = 13, margin = margin(0, 30, 0, 0))) +
  labs(title = "Observed Listing Count ~ Week, post-SR2 shutdown (Sept 2014 - July 2015)", 
       x = "", y = "number of product listings")

wk01.03 + annotate("text", x = as.Date("2014-09-28"), y = 40379, label = "SR2 complaint filed",
                   family = "Times New Roman", fontface = "italic", size = 4, hjust = 0) + 
  annotate("segment", x = as.Date("2014-09-28"), xend = as.Date("2014-11-10"), y = 38379, yend = 38379,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.3, "cm"))) +
  annotate("rect", xmin = as.Date("2014-09-28"), xmax = as.Date("2014-11-10"), ymin = 0, ymax = 38379,
           fill = "bisque3", alpha = 0.125) +
  annotate("text", x = as.Date("2014-10-25"), y = 98240, label = "SR2 shut down",
           family = "Times New Roman", fontface = "italic", size = 4, hjust = 1) +
  annotate("segment", x = as.Date("2014-10-28"), xend = as.Date("2014-11-10"), y = 98240, yend = 98240,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.3, "cm"))) +
  annotate("rect", xmin = as.Date("2014-10-28"), xmax = as.Date("2014-11-10"), ymin = 0, ymax = 98240,
           fill = "bisque4", alpha = 0.125)

# observed values - postSR2 - wk04 --------------------------------------------

wk01.04 <- ggplot(weekly04, aes(week, count)) +
  geom_point(size = 3.25, color = "firebrick3", alpha = 1) +
  scale_y_continuous(limits = c(0, 100000)) +
  scale_x_date(breaks = dates04, labels = c("Nov", "December\n2014",
                                            "January\n2015", "Feb", "Mar", "Apr", 
                                            "May", "June", "July")) + 
  theme_minimal(base_size = 12.5, base_family = "GillSans") +
  theme(plot.margin = unit(c(0.25, 0.25, 0.10, 0.25), "cm")) +
  theme(axis.text.x = element_text(size = 12, angle = 0),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(family = "Times New Roman", face = "italic",
                                    size = 13, margin = margin(0, 30, 0, 0))) +
  labs(title = "Observed Listing Count ~ Week, post-SR2 shutdown (Nov 2014 - July 2015)", 
       x = "", y = "number of product listings")

wk01.04 + annotate("text", x = as.Date("2014-11-11"), y = 98240, label = "SR2 shut down",
           family = "Times New Roman", fontface = "italic", size = 4, hjust = 0) +
  annotate("segment", x = as.Date("2014-10-28"), xend = as.Date("2014-11-08"), y = 98240, yend = 98240,
           arrow = arrow(ends = "last", angle = 90, length = unit(0.3, "cm"))) +
  annotate("rect", xmin = as.Date("2014-10-28"), xmax = as.Date("2014-11-08"), ymin = 0, ymax = 98240,
           fill = "bisque4", alpha = 0.125)

# plot observed population ----------------------------------------------------

wk01 <- ggplot(wk, aes(week, count)) +
  geom_point(size = 3.25, color = "firebrick3", alpha = 1) +
  scale_y_continuous(limits = c(0, 100000)) +
  scale_x_date(breaks = dates, labels = c("Jan", "Feb", "Mar", "Apr", "May", "June", 
                                            "July", "Aug", "Sept", "Oct", "Nov", "December\n2014",
                                            "January\n2015", "Feb", "Mar", "Apr", 
                                            "May", "June", "July")) +
  theme_minimal(base_size = 12.5, base_family = "GillSans") +
  theme(plot.margin = unit(c(0.25, 0.75, 0.25, 0.25), "cm"),
        panel.border = element_rect(colour = "gray85", fill = NA, size = 0.5)) +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(family = "Times New Roman", face = "italic",
                                    size = 13, margin = margin(0, 30, 0, 0))) +
  labs(title = "Observed Listing Count ~ Week (Jan 2014 - July 2015)", x = "", 
       y = "number of product listings")

wk01 + annotate("text", x = as.Date("2014-09-24"), y = 38379, label = "SR2 complaint filed",
                family = "Times New Roman", fontface = "italic", size = 4, hjust = 1) + 
  annotate("segment", x = as.Date("2014-09-28"), xend = as.Date("2014-11-08"), y = 38379, yend = 38379,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.3, "cm"))) +
  annotate("rect", xmin = as.Date("2014-09-28"), xmax = as.Date("2014-11-08"), ymin = 0, ymax = 38379,
           fill = "bisque3", alpha = 0.125) +
  annotate("text", x = as.Date("2014-10-25"), y = 98240, label = "SR2 shut down",
           family = "Times New Roman", fontface = "italic", size = 4, hjust = 1) +
  annotate("segment", x = as.Date("2014-10-28"), xend = as.Date("2014-11-08"), y = 98240, yend = 98240,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.3, "cm"))) +
  annotate("rect", xmin = as.Date("2014-10-28"), xmax = as.Date("2014-11-08"), ymin = 0, ymax = 98240,
           fill = "bisque4", alpha = 0.125)

# POISSON REGRESSION MODELS ---------------------------------------------------
  
pop00 <- glm(count ~ week, data = wk,
             family = quasi(link = "log", variance = "mu^2"))
  
summary(pop00)
#                  Estimate  Std. Error t value            Pr(>|t|)    
#   (Intercept) -96.0231972  10.3609211  -9.268 0.00000000000007527 ***
#   week          0.0064864   0.0006337  10.236 0.00000000000000128 ***
# (Dispersion parameter for quasi family taken to be 0.7515571)
#     Null deviance: 175.35  on 72  degrees of freedom
# Residual deviance: 125.06  on 71  degrees of freedom
# AIC: NA

# Number of Fisher Scoring iterations: 13

# It doesn't *SEEM* bad.

tidy(pop00, conf.int = T)
#          term      estimate     std.error statistic                 p.value      conf.low     conf.high
# 1 (Intercept) -96.023197157 10.3609211200 -9.267824 0.000000000000075266669 -119.67018646 -71.873897711
# 2        week   0.006486419  0.0006337028 10.235743 0.000000000000001280221    0.00501057   0.007933962

pop00lm <- lm(count ~ week, data = wk)
tidy(pop00lm, conf.int = T)
#          term      estimate    std.error statistic             p.value       conf.low     conf.high
# 1 (Intercept) -1965368.4852 253298.40859 -7.759103 0.00000000004674878 -2470431.16711 -1460305.8033
# 2        week      122.1596     15.49244  7.885109 0.00000000002730882       91.26851      153.0506

summary(pop00lm)
# Multiple R-squared:  0.4669,	Adjusted R-squared:  0.4594 

# plot Population -------------------------------------------------------------

wk$pop00.fitted <- pop00$fitted.values
wk$pop00lm.fitted <- pop00lm$fitted.values

pop00p <- ggplot(wk, aes(week, count)) + 
  geom_point(size = 3.5, colour = "firebrick3", shape = 19) + 
  geom_line(colour = "gold2", alpha = 0.75, linetype = "dashed", size = 0.5,  aes(week, pop00lm.fitted)) +  
  geom_point(size = 2.5, colour = "deepskyblue3", shape = 1, aes(week, pop00.fitted)) +
  geom_line(colour = "deepskyblue3", linetype = "solid", size = 1.25, alpha = 0.75, aes(week, pop00.fitted)) +
  geom_line(size = 0.5, alpha = 0.75, colour = "deepskyblue4", 
            linetype = "dotted", aes(week, pop00.fitted)) +
  
  scale_y_continuous(limits = c(0, 100000)) +
  scale_x_date(breaks = dates,
               labels = c("January", "Feb", "Mar", "Apr", "May", 
                          "June", "July", "Aug", "Sept", "Oct", "Nov", "December\n2014",
                          "January\n2015", "Feb", "Mar", "Apr", "May", "June", "July")) + 
  
  theme_minimal(base_size = 12, base_family = "GillSans") +
  theme(panel.border = element_rect(color = "gray85", fill = NA, size = 0.5),
        plot.margin = unit(c(0.25, 0.75, 0.10, 0.25), "cm"),
        axis.text.x = element_text(size = 12, angle = 0, vjust = 1),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(family = "Times New Roman", face = "italic", 
                                    margin = margin(0, 30, 0, 0), size = 13.8)) +
  
  labs(title = substitute(
    paste("QuasiPoisson (", sigma^2, " = ", mu^2, ") Regressions on Product Listings ~ Week (pre-SR2 shutdown)")), 
    x = "", y = "number of product listings")

pop00p


# pre-SR2 Poisson Regression 00 -----------------------------------------------  

preSR2.00a <- glm(count ~ week, data = weekly00, family = "poisson")
summary(preSR2.00a)
#                  Estimate   Std. Error t value Pr(>|t|)  
# (Intercept) -96.96243445   0.40821143  -237.5 <0.0000000000000002 ***
# week          0.00653031   0.00002513   259.9 <0.0000000000000002 ***
#     Null deviance: 341449  on 35  degrees of freedom
# Residual deviance: 268766  on 34  degrees of freedom
# AIC: 269099

# Highly overdispersed but with strong effect.

preSR2.00 <- glm(count ~ week, data = weekly00,
                 family = quasi(link = "log", variance = "mu^2"))
  
summary(preSR2.00)
#                  Estimate Std. Error t value Pr(>|t|)  
#   (Intercept) -91.379087  37.463816  -2.439   0.0201 *
#   week          0.006186   0.002311   2.676   0.0114 *

# (Dispersion parameter for quasi family taken to be 1.191639)

#     Null deviance: 109.04  on 35  degrees of freedom
# Residual deviance: 100.35  on 34  degrees of freedom
# AIC: NA

# Number of Fisher Scoring iterations: 11

# Just a touch overdispersed at 1.19 but very close.
# But `week` is not showing up as significant...

wk00 <- tidy(preSR2.00, conf.int = T)

# look at coefficient estimate: one variable only though.
ggplot(wk00, aes(estimate, term, color = term)) +
  geom_point(aes(std.error)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high))

pre00lm <- lm(count ~ week, data = weekly00)
tidy(pre00lm, conf.int = T)
# Multiple R-squared:  0.3049,	Adjusted R-squared:  0.2866
#         term      estimate    std.error statistic      p.value       conf.low    conf.high
#1 (Intercept) -1744530.0392 430225.09432 -4.054924 0.0002398419 -2615475.20928 -873584.8690
#2        week      108.2742     26.51825  4.083007 0.0002205700       54.59081     161.9576


# plot Weekly00 ---------------------------------------------------------------

weekly00$pre00.fitted <- preSR2.00$fitted.values
weekly00$pre00pm.fitted <- preSR2.00$fitted.values
weekly00$pre00lm.fitted <- pre00lm$fitted.values

pre00p <- ggplot(weekly00, aes(week, count)) + 
  geom_point(size = 3.5, colour = "firebrick3", shape = 19) + 
  geom_line(colour = "gold2", alpha = 0.75, linetype = "dashed", size = 0.5,  aes(week, pre00lm.fitted)) +  
  geom_point(size = 2.5, colour = "deepskyblue3", shape = 1, aes(week, pre00.fitted)) +
  geom_line(colour = "deepskyblue3", linetype = "solid", size = 1.25,  aes(week, pre00.fitted)) +
  geom_line(size = 0.5, alpha = 0.75, colour = "deepskyblue4", 
            linetype = "dotted", aes(week, pre00.fitted)) +
  
  scale_y_continuous(limits = c(0, 100000)) +
  scale_x_date(breaks = dates00,
               labels = c("2014 • January", "Feb", "Mar", "Apr", "May", 
                          "June", "July", "Aug", "Sept", "Oct")) + 
  
  theme_minimal(base_size = 12, base_family = "GillSans") +
  theme(panel.border = element_rect(color = "gray85", fill = NA, size = 0.5),
        plot.margin = unit(c(0.25, 0.75, 0.10, 0.25), "cm"),
        axis.text.x = element_text(size = 13.8, angle = 0, vjust = 1),
        axis.text.y = element_text(size = 13.8),
        axis.title.y = element_text(family = "Times New Roman", face = "italic", 
                                    size = 14,  margin = margin(0, 30, 0, 0))) +
  
  labs(title = substitute(
    paste("QuasiPoisson (", sigma^2, " = ", mu^2, ") Regressions on Product Listings ~ Week (pre-SR2 shutdown)")), 
    x = "", y = "number of product listings")


# pre-SR2 Poisson Regression 01 -----------------------------------------------
preSR2.01 <- glm(count ~ week, data = weekly01,
                 family = quasi(link = "log", variance = "mu^2"))
summary(preSR2.01)
#                Estimate Std. Error t value Pr(>|t|)  
#   (Intercept) -130.851987   30.722299  -4.259 0.000125 ***
#   week           0.008628    0.001893   4.557  0.00005 ***

# (Dispersion parameter for quasi family taken to be 1.186086)

#     Null deviance: 132.68  on 40  degrees of freedom
# Residual deviance: 104.33  on 39  degrees of freedom
# AIC: NA
# Number of Fisher Scoring iterations: 12

# slight overdispersion: 1.18. Week is deemed significant, and seeing a stronger
# effect comparing the Null and Residual deviances.

# pre-SR2 - Standard Poisson Regresssion 01 -----------------------------------
pre01pm <- glm(count ~ week, data = weekly01, family = "poisson")
summary(pre01pm)

#                  Estimate   Std. Error z value            Pr(>|z|)
# (Intercept) -171.10915966    0.31394739  -545.0 <0.0000000000000002 ***
# week           0.01110211    0.00001926   576.5 <0.0000000000000002 ***
#     Null deviance: 806719  on 40  degrees of freedom
# Residual deviance: 379800  on 39  degrees of freedom

mv <- c(mean(weekly01$count), var(weekly01$count))
#  13441.76 373468348.04

13441.76^2
180680912/373468348.04
# 0.4837918 - mean squared ends up as 48% of the variance.

var(weekly01$count)/mean(weekly01$count)
#  27784.19 times

# 5% critical value for chi-squared with 34 d.f.
qchisq(0.95, df.residual(pre01pm))
# 54.57223

deviance(pre01pm)
# 379800.5

pr <- residuals(pre01pm,"pearson")
sum(pr^2)
# 269199.8

phi <- sum(pr^2)/df.residual(pre01pm)
# 7917.642
round(c(phi,sqrt(phi)),4)
# 7917.6416   88.9811

7917.6416/88.9811
# 88.98116, var is 88 times the mean
sqrt(7917.6416)
# 88.98113 - the multiplicative factor

pre01qm <- glm(count ~ week, data = weekly01, family = "quasipoisson")
summary(pre01qm)

# week estimate pm/qpm
0.00002513/0.002236

# pre-SR2 Linear Model --------------------------------------------------------
preSR2.lm01 <- lm(count ~ week, data = weekly01)
summary(preSR2.lm01)
#                 Estimate Std. Error t value Pr(>|t|)   
#   (Intercept) -2043014.15   443146.05  -4.610 0.0000425 ***
#   week             126.73       27.31   4.641 0.0000386 ***

# Residual standard error: 8210 on 34 degrees of freedom
# Multiple R-squared:  0.3558,	Adjusted R-squared:  0.3392  
# F-statistic: 21.54 on 1 and 39 DF,  p-value: 0.00003865

weekly01$qSR201.fitted <- preSR2.01$fitted.values
weekly01$lm.fitted <- preSR2.lm01$fitted.values
weekly01$pop.fitted <-wk$pop00.fitted[1:41]
# write.csv(weekly01, file = "wk2-preSR2.csv", row.names = F)

# plot Weekly 01  --------------------------------------------------------
preSR2.01p <- ggplot(weekly01, aes(week, count)) + 
  geom_point(size = 3.5, colour = "firebrick3", shape = 19) + 
  geom_line(colour = "gold2", alpha = 0.75, linetype = "dashed", size = 0.5,  aes(week, lm.fitted)) +  
  geom_point(size = 2.5, colour = "deepskyblue3", shape = 1, aes(week, qSR201.fitted)) +
  geom_line(colour = "deepskyblue3", linetype = "solid", size = 1.25,  aes(week, qSR201.fitted)) +
  geom_line(size = 0.5, alpha = 0.75, colour = "deepskyblue4", 
            linetype = "dotted", aes(week, pop.fitted)) +
  scale_y_continuous(limits = c(0, 100000)) +
  scale_x_date(breaks = dates01,
               labels = c("2014 • January", "Feb", "Mar", "Apr", "May", 
                          "June", "July", "Aug", "Sept", "Oct", "Nov")) + 
  theme_minimal(base_size = 12, base_family = "GillSans") +
  theme(panel.border = element_rect(color = "gray85", fill = NA, size = 0.5),
        plot.margin = unit(c(0.25, 0.75, 0.10, 0.25), "cm")) +
  theme(axis.text.x = element_text(size = 12, angle = 0, vjust = 1),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(family = "Times New Roman", face = "italic", 
                                    size = 13, margin = margin(0, 30, 0, 0))) +
  labs(title = substitute(
    paste("QuasiPoisson (", sigma^2, " = ", mu^2, ") Regressions on Product Listings ~ Week (pre-SR2 shutdown)")), 
    x = "", y = "number of product listings")

# post-SR2 Poisson Regression -------------------------------------------------

post.sr2.q01 <- glm(count ~ week, data = weekly02,
                    family = quasi(link = "log", variance = "mu^2"))
summary(post.sr2.q01)
#                 Estimate Std. Error t value Pr(>|t|)    
#   (Intercept) 32.7194087 12.8119455   2.554   0.0150 *
#   week        -0.0013239  0.0007773  -1.703   0.0971 .

# (Dispersion parameter for quasi family taken to be 0.1582069)
#     Null deviance: 9.0146  on 37  degrees of freedom
# Residual deviance: 8.6089  on 36  degrees of freedom
# AIC: NA
# Number of Fisher Scoring iterations: 4

# Highly Underdispersed - counts became unstable after the SR2 shutdown.

post.sr2.q02 <- glm(count ~ week, data = weekly02, family = "poisson")
summary(post.sr2.q02)
#     Null deviance: 365738  on 37  degrees of freedom
# Residual deviance: 346336  on 36  degrees of freedom

# Highly overdispersed without the quasi parameter; model has weak effect.

# linear model comparison
post.sr2.lm01 <- lm(count ~ week, data = weekly02)
summary(post.sr2.lm01)
# Multiple R-squared:  0.05998,	Adjusted R-squared:  0.03387  
# and no significance in the coefficients, (week$Pr(>|t| = 0.138)),
# rather high.

# post-SR2 Poisson Plot -------------------------------------------------------

# manually fortify
weekly02$postsr2q01.fitted <- post.sr2.q01$fitted.values
weekly02$postsr2lm01.fitted <- post.sr2.lm01$fitted.values
weekly02$pop.fitted <- wk$pop00.fitted[36:73]
# write.csv(weekly02, file = "wk2-postSR2.csv", row.names = F)

# plot Weekly 02 Poisson ------------------------------------------------------
postSR2.01p <- ggplot(weekly02, aes(week, count)) + 
  geom_point(size = 3.5, colour = "firebrick3", shape = 19) + 
  geom_line(size = 0.5, colour = "gold2", alpha = 0.75, linetype = "dashed",  aes(week, postsr2lm01.fitted)) + 
  geom_line(size = 0.5, colour = "deepskyblue4", alpha = 0.75, linetype = "dotted", aes(week, pop.fitted)) +
  geom_point(size = 2.25, colour = "deepskyblue3", shape = 1, aes(week, postsr2q01.fitted)) +
  geom_line(colour = "deepskyblue3", linetype = "solid", size = 1.25,  aes(week, postsr2q01.fitted)) +
  
  scale_y_continuous(limits = c(0, 100000)) +
  scale_x_date(breaks = dates02, labels = c("Oct", "Nov", "December\n2014", "January\n2015", "Feb", 
                                            "Mar", "Apr", "May", "June", "July")) + 
  
  theme_minimal(base_size = 12, base_family = "GillSans") +
  theme(panel.border = element_rect(color = "gray85", fill = NA, size = 0.5),
        plot.margin = unit(c(0.25, 0.75, 0.10, 0.25), "cm")) +
  theme(axis.text.x = element_text(size = 12, angle = 0),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(family = "Times New Roman", face = "italic", 
                                    margin = margin(0, 30, 0, 0))) +
  
  labs(title = substitute(
    paste("QuasiPoisson (", sigma^2, " = ", mu^2, ") Regressions on Product Listings ~ Week (post-SR2 shutdown)")), 
    x = "", y = "number of product listings") 

# post-SR2 Poisson Regression 02 ----------------------------------------------

# poisson model 
pSR.q01 <- glm(count ~ week, data = weekly03,
               family = quasi(link = "log", variance = "mu^2"))

summary(pSR.q01)
#                Estimate  Std. Error t value Pr(>|t|)
#   (Intercept)  8.3587833 13.5919852   0.615    0.542
#   week         0.0001509  0.0008252   0.183    0.856

# (Dispersion parameter for quasi family taken to be 0.2239512)

#     Null deviance: 14.214  on 40  degrees of freedom
# Residual deviance: 14.209  on 39  degrees of freedom
# AIC: NA

# quite underdispersed (0.23) and null and model deviance are essentially equal.

# linear
pSR.lm01 <- lm(count ~ week, data = weekly03)
summary(pSR.lm01)
# Multiple R-squared:  0.0005229,	Adjusted R-squared:  -0.0251

# wow, that's the worst fit i've ever done.

weekly03$pSR.q01.fitted <- pSR.q01$fitted.values
weekly03$pSR.lm01.fitted <- pSR.lm01$fitted.values
weekly03$pop.fitted <- wk$pop00.fitted[33:73]
# write.csv(weekly03, file = "wk3-postSR2.csv", row.names = F)

# plot Weekly 03 Poisson ------------------------------------------------------

postSR2.02p <- ggplot(weekly03, aes(week, count)) + 
  geom_point(size = 3.5, colour = "firebrick3", shape = 19) + 
  geom_line(size = 0.5, colour = "deepskyblue4", alpha = 0.75, linetype = "dotted", aes(week, pop.fitted)) +
  geom_line(size = 0.5, colour = "gold2", alpha = 0.75, linetype = "dashed",  aes(week, pSR.lm01.fitted)) + 
  geom_line(size = 1.25, colour = "deepskyblue3", alpha = 0.95, linetype = "solid",  aes(week, pSR.q01.fitted)) +
  geom_point(size = 2, colour = "deepskyblue3", shape = 1, aes(week, pSR.q01.fitted)) +
  scale_y_continuous(limits = c(0, 100000)) +
  scale_x_date(breaks = dates03, labels = c("Sept", "Oct", "Nov", "December\n2014", "January\n2015", "Feb", 
                                            "Mar", "Apr", "May", "June", "July")) + 
  theme_minimal(base_size = 12, base_family = "GillSans") +
  theme(panel.border = element_rect(colour = "gray85", fill = NA, size = 0.5),
        plot.margin = unit(c(0.25, 0.75, 0.10, 0.25), "cm")) +
  theme(axis.text.x = element_text(size = 12, angle = 0),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(family = "Times New Roman", face = "italic", 
                                    margin = margin(0, 30, 0, 0))) +
  labs(title = substitute(
    paste("QuasiPoisson (", sigma^2, " = ", mu^2, ") Regressions on Product Listings ~ Week (post-SR2 shutdown)")), 
    x = "", y = "number of product listings") 

# post-SR2 Poisson Regression 03 Weekly04 -------------------------------------

# poisson model 
pSR.q02 <- glm(count ~ week, data = weekly04,
               family = quasi(link = "log", variance = "mu^2"))

summary(pSR.q02)
#                Estimate  Std. Error t value Pr(>|t|)
#   (Intercept) 53.7923855 14.5202643   3.705 0.000824 ***
#   week        -0.0025986  0.0008799  -2.953 0.005953 ** 

# (Dispersion parameter for quasi family taken to be 0.1320363)

#     Null deviance: 7.5085  on 32  degrees of freedom
# Residual deviance: 6.3789  on 31  degrees of freedom
# AIC: NA

# quite underdispersed (0.13) and null and model deviance are essentially equal.
# pretty pretty bad.

# linear
pSR.lm02 <- lm(count ~ week, data = weekly04)
summary(pSR.lm02)
#               Estimate Std. Error t value Pr(>|t|) 
# (Intercept) 2389903.8   719518.8   3.322  0.00230 **
# week           -141.4       43.6  -3.244  0.00282 **
# Multiple R-squared:  0.2534,	Adjusted R-squared:  0.2294 


weekly04$wk04.q02.fitted <- pSR.q02$fitted.values
weekly04$wk04.lm02.fitted <- pSR.lm02$fitted.values
weekly04$pop.fitted <- wk$pop00.fitted[41:73]
# write.csv(weekly03, file = "wk3-postSR2.csv", row.names = F)

# plot Weekly 04 Poisson ------------------------------------------------------

postSR2.03p <- ggplot(weekly04, aes(week, count)) + 
  geom_point(size = 3.5, colour = "firebrick3", shape = 19) + 
  geom_line(size = 0.5, colour = "deepskyblue4", alpha = 0.75, linetype = "dotted", aes(week, pop.fitted)) +
  geom_line(size = 0.5, colour = "gold2", alpha = 0.75, linetype = "dashed",  aes(week, wk04.lm02.fitted)) + 
  geom_line(size = 1.25, colour = "deepskyblue3", alpha = 0.95, linetype = "solid",  aes(week, wk04.q02.fitted)) +
  geom_point(size = 2, colour = "deepskyblue3", shape = 1, aes(week, wk04.q02.fitted)) +
  scale_y_continuous(limits = c(0, 100000)) +
  scale_x_date(breaks = dates04, labels = c("Nov", "December\n2014", "January\n2015", "Feb", 
                                            "Mar", "Apr", "May", "June", "July")) + 
  theme_minimal(base_size = 12, base_family = "GillSans") +
  theme(panel.border = element_rect(colour = "gray85", fill = NA, size = 0.5),
        plot.margin = unit(c(0.25, 0.75, 0.10, 0.25), "cm")) +
  theme(axis.text.x = element_text(size = 12, angle = 0),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(family = "Times New Roman", face = "italic", 
                                    size = 13, margin = margin(0, 30, 0, 0))) +
  labs(title = substitute(
    paste("QuasiPoisson (", sigma^2, " = ", mu^2, ") Regressions on Product Listings ~ Week (post-SR2 shutdown)")), 
    x = "", y = "number of product listings") 


# observed values: June to June -----------------------------------------------

junes <- ggplot(weekly05, aes(week, count)) +
  geom_point(size = 3.25, color = "firebrick3", alpha = 1) +
  scale_y_continuous(limits = c(0, 100000)) +
  scale_x_date(breaks = dates04, labels = c("June", "July", "Aug", "Sept", "Oct", 
                                            "Nov", "December\n2014", "January\n2015", 
                                            "Feb", "Mar", "Apr", "May", "June")) +
  theme_minimal(base_size = 12.5, base_family = "GillSans") +
  theme(axis.text.x = element_text(size = 12),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(family = "Times New Roman", face = "italic",
                                    margin = margin(0, 30, 0, 0))) +
  theme(plot.margin = unit(c(0.25, 0.75, 0.25, 0.25), "cm"),
        panel.border = element_rect(colour = "gray85", fill = NA, size = 0.5)) +
  labs(title = "Observed Product Listings ~ Week (June 2014 - June 2015)", 
       x = "", y = "number of product listings")

# Junes Poisson Regression 01--------------------------------------------------

junes01 <- glm(count ~ week, data = weekly05,
               family = quasi(link = "log", variance = "mu^2"))

summary(junes01)
#                  Estimate  Std. Error t value    Pr(>|t|)    
#   (Intercept) -73.8395324  14.3310038  -5.152 0.000004382 ***
#   week          0.0051385   0.0008727   5.888 0.000000326 ***

# (Dispersion parameter for quasi family taken to be 0.5146814)

#     Null deviance: 66.037  on 51  degrees of freedom
# Residual deviance: 55.693  on 50  degrees of freedom
# AIC: NA
# Number of Fisher Scoring iterations: 11

# This is the best model yet in terms of r.deviance:DF ratio.
# And a decent effect, comparing null and r. deviances.
# Dispersion parameter is low at 0.51 - underdispersed...

jlm <- lm(count ~ week, data = weekly04)
summary(jlm)
# Multiple R-squared:  0.2599,	Adjusted R-squared:  0.2451 
# nothing to write home about here...but good as we're comparing
# it to a hopefully more effective poisson model.

weekly05$jq.fitted <- junes01$fitted.values
weekly05$jlm.fitted <- jlm$fitted.values
# write.csv(weekly04, file = "weekly-04-junes.csv", row.names = F)

tidy(junes01)
head(augment(junes01))
glance(junes01)

jb <- tidy(junes01, conf.int = T)
jf <- augment(junes01)

junes.b <- ggplot(jb, aes(estimate, term, color = term)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high))

junes.b

# plot Junes ------------------------------------------------------------------

junes.p <- ggplot(weekly05, aes(week, count)) + 
  geom_point(size = 4, colour = "firebrick3", shape = 19) + 
  geom_line(size = 0.5, colour = "deepskyblue4", alpha = 0.75, linetype = "dotted", aes(week, qm02.fitted)) +
  geom_line(size = 0.5, colour = "gold2", alpha = 0.75, linetype = "dashed",  aes(week, jlm.fitted)) + 
  geom_line(size = 1.25, colour = "deepskyblue3", alpha = 0.95, linetype = "solid",  aes(week, jq.fitted)) +
  geom_point(size = 2, colour = "deepskyblue3", shape = 1, aes(week, jq.fitted)) +
  
  scale_y_continuous(limits = c(0, 100000)) +
  scale_x_date(breaks = dates05, labels = c("June", "July", "Aug", "Sept", "Oct", 
                                            "Nov", "December\n2014", "January\n2015", 
                                            "Feb", "Mar", "Apr", "May", "June")) +
  
  theme_minimal(base_size = 12, base_family = "GillSans") +
  theme(panel.border = element_rect(colour = "gray85", fill = NA, size = 0.5),
        plot.margin = unit(c(0.25, 0.75, 0.10, 0.25), "cm")) +
  theme(axis.text.x = element_text(size = 12, angle = 0),
        axis.text.y = element_text(size = 12),
        axis.title.y = element_text(family = "Times New Roman", face = "italic", 
                                    margin = margin(0, 30, 0, 0))) +
  
  labs(title = substitute(
    paste("QuasiPoisson (", sigma^2, " = ", mu^2, ") Regressions on Product Listings ~ Week (June to June)")), 
    x = "", y = "number of product listings") 

# Domain Annotation for all plots ---------------------------------------------

junes.p + annotate("text", x = as.Date("2014-09-20"), y = 99192, label = "SR2 shuts down",
                   family = "Times New Roman", fontface = "italic", size = 4, hjust = 1) + 
  annotate("segment", x = as.Date("2014-09-22"), xend = as.Date("2014-10-08"), y = 99192, yend = 99192,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.3, "cm"))) +
  annotate("rect", xmin = as.Date("2014-09-22"), xmax = as.Date("2014-10-08"), ymin = 0, ymax = 99192,
           fill = "bisque3", alpha = 0.15) +
  
  annotate("text", x = as.Date("2015-03-14"), y = 94267, label = "Evolution: exit scam",
           family = "Times New Roman", fontface = "italic", size = 4, hjust = 1) + 
  annotate("segment", x = as.Date("2015-03-16"), xend = as.Date("2015-03-18"), y = 94267, yend = 94267,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.15, "cm"))) +
  annotate("rect", xmin = as.Date("2015-03-16"), xmax = as.Date("2015-03-18"), ymin = 0, ymax = 94267,
           fill = "gold3", alpha = 0.175) +
  
  annotate("text", x = as.Date("2015-04-14"), y = 31291, label = "Agora:\npayment &\nwithdrawal\nissues",
           family = "Times New Roman", fontface = "italic", size = 4, lineheight = 0.85, hjust = 1) + 
  annotate("segment", x = as.Date("2015-04-16"), xend = as.Date("2015-05-06"), y = 31291, yend = 31291,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.275, "cm"))) +
  annotate("rect", xmin = as.Date("2015-04-16"), xmax = as.Date("2015-05-06"), ymin = 0, ymax = 31291,
           fill = "red3", alpha = 0.15) +
  
  annotate("text", x = as.Date("2015-06-19"), y = 17183, label = "Agora:\nDoS attack;\nJS exploit\n(CSRF)",
           family = "Times New Roman", fontface = "italic", size = 4, lineheight = 0.85, hjust = 0) + 
  annotate("segment", x = as.Date("2015-05-28"), xend = as.Date("2015-06-16"), y = 17183, yend = 17183,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.275, "cm"))) +
  annotate("rect", xmin = as.Date("2015-05-28"), xmax = as.Date("2015-06-16"), ymin = 0, ymax = 17183,
           fill = "firebrick3", alpha = 0.15)
