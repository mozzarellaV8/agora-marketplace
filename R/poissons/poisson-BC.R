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

wk <- fread("data/WeeklyCountsFitted.csv")
wk$week <- as.Date(wk$week)

# w14 <- fread("data/WeeklyCounts14.csv")
# w15 <- fread("data/WeeklyCounts15.csv")
# mo <- fread("data/MonthlyAll.csv")

# filter dates ----------------------------------------------------------------

# split is determined by Silk Road 2 (SR2) shut down date - 
# an interval loosely defined between 2014-09-25 - 2014-10-10
weekly00 <- wk[1:40, ]
weekly01 <- wk[1:36, ]
weekly02 <- wk[37:73, ]
weekly03 <- wk[33:73, ]
weekly04 <- wk[21:72, ]

# date sequences for plot labels
dates <- seq(as.Date("2014-01-01"), as.Date("2015-07-01"), by = "month")
dates00 <- seq(as.Date("2014-01-01"), as.Date("2014-11-14"), by = "month")
dates01 <- seq(as.Date("2014-01-01"), as.Date("2014-10-01"), by = "month")
dates02 <- seq(as.Date("2014-10-01"), as.Date("2015-07-01"), by = "month")
dates03 <- seq(as.Date("2014-09-01"), as.Date("2015-07-01"), by = "month")
dates04 <- seq(as.Date("2014-06-01"), as.Date("2015-06-30"), by = "month")

# observed values - Population -----------------------------------------
# annotate with seperate script or below


# POISSON REGRESSION MODELS ---------------------------------------------------


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
