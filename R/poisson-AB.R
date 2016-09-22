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

# split B/A -------------------------------------------------------------------

weekly01 <- wk[1:36, ]
weekly02 <- wk[37:73, ]
weekly03 <- wk[33:73, ]

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
    paste("pre-QuasiPoisson (", sigma, "=", mu^2, ") - Observed Listing Count ~ Date (Jan 2015 - Oct 2014)")), 
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
    paste("pre-QuasiPoisson (", sigma, "=", mu^2, ") 2 - Observed Listing Count ~ Date (Oct 2014 - July 2015)")), 
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
    paste("pre-QuasiPoisson (", sigma, "=", mu^2, ") 2 - Observed Listing Count ~ Date (Sept 2014 - July 2015)")), 
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
    paste("pre-QuasiPoisson (", sigma, "=", mu^2, ") 2 - Observed Listing Count ~ Date (Jan 2014 - July 2015)")), 
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
    paste("pre-QuasiPoisson (", sigma, "=", mu^2, ") 2 - Observed Listing Count ~ Date (Jan 2014 - July 2015)")), 
    x = "", y = "", fill = "") +
  annotate("text", x = as.Date("2014-09-25"), y = 48000, label = "Sept/Oct 2014: SR2 shuts down:\n market size transition",
           family = "GillSans", size = 4) +
  annotate("segment", x = as.Date("2014-09-10"), xend = as.Date("2014-10-15"), y = 42000, yend = 42000,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.2, "cm")))



# Piecewise Poisson -----------------------------------------------------------

# pre-SR2

preSR2.01 <- 





