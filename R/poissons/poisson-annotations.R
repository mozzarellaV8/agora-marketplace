# Agora Marketplace Analysis
# Poisson Regressions - 
# Monthly, Weekly, and Daily counts of product listings
# "How much is there? --> How much could there be?"

library(data.table)
library(ggplot2)
library(broom)
library(vcd)
library(extrafont)
library(extrafontdb)

# load data -------------------------------------------------------------------

agora <- fread("~/GitHub/agora-data/agora-01b.csv", stringsAsFactors = T)
agora$date <- as.Date(agora$date)

wk <- fread("data/WeeklyCountsFitted.csv")
wk$week <- as.Date(wk$week)

# w14 <- fread("data/WeeklyCounts14.csv")
# w15 <- fread("data/WeeklyCounts15.csv")
# mo <- fread("data/MonthlyAll.csv")

# Domain Annotation for all plots ---------------------------------------------

# With ggplot2 object, these annotations can be added to most plots.
# Annotations refer to significant events in the domain. 
# Shaded temporal region with correpsonding heights: Bar Chart effect.

pop00p + annotate("text", x = as.Date("2014-09-20"), y = 99192, label = "SR2 shuts down",
                   family = "Times New Roman", fontface = "italic", size = 4, hjust = 1) + 
  annotate("segment", x = as.Date("2014-09-22"), xend = as.Date("2014-10-08"), y = 99192, yend = 99192,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.3, "cm"))) +
  annotate("rect", xmin = as.Date("2014-09-22"), xmax = as.Date("2014-10-08"), ymin = 0, ymax = 99192,
           fill = "bisque3", alpha = 0.125) +
  
  annotate("text", x = as.Date("2015-03-14"), y = 94267, label = "Evolution: exit scam",
           family = "Times New Roman", fontface = "italic", size = 4, hjust = 1) + 
  annotate("segment", x = as.Date("2015-03-16"), xend = as.Date("2015-03-18"), y = 94267, yend = 94267,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.15, "cm"))) +
  annotate("rect", xmin = as.Date("2015-03-16"), xmax = as.Date("2015-03-18"), ymin = 0, ymax = 94267,
           fill = "gold3", alpha = 0.15) +
  
  annotate("text", x = as.Date("2015-04-14"), y = 31291, label = "Agora:\npayment &\nwithdrawal\nissues",
           family = "Times New Roman", fontface = "italic", size = 4, lineheight = 0.85, hjust = 1) + 
  annotate("segment", x = as.Date("2015-04-16"), xend = as.Date("2015-05-06"), y = 31291, yend = 31291,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.275, "cm"))) +
  annotate("rect", xmin = as.Date("2015-04-16"), xmax = as.Date("2015-05-06"), ymin = 0, ymax = 31291,
           fill = "red3", alpha = 0.125) +
  
  annotate("text", x = as.Date("2015-06-19"), y = 17183, label = "Agora:\nDoS attack;\nJS exploit\n(CSRF)",
           family = "Times New Roman", fontface = "italic", size = 4, lineheight = 0.85, hjust = 0) + 
  annotate("segment", x = as.Date("2015-05-28"), xend = as.Date("2015-06-16"), y = 17183, yend = 17183,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.275, "cm"))) +
  annotate("rect", xmin = as.Date("2015-05-28"), xmax = as.Date("2015-06-16"), ymin = 0, ymax = 17183,
           fill = "firebrick3", alpha = 0.125)

# annotations pre-SR2 ---------------------------------------------------------

pre00p + annotate("text", x = as.Date("2014-09-20"), y = 98240, label = "SR2 shuts down",
                   family = "Times New Roman", fontface = "italic", size = 4, hjust = 1) + 
  annotate("segment", x = as.Date("2014-09-22"), xend = as.Date("2014-10-08"), y = 98240, yend = 98240,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.3, "cm"))) +
  annotate("rect", xmin = as.Date("2014-09-22"), xmax = as.Date("2014-10-08"), ymin = 0, ymax = 98240,
           fill = "bisque3", alpha = 0.15)
  

# Original Annotations --------------------------------------------------------

wk01.04 + annotate("text", x = as.Date("2014-09-20"), y = 58000, label = "SR2 shuts down",
                   family = "Times New Roman", fontface = "italic", size = 4.25, hjust = 1) + 
  annotate("segment", x = as.Date("2014-09-22"), xend = as.Date("2014-10-08"), y = 58000, yend = 58000,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.25, "cm"))) +
  annotate("rect", xmin = as.Date("2014-09-22"), xmax = as.Date("2014-10-08"), ymin = 0, ymax = 100000,
           fill = "bisque3", alpha = 0.25) + 
  
  annotate("text", x = as.Date("2015-03-14"), y = 38000, label = "Evolution: exit scam",
           family = "Times New Roman", fontface = "italic", size = 4, hjust = 1) + 
  annotate("segment", x = as.Date("2015-03-16"), xend = as.Date("2015-03-18"), y = 38000, yend = 38000,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.15, "cm"))) +
  annotate("rect", xmin = as.Date("2015-03-16"), xmax = as.Date("2015-03-18"), ymin = 0, ymax = 100000,
           fill = "gold3", alpha = 0.35) +
  
  annotate("text", x = as.Date("2015-04-14"), y = 24000, label = "Agora:\npayment,\nwithdrawal\nissues",
           family = "Times New Roman", fontface = "italic", size = 3.75, lineheight = 0.85, hjust = 1) + 
  annotate("segment", x = as.Date("2015-04-16"), xend = as.Date("2015-05-06"), y = 24000, yend = 24000,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.3, "cm"))) +
  annotate("rect", xmin = as.Date("2015-04-16"), xmax = as.Date("2015-05-06"), ymin = 0, ymax = 100000,
           fill = "red3", alpha = 0.25) +
  
  annotate("text", x = as.Date("2015-06-20"), y = 7000, label = "Agora:\nDoS attack;\nJS exploit\n(CSRF)",
           family = "Times New Roman", fontface = "italic", size = 3.75, lineheight = 0.85, hjust = 0) + 
  annotate("segment", x = as.Date("2015-05-28"), xend = as.Date("2015-06-16"), y = 7000, yend = 7000,
           arrow = arrow(ends = "both", angle = 90, length = unit(0.3, "cm"))) +
  annotate("rect", xmin = as.Date("2015-05-28"), xmax = as.Date("2015-06-16"), ymin = 0, ymax = 100000,
           fill = "firebrick3", alpha = 0.25) 


# -----------------------------------------------------------------------------
# preSR2.01p + annotate("text", x = as.Date("2014-09-05"), y = 35000, label = "SR2 shuts down",
#                       family = "Times New Roman", fontface = "italic", size = 4.25) + 
#   annotate("segment", x = as.Date("2014-09-22"), xend = as.Date("2014-10-08"), y = 35000, yend = 35000,
#            arrow = arrow(ends = "both", angle = 90, length = unit(0.3, "cm"))) +
#   annotate("rect", xmin = as.Date("2014-09-22"), xmax = as.Date("2014-10-08"), ymin = 0, ymax = 40000,
#            fill = "bisque3", alpha = 0.25)

# plot with entire population of observed values

# preSR2.01p2 <- ggplot(wk, aes(week, count)) + 
#  geom_point(size = 3.5, colour = "firebrick3", shape = 20) + 
#  geom_point(size = 3.5, colour = "deepskyblue3", shape = 1, aes(week, qSR201.fitted), data = weekly01) +
#  geom_line(colour = "deepskyblue3", linetype = "solid", size = 1.75, aes(week, qSR201.fitted), data = weekly01) +
#  geom_line(size = 1.4, colour = "deepskyblue4", linetype = "dotdash", aes(week, qm02.fitted), data = wk) +

#  scale_y_continuous(limits = c(0, 100000)) +
#  scale_x_date(breaks = dates, labels = c("Jan", "Feb", "Mar", "Apr", "May", "June", 
#                                          "July", "Aug", "Sept", "Oct", "Nov", 
#                                          "Dec\n2014", "Jan", "Feb", "Mar", "Apr", 
#                                          "May", "June", "July")) + 
#  
#  theme_minimal(base_size = 12, base_family = "GillSans") +
#  theme(axis.text.x = element_text(size = 12, angle = 0, vjust = 0)) +
#  theme(axis.text.y = element_text(size = 12)) +
#  theme(axis.title.y = element_text(family = "Times New Roman", face = "italic", 
#                                    margin = margin(0, 30, 0, 0))) +
#  theme(plot.margin = unit(c(0.25, 0.25, 0.10, 0.25), "cm")) +
#  labs(title = substitute(
#    paste("QuasiPoisson (", sigma, "=", mu^2, ") Regressions on Product Listings ~ Week (pre-SR2 shutdown)")), 
#    x = "", y = "number of product listings") 

# preSR2.01p2 + annotate("text", x = as.Date("2014-08-24"), y = 50000, 
#                       label = "SR2 shuts down",family = "Times New Roman", 
#                       fontface = "italic", size = 4) + 
#  annotate("segment", x = as.Date("2014-09-22"), xend = as.Date("2014-10-08"), 
#           y = 50000, yend = 50000, arrow = arrow(ends = "both", angle = 90, 
#                                                  length = unit(0.3, "cm"))) +
#  annotate("rect", xmin = as.Date("2014-09-22"), xmax = as.Date("2014-10-08"), 
#           ymin = 0, ymax = 100000, fill = "bisque3", alpha = 0.25)

# -----------------------------------------------------------------------------
# postSR2.01p + annotate("text", x = as.Date("2014-10-24"), y = 88000, label = "SR2 shuts down",
#                        family = "Times New Roman", fontface = "italic", size = 4) + 
#   annotate("segment", x = as.Date("2014-09-22"), xend = as.Date("2014-10-08"), y = 88000, yend = 88000,
#            arrow = arrow(ends = "both", angle = 90, length = unit(0.3, "cm"))) +
#  annotate("rect", xmin = as.Date("2014-09-22"), xmax = as.Date("2014-10-08"), ymin = 0, ymax = 100000,
#           fill = "bisque3", alpha = 0.15) +
#  annotate("text", x = as.Date("2015-02-25"), y = 38000, label = "Evolution: exit scam",
#           family = "Times New Roman", fontface = "italic", size = 3.75) + 
#  annotate("segment", x = as.Date("2015-03-16"), xend = as.Date("2015-03-18"), y = 38000, yend = 38000,
#           arrow = arrow(ends = "both", angle = 90, length = unit(0.15, "cm"))) +
#  annotate("rect", xmin = as.Date("2015-03-16"), xmax = as.Date("2015-03-18"), ymin = 0, ymax = 100000,
#           fill = "gold3", alpha = 0.175) +
#  annotate("text", x = as.Date("2015-04-12"), y = 25000, label = "Agora:\npayment &\nwithdrawal\nissues",
#           family = "Times New Roman", fontface = "italic", size = 3.75, lineheight = 0.85, hjust = 1) + 
#  annotate("segment", x = as.Date("2015-04-15"), xend = as.Date("2015-05-06"), y = 25000, yend = 25000,
#          arrow = arrow(ends = "both", angle = 90, length = unit(0.275, "cm"))) +
#  annotate("rect", xmin = as.Date("2015-04-15"), xmax = as.Date("2015-05-06"), ymin = 0, ymax = 100000,
#           fill = "red3", alpha = 0.15) +
#  annotate("text", x = as.Date("2015-06-19"), y = 7000, label = "DoS attack;\nJS exploit\n(CSRF)",
#           family = "Times New Roman", fontface = "italic", size = 3.75, lineheight = 0.85, hjust = 0) + 
#  annotate("segment", x = as.Date("2015-05-28"), xend = as.Date("2015-06-16"), y = 7000, yend = 7000,
#           arrow = arrow(ends = "both", angle = 90, length = unit(0.275, "cm"))) +
#  annotate("rect", xmin = as.Date("2015-05-28"), xmax = as.Date("2015-06-16"), ymin = 0, ymax = 100000,
#           fill = "firebrick3", alpha = 0.15)