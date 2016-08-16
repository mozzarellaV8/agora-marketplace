# Agora Marketplace Analysis
# total page counts
# distribution by crawl date

# load data -------------------------------------------------------------------

pv <- read.csv("data/crawl-distribution.csv")
str(pv)

pv$date <- as.Date(pv$date)
pv$vendor <- as.integer(pv$vendor)
summary(pv)

#         date                  p             vendor     
# Min.   :2014-01-01   Min.   :    1   Min.   :  1.0  
# 1st Qu.:2014-09-11   1st Qu.: 4453   1st Qu.: 49.5  
# Median :2014-12-07   Median :12697   Median : 96.0  
# Mean   :2014-11-26   Mean   :12154   Mean   : 94.8  
# 3rd Qu.:2015-03-17   3rd Qu.:19030   3rd Qu.:140.5  
# Max.   :2015-07-07   Max.   :27654   Max.   :184.0 

sum(pv$p)
# [1] 2467200
# so we have 2467200 total product pages from 2914-01-01 til 2015-07-07.

sum(pv$vendor)
# [1] 19245
# so we have 19425 total vendor pages from 2914-01-01 til 2015-07-07.

# these are raw counts that don't take into account bad crawls, pages
# with no information, and repeat listings.

# exploratory -----------------------------------------------------------------

par(mar = c(4, 4, 4, 4), mfrow = c(2, 2))

# product page count histogram
hist(pv$p, breaks = 75, xlim = c(0, 28000))
hist(pv$vendor, breaks = 100, xlim = c(0, 200))
plot(pv$p, main = "scatterplot: product page count")
plot(pv$vendor, main = "scatterplot: vendor page count")

# product by date
par(mar = c(6, 6, 6, 6), mfrow = c(1, 1))
plot(pv$date, pv$p, main = "AgMarket - product page count by date", xlab = "date",
     ylab = "product page count")

# tempting to fit a linear model but it likely wouldnt be accurate.
# needs to be converted to a time series.

# vendor pages by date
plot(pv$date, pv$vendor, main = "AgMarket - vendor page count by date", 
     xlab = "date", ylab = "no. of vendor pages")

# area - number of products by date -------------------------------------------
library(ggplot2)

products <- ggplot(pv, aes(date, p)) + theme_minimal() +
  geom_area(fill = "gray50", alpha = 0.25) +
  geom_line() 
  
products + stat_smooth(method = lm, level = 0.95, se = FALSE, color = "#CD2626")

# linear models ---------------------------------------------------------------

# number of product listings by date ----------------------
pd.lm <- lm(p ~ date, data = pv)
summary(pd.lm)
#                  Estimate Std. Error t value Pr(>|t|)    
#   (Intercept) -4.542e+05  5.579e+04  -8.142 4.04e-14 ***
#   date         2.843e+01  3.401e+00   8.360 1.03e-14 ***
# Multiple R-squared:  0.258,	Adjusted R-squared:  0.2543 

par(mar = c(4, 4, 4, 4), mfrow = c(2, 2))
plot(pd.lm$residuals)
plot(pd.lm$coefficients)
plot(pd.lm$rank)
plot(pd.lm$fitted.values)

plot(pd.lm)

pd.lm01 <- ggplot(pv, aes(date, p)) + 
  theme_minimal() +
  geom_point(aes(color = p), size = 4.75, shape = 17) +
  ggtitle("Agora Marketplace: Number of Product Listings ~ Date") +
  theme(plot.title = element_text(family = "Times", face = "bold", size = 18)) +
  labs(x = "Date", y = "number of product listings (pages)") +
  theme(axis.title.x = element_text(family = "Times", face = "italic", size = 14)) +
  theme(axis.title.y = element_text(family = "Times", face = "italic", size = 14)) +
  theme(axis.text.x = element_text(family = "Times", face = "plain", size = 11)) +
  theme(axis.text.y = element_text(family = "Times", face = "plain", size = 11)) +
  theme(plot.margin = unit(c(3, 3, 3, 3), "cm"))

pd.lm01 + stat_smooth(method = lm, level = 0.95, se = FALSE, colour = "#CD2626") +
  theme(axis.title.y = element_text(margin = margin(0, 20, 0, 0))) + 
  theme(axis.title.x = element_text(margin = margin(40, 0, 0, 0)))

# number of product listings by date 02 ---------------------------------------

# What if the market never got shut down? What would the trend be? 
# We know Agora began operations sometime in 2013, and picked up steam
# in 2014 after the downfall of Silk Road 2 - many buyers and vendors migrated 
# over to Agora to continue business. 

# Agora was also subject to frequent downtime, which likely accounts for sudden 
# drops in number of product listings. What if we assume there was no downtime
# for security issues? Can we project a reasonable market size if growth 
# were to stabilize? 

# number of products by downtime ----------------------------------------------

# read in downtime data
updown <- read.csv("data/agoraDNS.csv")
str(updown)
summary(updown)
# range is from 2014-04-23 - 2015-04-04 - April to April

# look at highest downtimes and compare to number products/vendors
# is it possible to throw out product/vendor counts on days of highest downtime?

max(updown$hours_down)
# [1] 30.45 // more hours than in a day
max(updown$hours_up)
# [1] 23.92

updown$date <- as.Date(updown$date)

par(mfrow = c(1, 1), mar = c(6, 6, 6, 6))
plot(updown$date, updown$hours_down, 
     main = "Agora Marketplace: # of hours downtime by date",
     ylab = "number of hours downtime")

agDown <- ggplot(updown, aes(date, hours_down)) + 
  theme_minimal() +
  geom_point(aes(color = hours_down), size = 4.75, shape = 17) +
  ggtitle("Agora Marketplace: Downtime (hours) by Date") +
  theme(plot.title = element_text(family = "Times", face = "bold", size = 18)) +
  labs(x = "Date", y = "number of hours down") +
  theme(axis.title.x = element_text(family = "Times", face = "italic", size = 14)) +
  theme(axis.title.y = element_text(family = "Times", face = "italic", size = 14)) +
  theme(axis.text.x = element_text(family = "Times", face = "plain", size = 11, angle = 45)) +
  theme(axis.text.y = element_text(family = "Times", face = "plain", size = 11)) +
  theme(plot.margin = unit(c(3, 3, 3, 3), "cm"))

agDown

# assuming a 8 hour workday - more than 10 hours of downtime would be a 'day off'
# online markets are 24-7 though; so perhaps 15 hours of downtime is a more 
# reasonable cutoff to say that a market is 'off'.

d10 <- subset(updown, updown$hours_down >= 10)
summary(d10)
# 28 days

d15 <- subset(updown, updown$hours_down >= 15)
summary(d15)
# 13 days


# Determine what is an 'active' marketplace -----------------------------------
# 10000 listings as one cutoff point for determining an active market.
quantile(pv$p)
#    0%   25%   50%   75%  100% 
#     1  4453 12697 19030 27654 
summary(pv$p)
#    Min.  1st Qu.  Median   Mean 3rd Qu.     Max. 
#       1    4453   12700   12150   19030   27650 
pvStd <- subset(pv, pv$p >= 10000)
plot(pvStd$date, pvStd$p)

pv10k.lm <- lm(p ~ date, data = pvStd)
summary(pv10k.lm)
#                 Estimate Std. Error t value Pr(>|t|)    
#   (Intercept) -2.417e+05  6.761e+04  -3.575 0.000517 ***
#   date         1.579e+01  4.109e+00   3.843 0.000201 ***
# Multiple R-squared:  0.1156,	Adjusted R-squared:  0.1078 

RMSE_10k <- (sqrt(sum(pv10k.lm$residuals^2)))/(nrow(pv))
RMSE_10k
# 214.5388

# 5000 listings as cutoff - quarter of listings in general.
pv5000 <- subset(pv, pv$p >= 5000)
plot(pv5000$date, pv5000$p)

pv5k.lm <- lm(p ~ date, data = pv5000)
summary(pv5k.lm)
#               Estimate Std. Error t value Pr(>|t|)   
# (Intercept) -2.787e+05  6.565e+04  -4.245 3.84e-05 ***
# date         1.791e+01  3.994e+00   4.484 1.46e-05 ***
# Multiple R-squared:  0.1196,	Adjusted R-squared:  0.1137 
# given only 3 variables total in this data, they're bound to be significant.

par(mfrow = c(2, 2), mar = c(4, 4, 4, 4))
plot(pv5k.lm)

SSE_5k <- sum(pv5k.lm$residuals^2)
# [1] 4536483133
RMSE_5k <- (sqrt(sum(pv5k.lm$residuals^2)))/(nrow(pv))
RMSE_5k
# 331.7902










# number of vendors by date -------------------------------

vd.lm <- lm(vendor ~ date, data = pv)
summary(vd.lm)
# Coefficients:
#                  Estimate Std. Error t value Pr(>|t|)    
#   (Intercept) -1.505e+03  4.271e+02  -3.523 0.000528 ***
#   date         9.752e-02  2.604e-02   3.745 0.000235 ***
# Multiple R-squared:  0.06523,	Adjusted R-squared:  0.06058

# I've seen better values than these. Plot it quick and check out p ~ vendor.

vd.lm01 <- ggplot(pv, aes(date, vendor)) + theme_minimal() +
  geom_point(aes(color = vendor), size = 4.75, shape = 18) +
  ggtitle("AgMarket: Number of Vendors ~ Date") +
  theme(plot.title = element_text(family= "Times", face = "bold", size = 18)) +
  labs(x = "Date", y = "number of vendors (by page count)") +
  theme(axis.title.x = element_text(family = "Times", face = "italic", size = 14)) +
  theme(axis.title.y = element_text(family = "Times", face = "italic", size = 14)) +
  theme(axis.text.x = element_text(family = "Times", face = "plain", size = 11)) +
  theme(axis.text.y = element_text(family = "Times", face = "plain", size = 11)) +
  theme(axis.title.y = element_text(margin = margin(0, 20, 0, 0))) + 
  theme(axis.title.x = element_text(margin = margin(40, 0, 0, 0))) +
  theme(plot.margin = unit(c(3, 3, 3, 2), "cm"))

vd.lm01 + stat_smooth(method = lm, level = 0.95, se = FALSE, colour = "#CD2626")

par(mar = c(4, 4, 4, 4), mfrow = c(2, 2))
plot(vd.lm)


# number of products by vendor ----------------------------

product.v.lm <- lm(p ~ vendor, data = pv)
summary(product.v.lm)
#               Estimate Std. Error t value Pr(>|t|)    
#  (Intercept) 5248.481    976.419   5.375 2.11e-07 ***
#  vendor        72.838      8.975   8.116 4.75e-14 ***
# Multiple R-squared:  0.2468,	Adjusted R-squared:  0.2431

# again, seen better. 

# number of products by vendor + date ---------------------
product.vd.lm <- lm(p ~ vendor + date, data = pv)
summary(product.vd.lm)
#                 Estimate Std. Error t value Pr(>|t|)    
#   (Intercept) -3.676e+05  5.173e+04  -7.106 2.06e-11 ***
#   vendor       5.757e+01  8.291e+00   6.944 5.22e-11 ***
#   date         2.282e+01  3.166e+00   7.208 1.14e-11 ***

# Multiple R-squared:  0.4021,	Adjusted R-squared:  0.3962
par(mar = c(4, 4, 4, 4), mfrow = c(2, 2))
plot(product.vd.lm)






# prediction of number products ---------------------------
til_now <- data.frame(date = seq(as.Date("2015-07-08"), 
                             as.Date("2016-08-31"), 
                             by = "day"), vendor = 0)

all_time <- data.frame(date = seq(as.Date("2014-01-01"),
                                  as.Date("2016-08-31"),
                                  by = "day"))

p.prediction <- predict.lm(product.vd.lm, newdata = til_now)
summary(p.prediction)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  11800   14200   16590   16590   18990   21380 

par(mar = c(6, 6, 6, 6), mfrow = c(1, 1))
plot(pv$p, pch = 1)
points(p.prediction, pch = 17, col = "red")

plot(p.prediction)
