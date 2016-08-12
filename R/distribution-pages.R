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

sum(pv$vendor)
# [1] 19245

# explore ---------------------------------------------------------------------

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

# add lines to product
plot(pv$date, pv$p, type = "b", main = "AgMarket - product page count by date", 
     xlab = "date", ylab = "product page count")

# vendor pages by date
plot(pv$date, pv$vendor, main = "AgMarket - vendor page count by date", 
     xlab = "date", ylab = "no. of vendor pages")

# area - number of products by date -------------------------------------------
library(ggplot2)

products <- ggplot(pv, aes(date, p)) + theme_minimal() +
  geom_area(fill = "gray50", alpha = 0.25) +
  geom_line() 
  
products + stat_smooth(method = lm, level = 0.95, se = FALSE, color = "#CD2626")

# linear model ----------------------------------------------------------------
# give in to temptation

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
  ggtitle("AgMarket: Number of Product Listings ~ Date") +
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

par(mar = c(4, 4, 4, 4), mfrow = c(2, 2))
plot(product.vd.lm)


# prediction of product listings
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
