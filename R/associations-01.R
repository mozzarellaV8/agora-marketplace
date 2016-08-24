# Agora Marketplace Analysis
# arules - 2014 data

# load data -------------------------------------------------------------------

library(data.table)

# clear workspace and set directory
getwd()
setwd("~/GitHub/agora-marketplace")

p2014 <- fread("~/GitHub/agora-data/agora-2014.csv", verbose = T, 
               stringsAsFactors = F)

fb <- fread("~/GitHub/agora-data/2014-AgFb2.csv")
fb$V1 <- NULL

# cleanse further -------------------------------------------------------------

# What do I want? clean url paths, no duplicates in locations, 
# possibly impute categorical data (replace NAs, for transactions)
# Sooner or Later: subset for positive feedbacks (inferring transaction)
# Later: url paths, BTC-dollar ratio.

library(tm)
library(dplyr)
library(tidyr)

p2014$date <- as.Date(p2014$date)


# explore ---------------------------------------------------------------------

length(unique(p2014$date)) # 112
772632/112 # 6898.5 - avg. number of listings per day

# This average is a loose approximation, contingent on crawl frequency
# and accuracy. Cross-reference these counts with the grams dataset.

summary(fb$price)
plot(fb$price)

# subset prices at or under 1000 BTC / approx 5K USD
fb <- subset(fb, fb$price <= 1000)
fb$date <- as.Date(fb$date)

summary(fb$price)
#      Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
#   0.0000    0.0494    0.1534    0.9518    0.4583 1000.0000

options(scipen = 999)
quantile(fb$price)
#            0%           25%           50%           75%          100% 
#    0.00000010    0.04944194    0.15342762    0.45833938 1000.00000000

par(mar = c(6, 6, 6, 6), mfrow = c(1, 1), family = "FranklinGothicSSK")
plot(fb$date, fb$price)
plot(fb$date, fb$price, ylim = c(0, 400))
plot(fb$date, fb$price, ylim = c(0, 50),
     main = "Agora Marketplace 2014: List Prices under 50 BTC")

# density of listings (likely based on more frequent crawls) goes up
# mid september and becomes quite dense in Nov. Silk Road 2 shut down 
# in early November, with multiple news outlets reporting this as early
# as November 8th, 2014.

# explore prices --------------------------------------------------------------


library(ggplot2)
library(hexbin)

p1 <- ggplot(fb, aes(date, price, colour = price)) +
  geom_point(size = 4, shape = 19, alpha = 0.40) +
  theme_minimal(base_size = 12, base_family = "FranklinGothicSSK") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm")) +
  labs(title = "Agora 2014: Product Prices in BTC", x = "", y = "")
  
p1

# subset for large but not outrageous price
fb2 <- subset(fb, fb$price <= 300)
# subset for casual purchase price
fb3 <- subset(fb, fb$price <= 50)
# prices above 1 and below 10
fb4 <- subset(fb, fb$price >= 1 & fb$price <= 10)

p2 <- ggplot(fb2, aes(date, price, colour = price)) +
  geom_point(size = 4, shape = 19, alpha = 0.30) + 
  theme_minimal(base_size = 12, base_family = "FranklinGothicSSK") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm")) +
  labs(title = "Agora 2014: Product Prices in BTC", x = "", y = "")

p2

p3 <- ggplot(fb3, aes(date, price, colour = price)) +
  geom_point(size = 4, shape = 19, alpha = 0.20) + 
  theme_minimal(base_size = 12, base_family = "FranklinGothicSSK") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm")) +
  labs(title = "Agora 2014: Product Prices in BTC", x = "", y = "")

p3

p4 <- ggplot(fb4, aes(date, price, colour = price)) +
  geom_point(size = 4, shape = 19, alpha = 0.15) + 
  theme_minimal(base_size = 12, base_family = "FranklinGothicSSK") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm")) +
  labs(title = "Agora 2014: Product Prices in BTC", x = "", y = "")

p4 + annotate("text", x = as.Date("2014-11-02"), y = 2,
              label = "Silk Road 2 shuts down (2014-11-04)",
              colour = "red3", size = 4, angle = 45)

summary(fb4$date)
class(fb$date)

# look at counts --------------------------------------------------------------

# frequencies of specific products
c1 <- as.data.frame(table(fb$product))
rownames(c1) <- NULL
colnames(c1) <- c("product", "pCount")

c3 <- as.data.frame(table(fb$date, fb$vendor))
rownames(c3) <- NULL
colnames(c3) <- c("date", "vendor", "vCount")
c3$vendor <- gsub("%7E", "", c3$vendor)
c3$vendor <- gsub("/user/", "", c3$vendor)
c3 <- subset(c3, c3$vCount != 0)

c2 <- as.data.frame(table(fb$date))
rownames(c2) <- NULL
colnames(c2) <- c("date", "productCount")
c2$date <- as.Date(c2$date)

sum(c2$productCount) # 349478
mean(c2$productCount) # 3120.339
summary(c2)
#               date     productCount   
# Min.   :2014-01-01   Min.   :   1.0  
# 1st Qu.:2014-05-31   1st Qu.: 794.8  
# Median :2014-09-27   Median :2539.0  
# Mean   :2014-08-14   Mean   :3120.3  
# 3rd Qu.:2014-11-04   3rd Qu.:5222.5  
# Max.   :2014-12-03   Max.   :9300.0 

# number of pages by date
plot(c2$date, c2$productCount)

c2p <- ggplot(c2, aes(date, productCount, colour = productCount)) +
  geom_point(size = 3, shape = 19, alpha = 0.80) +
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm")) +
  theme(axis.text.x = element_text(angle = 45)) +
  theme(plot.title = element_text(vjust = 2)) +
  labs(title = "Agora Marketplace 2014: Product Count by Date",
       x = "", y = "")

c2p
c2p + geom_rug(position = "jitter", size = 0.2)

c2p2 <- c2p + theme_minimal(base_family = "FranklinGothicSSK") +
  geom_rug(size = 0.2)

# look at total crawl counts --------------------------------------------------

p <- read.csv("data/counts/crawl-distribution.csv")
str(p)
p$date <- as.Date(p$date)
p$vendor <- as.integer(p$vendor)

# linear model 01 -------------------------------------------------------------

pd.lm01 <- ggplot(p, aes(date, p)) + 
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

pd.lm01 + stat_smooth(method = lm, level = 0.95, se = FALSE, colour = "#CD2626",
                      linetype = "dashed") +
  theme(axis.title.y = element_text(margin = margin(0, 20, 0, 0))) + 
  theme(axis.title.x = element_text(margin = margin(40, 0, 0, 0)))

# Poisson model on Product Counts by date -------------------------------------

pd.glm01 <- glm(p ~ date, family = "poisson", data = p)
summary(pd.glm01)
# Deviance Residuals: 
#      Min        1Q    Median        3Q       Max  
# -187.420   -59.783    -0.658    53.341    93.563 

#                     Estimate Std. Error z value Pr(>|z|)    
#  (Intercept)     -3.319e+01  8.372e-02  -396.4   <2e-16 ***
#  date             2.593e-03  5.090e-06   509.5   <2e-16 ***

#     Null deviance: 1214565  on 202  degrees of freedom
# Residual deviance:  932944  on 201  degrees of freedom
# AIC: 935136

# That Z-Value is through the roof. 

options(scipen = 4)
pd.glm02 <- glm(p ~ date + vendor, family = "poisson", data = p)
summary(pd.glm02)
# Deviance Residuals: 
#      Min        1Q    Median        3Q       Max  
# -223.648   -38.573    -1.081    30.056   143.665 

#                     Estimate Std. Error    z value Pr(>|z|)    
#   (Intercept) -28.620540551   0.087994160  -325.3   <2e-16 ***
#   date          0.002283534   0.000005363   425.8   <2e-16 ***
#   vendor        0.005016896   0.000012577   398.9   <2e-16 ***

#     Null deviance: 1214565  on 202  degrees of freedom
# Residual deviance:  768506  on 200  degrees of freedom
# AIC: 770700




