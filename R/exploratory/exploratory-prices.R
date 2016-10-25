# Agra Marketplace Analysis
# Exploratory: Prices

# load data -------------------------------------------------------------------

library(data.table)
library(ggplot2)
library(tidyr)
library(dplyr)
library(zoo)
library(corrplot)
library(broom)

# population with: character counts, subcat agregated, all cats
a <- fread("~/GitHub/agora-data/agora-04.csv", stringsAsFactors = T)
# 2322961 obs of 18 variables
a$date <- as.Date(a$date)
str(a)

# bound to data in extraction/bpi.R

# thmq - Cannabis oz
high <- read.csv("data/thmq.csv")

# Crude Oil Futures
brent <- read.csv("data/oil-Brent.csv")
wti <- read.csv("data/oil-WTI.csv")

# investigate the price bins --------------------------------------------------

summary(a$usd)
# Min.    1st Qu.     Median       Mean    3rd Qu.       Max. 
#    0         24         85       7827        294 2215000000 

# Maximum price of 2,215,000,000 is suspicious.

# check under 150
a1 <- subset(a, a$usd <= 150)
nrow(a1) # 1457401

1457401/nrow(a) # 0.6273894 63% of listings $150 or less
summary(a1$usd)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  0.00    9.97   35.43   46.78   75.87  150.00

nrow(subset(a, a$usd > 150 & a$usd <= 600))   # 515111
nrow(subset(a, a$usd > 600 & a$usd <= 2000))  # 230701
nrow(subset(a, a$usd > 2000 & a$usd <= 10000)) # 106747
nrow(subset(a, a$usd > 10000 & a$usd <= 60000)) # 10259
nrow(subset(a, a$usd > 60000)) # 2742

onedollar <- subset(a, a$usd <= 1) # 104040
summary(onedollar)
dollar.cats <- as.data.frame(table(onedollar$all.c))

# correlations ----------------------------------------------------------------

ap <- subset(a, select = c("date", "from", "to", "vendor", "product", "price", 
                           "usd", "rate", "gold.oz", "brent.oil", "wti.oil", 
                           "Index", "Kind", "Mids", "Schwag", "cat", "all.c", 
                           "sc", "fb", "feedback"))

nrow(subset(ap, ap$usd > 1000000)) # 808
nrow(subset(ap, ap$usd > 10000000)) # 205
nrow(subset(ap, ap$usd > 100000000)) # 7
# 808 listings over 1,000,000. 
# Investigating shows all are placeholder/false listings.

ap <- subset(ap, ap$usd < 1000000)

cor(ap$usd, ap$price) 
# 0.9676931

# sum of squared difference between value and mean
var(ap$usd)          # 89820611
var(ap$price)        # 1143.782
cov(ap$usd, ap$price) # 310168.3

cor(ap$usd, ap$gold.oz)
# 0.001199429
# less than 1% correlated

cor(ap$price, ap$gold.oz)
# -0.00291547
# inversely correlated at less than 1%

# select price-related variables
ap.cor <- subset(ap, select = c("price", "usd", "rate", "gold.oz", "brent.oil",
                                "wti.oil", "Index", "Kind", "Mids", "Schwag"))

ap.corNA <- subset(ap.cor, is.na(ap.cor$Index) == F)
colnames(ap.corNA) <- c("btc", "usd", "btc-usd rate", "gold.oz", "brent.oil",
                        "wti.oil", "Index", "Kind", "Mids", "Schwag")


# correlation matrices ------------------------------------
# circle by variable order
corrplot(cor(ap.corNA), method = "circle", type = "full", mar = c(4, 4, 4, 4),
         addCoef.col = "black", tl.srt = 45, tl.col = "black", tl.offset = 1.5,
         title = "Agora Marketplace: Correlation of Price Indices", 
         order = "hclust", addgrid.col = NA)

# ellipse by first principal component
corrplot(cor(ap.corNA), method = "ellipse", type = "full", mar = c(4, 4, 4, 4),
         addCoef.col = "black", tl.srt = 45, tl.col = "black", tl.offset = 1.5,
         title = "Agora Marketplace: Correlation of Price Indices",
         order = "FPC", addgrid.col = NA)

# color by hierarchical cluster
corrplot(cor(ap.corNA), method = "color", type = "full", mar = c(4, 4, 4, 4),
         addCoef.col = "black", tl.srt = 45, tl.col = "black", tl.offset = 1.5,
         title = "Agora Marketplace: Correlation of Price Indices",
         order = "hclust")

# shade by first principal component
corrplot(cor(ap.corNA), method = "shade", type = "full", mar = c(4, 4, 4, 4),
         addCoef.col = "black", tl.srt = 45, tl.col = "red3", tl.offset = 1.5,
         title = "Agora Marketplace: Correlation of Price Indices",
         order = "FPC")

corrplot(cor(ap.corNA), method = "shade", type = "full", mar = c(4, 4, 4, 4),
         addCoef.col = "black", tl.srt = 45, tl.col = "red3", tl.offset = 1.5,
         title = "Agora Marketplace: Correlation of Price Indices",
         order = "FPC")


# Linear Model ----------------------------------------------------------------

lm01 <- lm(usd ~ ., data = ap.corNA)
summary(lm01)
# Multiple R-squared:  0.9388,	Adjusted R-squared:  0.9388 
# F-statistic: 3.901e+06 on 9 and 2286863 DF,  p-value: < 0.00000000000000022

# Every variable was significant

lm02 <- lm(usd ~ gold.oz + brent.oil + Index, data = ap.corNA)
summary(lm02)


tidy(lm01)
tidy(lm02)
#          term     estimate   std.error  statistic              p.value
# 1 (Intercept) 252.70934090 256.1385091  0.9866121 0.323832962444510652
# 2     gold.oz  -0.05868127   0.1579572 -0.3715011 0.710264392228710295
# 3   brent.oil   2.95762269   0.4191481  7.0562715 0.000000000001710777
# 4       Index   0.93886575   0.5702570  1.6463905 0.099683529578604838






