# Agora Marketplace Analysis
# Product and Vendor distributions
# Time Series

rm(list = ls())

# load data -------------------------------------------------------------------

library(tidyr)

# products-vendors-dates
pvd <- read.csv("data/crawl-distribution.csv")
str(pvd)

pvd$date <- as.Date(pvd$date)
pvd$vendor <- as.integer(pvd$vendor)
summary(pvd)


# convert to time series object
pvd.ts <- ts(pvd)
head(pvd.ts)
#           date    p vendor
#     [1,] 16071 1539     14
#     [2,] 16079 1859     21
#     [3,] 16086 2533     27
#     [4,] 16096 3051     29
#     [5,] 16103 3526     32
#     [6,] 16106 3618     34
tail(pvd.ts)
#           date     p vendor
#   [198,] 16612 24950      3
#   [199,] 16614 17175    131
#   [200,] 16616 19714    139
#   [201,] 16617   538    180
#   [202,] 16620 11124     99
#   [203,] 16623 22775    168
summary(pvd.ts)

pvd.ts.acf <- acf(pvd.ts, plot = F)
pvd.ts.acf

# time series autoregressive model
pvd.ts.ar <- ar(pvd.ts)
pvd.ts.ar

# predict 12 time units ahead
pvd.prediction <- predict(pvd.ts.ar, n.ahead = 12)
pvd.prediction
# $pred
# Time Series:
#   Start = 204 
#   End = 215 
#   Frequency = 1 
#         date        p   vendor
# 204 16615.07 17573.70 131.3312
# 205 16611.24 19131.04 119.7720
# 206 16606.46 18309.98 119.9341
# 207 16602.29 18103.81 113.5713
# 208 16598.18 17921.86 113.7136
# 209 16594.16 17690.80 112.0132
# 210 16590.25 17557.22 111.5792
# 211 16586.40 17406.86 111.0431
# 212 16582.63 17286.36 110.6380
# 213 16578.94 17170.06 110.2961
# 214 16575.32 17061.88 109.9602
# 215 16571.77 16958.54 109.6514

pvd.prediction02 <- predict(pvd.ts.ar, n.ahead = 365)
as.data.frame(pvd.prediction02)
pvd.prediction02 <- as.data.frame(pvd.prediction02)
pvd.prediction02[c(4, 5, 6)] <- NULL

plot(pvd.prediction02$pred.date, pvd.prediction02$pred.p)
