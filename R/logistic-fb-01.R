# logistic / linear

library(data.table)
library(broom)
library(qdap)
library(dplyr)

agora <- fread("~/GitHub/agora-data/agora-01b.csv", stringsAsFactors = T)
agora$date <- as.Date(agora$date)
# 2322961

# start with a sample
summary(agora$fb)
ag <- subset(agora, agora$fb == "-1" | agora$fb == "5")
nrow(ag)
# 2274834
# 48127 observations less.

# August 2014 sample to split -------------------------------------------------
ag08 <- subset(ag, ag$month == "08")
nrow(ag08)
# 39505

# find number of words per listing
# "if number of words exceeds x, then feedback will be -1"

# subset cannabis -------------------------------------------------------------
w8 <- subset(ag08, ag08$subcat == "Cannabis")
# 6623 obs

library(caTools)

set.seed(144)
split <- sample.split(ag08$fb, SplitRatio = 0.75)

w8train <- subset(ag08, split == T)
w8test <- subset(ag08, split == F)

# this makes for a poor fit - fb ~ usd.
# usd would need to be discretized probably.
wmusd <- glm(fb ~ usd, data = w8train, family = binomial)
summary(wmusd)

# model 01 --------------------------------------------------------------------
wm8 <- glm(fb ~ usd + cat + subcat + subsubcat, family = binomial,
           data = w8train)
summary(wm8)

predict.test <- predict(wm8, type = "response", newdata = w8test)
table(w8test$fb, predict.test > 0.5)
           
#              FALSE TRUE
#   -1          4210 1578
#   5           2216 1872

acc <- (4210+1872)/nrow(w8test)
# 0.6158364
baseline <- (4210+1578)/nrow(w8test)
# 0.5860672

# So this model barely did better than baseline, and both were
# not very strong. Let's tighten things up.

# model 02 --------------------------------------------------------------------

library(arules)
summary(ag08$usd)

# tighten up data
# remove high price 'placeholders'
ag08 <- subset(ag08, ag08$usd < 90000)
ag08$usd <- round(ag08$usd, 2)
quantile(ag08$usd)
#              0%             25%             50%             75%            100% 
#     0.000046067    25.297865304    81.552993642   299.232028032 84633.940109219

ag08$dollars <- discretize(ag08$usd, method = "cluster", categories = 10)
summary(ag08$dollars)
levels(ag08$from)
levels(ag08$cat)
levels(ag08$subcat)

# drop unused levels
ag08$fb <- factor(ag08$fb)
levels(ag08$fb)

# train/test split
set.seed(64)
split <- sample.split(ag08$fb, SplitRatio = 0.75)
w8train <- subset(ag08, split == T)   # 29605
w8test <- subset(ag08, split == F)    # 9868

mod2 <- glm(fb ~ dollars + from + cat, data = w8train, family = "binomial")
summary(mod2)

# naive baseline:
table(w8train$fb)
#    -1     5 
# 17346 12259
17346/nrow(w8train) # 0.5859145
12259/nrow(w8train) # 0.4140855

w8train <- as.data.frame(w8train)
w8test <- as.data.frame(w8test)

predict.test <- predict(mod2, type = "response", newdata = w8test)
table(w8test$fb, predict.test > 0.5)
#      FALSE TRUE
#  -1  3927 1855
#  5   1565 2521

acc <- (3927+2521)/nrow(w8test)   # 0.6534252
base <- (3927+1855)/nrow(w8test)  # 0.5859343
sense <- 1855/(1855+1565)         # 0.5423977
spec <- 3927/(3927+1855)          # 0.6791768
error <- (1565+1855)/nrow(w8test) # 0.3465748
fn <- 1565/(2521+1565)            # 0.3830152
fp <- 1855/(3927+1855)            # 0.3208232


plot(log(ag$usd))


