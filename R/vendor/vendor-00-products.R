# Agora Marketplace
# product lexicon
# vendor pages extraction

# load data -------------------------------------------------------------------

library(data.table)
library(qdap)
library(arules)
library(arulesViz)

v14 <- fread("~/GitHub/agora-data/v14-00d.csv", stringsAsFactors = T) # 304148


# association rules -----------------------------------------------------------

summary(v14$price)
summary(v14$usd)

# subset under 50k usd  -------------------------------------------------------
v14 <- subset(v14, v14$usd <= 425276.50) # 304066
summary(v14$usd)
#   Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#    0.0     26.2     89.2    638.1    315.2 425300.0
hist(v14$usd, breaks = 100)

# subset uder 100k
v14b <- subset(v14, v14$usd <= 100000) # 303945
summary(v14b$usd)
#   Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#   0.00    26.18    89.13   558.50   314.30 99890.00
hist(v14b$usd, breaks = 100)

# subset under 20k
v14c <- subset(v14, v14$usd <= 20000) # 303322
summary(v14c$usd)
#   Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#   0.00    26.07    88.88   462.30   309.80 19920.00
hist(v14c$usd, breaks = 10)

# subset under 5k
v14d <- subset(v14, v14$usd <= 5000) # 298560
hist(v14d$usd, breaks = 100)


