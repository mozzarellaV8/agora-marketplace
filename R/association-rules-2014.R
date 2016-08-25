# Agora Marketplace Analysis
# Association Rules - 2014 Product data

# load data -------------------------------------------------------------------

library(arules)
library(arulesViz)
library(data.table)
library(tm)

p14 <- fread("~/GitHub/agora-data/agora-2014.csv", stringsAsFactors = T)
str(p14)

summary(p14) # 772632

p14$list <- as.character(p14$list)
p14$list <- removeNumbers(p14$list)
p14$list <- gsub("--__", "", p14$list)
p14$list <- gsub(".html", "", p14$list)
p14$list <- as.factor(p14$list)

p14$vendor <- gsub("%7E", "", p14$vendor)

p14$feedback <- as.character(p14$feedback)
p14$feedback <- stripWhitespace(p14$feedback)

quantile(p14$price)

# subset out the placeholder (high) prices
p14 <- subset(p14, p14$price < 3500) # 772357
# I chopped that subset down pretty quick. A lot of vendors keep placeholder listings when they're
# out of stock - with outrageously high prices. Not sure who is going to pay for over $1M USD 
# for 0.1 grams of speed paste, but I feel OK to make a judgement call subsetting that out.  

# switched from 8 to 12 clusters - curious how the prices distribute.
# during the time period covered in the data, the lowest BTC-USD exchange rate
# might have just dipped below $200.
# But it also soared above $600 too. 
# There's a WSS plot in the plot directory that might back the decision for 12. 

p14$price <- discretize(p14$price, method = "cluster", categories = 12)
levels(p14$price)
# 8 clusters:
# [1] "[   0.0000001,   4.2529326)" "[   4.2529326,  18.0214855)" "[  18.0214855,  63.4096931)"
# [4] "[  63.4096931, 180.7799059)" "[ 180.7799059, 421.0974602)" "[ 421.0974602, 939.8462904)"
# [7] "[ 939.8462904,2088.7955777)" "[2088.7955777,3199.0000000]"

#  [1] "[   0.0000001,   1.4851368)" "[   1.4851368,   4.8476172)" "[   4.8476172,  10.6721562)" "[  10.6721562,  19.9506919)"
#  [5] "[  19.9506919,  35.5722542)" "[  35.5722542,  63.6548653)" "[  63.6548653, 118.9146899)" "[ 118.9146899, 222.2199907)"
#  [9] "[ 222.2199907, 441.8217437)" "[ 441.8217437, 947.7948146)" "[ 947.7948146,2088.7955777)" "[2088.7955777,3199.0000000]"

# First subset ----------------------------------------------------------------
pRules01 <- subset(p14, select = c("list", "date", "vendor", "product", "price",
                                   "cat", "subcat", "subsubcat", "from"))
pRules01$date <- as.factor(pRules01$date)

pRules01 <- as(pRules01, "transactions")
pRules01
# transactions in sparse format with
# 772356 transactions (rows) and
# 112280 items (columns)

head(itemLabels(pRules01))
# let's prune before we even start.

# Second subset ---------------------------------------------------------------

# seven features chosen for extraction - let's do less next time. 
# maybe unite cat, subcat, and subsubcat - but watch out with the NA's.

# oh, and we're assuming every listing is a transaction - for the time being. 
# will have to examine feedbacks later to see about inferring transaction likelihood
# based on reviews. 

p14$vendor <- as.factor(p14$vendor) # 2178 levels
ps <- subset(p14, select = c("vendor", "product", "price",
                             "cat", "subcat", "subsubcat", "from"))

p1 <- as(ps, "transactions")
p1
# transactions in sparse format with
# 772356 transactions (rows) and
# 59500 items (columns)

summary(p1)
# price=[   0.0000001,   1.4851368)                         cat=Drugs                      subsubcat=NA 
#                            642766                            544220                            411888 
#                         from= USA                    subcat=Cannabis                          (Other) 
#                            157680                            136203                           3513735 

# element (itemset/transaction) length distribution:
# sizes
# 7 
# 772356 

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   7       7       7       7       7       7 

# every transaction is of length 7. hmm. 

dim(p1)
# [1] 772356  59500

itemLabels(p1)
# lotta products. 59500 doesnt fit on console.

# view head a matrix (data frome doesnt work)
# def subset for just categories after.
p1m <- as(p1[1:100, 1:100], "matrix")

par(mar = c(16, 10, 6, 6), mfrow = c(1, 1), family = "FranklinGothicSSK")
itemFrequencyPlot(p1, topN = 50, cex.names = 0.7,
                  mar = c(18, 10, 6, 6))

itemFrequencyPlot(p1, topN = 25, cex.names = 0.7, type = "absolute",
                  mar = c(18, 18, 6, 6))

# look at similarity between items
d <- dissimilarity(sammple(p1, 50000), method = "phi", which = "items")




can <- subset(p1, items %in% "Cannabis")







