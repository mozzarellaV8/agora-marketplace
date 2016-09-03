# Agora Marketplace Analysis
# arules vignette

# load data -------------------------------------------------------------------

install.packages("arules")

library(arules)
data("Epub")
Epub
# transactions in sparse format with
# 15729 transactions (rows) and
# 936 items (columns)

summary(Epub)
year <- strftime(as.POSIXlt(transactionInfo(Epub)[["Timesstamp"]]), "%Y")
rm(Epub)

# try on Agora data -----------------------------------------------------------

library(arules)
library(arulesViz)

p0114 <- read.csv("data/products-2014-01.csv")
p14$date <- as.Date(p14$date)
p14$date <- as.factor(p14$date)

rc <- subset(p0114, p0114$subcat == "RCs")
psyche <- subset(p0114, p0114$subcat == "Psychedelics")
levels(p0114$subcat)

p14$price <- discretize(p14$price, method = "interval", categories = 4)
p14 <- as(p14, "transactions")
p14
# transactions in sparse format with
# 7986 transactions (rows) and
# 13180 items (columns)

summary(p14)
par(mar = c(6, 6, 6, 6))
itemFrequencyPlot(p14, support = 0.1, cex.names = 0.6,
                  main = "Agora - ItemFreqPlot - support 10%")
par(mar = c(6, 6, 6, 6))
itemFrequencyPlot(p14, support = 0.2, cex.names = 0.6,
                  main = "Agora - ItemFreqPlot - support 20%")

rules <- apriori(p14, parameter = list(support = 0.01, confidence = 0.6))
# Apriori

# Parameter specification:
#   confidence minval smax arem  aval originalSupport support minlen maxlen target   ext
#          0.6    0.1    1 none FALSE            TRUE    0.01      1     10  rules FALSE

# Algorithmic control:
#   filter tree heap memopt load sort verbose
# 0.1 TRUE TRUE  FALSE TRUE    2    TRUE

# Absolute minimum support count: 79 

# set item appearances ...[0 item(s)] done [0.00s].
# set transactions ...[13178 item(s), 7986 transaction(s)] done [0.01s].
# sorting and recoding items ... [89 item(s)] done [0.00s].
# creating transaction tree ... done [0.00s].
# checking subsets of size 1 2 3 4 5 6 7 8 done [0.00s].
# writing ... [8080 rule(s)] done [0.00s].
# creating S4 object  ... done [0.01s].

summary(rules)
# set of 8080 rules

# rule length distribution (lhs + rhs):sizes
#    1    2    3    4    5    6    7    8 
#    3  343 1532 2608 2233 1061  272   28 

# summary of quality measures:
#          support          confidence          lift        
# Min.   :0.01002   Min.   :0.6000   Min.   : 0.7351  
# 1st Qu.:0.01315   1st Qu.:0.9250   1st Qu.: 1.0001  
# Median :0.01741   Median :1.0000   Median : 1.3903  
# Mean   :0.02510   Mean   :0.9444   Mean   : 7.6309  
# 3rd Qu.:0.02329   3rd Qu.:1.0000   3rd Qu.: 9.9452  
# Max.   :0.99987   Max.   :1.0000   Max.   :74.6355 

# mining info:
# data ntransactions support confidence
# p14          7986    0.01        0.6

rulesConf <- subset(rules, confidence == 1.000)
inspect(head(rulesConf, n = 7, by = "lift"))

# all 2014 data ---------------------------------------------------------------

library(arules)
library(arulesViz)
library(tm)
library(data.table)
p14 <- fread("~/GitHub/agora-data/agora-2014.csv", stringsAsFactors = T)
str(p14)
summary(p14) # 772632

p14$date <- as.Date(p14$date)

p14$list <- as.character(p14$list)
p14$list <- removeNumbers(p14$list)
p14$list <- gsub("--__", "", p14$list)
p14$list <- gsub(".html", "", p14$list)
p14$list <- as.factor(p14$list)

p14$subcat[is.na(p14$subcat)] <- p14$cat

quantile(p14$price)

# subset out the placeholder (high) prices
p14 <- subset(p14, p14$price < 7000) # 772400
p14 <- subset(p14, p14$price < 4500) # 772358
p14 <- subset(p14, p14$price < 4400) # 772357
p14 <- subset(p14, p14$price < 3500) # 772357

p14$price <- discretize(p14$price, method = "cluster", categories = 8)
levels(p14$price)
# [1] "[   0.0000001,   4.2529326)" "[   4.2529326,  18.0214855)" "[  18.0214855,  63.4096931)"
# [4] "[  63.4096931, 180.7799059)" "[ 180.7799059, 421.0974602)" "[ 421.0974602, 939.8462904)"
# [7] "[ 939.8462904,2088.7955777)" "[2088.7955777,3199.0000000]"

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

p2 <- subset(p14, select = c("vendor", "product", "price",
                             "cat", "subcat", "subsubcat", "from"))

p2 <- as(p2, "transactions")
p2
# transactions in sparse format with
# 772356 transactions (rows) and
# 59496 items (columns)

summary(p2)
# most frequent items:
# price=[   0.0000001,   4.2529326)                         cat=Drugs                      subsubcat=NA 
# 716965                            544220                            411888 
# from= USA                    subcat=Cannabis                           (Other) 
# 157680                            136203                           3439536




