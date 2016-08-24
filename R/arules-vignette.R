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



