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
#  8 clusters:
# [1] "[   0.0000001,   4.2529326)" "[   4.2529326,  18.0214855)" "[  18.0214855,  63.4096931)"
# [4] "[  63.4096931, 180.7799059)" "[ 180.7799059, 421.0974602)" "[ 421.0974602, 939.8462904)"
# [7] "[ 939.8462904,2088.7955777)" "[2088.7955777,3199.0000000]"

#  12 clusters:
#  [1] "[   0.0000001,   1.4851368)" "[   1.4851368,   4.8476172)" "[   4.8476172,  10.6721562)" "[  10.6721562,  19.9506919)"
#  [5] "[  19.9506919,  35.5722542)" "[  35.5722542,  63.6548653)" "[  63.6548653, 118.9146899)" "[ 118.9146899, 222.2199907)"
#  [9] "[ 222.2199907, 441.8217437)" "[ 441.8217437, 947.7948146)" "[ 947.7948146,2088.7955777)" "[2088.7955777,3199.0000000]"

# Second subset ---------------------------------------------------------------

# seven features chosen for extraction - let's do less next time. 
# maybe unite cat, subcat, and subsubcat - but watch out with the NA's.

# oh, and we're assuming every listing is a transaction - for the time being. 
# will have to examine feedbacks later to see about inferring transaction likelihood
# based on reviews. 

p14$feedback <- as.factor(p14$feedback)
p14$vendor <- as.factor(p14$vendor) # 2178 levels

ps <- subset(p14, select = c("product", "price", "cat", "subcat", "subsubcat", "from"))

p1 <- as(ps, "transactions")
p1
# transactions in sparse format with
# 772356 transactions (rows) and
# 59500 items (columns)

summary(p1)
#   transactions as itemMatrix in sparse format with
#   772356 rows (elements/itemsets/transactions) and
#   57322 columns (items) and a density of 0.0001046719 

#     most frequent items:
#   price=[1.00e-07,1.49e+00)                 cat=Drugs              subsubcat=NA 
#                     642766                    544220                    411888 
#                  from= USA            subcat=Cannabis                   (Other) 
#                      157680                    136203                   2741379 
  

dim(p1)
# [1] 772356  57322

itemLabels(p1)
# lotta products. 59500 doesnt fit on console.

# view head a matrix (data frome doesnt work)
# def subset for just categories after.
p1m <- as(p1[100:200, 100:200], "matrix")

par(mar = c(16, 10, 6, 6), mfrow = c(1, 1), family = "FranklinGothicSSK")
itemFrequencyPlot(p1, topN = 50, cex.names = 0.7,
                  mar = c(18, 10, 6, 6))

itemFrequencyPlot(p1, topN = 25, cex.names = 0.7, type = "absolute",
                  mar = c(18, 18, 6, 6))


# Mine Frequent Itemsets ------------------------------------------------------
nrow(p1)
500/nrow(p1) # 0.0006473699
# 'find an interesting support (have at least 500 observations)'

itemsets <- apriori(p1, parameter = list(target = "frequent",
                                        supp = 0.0001, minlen = 2, maxlen = 4))

# Apriori

# Parameter specification:
#   confidence minval smax arem  aval originalSupport support minlen maxlen            target   ext
# NA    0.1    1 none FALSE            TRUE   1e-04      2      4 frequent itemsets FALSE
# 
# Algorithmic control:
#   filter tree heap memopt load sort verbose
# 0.1 TRUE TRUE  FALSE TRUE    2    TRUE

# Absolute minimum support count: 77 
# 
# set item appearances ...[0 item(s)] done [0.00s].
# set transactions ...[57315 item(s), 772356 transaction(s)] done [0.47s].
# # sorting and recoding items ... [548 item(s)] done [0.03s].
# creating transaction tree ... done [0.53s].
# checking subsets of size 1 2 3 4 done [0.01s].
# writing ... [13177 set(s)] done [0.00s].
# creating S4 object  ... done [0.16s].


inspect(head(sort(itemsets), n = 10))
head(itemsets)
arules::inspect(head(itemsets))



# try a new subset ------------------------------------------------------------

p14$feedback <- as.character(p14$feedback)
fb <- subset(p14, p14$feedback != " Feedbacks: No feedbacks found. ")
# 349545
fb$greatFB <- grepl("^\\sFeedbacks: 5/5(.*)", fb$feedback)
fb$goodFB <- grepl("^\\sFeedbacks: 4/5(.*)", fb$feedback)
fb$okFB <- grepl("^\\sFeedbacks: 3/5(.*)", fb$feedback)
fb$badFB <- grepl("^\\sFeedbacks: 2/5(.*)", fb$feedback)
fb$poorFB <- grepl("^\\sFeedbacks: 1/5(.*)", fb$feedback)
fb$worstFB <- grepl("^\\sFeedbacks: 0/5(.*)", fb$feedback)

length(fb$greatFB[fb$greatFB == TRUE]) # 332500
length(fb$greatFB[fb$greatFB == FALSE]) # 17045

vs <- subset(p14, select = c("vendor", "cat", "subcat", "subsubcat"))

v1 <- as(vs, "transactions")
v1
# transactions in sparse format with
# 772356 transactions (rows) and
# 2280 items (columns)
summary(v1)

write.csv(fb, file = "~/GitHub/agora-data/feedback-2014.csv", row.names = F)


