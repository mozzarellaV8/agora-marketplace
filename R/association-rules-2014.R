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


# feedback table ------------------------------------------------------------

fb.table <- fread("~/GitHub/agora-data/vfb-table-2014.csv")

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
332500/349545 #  0.9512366

length(fb$goodFB[fb$goodFB == TRUE]) # 4703
length(fb$goodFB[fb$goodFB == FALSE]) # 344842
4703/349545 #  0.01345463

length(fb$okFB[fb$okFB == TRUE]) # 2834
length(fb$okFB[fb$okFB == FALSE]) # 346711
2843/349545 # 0.008133431

length(fb$badFB[fb$badFB == TRUE]) # 1169
length(fb$badFB[fb$badFB == FALSE]) # 348376
1169/349545 # 0.003344348

length(fb$poorFB[fb$poorFB == TRUE]) # 1422
length(fb$poorFB[fb$poorFB == FALSE]) # 348123
1422/349545 # 0.004068146

length(fb$worstFB[fb$worstFB == TRUE]) # 6911
length(fb$worstFB[fb$worstFB == FALSE]) # 342634
6911/349545 # 0.01977142

fb.table <- data.frame(vendor = fb$vendor, great = fb$greatFB, good = fb$goodFB, 
                       ok = fb$okFB, bad = fb$badFB, poor = fb$poorFB, worst = fb$worstFB)

write.csv(fb.table, file = "~/GitHub/agora-data/vfb-table-2014.csv",
          row.names = F)

fb.table$vendor <- gsub("%7E", "", as.character(fb.table$vendor))
fb.table$vendor <- as.factor(fb.table$vendor)

# Vendor-Category-Feedback subset ---------------------------------------------

fb <- fread("~/GitHub/agora-data/feedback-2014.csv", stringsAsFactors = T)
v2 <- subset(fb, select = c("vendor", "cat", "subcat", "subsubcat", "greatFB", "worstFB"))

v2 <- as(v2, "transactions")
v2
# transactions in sparse format with
# 349545 transactions (rows) and
# 1968 items (columns)

summary(v2)
# density of 0.002525919

# most frequent items:
# greatFB       cat=Drugs    subsubcat=NA subcat=Cannabis  subsubcat=Weed         (Other) 
# 332500          259558          164720           75952           49536          855325 

# element (itemset/transaction) length distribution:
# sizes
# 4      5 
# 10134 339411

# LHS all have length of 4 or 5. this may need to change.

# VCF - Mine Frequent Itemsets ------------------------------------------------

nrow(v2)
500/nrow(v2) # 0.001430431
# 'find an interesting support (have at least 500 observations)'

v2items <- apriori(v2, parameter = list(target = "frequent", 
                                        supp = 0.0014, minlen = 2, maxlen = 4))

summary(v2items)
# set of 1889 itemsets

# greatFB  983
# Drugs    686
# sscat    516
# Cannabis 217
# Weed     124
# other    2839

# element (itemset/transaction) length distribution:sizes
# 2   3   4 
# 699 793 397

# mean: 2.84

# summary of quality measures:
# support        
# Min.   :0.001402  
# 1st Qu.:0.001759  
# Median :0.002526  
# Mean   :0.008199  
# 3rd Qu.:0.005321  
# Max.   :0.707331 

# mining info:
# data ntransactions support confidence
#   v2        349545  0.0014          1

par(mar = c(20, 6, 4, 2), family = "FranklinGothicSSK")
itemFrequencyPlot(v2, support = 0.005, cex.names = 0.75)
itemFrequencyPlot(v2, support = 0.0095, cex.names = 0.8)

par(mar = c(20, 6, 4, 2), family = "FranklinGothicSSK")
itemFrequencyPlot(v2, support = 0.0095, cex.names = 0.8,
                  main = "Agora 2014: Frequent Items (support = 0.0095)")

# Need to cleanse or format the NA subsubcategory. 
# Perhaps can break up the `Drug` Category after establishing
# some ground truths on the population. 

# VCF - Mine Association Rules ------------------------------------------------

# Going to start out here with the same measure value from the 
# frequent itemset mining (0.0014), and a confidence of 0.60.
# Confidence close to one is ideal, so 0.6 hopefully pushes 
# towards that with generous flexibility to start.

v2rules <- apriori(v2, parameter = list(support = 0.0014, confidence = 0.6))
v2rules
# set of 3492 rules 
summary(v2rules)

cannabis <- subset(v2rules, subset = rhs %in% "cat=Drugs" & lift > 1.2) # 709 rules
synthetics <- subset(v2rules, subset = rhs %in% "subcat=Cannabis" & lift > 1.2)
# 230 rules

summary(cannabis)
summary(synthetics)

arules::inspect(head(cannabis))
arules::inspect(head(synthetics))
arules::inspect(tail(synthetics))
arules::inspect(tail(cannabis))

