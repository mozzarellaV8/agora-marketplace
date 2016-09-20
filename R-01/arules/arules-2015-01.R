# Agora Marketplace Analysis
# Association Rules - 2014 Product Data
# Cannabis Analysis - SubSubCategories

# load data -------------------------------------------------------------------

library(arules)
library(arulesViz)
library(data.table)
library(tm)

# v15 <- fread("~/GitHub/agora-data/agora-2014.csv", stringsAsFactors = T)
str(v15)
summary(v15)

# subset ----------------------------------------------------------------------

quantile(v15$usd)
#          0%                25%                50%                75%               100% 
# 0.000017728       20.184994602       80.164550000      267.745375000 61413750.000000000 

summary(v15$usd)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#       0       20       80     1490      268 61410000

plot(v15$usd)
# 3 listings above 20,000,000. Cut it down to 10 million and plot

v15 <- subset(v15, v15$usd < 10000000)  # 1133664
v15 <- subset(v15, v15$usd <= 1000000)  # 1133312
# total difference of 352 listings.

v15 <- subset(v15, v15$usd <= 429972.2) # 1133196
# 116 less. The price is chosen because of the listing from vendor HonestCocaine
# He offers 10 kg cocaine for that price, which is somewhat reasonable 
# @ ~43k/kilo. Although there should be a bulk deal.

v15b <- subset(v15, v15$usd <= 100000)  # 1132795
# 401 less.

v15b <- subset(v15b, v15b$usd <= 50000) # 1132476
quantile(v15b$usd)
# 75% is at $265.96

plot(v15b$usd)
# OK. Stick with under 50k as one subset and do another at under 2k.

v15c <- subset(v15, v15$usd <= 2000)  # 1080082
v15c$usd <- discretize(v15c$usd, method = "cluster", categories = 10)
levels(v15c$usd)
#  10 clusters:
# 0-46, 46-117, 117-212, 212-335, 335-490, 490-690, 690-939, 
# 939-1239, 1239-1598, 1598-2000.

v15d <- subset(v15b, v15b$usd <= 2000)
v15d$usd <- discretize(v15d$usd, method = "interval", categories = 4)
levels(v15d$usd)
# "[   0.0000177, 500.0000133)" "[ 500.0000133,1000.0000089)" 
# "[1000.0000089,1500.0000044)" "[1500.0000044,2000.0000000]"

v15c <- v15d
rm(v15d)

# vendor-category subset -----------------------------------------------------

vendors <- as.data.frame(table(v15c$vendor))
colnames(vendors) <- c("vendor", "count")
vendors <- vendors[order(vendors$count, decreasing = T), ]
rownames(vendors) <- NULL
write.csv(vendors, file = "data/counts/vendor-counts.csv", row.names = F)

v2 <- subset(v15c, select = c("vendor", "allcat", "product", "from"))
summary(v2)
colnames(v2) <- c("vendor", "category", "product", "location")
head(v2)


# convert to transactions -----------------------------------------------------
v2 <- as(v2, "transactions")
v2
# transactions in sparse format with
# 1080082 transactions (rows) and
# 63752 items (columns) transactions (rows)

# maybe subset unique product listings? or aggregate them somehow.

summary(v2)
# density of 0.00006274313 

# VCF - Mine Frequent Itemsets ------------------------------------------------

nrow(v2)
1000/nrow(v2) # 0.0009258556
# 'find an interesting support (have at least 500 observations)'

v2items <- apriori(v2, parameter = list(target = "frequent", 
                                        supp = 0.000925, minlen = 2, maxlen = 4))

summary(v2items)
# set of 723 itemsets

# most frequent items:
# location=USA  location=No Info    location=UK   location=China  category=Drugs, Benzos, NA    (Other) 
# 116           89                  58            47              42                            1244 

# element (itemset/transaction) length distribution:sizes
#   2   3 
# 573 150 

# summary of quality measures:
#     support         
#  Min.   :0.0009259  
#  1st Qu.:0.0011893  
#  Median :0.0016304  
#  Mean   :0.0030600  
#  3rd Qu.:0.0030863  
#  Max.   :0.0433986 

# mining info:
# data ntransactions support confidence
#   v2        349545  0.0014          1

par(mar = c(20, 6, 2, 2), family = "GillSans")
itemFrequencyPlot(v2, support = 0.005, cex.names = 0.75)
itemFrequencyPlot(v2, support = 0.0095, cex.names = 0.8)

par(mar = c(20, 6, 4, 2), family = "GillSans")
itemFrequencyPlot(v2, support = 0.05, cex.names = 0.8, 
                  main = "Agora 2015: Frequent Items (support > 0.05)")

par(mar = c(20, 6, 4, 2), family = "GillSans")
itemFrequencyPlot(v2, support = 0.01, cex.names = 0.8, col = "white",
                  main = "Agora 2015: Frequent Items (support > 0.01)")


# VCF - Mine Association Rules ------------------------------------------------
# Going to start out here with the same measure value from the 
# frequent itemset mining (0.0095), and a confidence of 0.60.
# Confidence close to one is ideal, so 0.6 hopefully pushes 
# towards that with generous flexibility to start.

v2rules <- apriori(v2, parameter = list(support = 0.0095, confidence = 0.6))
v2rules
# set of 33 rules 
summary(v2rules)

arules::inspect(v2rules, by = "lift")

plot(v2rules, method = "matrix", measure = "lift")
plot(v2rules, method = "matrix", measure = "confidence")
plot(v2rules, method = "matrix", measure = c("confidence", "lift"))

par(mar = c(8, 8, 8, 8), family = "GillSans")
plot(v2rules, method = "grouped", control = list(k = 50))

# New Parameters -------------------------------------------------------------

v3rules <- apriori(v2, parameter = list(support = 0.00095, confidence = 0.6))
v3rules
summary(v3rules)

arules::inspect(head(sort(v3rules, by = ""), 30))
v32 <- subset(v3rules, subset = rhs %in% "category=Drugs, Cannabis, Weed")
arules::inspect(head(v32, 100))
arules::inspect(v32)

plot(v3rules, method = "matrix", measure = "confidence")



