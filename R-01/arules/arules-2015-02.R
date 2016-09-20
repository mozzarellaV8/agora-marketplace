# Agora Marketplace Analysis
# Association Rules - 2014 Product Data
# Cannabis Analysis - SubSubCategories

# load data -------------------------------------------------------------------

library(arules)
library(arulesViz)
library(data.table)

v15 <- fread("~/GitHub/agora-data/v15-arules-00.csv", stringsAsFactors = T)
str(v15)
summary(v15)

# subset ----------------------------------------------------------------------

quantile(v15$usd)
#          0%                25%                50%                75%               100% 
# 0.000017728       20.184994602       80.164550000      267.745375000 61413750.000000000 

summary(v15$usd)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#       0       20       80     1490      268 61410000


# subset under 20k ---------------------------------------
v15b <- subset(v15, v15$usd <= 20000) # 1131253
quantile(v15b$usd)
# 75% is at $264

plot(v15b$usd, main = "Agora 2015: Prices under 20k USD")
v15b$usd <- discretize(v15b$usd, method = "cluster", categories = 8)
levels(v15b$usd)
# [1] "[    0.0000177,  257.3572043)" "[  257.3572043,  785.2310794)"
# [3] "[  785.2310794, 1654.5506170)" "[ 1654.5506170, 2929.6300153)"
# [5] "[ 2929.6300153, 4810.5473283)" "[ 4810.5473283, 7775.7107393)"
# [7] "[ 7775.7107393,12325.2813671)" "[12325.2813671,20000.0000000]"

v15b <- subset(v15b, select = c("vendor", "product", "usd", "allcat",
                                "from"))

v2 <- v15b[!duplicated(v15b), ] # 231383

colnames(v2) <- c("vendor", "product", "usd", "category", "location")
head(v2)

# convert to transactions -----------------------------------------------------
v2 <- as(v2, "transactions")
v2
# transactions in sparse format with
#  73748 transactions (rows) and
#  63775 items (columns)

summary(v2)
# density of 0.00006274313 

# VCF - Mine Frequent Itemsets ------------------------------------------------

nrow(v2)
500/nrow(v2) # 0.006779845
# 'find an interesting support (have at least 500 observations)'

v2items <- apriori(v2, parameter = list(target = "frequent",
                                        supp = 0.006779845, minlen = 3, maxlen = 4))

summary(v2items)
# set of 1622 itemsets

#     support        
# Min.   :0.006821  
# 1st Qu.:0.007526  
# Median :0.008163  
# Mean   :0.010798  
# 3rd Qu.:0.011095  
# Max.   :0.046957


par(mfrow = c(1, 1), mar = c(20, 6, 4, 2), family = "GillSans")
itemFrequencyPlot(v2, support = 0.1, cex.names = 0.8, 
                  main = "Agora 2015: Frequent Items (support > 0.1)")


# VCF - Mine Association Rules ------------------------------------------------
# Going to start out here with the same measure value from the 
# frequent itemset mining (0.006821), and a confidence of 0.60.
# Confidence close to one is ideal, so 0.6 hopefully pushes 
# towards that with generous flexibility to start.

v2rules <- apriori(v2, parameter = list(support = 0.006821, confidence = 0.6))
v2rules
# set of 106 rules 
summary(v2rules)

arules::inspect(v2rules, by = "lift")

plot(v2rules, method = "matrix", measure = "lift")
plot(v2rules, method = "matrix", measure = c("support", "confidence"))
plot(v2rules, method = "matrix", measure = c("confidence", "lift"))
plot(v2rules, method = "grouped", control = list(k = 100))


subrules <- head(sort(v2rules, by = "lift"), 25)
plot(subrules, method = "graph")
plot(subrules, method = "graph", control = list(type = "itemsets"))

plot(subrules, method = "paracoord")
