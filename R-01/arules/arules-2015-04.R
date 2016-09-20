# Agora Marketplace Analysis
# Association Rules - 2014 Product Data
# Cannabis Analysis - SubSubCategories

# load data -------------------------------------------------------------------

library(arules)
library(arulesViz)
library(data.table)
library(igraph)

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

# subset under 40k ---------------------------------------
v15b <- subset(v15, v15$usd <= 40000) # 1132307
quantile(v15b$usd)
# 75% is at $265

plot(v15b$usd, main = "Agora 2015: Prices under 40k USD")
v15b$usd <- discretize(v15b$usd, method = "cluster", categories = 10)
levels(v15b$usd)
# [1] "[    0.0000177,  359.4984838)" "[  359.4984838, 1139.7731641)" "[ 1139.7731641, 2372.9194491)"
# [4] "[ 2372.9194491, 4422.8807377)" "[ 4422.8807377, 7761.6439127)" "[ 7761.6439127,12970.8810281)"
# [7] "[12970.8810281,22015.2079275)" "[22015.2079275,39965.7600000

v15b <- subset(v15b, select = c("vendor", "product", "allcat", "from"))
colnames(v15b) <- c("vendor", "product", "category", "location")
head(v15b)

v2 <- v15b[!duplicated(v15b), ] # 70703
head(v2)
v2b <- v15b

# convert to transactions -----------------------------------------------------

v2 <- as(v2, "transactions")
v2
# transactions in sparse format with
#  70703 transactions (rows) and
#  63752 items (columns)

summary(v2)

v2b <- as(v2b, "transactions")
summary(v2b)

# VCF - Mine Frequent Itemsets ------------------------------------------------

nrow(v2)
500/nrow(v2) # 0.006779845
# 'find an interesting support (have at least 500 observations)'

v2items <- apriori(v2, parameter = list(target = "frequent",
                                        supp = 0.0007071836, minlen = 3, maxlen = 5))

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
itemFrequencyPlot(v2, support = 0.5, cex.names = 0.8, 
                  main = "Agora 2015: Frequent Items (support > 0.5)")


# VCF - Mine Association Rules ------------------------------------------------

v2rules <- apriori(v2, parameter = list(support = 0.001, confidence = 0.6,
                                        minlen = 3, maxlen = 5))
v2rules
# set of 167 rules 
summary(v2rules)

arules::inspect(v2rules)



plot(v2rules, method = "matrix", measure = "lift")
plot(v2rules, method = "matrix", measure = c("support", "confidence"))
plot(v2rules, method = "matrix", measure = c("confidence", "lift"))
plot(v2rules, method = "grouped", control = list(k = 125))

subrules <- head(sort(v2rules, by = "lift"), 25)
plot(subrules, method = "graph", main = "25 rules ~ lift")
# plot(subrules, method = "graph", control = list(type = "itemsets"))

subrules2 <- head(sort(v2rules, by = "confidence"), 25)
plot(subrules2, method = "graph", measure = "confidence", 
     edge.color = "gray66",
     main = "25 rules ~ confidence")

subrules3 <- head(sort(v2rules, by = c("confidence", "lift")), 64)
plot(subrules3, method = "graph", edge.color = "gray88", 
     main = "64 rules ~ confidence + lift", 
     vertex.color = "#00688B80", vertex.label.color = "black", 
     vertex.label.cex = 0.74, layout = layout_with_gem,
     vertex.label.dist = 0)

# interesting one:
subrules4 <- head(sort(v2rules, by = c("support", "confidence", "lift")), 64)
ig <- plot(subrules4, method = "graph", edge.color = "gray88", 
           main = "64 rules ~ support + confidence + lift", 
           vertex.color = "#00688B80", vertex.label.color = "grey2", 
           vertex.label.cex = 0.72, layout = layout_with_dh,
           vertex.label.dist = 0)

subrules5 <- head(sort(v2rules, by = "support"), 25)
plot(subrules5, method = "graph", main = "25 rules ~ support")

subrules6 <- head(sort(v2rules, by = c("support", "confidence")), 64)
plot(subrules6, method = "graph", main = "64 rules ~ support + confidence",
     vertex.label.color = "grey12", vertex.color = "#00688B80",
     vertex.label.cex = 0.76, layout = layout_nicely)


# extraction methods ----------------------------------------------------------

v2g <- get.data.frame(ig, "both")
plot(ig, layout=layout_with_fr, vertex.size = 18, vertex.label.cex = 0.55,
     vertex.color = "#00688B80")

layouts <- grep("^layout_", ls("package:igraph"), value = T)[-1]


# rename/relabel --------------------------------------------------------------

### this is how you replace item labels and set a hierachy (here level1)
myLabels <- c("milk", "butter", "beer")
myLevel1 <- c("dairy", "dairy", "beverage")
itemInfo(trans) <- data.frame(labels = myLabels, level1 = myLevel1)


vLabels <- as.character(levels(v15b$vendor))
cLabels <- as.character(levels(v15b$category))
fLabels <- as.character(levels(v15b$from))
pLabels <- as.character(levels(v15b$product))
inspect(v2items)




