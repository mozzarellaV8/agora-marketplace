# Agora Marketplace Analysis
# Association Rules - 2014 Product Data
# Cannabis Analysis - SubCategories

# load data -------------------------------------------------------------------

library(arules)
library(arulesViz)
library(data.table)
library(lattice)

p14 <- fread("~/GitHub/agora-data/ag05-2014.csv", stringsAsFactors = T)
p14$date <- as.Date(p14$date)
# 1018109

# subset to 50000USD and under
# consider bulk subset vs. personal use subset

p14 <- subset(p14, p14$usd < 50000) #1016474
p14 <- as.data.frame(p14)

# quick looks ---------------------------------------------
length(unique(p14$product)) # 62817
summary(p14$usd)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max.
#    0.00    29.27    95.03   506.40   329.10 49940.00

quantile(p14$usd)
#           0%            25%             50%              75%            100% 
#  0.000031244    29.274128537    95.034457881   329.098886794 49941.866473735

# bulk vs personal
pb <- subset(p14, p14$usd >= 1000 & p14$usd <= 50000) # 110669
pp <- subset(p14, p14$usd <= 1200) # 921651

par(mar = c(6, 6, 6, 6), mfrow = c(2, 3), bty = "l", family = "HersheySans")
hist(pb$usd, breaks = 500)
hist(pb$usd, breaks = 200, xlim = c(1000, 5000), 
     main = "hist(pb$usd, breaks = 100, xlim = c(1000, 5000)")
hist(pb$usd, breaks = 500, xlim = c(20000, 40000), ylim = c(0, 200),
     main = "hist(pb$usd, breaks = 100, xlim = c(20000, 40000)")
hist(pp$usd, breaks = 200, xlim = c(0, 1000), ylim = c(0, 100000),
     main = "hist(pp$usd, breaks = 200)")
hist(pp$usd, breaks = 200, xlim = c(0, 500), ylim = c(0, 100000),
     main = "hist(pp$usd, breaks = 200)")
hist(pp$usd, breaks = 200, xlim = c(0, 200), ylim = c(0, 100000),
     main = "hist(pp$usd, breaks = 100)")

p14$usd <- discretize(p14$usd, method = "cluster", categories = 12)
levels(p14$usd)
# "[    0.0000312,  221.7067938)" "[  221.7067938,  659.8052618)" "[  659.8052618, 1349.7514109)"
# "[ 1349.7514109, 2255.7513825)" "[ 2255.7513825, 3382.3257407)" "[ 3382.3257407, 4803.3017416)"
# "[ 4803.3017416, 6614.4856014)" "[ 6614.4856014, 9183.9096242)" "[ 9183.9096242,13299.6022969)"
# "[13299.6022969,20434.6535707)" "[20434.6535707,32479.2854749)" "[32479.2854749,49941.8664737]""


# Vendor-Category-Feedback subset ---------------------------------------------

f5 <- subset(p14, p14$great == T) # 445127
length(unique(f5$product)) # 29922

pm <- subset(f5, select = c("product"))
pm <- as(pm, "transactions")
pm
# transactions in sparse format with
# 445127 transactions (rows) and
# 62873 items (columns)

summary(pm)
# density of 0.00001590508

# Mine Frequent Itemsets ------------------------------------------------------

1000/nrow(pm) #   0.00224655
# 'find an interesting support (have at least 500 observations)'

pmi <- apriori(pm, parameter = list(target = "frequent", support = 0.000000024655,
                                    minlen = 2, maxlen = 4))

summary(pmi)
# set of 61 itemsets

par(mfrow = c(1, 1))
par(mar = c(20, 6, 4, 2), family = "FranklinGothicSSK")
itemFrequencyPlot(pm, support = 0.005, cex.names = 0.75, 
                  main = "support = 0.005")
itemFrequencyPlot(pm, support = 0.0095, cex.names = 0.8)

par(mar = c(20, 6, 4, 2), family = "FranklinGothicSSK")
itemFrequencyPlot(pm, support = 0.0095, cex.names = 0.8,
                  main = "Agora 2014: Frequent Items (support = 0.0095)")


# Mine Association Rules ------------------------------------------------------
pmrules <- apriori(pm, parameter = list(support = 0.00000975, confidence = 0.2))
pmrules
# set of 249 rules
summary(pmrules)

# try out various subsets
v1 <- subset(pmrules, subset = rhs %in% "from=usa" & lift < 1)
inspect(v1)
v2 <- subset(pmrules, subset = lift > 80000)
inspect(v2)
v2

# visualize ----------------------------------------
plot(v1, method = "graph")
plot(pmrules, shading = "order")
plot(pmrules, measure = c("support", "lift"), shading = "confidence")
plot(v2, method = "graph")




