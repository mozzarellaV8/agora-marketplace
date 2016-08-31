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

pm <- subset(p14, select = c("product", "from", "cat" , "subcat", "subsubcat", "month"))
pm <- as(pm, "transactions")
pm
# transactions in sparse format with
# 1016474 transactions (rows) and
# 63095 items (columns)

summary(pm)
# density of 0.0000950947

# VCF - Mine Frequent Itemsets ------------------------------------------------

1016474*63095*0.0000950947
# 6098844

nrow(pm)
1000/nrow(pm) #   0.000983793
# 'find an interesting support (have at least 500 observations)'

pmi <- apriori(pm, parameter = list(target = "frequent", supp = 0.00098,
                                    minlen = 2, maxlen = 4))

summary(pmi)
# set of 3859 itemsets

par(mfrow = c(1, 1))
par(mar = c(20, 6, 4, 2), family = "FranklinGothicSSK")
itemFrequencyPlot(pm, support = 0.005, cex.names = 0.75, 
                  main = "support = 0.005")
itemFrequencyPlot(pm, support = 0.0095, cex.names = 0.8)

par(mar = c(20, 6, 4, 2), family = "FranklinGothicSSK")
itemFrequencyPlot(pm, support = 0.0095, cex.names = 0.8,
                  main = "Agora 2014: Frequent Items (support = 0.0095)")

# Need to cleanse or format the NA subsubcategory. 
# Perhaps can break up the `Drug` Category after establishing
# some ground truths on the population. 

# VCF - Mine Association Rules ------------------------------------------------
pmrules <- apriori(pm, parameter = list(support = 0.000975, confidence = 0.6))
pmrules
# set of 3950 rules
summary(pmrules)

# try out various subsets
cannabis <- subset(pmrules, subset = rhs %in% "subcat=Cannabis" & lift > 1)
# 218 rules
counterfeits <- subset(pmrules, subset = rhs %in% "cat=Counterfeits" & lift > 1)

summary(cannabis)     # mean lift 5.702
summary(counterfeits) # mean lift 22.94

arules::inspect(head(cannabis))
arules::inspect(head(counterfeits))
arules::inspect(tail(counterfeits))
arules::inspect(tail(cannabis))

# marijuana-related Rule Subsets ----------------------------------------------

# Weed ----------------------------------------------------
mj <- subset(pmrules, subset = rhs %in% "cat=Information" & lift > 1.2) # 36 rules
summary(mj)
# set of 134 rules
arules::inspect(head(mj))[1:10, ]
arules::inspect(tail(mj))

v2 <- subset(pmrules, subset = rhs %in% "subcat=Benzos")
inspect(v2)

v4 <- subset(pmrules, rhs %in% "subsubcat=MDMA")
inspect(v4)
summary(v4)

v5 <- subset(pmrules, rhs %in% "subcat=Ecstasy")
inspect(v5)

v6 <- subset(pmrules, rhs %in% "from=torland")
inspect(v6)

v7 <- subset(pmrules, rhs %in% "cat=Drug paraphernalia") # 14
inspect(v7)

v8 <- subset(pmrules, rhs %in% "cat=Services")
inspect(v8)

v9 <- subset(pmrules, rhs %in% "subsubcat=Weed")
inspect(v9)


# visualize ----------------------------------------
plot(v9, method = "matrix", measure = "lift")
plot(pmrules, shading = "order")

par(mar = c(8, 8, 8 , 8), family = "HersheySans")
plot(pmrules, measure = c("support", "lift"), shading = "confidence")

plot(v8, method = "graph")
plot(v4, method = "graph", control = list(type = "itemsets"))

plot(v2, method = "graph")
plot(counterfeits, method = "graph")



