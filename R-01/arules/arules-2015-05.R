# Agora Marketplace Analysis
# Association Rules - 2014 Product Data
# Cannabis Analysis - SubSubCategories

# load data -------------------------------------------------------------------

library(arules)
library(arulesViz)
library(data.table)
library(igraph)

v15 <- fread("~/GitHub/agora-data/v15-arules-00.csv", stringsAsFactors = T)
# 1133674 obs of 14 variables

# subset under 50k ---------------------------------------

v15b <- subset(v15, v15$usd <= 50000) # 1132476
v15b <- subset(v15b, select = c("vendor", "product", "cat", "subcat", "subsubcat", "from"))
colnames(v15b) <- c("v", "p", "c", "s", "s", "p")
head(v15b)

v15b <- as.data.frame(v15b)

v2 <- v15b[!duplicated(v15b), ] # 70703
head(v2)
v2b <- v15b

# write out - read in
write.csv(v15b, file = "v15b.csv", row.names = T)
v2b <-read.transactions("v15b.csv", rm.duplicates=TRUE, format = "basket", sep = ",")

summary(v2b)
# transactions in sparse format with
#  1132476 transactions (rows) and
#  63822 items (columns)

# VCF - Mine Frequent Itemsets ------------------------------------------------

nrow(v2b)
500/nrow(v2b) # 0.007070436
# 'find an interesting support (have at least 500 observations)'

v2items <- apriori(v2b, parameter = list(target = "frequent",
                                        supp = 0.0071, minlen = 3, maxlen = 5))

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
itemFrequencyPlot(v2b, support = 0.15, cex.names = 1, col = "white",
                  main = "Agora 2015: Frequent Items (support > 0.15)")


# VCF - Mine Association Rules ------------------------------------------------

v2rules <- apriori(v2b, parameter = list(support = 0.007, confidence = 0.6,
                                        minlen = 3, maxlen = 5))
v2rules
# set of 167 rules 
summary(v2rules)
arules::inspect(head(v2rules, 10))

plot(v2rules, method = "matrix", measure = "lift", main = "509 rules ~ lift")
plot(v2rules, method = "matrix", measure = c("support", "confidence"))
plot(v2rules, method = "matrix", measure = c("confidence", "lift"))
plot(v2rules, method = "matrix", measure = "confidence")
plot(v2rules, method = "matrix", measure = "support")
plot(v2rules, method = "matrix", measure = c("support", "lift"))

plot(v2rules, method = "grouped", control = list(k = 75))


pdpal <- colorRampPalette(c("firebrick3", "white", "deepskyblue4"))
pdpal(100)


subrules <- head(sort(v2rules, by = "lift"), 365)
plot(subrules, method = "graph", main = "365 rules ~ lift (dh)",
     vertex.label.color = "black", vertex.label.cex = 0.74, 
     measure = "support", layout = layout_with_dh, 
     vertex.label.dist = 0)


subrules2 <- head(sort(v2rules, by = "confidence"), 144)
plot(subrules3, method = "graph", edge.color = "gray88", 
     main = "144 rules ~ confidence (dh)", vertex.color = pdpal(101), 
     vertex.label.color = "black", 
     vertex.label.cex = 0.82, layout = layout_with_dh,
     vertex.label.dist = 0)

subrules3 <- head(sort(v2rules, by = c("confidence", "lift")), 212)
plot(subrules3, method = "graph", edge.color = "gray88", 
           main = "212 rules ~ confidence + lift (dh)", 
           vertex.color = pdpal(100), vertex.label.color = "black", 
           vertex.label.cex = 0.82, layout = layout_with_dh,
           vertex.label.dist = 0)

palf <- colorRampPalette(c(rgb(1,1,1, .7),rgb(.8,0,0, 1)), alpha=TRUE)

# interesting one:
subrules4 <- head(sort(v2rules, by = c("support", "confidence", "lift")), 212)
ig <- plot(subrules4, method = "graph", edge.color = "gray68", 
           main = "212 rules ~ support + confidence + lift (f-r)", 
           vertex.color = "#00688B60",
           vertex.label.color = "grey2", 
           vertex.label.cex = 0.68, layout = layout_with_fr,
           vertex.label.dist = 0)

subrules5 <- head(sort(v2rules, by = "support"), 48)
plot(subrules5, method = "graph", edge.color = "gray68", 
           main = "48 rules ~ support (nicely)", 
           vertex.color = palf(100), 
           vertex.label.color = "grey2", 
           vertex.label.cex = 0.8, layout = layout_nicely,
           vertex.label.dist = 0)

subrules6 <- head(sort(v2rules, by = c("support", "confidence")), 144)
plot(subrules6, method = "graph", main = "144 rules ~ support + confidence",
     layout = layout_with_graphopt,
     vertex.label.color = "grey12", vertex.color = "#00688B80",
     vertex.label.cex = .76)


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




