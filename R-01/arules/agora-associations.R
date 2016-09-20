# Agora Marketplace Analysis
# Association Rules - All Agora

# load data -------------------------------------------------------------------

library(arules)
library(arulesViz)
library(data.table)
library(igraph)
library(tidyr)

a <- fread("~/GitHub/agora-data/agora-01b.csv", stringsAsFactors = T)
# 2322961 obs of 18 variables

# subset under 50k ---------------------------------------
ag <- subset(a, a$usd <= 50000) # 2319949

# stack categories
ag <- subset(ag, select = c("vendor", "product", "cat", "subcat", "subsubcat", "from"))
agora <- gather(ag, key = level, value = category, cat, subcat, subsubcat)
nrow(agora) # 6,959,847

# write.csv(agora, file = "~/GitHub/agora-data/agora-stacked.csv", row.names = F)

colnames(agora) <- c("v", "p", "l", "t", "c")
head(agora)

agora <- as.data.frame(agora)
agora$c <- as.factor(agora$c) # 108 levels

# remove duplicates
a2 <- agora[!duplicated(agora), ] # 70703
nrow(a2) # 355808


head(a2)
a2b <- ag

# write out - read in
write.csv(ag, file = "ag.csv", row.names = T)
a2b <-read.transactions("ag.csv", rm.duplicates=TRUE, format = "basket", sep = ",")

summary(a2b)
# transactions in sparse format with
#  1132476 transactions (rows) and
#  63822 items (columns)

# VCF - Mine Frequent Itemsets ------------------------------------------------

nrow(a2b)
500/nrow(a2b) # 0.007070436
# 'find an interesting support (have at least 500 observations)'

a2items <- apriori(a2b, parameter = list(target = "frequent",
                                        supp = 0.0071, minlen = 3, maxlen = 5))

summary(a2items)
# set of 1622 itemsets

#     support        
# Min.   :0.006821  
# 1st Qu.:0.007526  
# Median :0.008163  
# Mean   :0.010798  
# 3rd Qu.:0.011095  
# Max.   :0.046957


par(mfrow = c(1, 1), mar = c(20, 6, 4, 2), family = "GillSans")
itemFrequencyPlot(a2b, support = 0.15, cex.names = 1, col = "white",
                  main = "Agora 2015: Frequent Items (support > 0.15)")


# VCF - Mine Association Rules ------------------------------------------------

a2rules <- apriori(a2b, parameter = list(support = 0.007, confidence = 0.6,
                                        minlen = 3, maxlen = 5))
a2rules
# set of 167 rules 
summary(a2rules)
arules::inspect(head(a2rules, 10))

plot(a2rules, method = "matrix", measure = "lift", main = "509 rules ~ lift")
plot(a2rules, method = "matrix", measure = c("support", "confidence"))
plot(a2rules, method = "matrix", measure = c("confidence", "lift"))
plot(a2rules, method = "matrix", measure = "confidence")
plot(a2rules, method = "matrix", measure = "support")
plot(a2rules, method = "matrix", measure = c("support", "lift"))

plot(a2rules, method = "grouped", control = list(k = 75))


pdpal <- colorRampPalette(c("firebrick3", "white", "deepskyblue4"))
pdpal(100)


subrules <- head(sort(a2rules, by = "lift"), 365)
plot(subrules, method = "graph", main = "365 rules ~ lift (dh)",
     vertex.label.color = "black", vertex.label.cex = 0.74, 
     measure = "support", layout = layout_with_dh, 
     vertex.label.dist = 0)


subrules2 <- head(sort(a2rules, by = "confidence"), 144)
plot(subrules3, method = "graph", edge.color = "gray88", 
     main = "144 rules ~ confidence (dh)", vertex.color = pdpal(101), 
     vertex.label.color = "black", 
     vertex.label.cex = 0.82, layout = layout_with_dh,
     vertex.label.dist = 0)

subrules3 <- head(sort(a2rules, by = c("confidence", "lift")), 212)
plot(subrules3, method = "graph", edge.color = "gray88", 
           main = "212 rules ~ confidence + lift (dh)", 
           vertex.color = pdpal(100), vertex.label.color = "black", 
           vertex.label.cex = 0.82, layout = layout_with_dh,
           vertex.label.dist = 0)

palf <- colorRampPalette(c(rgb(1,1,1, .7),rgb(.8,0,0, 1)), alpha=TRUE)

# interesting one:
subrules4 <- head(sort(a2rules, by = c("support", "confidence", "lift")), 212)
ig <- plot(subrules4, method = "graph", edge.color = "gray68", 
           main = "212 rules ~ support + confidence + lift (f-r)", 
           vertex.color = "#00688B60",
           vertex.label.color = "grey2", 
           vertex.label.cex = 0.68, layout = layout_with_fr,
           vertex.label.dist = 0)

subrules5 <- head(sort(a2rules, by = "support"), 48)
plot(subrules5, method = "graph", edge.color = "gray68", 
           main = "48 rules ~ support (nicely)", 
           vertex.color = palf(100), 
           vertex.label.color = "grey2", 
           vertex.label.cex = 0.8, layout = layout_nicely,
           vertex.label.dist = 0)

subrules6 <- head(sort(a2rules, by = c("support", "confidence")), 144)
plot(subrules6, method = "graph", main = "144 rules ~ support + confidence",
     layout = layout_with_graphopt,
     vertex.label.color = "grey12", vertex.color = "#00688B80",
     vertex.label.cex = .76)


# extraction methods ----------------------------------------------------------

a2g <- get.data.frame(ig, "both")
plot(ig, layout=layout_with_fr, vertex.size = 18, vertex.label.cex = 0.55,
     vertex.color = "#00688B80")

layouts <- grep("^layout_", ls("package:igraph"), value = T)[-1]


# rename/relabel --------------------------------------------------------------

### this is how you replace item labels and set a hierachy (here level1)
myLabels <- c("milk", "butter", "beer")
myLevel1 <- c("dairy", "dairy", "beverage")
itemInfo(trans) <- data.frame(labels = myLabels, level1 = myLevel1)


vLabels <- as.character(levels(ag$vendor))
cLabels <- as.character(levels(ag$category))
fLabels <- as.character(levels(ag$from))
pLabels <- as.character(levels(ag$product))
inspect(a2items)




