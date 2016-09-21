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

# combine categories
ag$subcat <- as.character(ag$subcat)
ag$subsubcat <- as.character(ag$subsubcat)
ag$subcat[is.na(ag$subcat)] <- ""
ag$subsubcat[is.na(ag$subsubcat)] <- ""

ag$ac <- paste(ag$cat, ag$subcat, ag$subsubcat, sep = ", ")
levels(as.factor(ag$ac))
ag$ac <- gsub("\\b,\\s$", "", ag$ac)

agora <- subset(ag, select = c("vendor", "from", "ac"))
agora$ac <- factor(agora$ac)

colnames(agora) <- c("v", "l", "c")
head(agora)

agora <- as.data.frame(agora)
a2 <- as(agora, "transactions")
summary(a2)

# remove duplicates
# a2 <- agora[!duplicated(agora), ] # 70703
# nrow(a2) # 355808
# head(a2)
# a2 <- ag

# write out - read in
# write.csv(ag, file = "ag.csv", row.names = T)
# a2 <-read.transactions("ag.csv", rm.duplicates=TRUE, format = "basket", sep = ",")

# summary(a2)
# transactions in sparse format with
#  1132476 transactions (rows) and
#  63822 items (columns)

# VCF - Mine Frequent Itemsets ------------------------------------------------

nrow(a2)
500/nrow(a2) # 0.000215522
# 'find an interesting support (have at least 500 observations)'

a2items <- apriori(a2, parameter = list(target = "frequent",
                                         supp = 0.00022, minlen = 3, maxlen = 5))

summary(a2items)
# set of 854 itemsets

par(mfrow = c(1, 1), mar = c(20, 6, 4, 2), family = "GillSans")
itemFrequencyPlot(a2, support = 0.025, cex.names = 1, col = "white",
                  main = "Agora Marketplace: Frequent Items (support > 0.025)")


# VCF - Mine Association Rules ------------------------------------------------

a2rules <- apriori(a2, parameter = list(support = 0.001, confidence = 0.6,
                                         minlen = 3, maxlen = 5))
a2rules
# set of 207 rules 
summary(a2rules)
arules::inspect(head(a2rules, 10))

plot(a2rules, method = "matrix", measure = "lift", main = "207 rules ~ lift")
plot(a2rules, method = "matrix", measure = c("support", "confidence"))
plot(a2rules, method = "matrix", measure = c("confidence", "lift"))
plot(a2rules, method = "matrix", measure = "confidence")
plot(a2rules, method = "matrix", measure = "support")
plot(a2rules, method = "matrix", measure = c("support", "lift"))

plot(a2rules, method = "grouped", control = list(k = 36))


pdpal <- colorRampPalette(c("#BFEFFF75", "#FFFFFF75", "#00688B75"), alpha = 0.5)
pdpal(100)

sr1 <- head(sort(a2rules, by = c("support", "confidence", "lift")), 24)
ig <- plot(sr1, method = "graph", 
           main = "24 rules ~ support + confidence + lift (dh)", 
           edge.color = "#00000025",
           vertex.color = pdpal(100),
           vertex.frame.color="#00000025",
           vertex.label.color = "grey2", 
           vertex.label.cex = 0.68, layout = layout_nicely,
           vertex.label.dist = 0)

sr2 <- head(sort(a2rules, by = "lift"), 128)
ig2 <- plot(sr1, method = "graph", 
           main = "128 rules ~  lift (dh)", 
           edge.color = "#00000025",
           vertex.frame.color="#00000025",
           vertex.label.color = "grey2", 
           vertex.label.cex = 0.68, layout = layout_with_dh,
           vertex.label.dist = 0)

usa <- subset(a2rules, rhs %in% "l=USA")
inspect(head(usa))
summary(usa)

plot(usa, method = "graph")

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




