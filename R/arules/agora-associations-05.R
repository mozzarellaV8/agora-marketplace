# Agora Marketplace Analysis
# Association Rules - Agora Population
# a6: 2,317,353 observations of 5 variables:
# price, from (location), subcategories

# load data -------------------------------------------------------------------

library(data.table)
library(arules)
library(arulesViz)
library(igraph)
library(geomnet)
library(ggplot2)

# population
# a <- fread("~/GitHub/agora-data/agora-01b.csv", stringsAsFactors = T)
# 2322961 obs of 18 variables

# prepped data: anonymized vendors
ag <- fread("~/GitHub/agora-data/06-arules/ag-arules20k-a5.csv", stringsAsFactors = T)
summary(ag$usd)

# Transactions Conversion -----------------------------------------------------

# subset variables
ag <- subset(ag, select = c("p", "from", "sc", "v3"))
colnames(ag) <- c("p", "f", "c", "v")
head(ag)

ag <- as.data.frame(ag)

# remove duplicates:
ag <- ag[!duplicated(ag), ]
nrow(ag) # 44176
head(ag)

a5 <- as(ag, "transactions")
summary(a5)

# Item Frequency Plot ---------------------------------------------------------

par(mfrow = c(1, 1), mar = c(4, 12, 4, 4), family = "GillSans")
itemFrequencyPlot(a5, support = 0.005, cex.names = 0.70, col = "white", horiz = T,
                  main = "Agora Marketplace: Frequent Items (support > 0.005)")

# Item Frequency Plot Loop ----------------------------------------------------

# define support intervals
sup <- seq(0.000, 0.1, by = 0.005)
sup

# plot loop
for (i in 1:length(sup)) {
  
  par(mfrow = c(1, 1), mar = c(4, 12, 4, 4), family = "GillSans")
  
  png(filename = paste("~/GitHub/agora-local-market/arules/ifp/a5-ItemFreq", sup[i], ".png"),
      width = 1800, height = 1400, pointsize = 18, bg = "transparent")
  
  itemFrequencyPlot(a5, support = sup[i], cex.names = 0.8, col = "#FFFFFF00", horiz = T,
                    main = paste("Agora Marketplace: Frequent Items (support >", 
                                 sup[i], ")"))
  
  dev.off()
  
}

# Mine Frequent Itemsets ------------------------------------------------------

# 'find an interesting support (have at least 500 observations)'
# via Michael Hahsler seminar tutorial: NYPD stop-and-frisk
# nrow(a5)
# 500/nrow(a5) # 0.000215522

# Looking at the itemFreqPlot outputs, 0.005 a sa minsup yielded a wide 
# but not overwhelmingly large range of transactions. I plotted 0.0025
# manually and noticed vendors appear in this range - so let's set the 
# - so let's set the minsup to that for now.

a5items <- apriori(a5, parameter = list(target = "frequent",
                                        supp = 0.0025, minlen = 2, maxlen = 5))

summary(a5items)
# set of 575 itemsets

a5items <- sort(a5items, by = "support", decreasing = T)
inspect(head(a5items, 12))
inspect(tail(a5items, 12))

inspect(a5items)[48:56,]


# Mine Association Rules ------------------------------------------------------

# minlength: 2
a5rules <- apriori(a5, parameter = list(support = 0.0025, confidence = 0.6, minlen = 2))
summary(a5rules)

a5rules <- sort(a5rules, by = "support", decreasing = T)
arules::inspect(head(a5rules, 10))
arules::inspect(tail(a5rules, 10))
inspect(a5rules)[123:128,]
inspect(a5rules)[48:56,]

# minlength: 3
a5r2 <- apriori(a5, parameter = list(support = 0.0025, confidence = 0.6, minlen = 3))
summary(a5r2)

a5rules <- sort(a5r2, by = "support", decreasing = T)
arules::inspect(head(a5r2, 10))
arules::inspect(tail(a5r2, 10))
inspect(a5r2)[123:128,]
inspect(a5r2)[48:56,]


# Plot Rules - Group -----------------------------------------------------------

# individual
plot(a5rules, method = "grouped", control = list(k = 36))

# loop
for (i in 1:10) {
  
  png(filename = paste("~/GitHub/agora-local-market/arules/groups/a5u-g1-",i,".png"),
      width = 1800, height = 1400, pointsize = 20, bg = "transparent")
  
  k = i * 12
  
  plot(a5rules, method = "grouped", control = list(k = k), 
       main = paste("k =", k))
  
  dev.off()
  
}

# Plot Rules - Graph ----------------------------------------------------------

# get layouts
grep("^layout_", ls("package:igraph"), value = T)[-1]

# define a palette
pdpal <- colorRampPalette(c("#B2DFEE85", "#FFFFFF75", "#00688B85"), alpha = 0.85)

# plot by Support, Confidence, and Lift
r1 <- head(sort(a5rules, by = c("support", "confidence", "lift")), 36)
p1 <- plot(r1, method = "graph", 
           main = "36 rules ~ support + confidence + lift (dh)", 
           edge.color = "#00000025",
           vertex.frame.color = "#00688B85",
           vertex.color = pdpal(100),
           vertex.label.color = "grey8", 
           vertex.label.cex = 0.68, layout = layout_with_dh,
           vertex.label.dist = 0)

hs <- hub_score(p1, weights=NA)$vector
as <- authority_score(p1, weights=NA)$vector
cliqueR1 <- sapply(cliques(p1), length)
largest_cliques(p1)


# plot by Lift
r2 <- head(sort(a5rules, by = "lift"), 36)
p2 <- plot(r2, method = "graph", 
           main = "36 rules ~  lift (kk)", 
           edge.color = "#00000025",
           vertex.frame.color = "#00688B85",
           vertex.label.color = "grey2", 
           vertex.color = pdpal(100),
           vertex.label.cex = 0.68, layout = layout_with_kk,
           vertex.label.dist = 0)

# plot by Support and confidence
r3 <- head(sort(a5rules, by = c("support", "confidence")), 48)
p3 <- plot(r3, method = "graph", 
           main = "48 rules ~ support + confidence (kk)", 
           edge.color = "#00000025",
           vertex.frame.color = "#00688B85",
           vertex.label.color = "grey2", 
           vertex.color = pdpal(100),
           vertex.label.cex = 0.68, layout = layout_with_kk,
           vertex.label.dist = 0)

# visNetwork ------------------------------------------------------------------
library(visNetwork)
p1df <- get.data.frame( p1, what = "both" )

visNetwork(
  nodes = data.frame(
    id = p1df$vertices$name
    ,value = p1df$vertices$lift # could change to lift or confidence
    ,title = ifelse(p1df$vertices$label == "",p1df$vertices$name, p1df$vertices$label)
    ,p1df$vertices
  )
  , edges = p1df$edges
)

# Plot Rules - Graph Loops ----------------------------------------------------

# r1: by Support, Confidence, and Lift ----------------------------------------

for (i in 24:98) {
  
  log <- head(sort(a5rules, by = c("support", "confidence", "lift")), i)
  
  png(filename = paste("~/GitHub/agora-local-market/arules/igraphs/a5u-r1-SCL-",i,".png"),
      width = 1800, height = 1400, pointsize = 18, bg = "transparent")
  
  par(family = "GillSans")
  set.seed(144)
  plot(log, method = "graph", 
       main = paste(i, "rules ~ support + confidence + lift (dh)"),
       edge.color = "#00000025",
       vertex.frame.color = "#00688B85",
       vertex.color = pdpal(100),
       vertex.label.color = "grey8", 
       vertex.label.cex = 0.68, layout = layout_with_dh,
       vertex.label.dist = 0)
  
  dev.off()
}

# r2: by Lift ----------------------------------------------------------------

even <- seq(2, 100, 2)

for (i in 1:length(even)) {
  
  tmp <- head(sort(a5rules, by = "lift"), even[i])
  
  png(filename = paste("~/GitHub/agora-local-market/arules/igraphs/a5b-r2-Lift-",even[i],".png"),
      width = 1800, height = 1400, pointsize = 16, bg = "transparent")
  
  par(family = "GillSans")
  
  set.seed(256)
  plot(tmp, method = "graph", 
       main = paste(even[i], "rules ~ lift (dh)"),
       edge.color = "#00000025",
       vertex.frame.color = "#00688B85",
       vertex.color = pdpal(100),
       vertex.label.color = "gray8", 
       vertex.label.cex = 0.70, layout = layout_with_dh,
       vertex.label.dist = 0)
  
  dev.off()
}

# r3: by Support and Confidence -----------------------------------------------

for (i in 48:144) {
  
  tmp <- head(sort(a5rules, by = c("support", "confidence")), i)
  
  png(filename = paste("~/GitHub/agora-local-market/arules/igraphs/a5-r3-SC-",i,".png"),
      width = 1800, height = 1400, pointsize = 16, bg = "transparent")
  
  par(family = "GillSans")
  
  set.seed(64)
  plot(tmp, method = "graph", 
       main = paste(i, "rules ~ support + confidence (dh)"),
       edge.color = "#00000025",
       vertex.frame.color = "#00688B85",
       vertex.color = pdpal(100),
       vertex.label.color = "grey8", 
       vertex.label.cex = 1, layout = layout_with_kk,
       vertex.label.dist = 0)
  
  dev.off()
}
