# Agora Marketplace Analysis
# Association Rules - Agora Population
# a6: 2,317,353 observations of 5 variables:
# price, from (location), subcategories, vendor

# load data -------------------------------------------------------------------

library(data.table)
library(arules)
library(arulesViz)
library(igraph)
library(geomnet)
library(ggplot2)

# population
# a <- fread("~/GitHub/agora-data/agora-02.csv", stringsAsFactors = T)
# 2322961 obs of 18 variables

library(zoo)
a$gold.oz <- na.locf(a$gold.oz)
summary(a$gold.oz)
var(a$gold.oz)
gold <- as.data.frame(table(a$gold.oz))

write.csv(a, file = "~/GitHub/agora-data/agora-02.csv", row.names = F)

# prepped data: anonymized vendors
ag <- fread("~/GitHub/agora-data/06-arules/ag-arules20k-a6.csv", stringsAsFactors = T)
summary(ag$usd)

# Transactions Conversion -----------------------------------------------------

# subset variables
ag <- subset(ag, select = c("p", "from", "sc"))
colnames(ag) <- c("p", "f", "c")
head(ag)

ag <- as.data.frame(ag)

# remove duplicates:
ag <- ag[!duplicated(ag), ]
nrow(ag) # 44176
head(ag)

a6 <- as(ag, "transactions")
summary(a6)

# Item Frequency Plot ---------------------------------------------------------

par(mfrow = c(1, 1), mar = c(4, 12, 4, 4), family = "GillSans")
itemFrequencyPlot(a6, support = 0.01, cex.names = 0.70, col = "white", horiz = T,
                  main = "Agora Marketplace: Frequent Items (support > 0.01)")

# Item Frequency Plot Loop ----------------------------------------------------

# define support intervals
sup <- seq(0.000, 0.1, by = 0.005)
sup

# plot loop
for (i in 1:length(sup)) {
  
  par(mfrow = c(1, 1), mar = c(4, 12, 4, 4), family = "GillSans")
  
  png(filename = paste("~/GitHub/agora-local-market/arules/ifp/a6-ItemFreq", sup[i], ".png"),
      width = 1800, height = 1400, pointsize = 18, bg = "transparent")
  
  itemFrequencyPlot(a6, support = sup[i], cex.names = 0.8, col = "#FFFFFF00", horiz = T,
                    main = paste("Agora Marketplace: Frequent Items (support >", 
                                 sup[i], ")"))
  
  dev.off()
  
}

# Mine Frequent Itemsets ------------------------------------------------------

# 'find an interesting support (have at least 500 observations)'
# via Michael Hahsler seminar tutorial: NYPD stop-and-frisk
# nrow(a6)
# 500/nrow(a6) # 0.000215522

# Looking at the itemFreqPlot outputs, 0.005 a sa minsup yielded a wide 
# but not overwhelmingly large range of transactions. I plotted 0.0025
# manually and noticed vendors appear in this range - so let's set the 
# - so let's set the minsup to that for now.

a6items <- apriori(a6, parameter = list(target = "frequent",
                                        supp = 0.0025, minlen = 2, maxlen = 5))

summary(a6items)
# set of 336 itemsets

a6items <- sort(a6items, by = "support", decreasing = T)
inspect(head(a6items, 12))
inspect(tail(a6items, 12))

inspect(a6items)[48:56,]

# Mine Association Rules ------------------------------------------------------

# minlength: 2
a6rules <- apriori(a6, parameter = list(support = 0.0005, confidence = 0.6, minlen = 2))
summary(a6rules)

a6rules <- sort(a6rules, by = "support", decreasing = T)
arules::inspect(head(a6rules, 10))
arules::inspect(tail(a6rules, 10))
inspect(a6rules)[123:128,]
inspect(a6rules)[48:56,]

a6rules <- sort(a6rules, by = "lift", decreasing = T)

# Plot Rules - Group -----------------------------------------------------------

# individual
plot(a6rules, method = "grouped", control = list(k = 36))

# loop
for (i in 1:10) {
  
  png(filename = paste("~/GitHub/agora-local-market/arules/groups/a6-g1-",i,".png"),
      width = 1800, height = 1400, pointsize = 20, bg = "transparent")
  
  k = i * 12
  
  plot(a6rules, method = "grouped", control = list(k = k), 
       main = paste("k =", k))
  
  dev.off()
  
}


# investigate {Physical documents} => {Prescription}
# 1 rule
scripts <- subset(a6rules, rhs %in% "c=Prescription")
summary(scripts)
inspect(scripts)

# 2 rules
docs <- subset(a6rules, lhs %in% "c=Physical documents")
summary(docs)
inspect(docs)

# 0 rules
sd <- subset(a6rules, lhs %in% "c=Prescription")
summary(sd)

# Plot Rules - Graph ----------------------------------------------------------

# get layouts
grep("^layout_", ls("package:igraph"), value = T)[-1]

# define a palette
pdpal <- colorRampPalette(c("#B2DFEE85", "#FFFFFF75", "#00688B85"), alpha = 0.85)

# plot by Support, Confidence, and Lift
r1 <- head(sort(a6rules, by = c("support", "confidence", "lift")), 36)
p1 <- plot(r1, method = "graph", 
           main = "36 rules ~ support + confidence + lift (dh)", 
           edge.color = "#00000025",
           vertex.frame.color = "#00688B85",
           vertex.color = pdpal(100),
           vertex.label.color = "grey8", 
           vertex.label.cex = 0.8, layout = layout_with_dh,
           vertex.label.dist = 0)

hs <- hub_score(p1, weights=NA)$vector
as <- authority_score(p1, weights=NA)$vector
cliqueR1 <- sapply(cliques(p1), length)
largest_cliques(p1)


# plot by Lift
r2 <- head(sort(a6rules, by = "lift"), 36)
p2 <- plot(r2, method = "graph", 
           main = "36 rules ~  lift (kk)", 
           edge.color = "#00000025",
           vertex.frame.color = "#00688B85",
           vertex.label.color = "grey2", 
           vertex.color = pdpal(100),
           vertex.label.cex = 0.8, layout = layout_with_dh,
           vertex.label.dist = 0)

# plot by Support and confidence
r3 <- head(sort(a6rules, by = c("support", "confidence")), 36)
p3 <- plot(r3, method = "graph", 
           main = "36 rules ~ support + confidence (kk)", 
           edge.color = "#00000025",
           vertex.frame.color = "#00688B85",
           vertex.label.color = "grey2", 
           vertex.color = pdpal(100),
           vertex.label.cex = 0.8, layout = layout_with_kk,
           vertex.label.dist = 0)

# Plot Rules - Graph Loops ----------------------------------------------------

# r1: by Support, Confidence, and Lift ----------------------------------------

even <- seq(2, 164, 2)

for (i in 1:length(even)) {
  
  log <- head(sort(a6rules, by = c("support", "confidence", "lift")), even[i])
  
  png(filename = paste("~/GitHub/agora-local-market/arules/igraphs/a6-r1-SCL-",i,".png"),
      width = 1800, height = 1400, pointsize = 18, bg = "transparent")
  
  par(family = "GillSans")
  set.seed(144)
  plot(log, method = "graph", 
       main = paste(i, "rules ~ support + confidence + lift (kk)"),
       edge.color = "#00000025",
       vertex.frame.color = "#00688B85",
       vertex.color = pdpal(100),
       vertex.label.color = "#00000096", 
       vertex.label.cex = 0.68, layout = layout_with_kk,
       vertex.label.dist = 0)
  
  dev.off()
}

# r2: by Lift ----------------------------------------------------------------

even <- seq(2, 164, 2)

for (i in 1:length(even)) {
  
  tmp <- head(sort(a6rules, by = "lift"), even[i])
  
  png(filename = paste("~/GitHub/agora-local-market/arules/igraphs/a6-r2-Lift-",even[i],".png"),
      width = 1800, height = 1400, pointsize = 16, bg = "transparent")
  
  par(family = "GillSans")
  
  set.seed(256)
  plot(tmp, method = "graph", 
       main = paste(even[i], "rules ~ lift (kk)"),
       edge.color = "#00000025",
       vertex.frame.color = "#00688B85",
       vertex.color = pdpal(100),
       vertex.label.color = "#00000095", 
       vertex.label.cex = 0.70, layout = layout_with_kk,
       vertex.label.dist = 0)
  
  dev.off()
}

# r3: by Support and Confidence -----------------------------------------------

even <- seq(2, 164, 2)

for (i in 1:length(even)) {
  
  tmp <- head(sort(a6rules, by = c("support", "confidence")), even[i])
  
  png(filename = paste("~/GitHub/agora-local-market/arules/igraphs/a6-r3-SC-",even[i],".png"),
      width = 1800, height = 1400, pointsize = 16, bg = "transparent")
  
  par(family = "GillSans")
  
  set.seed(64)
  plot(tmp, method = "graph", 
       main = paste(even[i], "rules ~ support + confidence (dh)"),
       edge.color = "#00000025",
       vertex.frame.color = "#00688B85",
       vertex.color = pdpal(100),
       vertex.label.color = "grey8", 
       vertex.label.cex = 1, layout = layout_with_dh,
       vertex.label.dist = 0)
  
  dev.off()
}

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

