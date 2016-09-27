# Agora Marketplace Analysis
# Association Rules - Agora Population
# a2:30954 observations of 5 variables:
# price, from (location), (main) category, subcategory, vendor

# load data -------------------------------------------------------------------

library(data.table)
library(arules)
library(arulesViz)
library(igraph)
library(geomnet)
library(ggplot2)
library(anonymizer)

# population
# a <- fread("~/GitHub/agora-data/agora-01b.csv", stringsAsFactors = T)
# 2322961 obs of 18 variables

# prepped data
ag <- fread("~/GitHub/agora-data/06-arules/ag-arules-20k.csv", stringsAsFactors = T)

# Transactions Conversion -----------------------------------------------------

# subset variables
ag <- subset(ag, select = c("p", "from", "cat", "sc", "v3"))
colnames(ag) <- c("p", "f", "c", "sc", "v")
head(ag)

ag <- as.data.frame(ag)
# write.csv(ag, file = "~/GitHub/agora-data/06-arules/ag20k-go.csv", row.names = F)

# convert to transactions method 01:

# remove duplicates:
ag.u <- ag[!duplicated(ag), ]
nrow(ag.u) # 30954
head(ag.u)

a2 <- as(ag.u, "transactions")
summary(a2)

# convert to transactions method 02:
# write out - read in

# write.csv(ag, file = "ag.csv", row.names = T)
# a2 <-read.transactions("ag.csv", rm.duplicates=TRUE, format = "basket", sep = ",")
# summary(a2)

# Item Frequency Plot ---------------------------------------------------------

par(mfrow = c(1, 1), mar = c(4, 12, 4, 4), family = "GillSans")
itemFrequencyPlot(a2, support = 0.0025, cex.names = 0.8, col = "white", horiz = T,
                  main = "Agora Marketplace: Frequent Items (support > 0.0025)")

# Item Frequency Plot Loop ----------------------------------------------------

# define support intervals
sup <- seq(0.000, 0.1, by = 0.005)
sup
sup[[12]]

# plot loop
for (i in 1:length(sup)) {
  
  par(mfrow = c(1, 1), mar = c(4, 12, 4, 4), family = "GillSans")
  
  png(filename = paste("~/GitHub/agora-local-market/arules/ifp/ItemFreq", sup[i], ".png"),
      width = 1800, height = 1400, pointsize = 18, bg = "transparent")
  
  itemFrequencyPlot(a2, support = sup[i], cex.names = 0.8, col = "#FFFFFF00", horiz = T,
                    main = paste("Agora Marketplace: Frequent Items (support >", 
                                 sup[i], ")"))
  
  dev.off()
  
}

# Mine Frequent Itemsets ------------------------------------------------------

# 'find an interesting support (have at least 500 observations)'
# via Michael Hahsler seminar tutorial: NYPD stop-and-frisk
# nrow(a2)
# 500/nrow(a2) # 0.000215522

# Looking at the itemFreqPlot outputs, 0.005 a sa minsup yielded a wide 
# but not overwhelmingly large range of transactions. I plotted 0.0025
# manually and noticed vendors appear in this range - so let's set the 
# - so let's set the minsup to that for now.

a2items <- apriori(a2, parameter = list(target = "frequent",
                                        supp = 0.0025, minlen = 2, maxlen = 5))

summary(a2items)
# set of 738 itemsets

# Mine Association Rules ------------------------------------------------------

a2rules <- apriori(a2, parameter = list(support = 0.0025, confidence = 0.6,
                                        minlen = 2, maxlen = 5))
a2rules
# set of 389 rules 
summary(a2rules)
arules::inspect(head(a2rules, 20))
arules::inspect(tail(a2rules, 20))
arules::inspect(a2rules)[101:111, ]

# Plot Rules - Group -----------------------------------------------------------
# plot(a2rules, method = "matrix", measure = "lift", main = "207 rules ~ lift")
# plot(a2rules, method = "matrix", measure = c("support", "confidence"))

# individual
plot(a2rules, method = "grouped", control = list(k = 36))

# loop
for (i in 1:10) {
  
  png(filename = paste("~/GitHub/agora-local-market/arules/groups/g1-",i,".png"),
      width = 1800, height = 1400, pointsize = 20, bg = "transparent")
  
  k = i * 12
  
  plot(a2rules, method = "grouped", control = list(k = k), 
       main = paste("k =", k))
  
  dev.off()
  
}

# Plot Rules - Graph ----------------------------------------------------------

# get layouts
grep("^layout_", ls("package:igraph"), value = T)[-1]

# define a palette
pdpal <- colorRampPalette(c("#B2DFEE85", "#FFFFFF75", "#00688B85"), alpha = 0.85)

# plot by Support, Confidence, and Lift
r1 <- head(sort(a2rules, by = c("support", "confidence", "lift")), 212)
p1 <- plot(r1, method = "graph", 
           main = "212 rules ~ support + confidence + lift (dh)", 
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
r2 <- head(sort(a2rules, by = "lift"), 36)
p2 <- plot(r2, method = "graph", 
           main = "36 rules ~  lift (kk)", 
           edge.color = "#00000025",
           vertex.frame.color = "#00688B85",
           vertex.label.color = "grey2", 
           vertex.color = pdpal(100),
           vertex.label.cex = 0.68, layout = layout_with_kk,
           vertex.label.dist = 0)

# plot by Support and confidence
r3 <- head(sort(a2rules, by = c("support", "confidence")), 48)
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

for (i in 103:212) {
  
  log <- head(sort(a2rules, by = c("support", "confidence", "lift")), i)
  
  png(filename = paste("~/GitHub/agora-local-market/arules/igraphs/r1-SCL-",i,".png"),
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

for (i in 27:201) {
  
  tmp <- head(sort(a2rules, by = "lift"), i)
  
  png(filename = paste("~/GitHub/agora-local-market/arules/igraphs/a4-r2-Lift-",i,".png"),
      width = 1800, height = 1400, pointsize = 18, bg = "transparent")
  
  par(family = "GillSans")
  
  set.seed(256)
  plot(tmp, method = "graph", 
       main = paste(i, "rules ~ lift (dh)"),
       edge.color = "#00000025",
       vertex.frame.color = "#00688B85",
       vertex.color = pdpal(100),
       vertex.label.color = "gray8", 
       vertex.label.cex = 0.70, layout = layout_with_dh,
       vertex.label.dist = 0)
  
  dev.off()
}

# r3: by Support and Confidence -----------------------------------------------

for (i in 1:80) {
  
  tmp <- head(sort(a2rules, by = c("support", "confidence")), i)
  
  png(filename = paste("~/GitHub/agora-local-market/arules/igraphs/a4-r3-SC-",i,".png"),
      width = 1800, height = 1400, pointsize = 20, bg = "transparent")
  
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

# Subset by Category ----------------------------------------------------------

# by category
levels(ag$sc)
table(ag.u$c)

par(mfrow = c(1, 1), mar = c(1, 1, 1, 1), family = "GillSans")
d1 <- head(sort(c.drugs, by = c("support", "confidence", "lift")), 24)
dp1 <- plot(d1, method = "graph", layout = layout_with_dh,
            main = "24 rules ~ Support + Confidence + Lift: 'Drugs'")

c.data <- subset(a2rules, rhs %in% "c=Data" | lhs %in% "c=Data")
d2 <- head(sort(c.data, by = c("support", "confidence", "lift")), 24)
dp2 <- plot(d2, method = "graph", layout = layout_with_fr, 
            main = "2 rules ~ Support + Confidence + Lift: 'Data'")

c.info <- subset(a2rules, rhs %in% "c=Information" | lhs %in% "c=Information")
d3 <- head(sort(c.info, by = c("support", "confidence", "lift")), 24)
dp3 <- plot(d3, method = "graph", layout = layout_with_fr, 
            main = "7 rules ~ Support + Confidence + Lift: 'Info'")

c.services <- subset(a2rules, rhs %in% "c=Services" | lhs %in% "c=Services")
d4 <- head(sort(c.services, by = c("support", "confidence", "lift")), 24)
dp4 <- plot(d4, method = "graph", layout = layout_with_dh, 
            main = "7 rules ~ Support + Confidence + Lift: 'Services'")

# no rules found:
# c.chemicals <- subset(a2rules, rhs %in% "c=Chemicals" | lhs %in% "c=Chemicals")
# c.counterfeits <- subset(a2rules, rhs %in% "c=Counterfeits" | lhs %in% "c=Counterfeits")
# c.drug.para <- subset(a2rules, rhs %in% "c=Drug paraphernalia" | lhs %in% "c=Drug paraphernalia")
# c.electronics <- subset(a2rules, rhs %in% "c=Electronics" | lhs %in% "c=Electronics")
# c.forgeries <- subset(a2rules, rhs %in% "c=Forgeries" | lhs %in% "c=Forgeries")
# c.eBooks <- subset(a2rules, rhs %in% "c=Info/eBooks" | lhs %in% "c=Info/eBooks")
# c.weapons <- subset(a2rules, rhs %in% "c=Weapons" | lhs %in% "c=Weapons")

# subset: Cannabis -----------------------------------------------------------

cannabis <- subset(a2rules, rhs %in% "sc=Cannabis-Weed" | lhs %in% "sc=Cannabis-Weed")
inspect(cannabis)

# subset: prices  ------------------------------------------------------------

levels(ag$p)
price010 <- subset(a2rules, rhs %in% "p=$0-10" | lhs %in% "p=$0-10")             # 28
price150 <- subset(a2rules, rhs %in% "p=$10-150" | lhs %in% "p=$10-150")         # 75
price600 <- subset(a2rules, rhs %in% "p=$150-600" | lhs %in% "p=$150-600")       # 57
price10k <- subset(a2rules, rhs %in% "p=$2000-10000" | lhs %in% "p=$2000-10000") # 17
price20k <- subset(a2rules, rhs %in% "p=$10000-20000" | lhs %in% "p=$10000-20000")

inspect(price010)
inspect(price150)
inspect(price600)
inspect(price10k)
inspect(price20k)

# Subset by Location ----------------------------------------------------------

# define palette
pdpal2 <- colorRampPalette(c("#FFE4C485", "#FFFFFF75", "#CD107685"), alpha = 0.85)
pdpal2(100)

# subset: USA -----------------------------------------------------------------
f.usa <- subset(a2rules, rhs %in% "f=USA" | lhs %in% "f=USA")
inspect(head(f.usa), 10)

for (i in 1:76) {
  
  i <- i * 2
  tmp <- head(sort(f.usa, by = c("support", "confidence", "lift")), i)
  
  png(filename = paste("~/GitHub/agora-local-market/arules/location/f1-usa-",i,".png"),
      width = 1800, height = 1400, pointsize = 17, bg = "transparent")
  
  par(family = "GillSans")
  
  set.seed(64)
  plot(tmp, method = "graph", 
       main = paste(i, "rules ~ support + confidence + lift: 'USA'"),
       edge.color = "#00000025",
       vertex.frame.color = "#00688B85",
       vertex.label.color = "grey8", 
       vertex.color = pdpal(100),
       vertex.label.cex = 0.68, layout = layout_with_dh,
       vertex.label.dist = 0)
  
  dev.off()
}


# subset: locations
f.agora <- subset(a2rules, rhs %in% "f=Agora/Internet/Torland" | lhs %in% "f=Agora/Internet/Torland")
f.belgium <- subset(a2rules, rhs %in% "f=Belgium" | lhs %in% "f=Belgium")
f.canada <- subset(a2rules, rhs %in% "f=Canada" | lhs %in% "f=Canada")
f.china <- subset(a2rules, rhs %in% "f=China" | lhs %in% "f=China")
f.eu <- subset(a2rules, rhs %in% "f=EU" | lhs %in% "f=EU")
f.germany <- subset(a2rules, rhs %in% "f=Germany" | lhs %in% "f=Germany")
f.netherlands <- subset(a2rules, rhs %in% "f=Netherlands" | lhs %in% "f=Netherlands")
f.sweden <- subset(a2rules, rhs %in% "f=Sweden" | lhs %in% "f=Sweden")
