# Agora Marketplace Analysis
# Association Rules - Agora Population

# load data -------------------------------------------------------------------

library(data.table)
library(arules)
library(arulesViz)
library(igraph)
library(geomnet)
library(ggplot2)
library(anonymizer)

a <- fread("~/GitHub/agora-data/agora-01b.csv", stringsAsFactors = T)
# 2322961 obs of 18 variables

# subset under 50k ------------------------------------------------------------
ag <- subset(a, a$usd <= 20000) # 2319949

# combine subcategories
ag$subcat <- as.character(ag$subcat)
ag$subsubcat <- as.character(ag$subsubcat)
ag$subcat[is.na(ag$subcat)] <- ""
ag$subsubcat[is.na(ag$subsubcat)] <- ""

ag$sc <- paste(ag$subcat, ag$subsubcat, sep = ", ")
levels(as.factor(ag$sc))
ag$sc <- gsub("\\b,\\s$", "", ag$sc)

levels(as.factor(ag$sc))
ag$sc <- factor(ag$sc) # 106 levels

# prep - discretize prices -----------------------------------------------------
# but into cluster or interval?

ag$usd <- round(ag$usd, 2)

summary(ag$usd)
#  Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.00    24.28    84.97   426.40   290.20 20000.00

quantile(ag$usd)
#   0%      25%      50%      75%     100% 
# 0.00    24.28    84.97   290.19 20000.00 

# prep - plot price distributions ----------------------------------------------
par(mfrow = c(2, 2), mar = c(6, 6, 6, 6), family = "GillSans")
hist(ag$usd, breaks = 100, main = "n < $20,000", 
     xlab = "", ylab = "Frequency")
hist(ag$usd, breaks = 100, xlim = c(0, 5000), 
     main = "n < $5,000", xlab = "", ylab = "")
hist(ag$usd, breaks = 1000, xlim = c(0, 1000), 
     main = "n < $1,000", xlab = "price in USD", ylab = "Frequency")
hist(ag$usd, breaks = 10000, xlim = c(0, 200),
     main = "n < $200", xlab = "price in USD", ylab = "")

# heavy on the left/long tail - quick check of the log()
ag$log.usd <- log(ag$usd)

par(mfrow = c(1, 1), mar = c(6, 6, 6, 6), las = 1, family = "GillSans")
hist(ag$log.usd, main = "log(usd) Distribution of Prices, n = 2316650",
     breaks = 100, xlab = "", ylab = "")
axis(1, at = seq(-5, 10, 1))

summary(log.usd)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -Inf   3.190   4.442    -Inf   5.671   9.903 
exp(4.25)

ag$log.usd <- log.usd
nrow(ag) - 703

ggplot(ag, aes(x = log.usd)) + 
  geom_histogram(binwidth = 0.25, color = "black", alpha = 0, size = 0.5) +
  scale_x_continuous(breaks = seq(-5, 10, 1)) +
  theme_minimal(base_size = 14, base_family = "GillSans") +
  theme(axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        panel.grid.major = element_line(color = "gray72"),
        plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  labs(title = "log Distribution of Prices, n = 2316650",
       x = "", y = "")

# prep - actually discretize --------------------------------------------------

# manually
ag$p <- ag$usd
ag$p <- ifelse(ag$p <= 10.00, "$0-10", 
               ifelse(ag$p > 10 & ag$p <= 150.00, "$10-150",
                      ifelse(ag$p > 150 & ag$p <= 600.00, "$150-600",
                             ifelse(ag$p > 600 & ag$p <= 2000.00, "$600-2000",
                                    ifelse(ag$p > 2000 & ag$p <= 10000, "$2000-10000",
                                           ifelse(ag$p > 10000, "$10000-20000", NA))))))


ag$p <- factor(ag$p)  # 6 levels

summary(ag$p)
#  $0-10      $10-150 $10000-20000     $150-600  $2000-10000    $600-2000 
# 371235      1086166         7393       515111       106747       230701 

371235/nrow(ag)   # 0.1601979
1086166/nrow(ag)  # 0.4687098
7393/nrow(ag)     # 0.003190278
515111/nrow(ag)   # 0.2222842
106747/nrow(ag)   # 0.04606419
230701/nrow(ag)   # 0.09955367

ggplot(ag, aes(reorder(p), color = "black", fill = p)) + geom_bar() +
  scale_fill_manual(values = c("#EE2C2C32", "#EE2C2C94", "#EE2C2C02", 
                               "#EE2C2C44", "#EE2C2C10", "#EE2C2C20"),
                    guide = F) +
  theme_minimal(base_size = 16, base_family = "GillSans") +
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm"),
        axis.text.y = element_text(size = 14.75),
        axis.text.x = element_text(size = 14.75),
        legend.position = "none") +
  labs(title = "Distribution of Discretized Prices", 
       x = "", y = "", colour = "", fill = "")

# discretize using arules:
# ag$p <- ag$usd
# ag$p <- discretize(ag$p, method = "cluster", categories = 10)
# levels(ag$p)

# levels(ag$p) <- list("$0-136" = "[    0,  136)", "$136-381" = "[  136,  381)",
#                     "$381-797" = "[  381,  797)", "$797-1507" = "[  797, 1507)",
#                     "$1507-2560" = "[ 1507, 2560)", "$2560-$4100" = "[ 2560, 4100)",
#                     "$4100-6166" = "[ 4100, 6166)" , "$6166-8930" = "[ 6166, 8930)",
#                     "$8930-13131" = "[ 8930,13131)", "$13131-20000" = "[13131,20000]")

# prep - anonymize vendors? ---------------------------------------------------

ag$v2 <- ag$vendor
ag$v2 <- anonymize(ag$v2, .algo = "sha256", .seed = 144, 
                   .chars = letters[seq(from = 1, to = 26)])

nchar(ag$v2[234]) # 64
ag$v3 <- abbreviate(ag$v2, minlength = 6, strict = F, method = "left.kept")
levels(as.factor(ag$v3))

ag$v3 <- factor(ag$v3)
summary(ag$v3)

# plotting by count ref:
# http://stackoverflow.com/questions/20204257/subset-data-frame-based-on-counts-of-items-in-one-variable

# write.csv(ag, file = "~/GitHub/agora-data/06-arules/ag20k.csv", row.names = F)
ag <- fread("~/GitHub/agora-data/06-arules/ag20k.csv", stringsAsFactors = T)

# Transactions Conversion -----------------------------------------------------

# subset variables
ag <- subset(ag, select = c("p", "from", "cat", "sc", "v3"))
colnames(ag) <- c("p", "f", "c", "sc", "v")
head(ag)

ag <- as.data.frame(ag)
# write.csv(ag, file = "~/GitHub/agora-data/06-arules/ag20k-go.csv", row.names = F)

# convert to transactions method 01:

# remove duplicates:
ag.u <- ag[!duplicated(ag), ] # 70703
nrow(ag.u) # 30956
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
  
  itemFrequencyPlot(a2, support = sup[i], cex.names = 0.8, col = "white", horiz = T,
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
# set of 854 itemsets

# Mine Association Rules ------------------------------------------------------

a2rules <- apriori(a2, parameter = list(support = 0.0025, confidence = 0.6,
                                        minlen = 3, maxlen = 5))
a2rules
# set of 79 rules 
summary(a2rules)
arules::inspect(head(a2rules, 20))
arules::inspect(tail(a2rules, 20))
arules::inspect(a2rules)[30:40, ]

# Plot Rules - Group -----------------------------------------------------------
# plot(a2rules, method = "matrix", measure = "lift", main = "207 rules ~ lift")
# plot(a2rules, method = "matrix", measure = c("support", "confidence"))

# individual
plot(a2rules, method = "grouped", control = list(k = 36))

# loop
for (i in 1:10) {
  
  png(filename = paste("~/GitHub/agora-local-market/arules/rule-groups/g1-",i,".jpeg"),
      width = 1800, height = 1400, pointsize = 20, bg = "transparent")
  
  k = i * 12
  
  plot(a2rules, method = "grouped", control = list(k = k))
  
  dev.off()
  
}

# Plot Rules - Graph ----------------------------------------------------------

# get layouts
grep("^layout_", ls("package:igraph"), value = T)[-1]

# define a palette
pdpal <- colorRampPalette(c("#BFEFFF85", "#FFFFFF75", "#00688B85"), alpha = 0.85)
pdpal(100)

r1 <- head(sort(a2rules, by = c("support", "confidence", "lift")), 24)
p1 <- plot(r1, method = "graph", 
           main = "24 rules ~ support + confidence + lift (dh)", 
           edge.color = "#00000025",
           vertex.frame.color = "#00688B85",
           vertex.color = pdpal(100),
           vertex.label.color = "grey8", 
           vertex.label.cex = 0.74, layout = layout_with_dh,
           vertex.label.dist = 0)

r2 <- head(sort(a2rules, by = "lift"), 64)
p2 <- plot(r2, method = "graph", 
           main = "64 rules ~  lift (dh)", 
           edge.color = "#00000025",
           vertex.frame.color = "#00688B85",
           vertex.label.color = "grey2", 
           vertex.color = pdpal(100),
           vertex.label.cex = 0.68, layout = layout_with_dh,
           vertex.label.dist = 0)


# Plot Rules - Graph Loops ----------------------------------------------------

# r1: by Support, Confidence, and Lift ----------------------------------------

for (i in 1:80) {
  
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
       vertex.label.cex = 1, layout = layout_with_dh,
       vertex.label.dist = 0)
  
  dev.off()
}

# r2: by Lift ----------------------------------------------------------------

for (i in 1:80) {
  
  tmp <- head(sort(a2rules, by = "lift"), i)
  
  png(filename = paste("~/GitHub/agora-local-market/arules/igraphs/r2-Lift-",i,".png"),
      width = 1800, height = 1400, pointsize = 18, bg = "transparent")
  
  par(family = "GillSans")
  
  set.seed(256)
  plot(tmp, method = "graph", 
       main = paste(i, "rules ~ lift (dh)"),
       edge.color = "#00000025",
       vertex.frame.color = "#00688B85",
       vertex.color = pdpal(100),
       vertex.label.color = "grey8", 
       vertex.label.cex = 1.2, layout = layout_with_dh,
       vertex.label.dist = 0)
  
  dev.off()
}

# r3: by Support and Confidence -----------------------------------------------

for (i in 1:80) {
  
  tmp <- head(sort(a2rules, by = c("support", "confidence")), i)
  
  png(filename = paste("~/GitHub/agora-local-market/arules/igraphs/r3-SC-",i,".png"),
      width = 1800, height = 1400, pointsize = 20, bg = "transparent")
  
  par(family = "GillSans")
  
  set.seed(64)
  plot(tmp, method = "graph", 
       main = paste(i, "rules ~ support + confidence (dh)"),
       edge.color = "#00000025",
       vertex.frame.color = "#00688B85",
       vertex.color = pdpal(100),
       vertex.label.color = "grey8", 
       vertex.label.cex = 1, layout = layout_with_dh,
       vertex.label.dist = 0)
  
  dev.off()
}

# Subset Rules ----------------------------------------------------------------

weed <- subset(a2rules, lhs %in% "c=Drugs, Cannabis, Concentrates")
inspect(head(weed))
summary(weed)

plot(usa, method = "graph")

# Extract Data Frame ----------------------------------------------------------

a2g <- get.data.frame(ig, "both")
plot(p1, layout=layout_with_fr, vertex.size = 18, vertex.label.cex = 0.55,
     vertex.color = "#00688B80")
