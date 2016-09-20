# Agora Marketplace Analysis
# 2015 data exploration
# vendor aggregation
# distributions

# load  -----------------------------------------------------------------------

library(data.table)
library(ggplot2)
library(dplyr)
library(qdap)
library(zoo)
library(beeswarm)
library(extrafont)
library(corrplot)
library(igraph)

v15 <- fread("~/GitHub/agora-data/v15-arules-00.csv")

# decompose products ----------------------------------------------------------

# subset under 1k
v15k <- subset(v15, v15$usd <= 1000)
v <- unique(v15$product)

# all words
vf <- subset(v15k, select = c("vendor", "product", "allcat", "from"))
vff <- all_words(vf$product, alphabetical = F)    # 15641
rownames(vff) <- NULL

# word freq top 500
vft <- freq_terms(vf$product, top = 500, stopwords = Top200Words)
rownames(vft) <- NULL

vft <-vft[-4, ]
vftt <- vft[1:4, ]
vftt <- vftt$WORD

# word freq data frame by location --------------------------------------------

vsdf <- with(vf, wfdf(product, from, stopwords = Top200Words))

vsdf2 <- subset(vsdf, select = c("Words", "Agora", "Australia", "Canada", "China", "Denmark", 
                                 "England", "EU", "France", "Germany", "Hong Kong", "India",
                                 "Italy", "Japan", "Netherlands", "Norway",
                                 "Pakistan", "Poland", "Singapore", "Switzerland",
                                 "Sweden", "South Africa", "Torland", "Turkey",
                                 "UK", "USA"))

vsdf3 <- subset(vsdf, select = c("Words", "Australia", "China", "Germany", "Netherlands",
                                 "UK", "USA"))

vsdf3 <- vsdf3[order(vsdf3$USA, decreasing = T), ]
rownames(vsdf3) <- NULL

vsdf4 <- vsdf3[1:100, ]
vsdf4b <- vsdf3[1:50, ]
vsdf4c <- vsdf3[1:30, ]

# correlation plot ------------------------------------------------------------

vsdf5 <- as.wfm(vsdf4)
v5cor <- cor(t(vsdf5))

vsdf5b <- as.wfm(vsdf4b)
v5c2 <- cor(t(vsdf5b))

vsdf5c <- as.wfm(vsdf4c)
v5c3 <- cor(t(vsdf5c))

par(family = "GillSans")
corrplot(v5cor, method = "circle", order = "alphabet", mar = c(2, 2, 8, 2),
         tl.col = "black", tl.srt = 90, tl.cex = 0.65, tl.offset = 1,
         cl.ratio = 0.2, cl.align = "l")

corrplot(v5c2, method = "circle", order = "FPC", mar = c(2, 2, 8, 2), type = "upper",
         tl.col = "black", tl.srt = 45, tl.cex = 0.8, tl.offset = 1,
         cl.ratio = 0.2, cl.align = "l")

corrplot(v5c2, method = "circle", order = "AOE", mar = c(2, 4, 8, 2),
         tl.col = "black", tl.srt = 45, tl.cex = 0.8, tl.offset = 1,
         cl.ratio=0.2, cl.align="r")

par(oma = c(0, 0, 2, 0))
corrplot(v5c3, method = "circle", order = "alphabet", mar = c(0, 2, 8, 0),
         tl.col = "black", tl.srt = 45, tl.cex = 0.85, tl.offset = 1)


# qheat -------------------------------------
par(mar = c(6, 6, 6, 6), family = "GillSans")
qheat(v5c3, low = "white", high = "firebrick4", text.size = 4,
      grid = "white", diag.na = F, by.column = NULL)

qheat(v5c2, low = "white", high = "deeppink4", text.size = 4,
      grid = "white", diag.na = F, by.column = NULL)

qheat(v5cor, low = "white", high = "deepskyblue4", text.size = 4,
      grid = "white", diag.na = F, by.column = NULL)


# word network -----------------------------
word_network_plot(vsdf4b$Words)
