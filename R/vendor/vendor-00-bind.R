# Agora Marketplace
# vendor pages
# bind and cleanse

# load and bind data ----------------------------------------------------------

library(data.table)

vDir <- "~/GitHub/agora-data/03-vendor"
setwd(vDir)
vList <- list.files(path = vDir, pattern = ".csv", all.files = T, recursive = T)

v14 <- data.frame()
for(i in 1:length(vList)) {
  temp <- fread(vList[i])
  v14 <- rbind(v14, temp)
}

write.csv(v14, file = "~/GitHub/agora-data/v14-00.csv", row.names = F)

str(v14)
# 'data.frame':	1134680 obs. of  7 variables:
# $ date       : Factor w/ 109 levels "2014-01-01","2014-01-09",..: 1 1 1 1 1 1 1 1 1 1 ...
# $ vendor     : Factor w/ 2245 levels "_drugs.inc_",..: 2 2 2 2 2 2 2 2 2 2 ...
# $ product    : Factor w/ 62402 levels "-INRODUCTION OFFER-  10 x LSD BLOTTER - $40 !!"
# $ description: Factor w/ 75745 levels "-INRODUCTION OFFER- 10 x LSD BLOTTER - $40 !! Hi 
# $ price      : num  0.0599 0.0639 0.0413 0.0785 0.0639 ...
# $ from       : Factor w/ 332 levels "Agora","Australia",..: 32 32 32 32 32 32 32 32 32 32 ...
# $ to         : Factor w/ 1270 levels "Africa","Aus",..: 53 43 53 53 53 53 53 53 53 53 ...

# cleanse some things ---------------------------------------------------------

library(tm)
library(qdap)

v14 <- fread("~/GitHub/agora-data/v14-00.csv")

# locations - change NA to no info
v14$from[is.na(v14$from) == T] <- "NoInfo"
v14$to <- as.character(v14$to)
v14$to[is.na(v14$to) == T] <- "NoInfo"

which(is.na(v14$from) == T)
which(is.na(v14$to) == T)

v14$from <- gsub("\\bBangkok\\b", "Thailand", v14$from)
v14$from <- gsub("\\bMe\\b", "Internet", v14$from)
levels(as.factor(v14$from)) # 58 clean levels
levels(as.factor(v14$to)) # 696 messy levels

write.csv(v14, file = "~/GitHub/agora-data/v14-00b.csv", row.names = F)

# add frequency of listings ---------------------------------------------------

library(dplyr)

v14 <- fread("~/GitHub/agora-data/v14-00b.csv")

# get counts of vendor listings
vendors <- as.data.frame(table(v14$vendor))
colnames(vendors) <-c("vendor", "NumListings")

# who has more than one listing?
vendors <- vendors[order(vendors$NumListings, decreasing = T), ]
rownames(vendors) <- NULL

# bind frequency to main frame
v14 <- as.data.frame(v14)
v14$vendor <- as.factor(v14$vendor)
v14 <- dplyr::left_join(v14, vendors, by = "vendor")
v14 <- v14[c(1, 2, 3, 10, 4, 5, 6, 8, 9, 7)]

write.csv(v14, file = "~/GitHub/agora-data/v14-00c.csv", row.names = F)
# write.csv(vendors, file = "~/GitHub/agora-data/vendor-table-01.csv", row.names = F)


