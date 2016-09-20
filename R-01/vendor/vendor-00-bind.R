# Agora Marketplace
# vendor pages
# bind and cleanse

# load and bind data ----------------------------------------------------------

library(data.table)
library(tm)
library(qdap)
library(dplyr)
library(tidyr)

vDir <- "~/GitHub/agora-data/03-vendor"
setwd(vDir)
vList <- list.files(path = vDir, pattern = ".csv", all.files = T, recursive = T)

v14 <- data.frame()
for(i in 1:length(vList)) {
  temp <- fread(vList[i])
  v14 <- rbind(v14, temp)
}

write.csv(v14, file = "~/GitHub/agora-data/v14-01.csv", row.names = F)

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
# v14 <- fread("~/GitHub/agora-data/v14-00.csv")

# locations
levels(as.factor(v14$to))
levels(as.factor(v14$from))

v14$from <- gsub("\\bBangkok\\b", "Thailand", v14$from)
v14$from <- gsub("\\bNoInfo\\b", "No Info", v14$from)
v14$from <- gsub("\\bMoldova,\\sRepublic\\sof\\b", "Moldova", v14$from)
v14$from <- gsub("\\bEuropean\\sUnion\\b", "EU", v14$from)
v14$from <- gsub("\\bGerman\\b", "Germany", v14$from)
v14$from <- gsub("^The\\sUnited\\sSnakes(.*)", "USA", v14$from)

# v14$from <- gsub("\\bMe\\b", "Internet", v14$from)
levels(as.factor(v14$from))   # 73 clean levels
levels(as.factor(v14$to))     # 990 messy levels

write.csv(v14, file = "~/GitHub/agora-data/v14-01b.csv", row.names = F)

# add frequency of listings ---------------------------------------------------
# v14 <- fread("~/GitHub/agora-data/v14-01b.csv")

# get counts of vendor listings
vendors <- as.data.frame(table(v14$vendor))
colnames(vendors) <-c("vendor", "NumListings")

# who has more than one listing?
vendors <- vendors[order(vendors$NumListings, decreasing = T), ]
rownames(vendors) <- NULL

mean(vendors$NumListings) # 502.882

# bind frequency to main frame
v14 <- as.data.frame(v14)
v14$vendor <- as.factor(v14$vendor)
v14 <- dplyr::left_join(v14, vendors, by = "vendor")
v14 <- v14[c(1, 2, 3, 10, 4, 5, 6, 8, 9, 7)]

v14$fulldate <- v14$date
v14 <- separate(v14, fulldate, into = c("year", "month", "day"), sepp  = "-")

write.csv(v14, file = "~/GitHub/agora-data/v14-02.csv", row.names = F)
# write.csv(vendors, file = "~/GitHub/agora-data/vendor-table-01.csv", row.names = F)
