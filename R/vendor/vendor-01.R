# Agora Marketplace
# vendor page 

# load data -------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(qdap)
library(data.table)
library(ggplot2)

v14 <- fread("data/vendor/vendorall_0114.csv", stringsAsFactors = T) # 8533
str(v14)

# locations - change NA to no info
v14$from <- as.character(v14$from)
v14$from[is.na(v14$from)] <- "no info"
v14$to <- as.character(v14$to)
v14$to[is.na(v14$to)] <- "no info"

# location duplicates and misspellings
v14$from <- gsub("Hong", "Hong Kong", v14$from)
v14$from <- gsub("usa", "USA", v14$from)
v14$from <- gsub("La", "USA", v14$from)
v14$from <- gsub("\\bUnited\\b", "USA", v14$from)
v14$from <- gsub("\\bThe\\b", "USA", v14$from)
v14$from <- gsub("\\bU\\b", "USA", v14$from)
v14$from <- gsub("\\bWest\\b", "USA", v14$from)
v14$from <- gsub("German", "Germany", v14$from)
v14$from <- gsub("my", "Internet", v14$from)
v14$from <- gsub("WORLD", "Worldwide", v14$from)
v14$from <- gsub("World", "Worldwide", v14$from)
v14$from <- gsub("worldwide", "Worldwide", v14$from)
v14$from <- gsub("\\bShipping\\b", "Worldwide", v14$from)
v14$from <- gsub("\\bWorldwidewide\\b", "Worldwide", v14$from)
v14$from <- gsub("undeclared", "Undeclared", v14$from)
v14$from <- gsub("Undelcared", "Undeclared", v14$from)

# get counts of vendor listings
vendors <- as.data.frame(table(v14$vendor))
colnames(vendors) <-c("vendor", "NumListings")

# bind frequency to main frame
v14 <- as.data.frame(v14)
v14 <- dplyr::left_join(v14, vendors, by = "vendor")
v14 <- v14[c(1, 2, 8, 3, 4, 5, 6, 7)]

vp1 <- ggplot(v14, aes(from, vendor, fill = from)) + geom_tile()
vp1

# who has more than one listing?
v14b <- subset(v14, v14$NumListings > 1) # 8517 


