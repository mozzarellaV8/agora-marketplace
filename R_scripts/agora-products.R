# Agora Marketplace
# Exploratory Vis - Names field
# Product listing headlines

# load data -------------------------------------------------------------------

library(data.table)

# agora <- fread("~/GitHub/agora-data/data/agora.csv)
products <- read.csv("~/GitHub/agora-data/data/products.csv")

# name (Product 'headlines') ------------------------------

# name and description variables are text based and 
# provide info on what is being sold and in what amount.

length(unique(agora$name))
# 85448 out of 4371382 listings
length(unique(agora$description))
# 67141 out of 4371382 listings

agora$name <- as.factor(agora$name)
products <- as.data.frame(table(agora$name))
colnames(products) <- c("Product", "NumListings")

# remove blanks
products$Product[products$Product == ""] <- NA
products <- na.omit(products)

products <- products[order(products$NumListings, decreasing = T), ]
rownames(products) <- NULL

write.table(products, file = "~/GitHub/agora-data/data/products.csv", 
            sep = ",", row.names = F)

book <- grep("book", products$Product)        # 781 listings contain 'book'
molly <- grep("mdma", products$Product)       # 7316 contain 'mdma'
molly2 <- grep("xtc", products$Product)       # 2323 contain 'xtc'
molly3 <- grep("molly", products$Product)     # 280 contain 'molly'
molly4 <- grep("moon rock", products$Product) # 47 contain 'moon rock'

# Name (Product) plots --------------------------------------------------------

library(wordcloud)
library(RColorBrewer)
library(extrafont)
library(extrafontdb)
font_import()
fonts()

redpal <- brewer.pal(4, "Reds")
redpal2 <- brewer.pal(6, "Reds")

# ok
par(mar = c(0, 0, 0, 0), family = "Arial Rounded MT Bold")
wordcloud(products$Product, products$NumListings, min.freq = 500, 
          max.words = 200, scale = c(4, 0.25), random.order = T, 
          random.color = F, color = redpal)

# ok
set.seed(8)
wordcloud(products$Product, products$NumListings, min.freq = 500, 
          max.words = 250, scale = c(2, 0.25), random.order = T, 
          random.color = F, color = redpal)

# good
set.seed(8)
wordcloud(products$Product, products$NumListings, min.freq = 500, 
          max.words = 250, scale = c(2, 0.25), random.order = T, 
          random.color = F, color = redpal2)

# ok
par(mar = c(0, 0, 0, 0), family = "Verdana")
set.seed(64)
wordcloud(products$Product, products$NumListings, min.freq = 1, 
          max.words = 250, scale = c(2.5, 0.25), random.order = T, 
          random.color = F, color = redpal)

# good
par(mar = c(0, 0, 0, 0), family = "AveriaSerif-Light")
set.seed(144)
wordcloud(products$Product, products$NumListings, min.freq = 1, 
          max.words = 250, scale = c(2.25, 0.25), random.order = T, 
          random.color = F, color = redpal)

# good
par(mar = c(0, 0, 0, 0), family = "AveriaSerif-Light")
set.seed(144)
wordcloud(products$Product, products$NumListings, min.freq = 1, 
          max.words = 200, scale = c(3, 0.25), random.order = T, 
          random.color = F, color = redpal)

