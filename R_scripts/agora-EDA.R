# Agora Marketplace
# Exploratory Vis

# load ------------------------------------------------------------------------

library(data.table)
library(ggplot2)
library(scales)
library(extrafont)
library(extrafontdb)

vendors <- fread("~/GitHub/agora-data/data/vendors.csv")
location <- fread("~/GitHub/agora-data/data/location.csv")
# all data:
# agora <- fread("~/GitHub/agora-data/data/agora.csv")

# explore ---------------------------------------------------------------------

fonts()
font_import()

vendors10k <- subset(vendors, vendors$NumListings > 10000) 

# good
vendorplot <- ggplot(vendors10k, aes(reorder(Vendor, NumListings),
                                     NumListings, fill = NumListings)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "antiquewhite2", high = "firebrick4") +
  scale_y_continuous(breaks = c(0, 10000, 25000, 50000, 75000, 100000, 125000)) +
  theme_minimal(base_size = 12, base_family = "AveriaSerif-Light") +
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  labs(title = "AgMarketplace: vendors w/ over 10k listings",
       x = "", y = "", fill = "number of listings: \n06.28.2014 -\n-07.21.2015")

vendorplot

# subset the top vendors
topvendors <- subset(vendors, vendors$NumListings > 25000)

# ok
vendorplot01 <- ggplot(topvendors, aes(x = reorder(Vendor, NumListings),
                                       y = NumListings, color = NumListings)) +
  geom_point(size = 6) + 
  theme_minimal(base_size = 12, base_family = "AveriaSerif-Light") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm")) +
  labs(title = "Agora Marketplace: Top Vendors by Number of Listings",
       x = "", y = "number of listings")

vendorplot01

# by Location -----------------------------------------------------------------

locationplot <- ggplot(location, aes(reorder(Location, NumListings),
                                     NumListings, fill = NumListings)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_gradient(low = "antiquewhite2", high = "firebrick4") +
  scale_y_continuous() +
  theme_minimal(base_size = 12, base_family = "AveriaSerif-Light") +
  theme(plot.margin = unit(c(1, 1.25, 2, 1), "cm")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 7)) +
  labs(title = "AgMarketplace: locations shipped from by number of listings",
       x = "", y = "", fill = "# of listings")

locationplot

location10k <- subset(location, location$NumListings >= 10000)
location10k2 <- subset(location, location$NumListings < 10000)

locplot <- ggplot(location10k, aes(reorder(Location, NumListings),
                                   NumListings, fill = NumListings)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_gradient(low = "antiquewhite2", high = "firebrick4") +
  scale_y_continuous(labels = comma) +
  theme_minimal(base_size = 12, base_family = "AveriaSerif-Light") +
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 10)) +
  labs(title = "AgMarketplace: locations shipped from, over 10k listings",
       x = "", y = "", fill = "# of listings")

locplot

locplot2 <- ggplot(location10k2, aes(reorder(Location, NumListings),
                                     NumListings, fill = NumListings)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_gradient(low = "antiquewhite2", high = "firebrick4", 
                      labels = comma) +
  scale_y_continuous(labels = comma) +
  theme_minimal(base_size = 12, base_family = "AveriaSerif-Light") +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  labs(title = "AgMarketplace: locations shipped from, under 10k listings",
       x = "", y = "", fill = "# of listings")

locplot2


# ok plots ------------------------------------------------
locationplot01 <- ggplot(location, aes(x = reorder(Location, NumListings),
                                       y = NumListings, colour = NumListings)) +
  geom_point(size = 4) +
  theme_minimal(base_size = 12, base_family = "Arial Rounded MT Bold") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm")) +
  labs(title = "Agora Marketplace: Locations Shipped From",
       x = "", y = "number of listings")

locationplot01

# subset locations to number of listings over 100
toplocations <- subset(location, location$NumListings > 100)

# ok
locationplot02 <- ggplot(toplocations, aes(x = reorder(Location, NumListings),
                                           y = NumListings, color = NumListings)) +
  geom_point(size = 6) +
  theme_minimal(base_size = 12, base_family = "AveriaSerif-Light") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm")) +
  labs(title = "Agora Marketplace: Locations Shipped From",
       x = "", y = "number of listings")

locationplot02

locationplot03 <- ggplot(toplocations, aes(x = NumListings, y = Location, 
                                           color = NumListings)) +
  geom_point(size = 6) +
  theme_minimal(base_size = 12, base_family = "Arial Rounded MT Bold") +
  theme(axis.text.x = element_text(angle = 0)) +
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm")) +
  labs(title = "Agora Marketplace: Locations Shipped From",
       x = "# of listings", y = "location shipped from") 

locationplot03


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

book <- grep("book", products$Product)
# 781 listings contain 'book'

molly <- grep("mdma", products$Product)
# 7316 contain 'mdma'

molly2 <- grep("xtc", products$Product)
# 2323 contain 'xtc'

molly3 <- grep("molly", products$Product)
# 280 contain 'molly'

molly4 <- grep("moon rock", products$Product)
# 47 contain 'moon rock'

# Name (Product) plots --------------------------------------------------------

library(wordcloud)
library(RColorBrewer)

redpal <- brewer.pal(7, "Reds")

par(mar = c(0, 0, 0, 0), family = "Arial Rounded MT Bold")
wordcloud(products$Product, products$NumListings, min.freq = 500, 
          max.words = 200, scale = c(4, 0.25), random.order = T, 
          random.color = F, color = redpal)

set.seed(8)
wordcloud(products$Product, products$NumListings, min.freq = 500, 
          max.words = 250, scale = c(2, 0.25), random.order = T, 
          random.color = F, color = redpal)


# Description (of product) --------------------------------

agora$description <- as.factor(agora$description)
descriptions <- as.data.frame(table(agora$description))
colnames(descriptions) <- c("Description", "NumListings")
descriptions <- descriptions[order(descriptions$NumListings, decreasing = T), ]
rownames(descriptions) <- NULL

write.table(descriptions, file = "~/GitHub/agora-data/descriptions.csv", 
            sep = ",", row.names = F)

descriptions[1, ]
# 1 this is wakeside917 formerly of sr now proud to be a part of the new site. 
# i 39 ve kept my pricing and shipping the same as before. 
# the pricing displayed is for high speed direct download links and is s ...
descriptions[2, ]
# this shipment is with tracking number. 
# the shipment must be signed to receive it. 
# in case that the track is stuck i offer 50 reship.
descriptions[3, ]
# we don 39 t have that many sales on agora because 
# all of our permanent customers are purchasing directly 
# from us via our clearnet website www.dutyfree.io 
# you are welcome to do so as it 39 s much cheape ...


# DF of names+descriptions ----------------------------------------------------
agoratexts <- subset(agora, select = c(agora$name, agora$description))
