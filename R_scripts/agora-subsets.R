# Agora Marketplace
# Clustering / Classification
# Market Basket / Association Rules
# Topic Model

# load data -------------------------------------------------------------------

library(data.table)
agora <- fread("~/GitHub/agora-data/data/agora.csv")
# 4,371,382 observations of 12 variables
str(agora)
summary(agora)

max(agora$usd) # 81,084,293

# clean data ----------------------------------------------

library(plyr)
library(tidyr)

agora$Date <- as.Date(agora$Date)
summary(agora)
# Dates ranging from 2014-06-28 until 2015-07-12

# replace blank vendor names with NA
agora$vendor_name <- gsub("\n ", NA, agora$vendor_name)
grep('novartis', agora$name) # check 

agora$vendor_name <- as.factor(agora$vendor_name) # for counts
summary(agora$vendor_name)

# Subset by Date --------------------------------------------------------------

june14 <- subset(agora, grepl("2014-06", agora$Date))
summary(june14$Date)
nrow(june14) # 35490
write.table(june14, file = "~/GitHub/agora-data/data/01-june2014.csv", 
            sep = ",", row.names = FALSE)

july14 <- subset(agora, grepl("2014-07", agora$Date))
summary(july14)
nrow(july14) # 305894
write.table(july14, file = "~/GitHub/agora-data/data/02-july2014.csv", 
            sep = ",", row.names = FALSE)

august14 <- subset(agora, grepl("2014-08", agora$Date))
summary(august14)
nrow(august14) # 178267
write.table(august14, file = "~/GitHub/agora-data/data/03-aug2014.csv", 
            sep = ",", row.names = FALSE)

sept14 <- subset(agora, grepl("2014-09", agora$Date))
summary(sept14)
nrow(sept14) # 311866
write.table(sept14, file = "~/GitHub/agora-data/data/04-sept2014.csv", 
            sep = ",", row.names = FALSE)

oct14 <- subset(agora, grepl("2014-10", agora$Date))
summary(oct14)
nrow(oct14) # 232987
write.table(oct14, file = "~/GitHub/agora-data/data/05-oct2014.csv", 
            sep = ",", row.names = FALSE)

nov14 <- subset(agora, grepl("2014-11", agora$Date))
summary(nov14)
nrow(oct14) # 232987
write.table(nov14, file = "~/GitHub/agora-data/data/06-nov2014.csv", 
            sep = ",", row.names = FALSE)

dec14 <- subset(agora, grepl("2014-12", agora$Date))
summary(dec14)
nrow(dec14) #  445531
write.table(dec14, file = "~/GitHub/agora-data/data/07-dec2014.csv", 
            sep = ",", row.names = FALSE)

jan15 <- subset(agora, grepl("2015-01", agora$Date))
summary(jan15) # incomplete month - only up to the 9th
nrow(jan15) # 90561
write.table(jan15, file = "~/GitHub/agora-data/data/08-jan2015.csv", 
            sep = ",", row.names = F)

feb15 <- subset(agora, grepl("2015-02", agora$Date))
summary(feb15)
nrow(feb15) # 143577
write.table(feb15, file = "~/GitHub/agora-data/data/09-feb2015.csv", 
            sep = ",", row.names = F)

mar15 <- subset(agora, grepl("2015-03", agora$Date))
summary(mar15)
nrow(mar15) # 456198
write.table(mar15, file = "~/GitHub/agora-data/data/10-mar2015.csv", 
            sep = ",", row.names = F)

apr15 <- subset(agora, grepl("2015-04", agora$Date))
summary(apr15) # low exchange rate
nrow(apr15) # 440566
write.table(apr15, file = "~/GitHub/agora-data/data/11-apr2015.csv", 
            sep = ",", row.names = F)

may15 <- subset(agora, grepl("2015-05", agora$Date))
summary(may15)
nrow(may15) # 345449
write.table(may15, file = "~/GitHub/agora-data/data/12-may2015.csv", 
            sep = ",", row.names = F)

june15 <- subset(agora, grepl("2015-06", agora$Date))
summary(june15)
nrow(june15) # 662711
write.table(june15, file = "~/GitHub/agora-data/data/13-june2015.csv", 
            sep = ",", row.names = F)

july15 <- subset(agora, grepl("2015-07", agora$Date))
summary(july15) # up to the 12th
nrow(july15) # 218751
write.table(july15, file = "~/GitHub/agora-data/data/14-july2015.csv", 
            sep = ",", row.names = F)

# Categorical Subsets ---------------------------------------------------------

library(ggplot2)
library(scales)

# Vendors -------------------------------------------------

# data frame for vendor counts by number of listings
vendors <- as.data.frame(table(agora$vendor_name))
vendors <- plyr::rename(vendors, replace = c("Var1" = "Vendor",
                                             "Freq" = "NumListings"))

# reorder vendors data frame by numer of listings
vendors <- vendors[order(vendors$NumListings, decreasing = TRUE), ]
rownames(vendors) <- NULL

# write out csv in case
# write.table(vendors, file = "data/vendors.csv", sep = ",", row.names = FALSE)

vendors10k <- subset(vendors, vendors$NumListings > 10000) 

vendorplot <- ggplot(vendors10k, aes(reorder(Vendor, NumListings),
                                     NumListings, fill = NumListings)) +
  geom_bar(stat = "identity") +
  scale_fill_gradient(low = "antiquewhite2", high = "firebrick4") +
  scale_y_continuous(breaks = c(0, 10000, 25000, 50000, 75000, 100000, 125000),
                                labels = comma) +
  theme_minimal(base_size = 12, base_family = "Arial Rounded MT Bold") +
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  labs(title = "AgMarketplace: vendors w/ over 10k listings",
       x = "", y = "", fill = "number of listings: \n06.28.2014 -\n07.21.2015")

vendorplot

10000/365 # 27.39726

# top 3 vendors by number of listings; daily average
132315/365 # 362.5068, 'captainkirk'
108135/365 # 296.2603, 'fake'
103014/365 # 282.2301, 'rc4me'

# ship_from -------------------------------------------------------------------
agora$ship_from <- as.factor(agora$ship_from)
# 117 levels

location <- as.data.frame(table(agora$ship_from))
location <- plyr::rename(location, replace = c("Var1" = "Location",
                                               "Freq" = "NumListings"))

location <- location[order(location$NumListings, decreasing = T), ]
rownames(location) <- NULL

# write.table(location, file = "data/byLocation.csv", sep = ",", row.names = F)

locationplot <- ggplot(location, aes(reorder(Location, NumListings),
                                     NumListings, fill = NumListings)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_gradient(low = "antiquewhite2", high = "firebrick4") +
  scale_y_continuous(labels = comma) +
  theme_minimal(base_size = 12, base_family = "Verdana") +
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
  scale_fill_gradient(low = "antiquewhite2", high = "firebrick4", 
                      labels = comma) +
  scale_y_continuous(labels = comma) +
  theme_minimal(base_size = 12, base_family = "Verdana") +
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
  theme_minimal(base_size = 12, base_family = "Verdana") +
  theme(plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 8)) +
  labs(title = "AgMarketplace: locations shipped from, under 10k listings",
       x = "", y = "", fill = "# of listings")

locplot2

# name (Product 'headlines') ------------------------------

# name and description variables are text based and 
# provide info on what is being sold and in what amount.

length(unique(agora$name))
# 85448 out of 4371382 listings
length(unique(agora$description))
# 67141 out of 4371382 listings

uniquenames <- as.data.frame(table(agora$name))
colnames(uniquenames) <- c("name", "frequency")
# remove blank names
uniquenames$name[uniquenames$name == ""] <- NA
uniquenames <- na.omit(uniquenames)

uniquenames <- uniquenames[order(uniquenames$frequency, decreasing = TRUE), ]
rownames(uniquenames) <- NULL

unique(agora$name)
unique(description)

agora$name <- as.factor(agora$name)
products <- as.data.frame(table(agora$name))
products <- rename(products, replace = c("Var1" = "Product", 
                                         "Freq" = "NumListings"))

products <- products[order(products$NumListings, decreasing = T), ]
rownames(products) <- NULL

write.table(products, file = "data/products.csv", sep = ",", row.names = F)

# Description (of product) --------------------------------

agora$description <- as.factor(agora$description)
descriptions <- as.data.frame(table(agora$description))
colnames(descriptions) <- c("Description", "NumListings")
descriptions <- descriptions[order(descriptions$NumListings, decreasing = T), ]
rownames(descriptions) <- NULL

write.table(descriptions, file = "descriptions.csv", sep = ",", row.names = F)

# DF of names+descriptions

agoratexts <- subset(agora, select = c(agora$name, agora$description))



