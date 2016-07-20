# Agora Marketplace
# Clustering / Classification
# Market Basket / Association Rules
# Topic Model


# load data -------------------------------------------------------------------

library(data.table)
agora <- fread("data/agora.csv")
# 4,371,382 observations of 12 variables
str(agora)
summary(agora)

max(agora$usd)

# clean data ----------------------------------------------

library(plyr)
library(tidyr)

agora$Date <- as.Date(agora$Date)
summary(agora)
# Dates ranging from 2014-06-28 until 2015-07-12

# remove log transforms
agora$log10.btc <- NULL
agora$log10.usd <- NULL
agora$log10.rate <- NULL

write.table(agora, file = "data/agora.csv", sep = ",", row.names = F)

# replace blank vendor names with NA
agora$vendor_name <- as.factor(agora$vendor_name) # for counts
agora$vendor_name[agora$vendor_name == "" ] <- NA
summary(agora$vendor_name)

# Subset by Date --------------------------------------------------------------

june14 <- subset(agora, grepl("2014-06", agora$Date))
summary(june14$Date)
write.table(june14, file = "data/01-june14.csv", sep = ",", row.names = FALSE)

july14 <- subset(agora, grepl("2014-07", agora$Date))
summary(july14)
nrow(july14) # 305894
write.table(july14, file = "data/02-july14.csv", sep = ",", row.names = FALSE)

august14 <- subset(agora, grepl("2014-08", agora$Date))
summary(august14)
nrow(august14) # 178267
write.table(august14, file = "data/03-aug14.csv", sep = ",", row.names = FALSE)

sept14 <- subset(agora, grepl("2014-09", agora$Date))
summary(sept14)
nrow(sept14) # 311866
write.table(sept14, file = "data/04-sept14.csv", sep = ",", row.names = FALSE)

oct14 <- subset(agora, grepl("2014-10", agora$Date))
summary(oct14)
nrow(oct14) # 232987
write.table(oct14, file = "data/05-oct14.csv", sep = ",", row.names = FALSE)

nov14 <- subset(agora, grepl("2014-11", agora$Date))
summary(nov14)
nrow(oct14) # 232987
write.table(nov14, file = "data/06-nov14.csv", sep = ",", row.names = FALSE)

dec14 <- subset(agora, grepl("2014-12", agora$Date))
summary(dec14)
nrow(dec14) #  445531
write.table(dec14, file = "data/07-dec14.csv", sep = ",", row.names = FALSE)

jan15 <- subset(agora, grepl("2015-01", agora$Date))
summary(jan15) # incomplete month - only up to the 9th
nrow(jan15) # 90561
write.table(jan15, file = "data/08-jan15.csv", sep = ",", row.names = F)

feb15 <- subset(agora, grepl("2015-02", agora$Date))
summary(feb15)
nrow(feb15) # 143577
write.table(feb15, file = "data/09-feb15.csv", sep = ",", row.names = F)

mar15 <- subset(agora, grepl("2015-03", agora$Date))
summary(mar15)
nrow(mar15) # 456198
write.table(mar15, file = "data/10-mar15.csv", sep = ",", row.names = F)

apr15 <- subset(agora, grepl("2015-04", agora$Date))
summary(apr15) # low exchange rate
nrow(apr15) # 440566
write.table(apr15, file = "data/11-apr15.csv", sep = ",", row.names = F)

may15 <- subset(agora, grepl("2015-05", agora$Date))
summary(may15)
nrow(may15) # 345449
write.table(may15, file = "data/12-may15.csv", sep = ",", row.names = F)

june15 <- subset(agora, grepl("2015-06", agora$Date))
summary(june15)
nrow(june15) # 662711
write.table(june15, file = "data/13-june15.csv", sep = ",", row.names = F)

july15 <- subset(agora, grepl("2015-07", agora$Date))
summary(july15) # up to the 12th
nrow(july15) # 218751
write.table(july15, file = "data/14-july15.csv", sep = ",", row.names = F)

# Categorical Subsets ---------------------------------------------------------

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

# ship_from -----------------------------------------------
agora$ship_from <- as.factor(agora$ship_from)
# 117 levels

location <- as.data.frame(table(agora$ship_from))
location <- plyr::rename(location, replace = c("Var1" = "Location",
                                               "Freq" = "NumListings"))

location <- location[order(location$NumListings, decreasing = F), ]
rownames(location) <- NULL

# write.table(location, file = "data/byLocation.csv", sep = ",", row.names = F)

# name (Product 'headlines') ------------------------------

# name and description variables are text based and 
# provide info on what is being sold and in what amount.

length(unique(agora$name))
# 85448 out of 4371382 listings
length(unique(agora$description))
# 67141 out of 4371382 listings

unique(name)
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



