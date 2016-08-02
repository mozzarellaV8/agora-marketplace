# Agora Marketplace
# Subset Script
# Since the dataset is so large, 
# this script is for writing out separate dataframes
# of categorical data for specific analyses/decompositions.

# load data -------------------------------------------------------------------

library(data.table)
agora <- fread("~/GitHub/agora-data/data/agora.csv")
str(agora)

agora$hash <- as.factor(agora$hash)
agora$Date <- as.Date(agora$Date)
agora$vendor_name <- as.factor(agora$vendor_name)
agora$description <- as.factor(agora$description)

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

# 'Description' ---------------------------------------------------------------

# name and description variables are text based and 
# provide info on the product, service, or offer within the listing.
# 'description' provides more detailed information, where 'name' functions
# as a 'headline' for the listing.

length(unique(agora$description))
# 67141 out of 4371382 listings

length(table(agora$description))
descriptions <- as.data.frame(table(agora$description))
colnames(descriptions) <- c("Description", "NumListings")

# remove blanks
descriptions$Description[descriptions$Description == ""] <- NA
descriptions <- na.omit(descriptions)

# order by frequency
descriptions <- descriptions[order(descriptions$NumListings, decreasing = T), ]
rownames(descriptions) <- NULL

write.table(descriptions, file = "~/GitHub/agora-data/data/descriptions.csv", 
            sep = ",", row.names = F)

# Vendor_Name -----------------------------------------------------------------

# data frame for vendor counts by number of listings
vendors <- as.data.frame(table(agora$vendor_name))
vendors <- plyr::rename(vendors, replace = c("Var1" = "Vendor",
                                             "Freq" = "NumListings"))

# reorder vendors data frame by numer of listings
vendors <- vendors[order(vendors$NumListings, decreasing = TRUE), ]
rownames(vendors) <- NULL

# write out csv in case
# write.table(vendors, file = "data/vendors.csv", sep = ",", row.names = FALSE)