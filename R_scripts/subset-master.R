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

##
# Subset Variables  -----------------------------------------------------------
##

# Given 4371382 observations across 9 variables, what follows is subsetting
# each variable from the dataset in order to look at some basic summary and 
# exploratory statistics. Getting a sense of the data this way. 

# Not every subset will likely be necessary, but will carry out this process
# nonetheless for completeness.

# Numeric Variables ------------------------------------------------------------

# for these variables, will be looking at btc, usd, and rate.

# 'usd' summary ---------------------------------------------------------------

# list prices for products, servcies in USD

summary(agora$usd)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#       0       24       89     4159      314 81080000
quantile(agora$usd)
#            0%          25%          50%          75%         100% 
# $2.206400e-05 2.422576e+01 8.851539e+01 3.145000e+02 8.108429e+07 

length(max(agora$usd)) # [1] 81084293
which(agora$usd == 81084293) # row 406257
agora[406257, ] # hbo account - likely a placeholder

# the question comes to mind - how much is one person willing to spend on a
# darknet market transaction at a time? is 1500 too much? It certainly doesn't 
# reduce the dataset size by much at all. 

avg1500 <- subset(agora, agora$usd <= 1500) # 4037383; seems a lot of listings
summary(avg1500$usd)
summary(avg1500$btc)

avg500 <- subset(agora, agora$usd <= 500) # 3572666 listings at 500 usd and below.
nrow(avg500)
summary(avg500)

avg100 <- subset(agora, agora$usd <= 100)
summary(avg100$usd)
#     Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0.00002   7.46700  26.83000  34.07000  56.30000 100.00000 

hist(avg100$usd)
hist(avg500$usd, breaks = 60)


# Date Variable ----------------------------------------------------------------

# the following subsets the entire dataset by month, for in-depth looks at each

# 'date'  ----------------------------------------------------------------------

# subsetting the entire dataset by month

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


##
# Factor Variables ------------------------------------------------------------
##


# The following subsets are for hash, ship_from, vendor_name, name, and description.

# 'hash'  ---------------------------------------------------------------------

length(unique(agora$hash))
# 82200 of 4371382
hash <- as.data.frame(table(agora$hash))
colnames(hash) <- c("hash", "freq")
summary(hash)


# 'name' ----------------------------------------------------------------------

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

# street name string search - curiousity
book <- grep("book", products$Product) # 781 listings contain 'book'
molly <- grep("mdma", products$Product) # 7316 contain 'mdma'
molly2 <- grep("xtc", products$Product) # 2323 contain 'xtc'
molly3 <- grep("molly", products$Product) # 280 contain 'molly'
molly4 <- grep("moon rock", products$Product) # 47 contain 'moon rock'

# 'description' ---------------------------------------------------------------

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

# 'vendor_name' -----------------------------------------------------------------

# data frame for vendor counts by number of listings
vendors <- as.data.frame(table(agora$vendor_name))
vendors <- plyr::rename(vendors, replace = c("Var1" = "Vendor",
                                             "Freq" = "NumListings"))

# reorder vendors data frame by numer of listings
vendors <- vendors[order(vendors$NumListings, decreasing = TRUE), ]
rownames(vendors) <- NULL

# write out csv in case
write.table(vendors, file = "data/vendors.csv", sep = ",", row.names = FALSE)


# 'ship_from' ------------------------------------------------------------------

# where products, services, offers purport to ship from
# usually a country - sometimes anonymous

agora$ship_from <- as.factor(agora$ship_from)
# 117 levels

location <- as.data.frame(table(agora$ship_from))
colnames(location) <- c("shipFrom", "NumListings")

# order by frequency/number of listings - option
location <- location[order(location$NumListings, decreasing = T), ]
rownames(location) <- NULL

write.table(location, file = "data/byLocation.csv", sep = ",", row.names = F)


