# Agora Marketplace
# product variable cleanse + subset

# load data -------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(qdap)
library(data.table)
library(ggplot2)

p14 <- fread("~/GitHub/agora-data/ag05-2014.csv", stringsAsFactors = F)
str(p14)

# check
# grep("wideutschland", p14$to)
# grep("deutschlandstination", p14$to)
# grep("swedeutschlandn", p14$to)
# grep("outsideutschland", p14$to)

write.csv(p14, file = "~/GitHub/agora-data/ag05-2014.csv", row.names = F)

# Subset by Time Interval -----------------------------------------------------

length(unique(p14$date)) # 139

p14$listdate <- p14$date
p14 <- separate(p14, listdate, into = c("year", "month", "day"), sep = "-")

mDist <- as.data.frame(table(p14$month))
colnames(mDist) <- c("month", "numListings")
dDist <- as.data.frame(table(p14$day))
colnames(dDist) <- c("day", "numListings")

par(mar = c(5, 5, 5, 5), mfrow = c(1, 2), bty = "l", family = "HersheySans")
plot(mDist$month, mDist$numListings, 
     main = "number of listings by month - Agora 2014")
plot(dDist$day, dDist$numListings, 
     main = "number of listings by day - Agora 2014")

# p14 <- as.data.frame(p14)
# mDist$month <- as.character(mDist$month)
# p14$monthlyTotal <- left_join(p14, mDist, by = "month")

# append with feedback variable -----------------------------------------------

p14$great <- grepl("^\\sFeedbacks: 5/5(.*)", p14$feedback)
p14$good <- grepl("^\\sFeedbacks: 4/5(.*)", p14$feedback)
p14$ok <- grepl("^\\sFeedbacks: 3/5(.*)", p14$feedback)
p14$poor <- grepl("^\\sFeedbacks: 2/5(.*)", p14$feedback)
p14$horrible <- grepl("^\\sFeedbacks: 1/5(.*)", p14$feedback)
p14$worst <- grepl("^\\sFeedbacks: 0/5(.*)", p14$feedback)
p14$none <- grepl("^\\sFeedbacks: No feedbacks found.(.*)", p14$feedback)

# subset only feedback ratings
feedback <- subset(p14, select = c(list, vendor, great, good, ok, poor,
                                   horrible, worst, none))

length(unique(feedback$vendor)) # 2284
fbUnique <- subset(feedback, select = c(list, unique(feedback$vendor), great, good,
                                        ok, poor, horrible, worst, none))

# random sample and plot
fSample <- sample(feedback$vendor, size = 10000, replace = F)

# write.csv(feedback, file = "~/GitHub/agora-data/feedback-matrix.csv", row.names = F)

# subset by list word/quantity ------------------------------------------------

# to double check on quanities:
prQ <- fread("~/GitHub/agora-data/02-lexical/qdapProductWF.csv")


cust <- grepl("custom", p14$product, ignore.case = T)
customs <- subset(p14, cust == T, ignore.case = T) # 29545
# write.csv(customs, file = "~/GitHub/agora-data/plex-customs.csv", row.names = F)

# product weights by number
oneH <- grepl("\\b100g\\b", p14$product, ignore.case = T)
g100 <- subset(p14, oneH == T, ignore.case = T) # 14287
oneT <- grepl("\\b1000g\\b", p14$product, ignore.case = T)
g1000 <- subset(p14, oneT == T, ignore.case = T) # 5252
fiftyG <- grepl("\\b50g\\b", p14$product, ignore.case = T)
g50 <- subset(p14, fiftyG == T, ignore.case = T) # 10686
twoFiftyG <- grepl("\\b250g\\b", p14$product, ignore.case = T)
g250 <- subset(p14, twoFiftyG == T, ignore.case = T) # 4928
oneK <- grepl("\\b1kg\\b", p14$product, ignore.case = T)
kg1 <- subset(p14, oneK == T, ignore.case = T) # 3741

# product weights by id
milligrams <- grepl("kg", p14$product, ignore.case = T)
mg <- subset(p14, milligrams = T, ignore.case = T)

# multipliers
oneX <- grepl("100x", p14$product, ignore.case = T)
x100 <- subset(p14, oneX == T, ignore.case = T) # 5551
oneT <- grepl("\\b1000x\\b", p14$product, ignore.case = T)
x1000 <- subset(p14, oneT == T, ignore.case = T) # 2064




