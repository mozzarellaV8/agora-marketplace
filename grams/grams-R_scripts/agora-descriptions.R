# Agora Marketplace
# Exploratory Vis - 'Description' Field
# Descriptions of description

# load data -------------------------------------------------------------------

library(data.table)
agora <- fread("~/GitHub/agora-data/grams/data/agora.csv")
str(agora)

agora$hash <- as.factor(agora$hash)
agora$Date <- as.Date(agora$Date)
agora$vendor_name <- as.factor(agora$vendor_name)
agora$description <- as.factor(agora$description)

# 2014 Listings ---------------------------------------------------------------

a2014 <- subset(agora, agora$Date < "2015-01-01")
# 2013569 obs of 9 variables

length(unique(a2014$name))
# 46419 unique listings

length(unique(a2014$Date))
# 120 days out of a possible 186 days from
# 2014-06-28 until 2014-12-31.

summary(a2014$Date)
#         Min.      1st Qu.       Median         Mean      3rd Qu.         Max. 
# "2014-06-28" "2014-08-31" "2014-10-18" "2014-10-11" "2014-11-25" "2014-12-31"

2013569/120 # 16779.74 - avg all listings over time period
46419/120 # 386 - avg unique listings over time period

summary(as.factor(a2014$name))


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


library(data.table)
description <- fread("~/GitHub/agora-data/data/descriptions.csv")
str(description)

# description field plots -----------------------------------------------------

library(wordcloud)
library(RColorBrewer)
library(extrafont)
library(extrafontdb)
library(Hmisc)
font_import()
fonts()

redpal <- brewer.pal(4, "Reds")
redpal2 <- brewer.pal(6, "Reds")

summary(descriptions$Description)
describe(descriptions)

set.seed(8)
wordcloud(description$descriptions, description$NumListings, min.freq = 500, 
          max.words = 250, scale = c(2, 0.25), random.order = T, 
          random.color = F, color = redpal2)

par(mar = c(0, 0, 0, 0), family = "AveriaSerif-Light")
set.seed(144)
wordcloud(description$descriptions, description$NumListings, min.freq = 1, 
          max.words = 200, scale = c(3, 0.25), random.order = T, 
          random.color = F, color = redpal)

