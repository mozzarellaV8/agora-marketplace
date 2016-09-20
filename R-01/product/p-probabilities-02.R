# Agora Marketplace Analysis
# Class/Category Probabilities 02 

# load data -------------------------------------------------------------------

library(data.table)

p14 <- fread("~/GitHub/agora-data/ag07-2014.csv", stringsAsFactors = T)

# take October 
o14 <- subset(p14, p14$month == "10") # 187091
length(unique(o14$product)) # 25832

# Dr_Mephistopheles is doing some cross posting across categories
# "! How to get a fresh US Identity" in 5 categories:
# Counterfeits, Data, Forgeries, Information, Services


# crawl counts ----------------------------------------------------------------
# generate new crawl counts - previous ones by hand are off.

nrow(p14)
# 1018109 - 2014 population

jan <- subset(p14, p14$month == "01")     # 7986
feb <- subset(p14, p14$month == "02")     # 27300
mar <- subset(p14, p14$month == "03")     # 18807
apr <- subset(p14, p14$month == "04")     # 20000
may <- subset(p14, p14$month == "05")     # 42535
jun <- subset(p14, p14$month == "06")     # 25422
jul <- subset(p14, p14$month == "07")     # 39989
aug <- subset(p14, p14$month == "08")     # 40465
sep <- subset(p14, p14$month == "09")     # 72666
oct <- subset(p14, p14$month == "10")     # 187091
nov <- subset(p14, p14$month == "11")     # 276028
dec <- subset(p14, p14$month == "12")     # 259820

