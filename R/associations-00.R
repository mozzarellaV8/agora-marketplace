# Agora Marketplace Analysis
# arules - 2014 data

# load data -------------------------------------------------------------------

library(data.table)

# clear workspace and set directory
getwd()
setwd("~/GitHub/agora-marketplace")

p2014 <- fread("~/GitHub/agora-data/agora-2014.csv", verbose = T, 
               stringsAsFactors = F)

# cleanse further -------------------------------------------------------------

# What do I want? clean url paths, no duplicates in locations, 
# possibly impute categorical data (replace NAs, for transactions)
# Sooner or Later: subset for positive feedbacks (inferring transaction)
# Later: url paths, BTC-dollar ratio.

library(tm)
library(dplyr)
library(tidyr)

p2014$date <- as.Date(p2014$date)
p2014$feedback <- stripWhitespace(p2014$feedback)

# subset for listings with feedback
# feedback can be broken down: 
# 5: great; 5: dubious (check forums for false feedback); 
# 4: good; 3 - average; 0-2 - poor

# 772 > 349601
fb14 <- subset(p2014, p2014$feedback != " Feedbacks: No feedbacks found. " )
# The number of listings drops from 772632 to 349601.

# location data duplicates
levels(as.factor(p2014$from)) #461 messy levels
levels(as.factor(p2014$to)) # 1229 messy levels

levels(as.factor(fb14$from)) # 315 and messy > 174
levels(as.factor(fb14$to)) # 899 and messy


# Cleanse Shipping Origin variables -------------------------------------------

# strip white space, remove 'From'
fb14$from <- gsub("From:", "", fb14$from)
fb14$from <- stripWhitespace(fb14$from)
fb14$from[fb14$from == " "] <- "blank"

# Stripping out the white space collapsed dupliate locations;
# levels drop from 315 to 174
# tolower() might also do this.

# this method will be troublesome on large lists
ag <- grep("Agora", fb14$from) # 1:73, 106, 113
ag[[1]] # 106

for (i in 1:length(ag)) {
  row <- ag[[i]]
  fb14[row, 10] <- "Agora"
}

levels(as.factor(fb14$from)) # 315 > 174 > 171

# this method will be too tedious moving forward
fb14$from[fb14$from == " Australia " ] <- "Australia"
fb14$from[fb14$from == " Australia To: Aus/NZ " ] <- "Australia"
fb14$from[fb14$from == " Australia To: Aus/NZ " ] <- "Australia"
fb14$from[fb14$from == " Australia To: Australia " ] <- "Australia"
fb14$from[fb14$from == " Australia To: Australia only " ] <- "Australia"
fb14$from[fb14$from == " Australia To: australia "] <- "Australia"

levels(as.factor(fb14$from)) # 315>174>171>167>155

fb14$from <- gsub("^\\sCanada\\s", "Canada", fb14$from)
fb14$from <- gsub("Canada(.*)", "Canada", fb14$from)
fb14$from <- gsub("^\\sDenmark(.*)", "Denmark", fb14$from)

levels(as.factor(fb14$from)) # 155>153>150>145>134>133>132

fb14$from <- gsub("^\\sChina(.*)", "China", fb14$from)
fb14$from <- gsub("^\\sEU(.*)", "EU", fb14$from)
fb14$from <- gsub("^\\sGermany(.*)", "Germany", fb14$from)

fb14$from <- gsub("Finland(.*)", "Finland", fb14$from)

fb14$from <- gsub("^\\sHong\\sKong(.*)", "Hong Kong", fb14$from)
fb14$from <- gsub("^\\sIndia(.*)", "India", fb14$from)
fb14$from <- gsub("^\\sInternet(.*)", "Internet", fb14$from)

levels(as.factor(fb14$from)) # 130>129>128>122>121>119

fb14$from <- gsub("^\\sItaly(.*)", "Italy", fb14$from)
fb14$from <- gsub("^\\sLa Jolla(.*)", "La Jolla", fb14$from)
fb14$from <- gsub("^\\smy\\spm(.*)", "my pm", fb14$from)

fb14$from <- gsub("^\\sNetherlands(.*)", "Netherlands", fb14$from)
fb14$from <- gsub("^\\sNorway(.*)", "Norway", fb14$from)
fb14$from <- gsub("^\\sSweden(.*)", "Sweden", fb14$from)

levels(as.factor(fb14$from)) #119>112>108>105>102

fb14$from <- gsub("^\\sThe United Snakes of Captivity(.*)", "The United Snakes of Captivity", fb14$from)
fb14$from <- gsub("^\\sTo:(.*)", "blank", fb14$from)
fb14$from <- gsub("^\\sTo:(.*)", "blank", fb14$from)
fb14$from <- gsub("^\\sTorland(.*)", "Torland", fb14$from)
fb14$from <- gsub("^\\storland(.*)", "Torland", fb14$from)

fb14$from <- gsub("^\\sUndeclared(.*)", "Undeclared", fb14$from)
fb14$from <- gsub("^\\sUndeclared;\\)(.*)", "Undeclared", fb14$from)
fb14$from <- gsub("undeclared(.*)", "Undeclared", fb14$from)
fb14$from <- gsub("Undelcared;\\)(.*)", "Undeclared", fb14$from)

levels(as.factor(fb14$from)) # 101>86

fb14$from <- gsub("^\\sUSA(.*)", "USA", fb14$from)
fb14$from <- gsub("^\\susa(.*)", "USA", fb14$from)
fb14$from <- gsub("^\\sUS(.*)", "USA", fb14$from)
fb14$from <- gsub("United States(.*)", "USA", fb14$from)

fb14$from <- gsub("^\\sWest of the Mississippi(.*)", "West of the Mississippi", fb14$from)
fb14$from <- gsub("^\\sWorld(.*)", "World", fb14$from)

levels(as.factor(fb14$from)) # 86>76>75>74>73>72>68

fb14$from <- gsub("^\\suk(.*)", "UK", fb14$from)
fb14$from <- gsub("^\\sUK(.*)", "UK", fb14$from)
fb14$from <- gsub("^\\suk(.*)", "UK", fb14$from)
fb14$from <- gsub("Untied Kingdom(.*)", "UK", fb14$from)
fb14$from <- gsub("United Kingdom(.*)", "UK", fb14$from)
fb14$from <- gsub("Uk(.*)", "UK", fb14$from)

fb14$from <- gsub("Earth Planet To:", "Earth Planet", fb14$from)

# leading and trailing whitespace
fb14$from <- sub("^\\s+", "", fb14$from)
fb14$from <- sub("\\s+$", "", fb14$from)

levels(as.factor(fb14$from)) # 65 > 64 > 61

# safety
write.csv(fb14, file = "~/GitHub/agora-data/2014-AgFb.csv", row.names = F)
# test <- fread("~/GitHub/agora-data/2014-AgFb.csv", stringsAsFactors = F)

# Cleanse Shipping Destinations -----------------------------------------------

levels(as.factor(fb14$to)) # 899 levels.
# 899>624>505

fb14$to <- stripWhitespace(fb$to)
fb14$to <- tolower(fb14$to)
fb14$to[is.na(fb14$to)] <- "blank"


