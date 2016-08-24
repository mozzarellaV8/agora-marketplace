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

fb14 <- fread("~/GitHub/agora-data/2014-AgFb.csv", stringsAsFactors = F)
levels(as.factor(fb14$to)) # 899 levels.
# 899>624>505>468

fb14$to <- stripWhitespace(fb14$to)
fb14$to <- tolower(fb14$to)
fb14$to[is.na(fb14$to)] <- "blank"

# fb14$to <- gsub("\\s", "", fb14$to)
fb14$to <- gsub("^\\s\'merica(.*)", "\'merica", fb14$to)
fb14$to <- gsub("\'merica(.*)", "\'merica", fb14$to)
fb14$to <- gsub("\"\"\"\"fcuk..your couch\"\"\"\" ", "fcuk..your couch", fb14$to)
fb14$to <- gsub("\"\"\"\"fcuk..your couch\"\"\"\"-rick james ", "fcuk..your couch", fb14$to)

# maybe merge these 
fb14$to <- gsub("all(.*)", "all", fb14$to)
fb14$to <- gsub("all(.*)", "all", fb14$to)
fb14$to <- gsub("^anywhere(.*)", "anywhere", fb14$to)
fb14$to <- gsub("americana ", "USA", fb14$to)

fb14$to <- gsub("any except Australiatralia ", "anywhere", fb14$to)

fb14$to <- gsub("^australia", "Australia", fb14$to)
fb14$to <- gsub("Australia only ", "Australia", fb14$to)
fb14$to <- gsub("austaliaonly", "Australia", fb14$to)
fb14$to <- gsub("aus", "Australia", fb14$to)
fb14$to <- gsub("Australia ", "Australia", fb14$to)
fb14$to <- gsub("Australians only ", "Australia", fb14$to)
fb14$to <- gsub("Australiatalia only ", "Australia", fb14$to)

fb14$to <- gsub("Australia,newzealand", "Australia and New Zealand", fb14$to)
fb14$to <- gsub("Australia/new zealand ", "Australia and New Zealand", fb14$to)
fb14$to <- gsub("Australia/nz", "Australia and New Zealand", fb14$to)
fb14$to <- gsub("Australia & newzealand", "Australia and New Zealand", fb14$to)
fb14$to <- gsub("Australia and nz ", "Australia and New Zealand", fb14$to)
fb14$to <- gsub("Australia, new zealand ", "Australia and New Zealand", fb14$to)
fb14$to <- gsub("Australia/worldwide ", "Australia and New Zealand", fb14$to)
                                                                                
fb14$to <- gsub("^can(.*)", "Canada/USA", fb14$to)
fb14$to <- gsub("^de(.*)", "Germany", fb14$to)

fb14$to <- gsub("^de(.*)", "Germany", fb14$to)

fb14$to <- gsub("^europe(.*)", "Europe", fb14$to)
fb14$to <- gsub("^eu", "EU", fb14$to)
fb14$to <- gsub("^EU(.*)", "EU", fb14$to)

fb14$to <- gsub("^everywhere(.*)", "Everywhere", fb14$to)
fb14$to <- gsub("^every where(.*)", "Everywhere", fb14$to)
fb14$to <- gsub("^everyplace(.*)", "Everywhere", fb14$to)

fb14$to <- gsub("^france(.*)", "France", fb14$to)
fb14$to <- gsub("^fr", "France", fb14$to)
fb14$to <- gsub("France\\s", "France", fb14$to)

fb14$to <- gsub("gentle hearts and kind people ", "Kind Hearts and Gentle People", fb14$to)
fb14$to <- gsub("kind and gentle people ", "Kind Hearts and Gentle People", fb14$to)

fb14$to <- gsub("^worldwide except(.*)", "Worldwide, except...", fb14$to)
fb14$to <- gsub("^worldwide\\s\\(except(.*)", "Worldwide, except...", fb14$to)
fb14$to <- gsub("^worldwide\\s\\(e(.*)", "Worldwide, except...", fb14$to)

fb14$to <- gsub("^worldwide,(.*)", "Worldwide, except...", fb14$to)
fb14$to <- gsub("^worldwide-(.*)", "Worldwide, except...", fb14$to)
fb14$to <- gsub("^worldwide\\s-\\s(.*)", "Worldwide, except...", fb14$to)

fb14$to <- gsub("^worldwide\\s-(.*)", "Worldwide, except...", fb14$to)
fb14$to <- gsub("^world\\s-(.*)", "Worldwide, except...", fb14$to)
fb14$to <- gsub("^world\\sexcept(.*)", "Worldwide, except...", fb14$to)

fb14$to <- gsub("^world\\swide\\sexcept(.*)", "Worldwide, except...", fb14$to)
fb14$to <- gsub("^world\\sno(.*)", "Worldwide, except...", fb14$to)
fb14$to <- gsub("^world\\sno(.*)", "Worldwide, except...", fb14$to)

fb14$to <- gsub("^wordwide(.*)", "Worldwide, except...", fb14$to)
fb14$to <- gsub("^ww except(.*)", "Worldwide, except...", fb14$to)


fb14$to <- gsub("^worldwide\\s[:A-z:](.*)", "Worldwide", fb14$to)
fb14$to <- gsub("worlwide\\s(.*)", "Worldwide", fb14$to)
fb14$to <- gsub("world(.*)", "Worldwide", fb14$to)
fb14$to <- gsub("^ww|wrld", "Worldwide", fb14$to)


fb14$to <- gsub("^ger", "Germany", fb14$to)
fb14$to <- gsub("^Germany(.*)", "Germany", fb14$to)

fb14$to <- gsub("we do not ship to usa / new zealand / Australiatralia / scandinavia ", 
                "we do not ship to usa / canada / new zealand / Australiatralia /scandinavia", fb14$to)
fb14$to <- gsub("we do not ship to usa / new zealand / Australiatralia /scandinavia ", 
                "we do not ship to usa / canada / new zealand / Australiatralia /scandinavia", fb14$to)

fb14$to <- gsub("^usa(.*)", "USA", fb14$to)
fb14$to <- gsub("^united\\sstates(.*)", "USA", fb14$to)

fb14$to <- gsub("^scandin(.*)", "Scandinavia", fb14$to)
fb14$to <- gsub("switzerland(.*)", "Switzerland", fb14$to)

fb14$to <- gsub("^internat(.*)", "International", fb14$to)

fb14$to <- gsub("^uk(.*)", "UK", fb14$to)

levels(as.factor(fb14$to))
# 899>624>505>468>464>461>460>458>456>430>428>367>363>360>358>335>327>308
# 299>298>292>288>283>279>274>269>237>232>186>178>169>126

# safety
write.csv(fb14, file = "~/GitHub/agora-data/2014-AgFb2.csv")





