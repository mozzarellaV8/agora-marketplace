# Agora Marketplace Analysis
# arules - 2014 data

# load data -------------------------------------------------------------------

library(data.table)
p14 <- fread("~/GitHub/agora-data/agora-2014.csv", stringsAsFactors = F)

# cleanse further -------------------------------------------------------------

# What do I want? clean url paths, no duplicates in locations, 
# possibly impute categorical data (replace NAs, for transactions)
# Sooner or Later: subset for positive feedbacks (inferring transaction)
# Later: url paths, BTC-dollar ratio.

library(tm)
library(dplyr)
library(tidyr)

p14$date <- as.Date(p14$date)

p14$list <- as.character(p14$list)
p14$list <- removeNumbers(p14$list)
p14$list <- gsub("--__", "", p14$list)
p14$list <- gsub(".html", "", p14$list)
p14$list <- as.factor(p14$list)

p14$vendor <- gsub("%7E", "", p14$vendor)
levels(as.factor(p14$vendor)) # 2178

p14$feedback <- stripWhitespace(p14$feedback)

# subset for listings with feedback
# feedback can be broken down: 
# 5: great; 5: dubious (check forums for false feedback); 
# 4: good; 3 - average; 0-2 - poor

# location data duplicates
levels(as.factor(p14$from)) #461 messy levels
levels(as.factor(p14$to)) # 1229 messy levels


# Cleanse Shipping Origin variables -------------------------------------------

# strip white space, remove 'From'
p14$from <- gsub("From:", "", p14$from)
p14$from <- stripWhitespace(p14$from)
p14$from[p14$from == " "] <- "no info"

# Stripping out the white space collapsed dupliate locations;
# levels drop from 315 to 174 (now 275)
# tolower() might also do this.

p14$from <- tolower(p14$from)
levels(as.factor(p14$from)) # 232

# this method will be troublesome on large lists
ag <- grep("agora", p14$from) 
ag[[1]] # 106

for (i in 1:length(ag)) {
  row <- ag[[i]]
  p14[row, 10] <- "Agora"
}

levels(as.factor(p14$from)) # 229

# this method will be too tedious moving forward
p14$from[p14$from == " australia " ] <- "australia"
p14$from[p14$from == " australia to: aus/nz " ] <- "australia"
p14$from[p14$from == " australia to: aus/nz " ] <- "australia"
p14$from[p14$from == " australia to: australia " ] <- "australia"
p14$from[p14$from == " australia to: australia only " ] <- "australia"
p14$from[p14$from == " australia to: australia "] <- "australia"

levels(as.factor(p14$from)) # 226

p14$from <- gsub("^\\scanada\\s", "canada", p14$from)
p14$from <- gsub("canada(.*)", "canada", p14$from)
p14$from <- gsub("^\\sdenmark(.*)", "denmark", p14$from)

levels(as.factor(p14$from)) # 213

p14$from <- gsub("^\\schina(.*)", "china", p14$from)
p14$from <- gsub("^\\seu(.*)", "eu", p14$from)
p14$from <- gsub("^\\sgermany(.*)", "germany", p14$from)

p14$from <- gsub("finland(.*)", "finland", p14$from)

p14$from <- gsub("^\\shong\\skong(.*)", "hong kong", p14$from)
p14$from <- gsub("^\\sindia(.*)", "india", p14$from)
p14$from <- gsub("^\\sinternet(.*)", "internet", p14$from)

levels(as.factor(p14$from)) # 182

p14$from <- gsub("^\\sitaly(.*)", "italy", p14$from)
p14$from <- gsub("^\\sja jolla(.*)", "la jolla", p14$from)
p14$from <- gsub("^\\smy\\spm(.*)", "my pm", p14$from)

p14$from <- gsub("^\\snetherlands(.*)", "netherlands", p14$from)
p14$from <- gsub("^\\snorway(.*)", "norway", p14$from)
p14$from <- gsub("^\\ssweden(.*)", "sweden", p14$from)

levels(as.factor(p14$from)) # 165

p14$from <- gsub("^\\sthe united snakes of captivity(.*)", "the united snakes of captivity", p14$from)
p14$from <- gsub("^\\sto:(.*)", "blank", p14$from)
p14$from <- gsub("^\\sto:(.*)", "blank", p14$from)
p14$from <- gsub("^\\storland(.*)", "torland", p14$from)
p14$from <- gsub("^\\storland(.*)", "torland", p14$from)

p14$from <- gsub("^\\sundeclared(.*)", "undeclared", p14$from)
p14$from <- gsub("^\\sundeclared;\\)(.*)", "undeclared", p14$from)
p14$from <- gsub("undeclared(.*)", "undeclared", p14$from)
p14$from <- gsub("undelcared;\\)(.*)", "undeclared", p14$from)

levels(as.factor(p14$from)) # 141

p14$from <- gsub("^\\susa(.*)", "usa", p14$from)
p14$from <- gsub("^\\susa(.*)", "usa", p14$from)
p14$from <- gsub("^\\sUS(.*)", "usa", p14$from)
p14$from <- gsub("united utates(.*)", "usa", p14$from)

p14$from <- gsub("^\\swest of the mississippi(.*)", "west of the mississippi", p14$from)
p14$from <- gsub("^\\sworld(.*)", "world", p14$from)

levels(as.factor(p14$from)) # 116

p14$from <- gsub("^\\suk(.*)", "uk", p14$from)
p14$from <- gsub("^\\suk(.*)", "uk", p14$from)
p14$from <- gsub("^\\suk(.*)", "uk", p14$from)
p14$from <- gsub("untied kingdom(.*)", "uk", p14$from)
p14$from <- gsub("united kingdom(.*)", "uk", p14$from)
p14$from <- gsub("uk(.*)", "uk", p14$from)

p14$from <- gsub("earth planet to:", "earth planet", p14$from)

# leading and trailing whitespace
p14$from <- sub("^\\s+", "", p14$from)
p14$from <- sub("\\s+$", "", p14$from)

levels(as.factor(p14$from)) # 99

# safety
write.csv(p14, file = "~/GitHub/agora-data/Ag2014.csv", row.names = F)
# test <- fread("~/GitHub/agora-data/2014-AgFb.csv", stringsAsFactors = F)

# Cleanse Shipping Destinations -----------------------------------------------

# read in if necessary:
# library(data.table)
# library(tm)
# library(tidyr)
# library(dplyr)
# p14 <- fread("~/GitHub/agora-data/Ag2014.csv", stringsAsFactors = F)
levels(as.factor(p14$to)) # 1299

p14$to <- stripWhitespace(p14$to)
p14$to <- tolower(p14$to)
p14$to[is.na(p14$to)] <- "no info"

levels(as.factor(p14$to)) # 690

# p14$to <- gsub("\\s", "", p14$to)
p14$to <- gsub("^\\s\'merica(.*)", "\'merica", p14$to)
p14$to <- gsub("\'merica(.*)", "\'merica", p14$to)
p14$to <- gsub("\"\"\"\"fcuk..your couch\"\"\"\" ", "fcuk..your couch", p14$to)
p14$to <- gsub("\"\"\"\"fcuk..your couch\"\"\"\"-rick james ", "fcuk..your couch", p14$to)

# maybe merge these 
p14$to <- gsub("all(.*)", "all", p14$to)
p14$to <- gsub("^anywhere(.*)", "all", p14$to)
p14$to <- gsub("anywhere\\s", "all", p14$to)
p14$to <- gsub("americana ", "usa", p14$to)
p14$to <- gsub("\'merica", "usa", p14$to)

levels(as.factor(p14$to)) # 675

# p14$to <- gsub("any except australia ", "anywhere", p14$to)
p14$to <- gsub("australia only ", "australia", p14$to)
p14$to <- gsub("aus", "australia", p14$to)
p14$to <- gsub("australia ", "australia", p14$to)
p14$to <- gsub("australians only ", "australia", p14$to)
p14$to <- gsub("australiatalia only ", "australia", p14$to)

levels(as.factor(p14$to)) # 674

p14$to <- gsub("australia,newzealand", "australia and new zealand", p14$to)
p14$to <- gsub("australia/new zealand ", "australia and new zealand", p14$to)
p14$to <- gsub("australia/nz", "australia and new zealand", p14$to)
p14$to <- gsub("australia & newzealand", "australia and new zealand", p14$to)
p14$to <- gsub("australia and nz ", "australia and new zealand", p14$to)
p14$to <- gsub("australia, new zealand ", "australia and new zealand", p14$to)
p14$to <- gsub("australia/worldwide ", "australia and new zealand", p14$to)

levels(as.factor(p14$to)) # 641
                                                                                
p14$to <- gsub("^can(.*)", "canada/USA", p14$to)
p14$to <- gsub("^de(.*)", "germany", p14$to)

p14$to <- gsub("^de(.*)", "germany", p14$to)

p14$to <- gsub("^europe(.*)", "Europe", p14$to)
p14$to <- gsub("^eu", "EU", p14$to)
p14$to <- gsub("^EU(.*)", "EU", p14$to)

p14$to <- gsub("^everywhere(.*)", "Everywhere", p14$to)
p14$to <- gsub("^every where(.*)", "Everywhere", p14$to)
p14$to <- gsub("^everyplace(.*)", "Everywhere", p14$to)

levels(as.factor(p14$to)) # 534

p14$to <- gsub("^france(.*)", "France", p14$to)
p14$to <- gsub("^fr", "France", p14$to)
p14$to <- gsub("France\\s", "France", p14$to)

levels(as.factor(p14$to)) # 529

p14$to <- gsub("gentle hearts and kind people ", "Kind Hearts and Gentle People", p14$to)
p14$to <- gsub("kind and gentle people ", "Kind Hearts and Gentle People", p14$to)
p14$to <- gsub("Kind Hearts and Gentle People", "kind hearts and gentle people", p14$to)

p14$to <- gsub("^worldwide except(.*)", "worldwide, except...", p14$to)
p14$to <- gsub("^worldwide\\s\\(except(.*)", "worldwide, except...", p14$to)
p14$to <- gsub("^worldwide\\s\\(e(.*)", "worldwide, except...", p14$to)

levels(as.factor(p14$to)) # 467

p14$to <- gsub("^worldwide,(.*)", "worldwide, except...", p14$to)
p14$to <- gsub("^worldwide-(.*)", "worldwide, except...", p14$to)
p14$to <- gsub("^worldwide\\s-\\s(.*)", "worldwide, except...", p14$to)

levels(as.factor(p14$to)) # 453

p14$to <- gsub("^worldwide\\s-(.*)", "worldwide, except...", p14$to)
p14$to <- gsub("^world\\s-(.*)", "worldwide, except...", p14$to)
p14$to <- gsub("^world\\sexcept(.*)", "worldwide, except...", p14$to)

p14$to <- gsub("^world\\swide\\sexcept(.*)", "worldwide, except...", p14$to)
p14$to <- gsub("^world\\sno(.*)", "worldwide, except...", p14$to)
p14$to <- gsub("^world\\sno(.*)", "worldwide, except...", p14$to)

p14$to <- gsub("^wordwide(.*)", "worldwide, except...", p14$to)
p14$to <- gsub("^ww except(.*)", "worldwide, except...", p14$to)

levels(as.factor(p14$to)) # 427

p14$to <- gsub("^worldwide\\s[:A-z:](.*)", "worldwide", p14$to)
p14$to <- gsub("worlwide\\s(.*)", "worldwide", p14$to)
p14$to <- gsub("world(.*)", "worldwide", p14$to)
p14$to <- gsub("^ww|wrld", "worldwide", p14$to)

levels(as.factor(p14$to)) # 336

p14$to <- gsub("^ger", "germany", p14$to)
p14$to <- gsub("^germany(.*)", "germany", p14$to)

p14$to <- gsub("we do not ship to usa / new zealand / australia /scandinavia ",
                "we do not ship to usa / canada / new zealand / australia / scandinavia", p14$to)
p14$to <- gsub("we do not ship to usa / new zealand / australiatralia /scandinavia ", 
                "we do not ship to usa / canada / new zealand / australia /scandinavia", p14$to)

p14$to <- gsub("we do not ship to usa / canada / new zealand / australiatralia /scandinavia ", 
               "we do not ship to usa / canada / new zealand / australia /scandinavia", p14$to)

levels(as.factor(p14$to)) # 334

p14$to <- gsub("^scandin(.*)", "Scandinavia", p14$to)
p14$to <- gsub("switzerland(.*)", "Switzerland", p14$to)

p14$to <- gsub("^internat(.*)", "International", p14$to)

p14$to <- gsub("the human race ", "the human race, only!!!", p14$to)
p14$to <- gsub("the human race, only!!!only!!! ", "the human race, only!!!", p14$to)

p14$to <- gsub("^uk(.*)", "UK", p14$to)
p14$to <- gsub("united kingdom ", "UK", p14$to)
p14$to <- gsub("u.k ", "UK", p14$to)
p14$to <- gsub("u.k ", "UK", p14$to)

p14$to <- gsub("^responsible(.*)", "responsible ninjas/adults/sippers/singles", p14$to)
p14$to <- gsub("^responsable(.*)", "responsible ninjas/adults/sippers/singles", p14$to)

p14$to <- gsub("^usa(.*)", "USA", p14$to)
p14$to <- gsub("^united\\sstates(.*)", "USA", p14$to)
p14$to <- gsub("us only! ", "USA", p14$to)
p14$to <- gsub("us only ", "USA", p14$to)
p14$to <- gsub("us ", "USA", p14$to)
p14$to <- gsub("u\\$a\\s", "USA", p14$to)
p14$to <- gsub("u.s.a. ", "USA", p14$to)
p14$to <- gsub("u.s.a ", "USA", p14$to)
p14$to <- gsub("u.s. only ", "USA", p14$to)
p14$to <- gsub("rUSA", "USA", p14$to)
p14$to <- gsub("only USA", "USA", p14$to)

p14$to <- removePunctuation(p14$to)

p14$to <- gsub("^australiatralia", "australia", p14$to)
p14$to <- gsub("australiatria", "australia", p14$to)

p14$to <- gsub("^inbox(.*)", "inbox", p14$to)
p14$to <- gsub("^inbox(.*)", "inbox", p14$to)

p14$to <- sub("^\\s+", "", p14$to)
p14$to <- sub("\\s+$", "", p14$to)

p14$to <- gsub("^widewolrd(.*)", "worldwide", p14$to)
p14$to <- gsub("^wiorldwide(.*)", "worldwide", p14$to)
p14$to <- gsub("woldwide", "worldwide", p14$to)

p14$to <- gsub("wolrd", "worldwide", p14$to)
p14$to <- gsub("wolrdwide", "worldwide", p14$to)
p14$to <- gsub("wordlwide", "worldwide", p14$to)
p14$to <- gsub("worl massive", "worldwide", p14$to)
p14$to <- gsub("worladwide", "worldwide", p14$to)
p14$to <- gsub("wortldwide", "worldwide", p14$to)
p14$to <- gsub("wroldwide", "worldwide", p14$to)

p14$to <- gsub("worldwidewide", "worldwide", p14$to)
p14$to <- gsub("worldwide prior", "worldwide", p14$to)
p14$to <- gsub("wideworldwide", "worldwide", p14$to)
p14$to <- gsub("w o r l d w i d e", "worldwide", p14$to)

p14$to <- gsub("uworldwide", "worldwide", p14$to)
p14$to <- gsub("worldwide excep australiatralia", "worldwide except australia", p14$to)
p14$to <- gsub("worldwide except australiatralia", "worldwide except australia", p14$to)

p14$to <- gsub("uk  escrow worldwide", "UK", p14$to)

p14$to <- gsub("uk  escrow worldwide", "UK", p14$to)

levels(as.factor(p14$to)) # 154

# safety
write.csv(p14, file = "~/GitHub/agora-data/Agora2014.csv")





