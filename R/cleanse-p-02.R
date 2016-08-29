# Agora Marketplace Analysis
# arules - 2014 data

# load ------------------------------------------------------------------------

library(data.table)
library(tm)
library(tidyr)
library(dplyr)
p14 <- fread("~/GitHub/agora-data/ag02-2014.csv", stringsAsFactors = F)
levels(as.factor(p14$to)) # 1281

# Cleanse Shipping Destinations -----------------------------------------------

# high-level
p14$to <- stripWhitespace(p14$to)
p14$to <- tolower(p14$to)
p14$to[is.na(p14$to)] <- "no info"
p14$to[p14$to == ""] <- "no info"

p14$to <- sub("\\s+$", "", p14$to)

levels(as.factor(p14$to)) # 730
p14$to <- gsub("\"\"\"\"fcuk..your couch\"\"\"\" ", "fcuk..your couch", p14$to)
p14$to <- gsub("\"\"\"\"fcuk..your couch\"\"\"\"-rick james ", "fcuk..your couch", p14$to)


# fine-grain ------------------------------------------------------------------
# p14$to <- gsub("\\s", "", p14$to)
p14$to <- gsub("\'murica(.*)", "\'merica", p14$to)
p14$to <- gsub("\'merica", "usa", p14$to)
p14$to <- gsub("americana ", "usa", p14$to)


# maybe merge these 
p14$to <- gsub("all(.*)", "all", p14$to)
p14$to <- gsub("^anywhere(.*)", "all", p14$to)
p14$to <- gsub("anywhere\\s", "all", p14$to)

levels(as.factor(p14$to)) # 675

# p14$to <- gsub("any except australia ", "anywhere", p14$to)
p14$to <- gsub("australia only ", "australia", p14$to)
p14$to <- gsub("aus", "australia", p14$to)
p14$to <- gsub("australia ", "australia", p14$to)
p14$to <- gsub("australians only ", "australia", p14$to)
p14$to <- gsub("australiatalia only ", "australia", p14$to)

levels(as.factor(p14$to)) # 728

p14$to <- gsub("gentle hearts and kind people ", "kind hearts and gentle people", p14$to)
p14$to <- gsub("kind and gentle people ", "kind hearts and gentle people", p14$to)
p14$to <- gsub("kind hearts and gentle people", "kind hearts and gentle people", p14$to)

p14$to <- gsub("the human race, only!!! only!!!", "the human race", p14$to)
p14$to <- gsub("the human race only!!!", "the human race", p14$to)
p14$to <- gsub("the human race, only!!!, only!!!", "the human race", p14$to)

p14$to <- gsub("united kingdom ", "uk", p14$to)
p14$to <- gsub("u.k", "uk", p14$to)

p14$to <- gsub("^united\\sstates(.*)", "usa", p14$to)
p14$to <- gsub("us only! ", "usa", p14$to)
p14$to <- gsub("us only ", "usa", p14$to)
p14$to <- gsub("us ", "usa", p14$to)
p14$to <- gsub("u\\$a\\s", "usa", p14$to)
p14$to <- gsub("u.s.a. ", "usa", p14$to)
p14$to <- gsub("u.s.a ", "usa", p14$to)
p14$to <- gsub("u.s. only ", "usa", p14$to)
p14$to <- gsub("rusa", "usa", p14$to)
p14$to <- gsub("only usa", "usa", p14$to)

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

levels(as.factor(p14$to)) # 718

# safety
write.csv(p14, file = "~/GitHub/agora-data/ag02-2014.csv", row.names = F)