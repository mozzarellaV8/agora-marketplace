# Agora Marketplace Analysis
# arules - 2014 data

# load data -------------------------------------------------------------------

library(data.table)
p14 <- fread("~/GitHub/agora-data/ag01-2014.csv", stringsAsFactors = F)
str(p14)

# cleanse further -------------------------------------------------------------

library(tm)
library(dplyr)
library(tidyr)

levels(as.factor(p14$vendor)) # 2284

# location data duplicates
levels(as.factor(p14$from))   # 465- mess
levels(as.factor(p14$to))     # 1281 - mess

# Cleanse Shipping Origin variables -------------------------------------------

# high-level 
p14$from <- stripWhitespace(p14$from)
p14$from <- tolower(p14$from)
p14$from <- sub("^\\s+", "", p14$from)
p14$from <- sub("\\s+$", "", p14$from)

p14$from <- gsub("from:", "", p14$from)
p14$from[p14$from == " "] <- "no info"

p14$from <- sub("^\\s+", "", p14$from)
p14$from <- gsub("to:(.*)", "", p14$from)

p14$from[p14$from == ""] <- "no info"

levels(as.factor(p14$from)) # 147

# fine grain
p14$from <- gsub("^agora(.*)", "agora", p14$from)    # trailing whitespace
p14$from <- gsub("^australia\\s", "agora", p14$from) # trailing whitespace
p14$from <- sub("\\s+$", "", p14$from)

p14$from <- gsub("u.s.a", "usa", p14$from)
p14$from <- gsub("united states", "usa", p14$from)
p14$from <- gsub("usa.", "usa", p14$from)
p14$from <- gsub("usa.", "usa", p14$from)

levels(as.factor(p14$from)) # 114

p14$from <- gsub("untied kingdom(.*)", "uk", p14$from)
p14$from <- gsub("united kingdom(.*)", "uk", p14$from)

p14$from <- gsub("undelcared;\\)", "undeclared;\\)", p14$from)
p14$from <- gsub("undeclared", "undeclared;\\)", p14$from)
p14$from <- gsub("undeclared;\\);\\)", "undeclared;\\)", p14$from)

p14$from <- gsub("world\\swide", "worldwide", p14$from)
p14$from <- gsub("world", "worldwide", p14$from)

p14$from <- gsub("pacific\\spalasades", "pacific palisades", p14$from)

p14$from <- gsub("german", "germany", p14$from)
p14$from <- gsub("germanyy", "germany", p14$from)

levels(as.factor(p14$from)) # 107

ukFranchise <- subset(p14c, p14c$from == c("uk , usaand worldwide", "uk and ireland", 
                                           "uk, usa philippines", "uk, usa& asia", "uk, usa& worldwide"))
write.csv(ukFranchise, file = "p14c-ukFranchise.csv", row.names = F)


usFranchise <- subset(p14c, p14c$from == "us & canada" | 
                        p14c$from == "usa uk & eu" |
                        p14c$from == "usa, uk & eu" | 
                        p14c$from == "usa, uk and worldwide" | 
                        p14c$from == "usa& uk" | 
                        p14c$from == "usaand uk" |
                        p14c$from == "west of the mississippi")

p14c$from <- gsub("us & canada", "usa", p14c$from)                 
p14c$from <- gsub("usa uk & eu", "usa", p14c$from)                            
p14c$from <- gsub("usa, uk & eu", "usa", p14c$from)                            
p14c$from <- gsub("usa, uk and worldwide", "usa", p14c$from)                   
p14c$from <- gsub("usa& uk", "usa", p14c$from)                                 
p14c$from <- gsub("usaand uk", "usa", p14c$from)                                
p14c$from <- gsub("west of the mississippi", "usa", p14c$from)

p14c$from <- gsub("uk and ireland", "uk", p14c$from)
p14c$from <- gsub("uk, usa philippines", "uk", p14c$from)
p14c$from <- gsub("uk, usa& asia", "uk", p14c$from) 
p14c$from <- gsub("uk, usa& worldwide", "uk", p14c$from)
p14c$from <- gsub("uk\\s,\\suk", "uk", p14c$from)

p14c$from <- gsub("usa uk", "usa", p14c$from)
p14c$from <- gsub("usawide", "usa", p14c$from)

p14c$from <- gsub("china\\sor\\seu", "china", p14c$from)

p14c$from <- gsub("the loins of our founding fathers", "usa", p14c$from)
p14c$from <- gsub("george washingtons boner holder", "usa", p14c$from)
p14c$from <- gsub("the home of the body bags, shotty, and mag", "usa", p14c$from)
p14c$from <- gsub("pacific palisades", "usa", p14c$from)
p14c$from <- gsub("pacific palisades", "usa", p14c$from)

p14c$from <- gsub("nosinfo", "no info", p14c$from)

p14c$from <- gsub("worldwidewide", "worldwide", p14c$from)
p14c$from <- sub("the home of the body bags, shotty, and mag, shotty, and mag", 
                 "the home of the body bags, shotty, and mag", p14c$from)
p14c$from <- gsub("shipping", "no info", p14c$from)
p14c$from <- gsub("worldwidewide", "worldwide", p14c$from)
p14c$from <- gsub("worldwidewide", "worldwide", p14c$from)

# safety
write.csv(p14, file = "~/GitHub/agora-data/ag02-2014.csv", row.names = F)



