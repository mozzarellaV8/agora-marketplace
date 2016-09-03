# Agora Marketplace Analysis
# Association Rules - 2014 Product Data
# Cleanse Locations - 03

# load data -------------------------------------------------------------------

library(data.table)

p14 <- fread("~/GitHub/agora-data/ag07-2014.csv", stringsAsFactors = F)
p14$date <- as.Date(p14$date)
str(p14)

length(unique(p14$from)) # 81
length(unique(p14$product)) # 62873
summary(p14$usd)
#    Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0         29         95      15890        332 2215000000

# clean locations (more) ------------------------------------------------------
p14$from[which(is.na(p14$from))] <- "no info"
p14$from[p14$from == "null"] <- "no info"

p14$from <- gsub("^usauk(.*)", "usa", p14$from)
p14$from <- gsub("us & canada", "usa", p14$from)             
p14$from <- gsub("usa uk & eu", "usa", p14$from)                            
p14$from <- gsub("usa, uk & eu", "usa", p14$from)                            
p14$from <- gsub("usa, uk and worldwide", "usa", p14$from)                   
p14$from <- gsub("usa& uk", "usa", p14$from)                                 
p14$from <- gsub("usaand uk", "usa", p14$from)                                
p14$from <- gsub("west of the mississippi", "usa", p14$from)
p14$from <- gsub("usa uk", "usa", p14$from)
p14$from <- gsub("usawide", "usa", p14$from)

p14$from <- gsub("uk and ireland", "uk", p14$from)
p14$from <- gsub("uk, usa philippines", "uk", p14$from)
p14$from <- gsub("uk, usa& asia", "uk", p14$from) 
p14$from <- gsub("uk, usa& worldwide", "uk", p14$from)
p14$from <- gsub("uk , usa& worldwide", "uk", p14$from)
p14$from <- gsub("uk\\s,\\suk", "uk", p14$from)
p14$from <- gsub("uk, usaand worldwide", "uk", p14$from)
p14$from <- gsub("uk , usaand worldwide", "uk", p14$from)
p14$from <- gsub("uk, usa eu, aus", "uk", p14$from)
p14$from <- gsub("ukwide", "uk", p14$from)

p14$from <- gsub("china or eu", "china", p14$from)

p14$from <- gsub("the loins of our founding fathers", "usa", p14$from)
p14$from <- gsub("george washingtons boner holder", "usa", p14$from)
p14$from <- gsub("the home of the body bags, shotty, and mag", "usa", p14$from)
p14$from <- gsub("the home of the body bags", "usa", p14$from)
p14$from <- gsub("pacific palisades", "usa", p14$from)
p14$from <- gsub("pacific palisades", "usa", p14$from)
p14$from <- gsub("la jolla", "usa", p14$from)
p14$from <- gsub("the united snakes of captivity", "usa", p14$from)

p14$from <- gsub("rusa federation", "russian federation", p14$from)

p14$from <- gsub("worldwidewide", "worldwide", p14$from)
p14$from <- sub("the home of the body bags, shotty, and mag, shotty, and mag", 
                "the home of the body bags, shotty, and mag", p14$from)
p14$from <- gsub("shipping", "no info", p14$from)
p14$from <- gsub("worldwidewide", "worldwide", p14$from)
p14$from <- gsub("worldwidewide", "worldwide", p14$from)

p14$from <- gsub("cheqdropz", "czech republic", p14$from)
p14$from <- gsub("moldova, republic of", "moldova", p14$from)
p14$from <- gsub("bangkok", "thailand", p14$from)
p14$from <- gsub("my\\spm", "internet", p14$from)
p14$from <- gsub("\\bme\\b", "internet", p14$from)
p14$from <- gsub("earth\\splanet", "worldwide", p14$from)
p14$from <- gsub("\\bcandyland\\b", "torland", p14$from)

p14$subcat[p14$subcat == "Ecstasy-MDMA"] <- "Ecstasy"
p14$subcat[p14$subcat == "Ecstasy-NoSub"] <- "Ecstasy"

write.csv(p14, file = "~/GitHub/agora-data/ag07b-2014.csv", row.names = F)

