# Agora Marketplace Analysis
# Association Rules - 2014 Product Data
# Cannabis Analysis - SubCategories

# load data -------------------------------------------------------------------

library(data.table)

p14 <- fread("~/GitHub/agora-data/ag06-2014.csv", stringsAsFactors = F)
p14$date <- as.Date(p14$date)
str(p14)

length(unique(p14$from)) # 81
length(unique(p14$product)) # 62873
summary(p14$usd)
#    Min.   1st Qu.    Median      Mean   3rd Qu.      Max. 
# 0         29         95      15890        332 2215000000

# Densities: Price, Categories, Locations, Products ---------------------------

library(ggplot2)
library(RColorBrewer)
library(vcd)
library(extrafont)
library(extrafontdb)
font_import()

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

p14$subcat[p14$subcat == "Ecstasy-MDMA"] <- "Ecstasy"
p14$subcat[p14$subcat == "Ecstasy-NoSub"] <- "Ecstasy"

write.csv(p14, file = "~/GitHub/agora-data/ag06-2014.csv", row.names = F)

# flawed - complete plot
p3 <- ggplot(p14, aes(subcat, from, fill = cat)) + geom_tile() + 
  labs(title = "Agora 2014: Category / Subcategory :: Location", 
       y = "", x = expression(subcat %subset% cat), fill = "category") +
  theme(plot.title = element_text(family= "FranklinGothicSSK", 
                                  face = "bold", size = 14,
                                  margin = margin(0, 0, 0, 0))) + 
  theme(plot.margin = unit(c(2, 2, 1, 2.5), "cm")) +
  theme(axis.text.x = element_text(family = "FranklinGothicSSK", size = 10.5,
                                   angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(family = "FranklinGothicSSK", size = 9))

p3 + scale_fill_manual(values = c("red3", "#CDAD00", "#00688B", "#9AC0CD", 
                                  "#FF7F00", "red2", "bisque3", "gray23",
                                  "bisque2", "bisque1", "darkorange3", "darkorange4",
                                  "firebrick4"))

# remove eletronics and jewelry
p14b <- subset(p14, p14$cat != "Electronics" & p14$cat != "Jewelry") # 10000591

p3b <- ggplot(p14b, aes(subcat, from, fill = cat)) + geom_tile() + 
  labs(title = "Agora 2014: Category / Subcategory :: Location", 
       y = "", x = "", fill = "category") +
  theme(plot.title = element_text(family= "FranklinGothicSSK", 
                                  face = "bold", size = 14,
                                  margin = margin(0, 0, 0, 0))) + 
  theme(plot.margin = unit(c(2, 2, 1, 2.5), "cm")) +
  theme(axis.text.x = element_text(family = "FranklinGothicSSK", size = 10.5,
                                   angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(family = "FranklinGothicSSK", size = 9))

p3b + scale_fill_manual(values = c("red3", 
                                  "bisque4", 
                                  "#00688B", 
                                  "#9AC0CD", 
                                  "red1", 
                                  "bisque1",
                                  "black", 
                                  "bisque3", 
                                  "darkorange2", 
                                  "darkorange4",
                                  "firebrick4"))

# remove listings too
p14c <- subset(p14, p14$cat != "Electronics" & p14$cat != "Jewelry"
               & p14$cat != "Listings" & p14$cat != "Other") #980979

p3c <- ggplot(p14c, aes(subcat, from, fill = cat)) + geom_tile() + 
  labs(title = "Agora 2014: Category / Subcategory ~ Location", 
       y = "", x = "", fill = "category") +
  theme(plot.title = element_text(family= "FranklinGothicSSK", 
                                  face = "bold", size = 14,
                                  margin = margin(0, 0, 0, 0))) + 
  theme(plot.margin = unit(c(2, 2, 1, 2.5), "cm")) +
  theme(axis.text.x = element_text(family = "FranklinGothicSSK", size = 10.5,
                                   angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(family = "FranklinGothicSSK", size = 9))

p3c + scale_fill_manual(values = c("red3", 
                                   "gold1", 
                                   "#00688B", 
                                   "lightblue3", 
                                   "red1", 
                                   "bisque1",
                                   "bisque4", 
                                   "darkorange4",
                                   "firebrick4"))

# subset under 1k -------------------------------------------------------------

p4 <- subset(p14, p14$usd <= 1000) # 905805

p4p <- ggplot(p4, aes(subsubcat, from, fill = usd)) + geom_tile() +
  labs(title = "Agora 2014: Drug ~ Location / Price (USD < 1,000)", 
       y = "", x = "", fill = "Price (USD)") +
  theme(plot.title = element_text(family= "FranklinGothicSSK", 
                                  face = "bold", size = 14,
                                  margin = margin(0, 0, 0, 0))) + 
  theme(plot.margin = unit(c(2, 2, 1, 2.5), "cm")) +
  theme(axis.text.x = element_text(family = "FranklinGothicSSK", size = 10.5,
                                   angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(family = "FranklinGothicSSK", size = 9))

p4p + scale_fill_gradient2(low = "deepskyblue4", mid = "bisque1", high = "firebrick3",
                           midpoint = 500)

# subset over 1k  -------------------------------------------------------------

p5 <- subset(p14, p14$usd > 1000 & p14$usd < 10000.000) # 11069

p5p <- ggplot(p5, aes(subsubcat, from, fill = usd)) + geom_tile() +
  labs(title = "Agora 2014: Drug ~ Location / Price (10,000 > USD > 1,000)", 
       y = "", x = "", fill = "Price (USD)") +
  theme(plot.title = element_text(family= "FranklinGothicSSK", 
                                  face = "bold", size = 14,
                                  margin = margin(0, 0, 0, 0))) + 
  theme(plot.margin = unit(c(2, 2, 1, 2.5), "cm")) +
  theme(axis.text.x = element_text(family = "FranklinGothicSSK", size = 10.5,
                                   angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(family = "FranklinGothicSSK", size = 9))

p5p + scale_fill_gradient2(low = "deepskyblue4", mid = "bisque1", high = "firebrick3",
                           midpoint = 5000)


# subset over 10-40k  -------------------------------------------------------------

p6 <- subset(p14, p14$usd > 10000 & p14$usd < 40000.000) # 4722

p6p <- ggplot(p6, aes(subsubcat, from, fill = usd)) + geom_tile() +
  labs(title = "Agora 2014: Drug ~ Location / Price (40,000 > USD > 10,000)", 
       y = "", x = "", fill = "Price (USD)") +
  theme(plot.title = element_text(family= "FranklinGothicSSK", 
                                  face = "bold", size = 14,
                                  margin = margin(0, 0, 0, 0))) + 
  theme(plot.margin = unit(c(2, 2, 1, 2.5), "cm")) +
  theme(axis.text.x = element_text(family = "FranklinGothicSSK", size = 10.5,
                                   angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(family = "FranklinGothicSSK", size = 10.5))

p6p + scale_fill_gradient2(low = "deepskyblue4", mid = "bisque1", high = "firebrick3",
                           midpoint = 25000)

# subset over 40k  ----------------------------------------------------------------

p7 <- subset(p14, p14$usd > 40000 & p14$usd < 1000000) # 1416

p7p <- ggplot(p7, aes(subsubcat, from, fill = usd)) + geom_tile() +
  labs(title = "Agora 2014: Drug ~ Location / Price (1,000,000 > USD > 40,000)", 
       y = "", x = "", fill = "Price (USD)") +
  theme(plot.title = element_text(family= "FranklinGothicSSK", 
                                  face = "bold", size = 14,
                                  margin = margin(0, 0, 0, 0))) + 
  theme(plot.margin = unit(c(2, 2, 1, 2.5), "cm")) +
  theme(axis.text.x = element_text(family = "FranklinGothicSSK", size = 10.5,
                                   angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(family = "FranklinGothicSSK", size = 10.5))

p7p + scale_fill_gradient2(low = "deepskyblue4", mid = "bisque1", high = "firebrick3",
                           midpoint = 520000)

# subset over 1m  ------------------------------------------------------------------

p8 <- subset(p14, p14$usd > 1000000 & p14$usd < 10020993 & 
               p14$vendor != "DutchTruthTeller") # 187

summary(p8$usd)
range(p8$usd)

p8p <- ggplot(p8, aes(subsubcat, from, fill = usd)) + geom_tile() +
  labs(title = "Agora 2014: Drug ~ Location / Price (USD > 1,000,000)", 
       y = "", x = "", fill = "Price (USD)") +
  theme(plot.title = element_text(family= "FranklinGothicSSK", 
                                  face = "bold", size = 14,
                                  margin = margin(0, 0, 0, 0))) + 
  theme(plot.margin = unit(c(2, 2, 1, 2.5), "cm")) +
  theme(axis.text.x = element_text(family = "FranklinGothicSSK", size = 10.5,
                                   angle = 45, hjust = 1)) +
  theme(axis.text.y = element_text(family = "FranklinGothicSSK", size = 10.5))

p8p + scale_fill_gradient2(low = "deepskyblue4", mid = "bisque1", high = "firebrick3",
                           midpoint = 4960000)

