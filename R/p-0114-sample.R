# Agora Marketplace Analysis
# January 2014 Sample
# explore categories

# load data -------------------------------------------------------------------
getwd()
setwd("~/GitHub/agora-marketplace")

p14 <- read.csv("~/GitHub/agora-marketplace/data/product/products-2014-01.csv")
str(p14)

p14$date <- as.Date(p14$date)

# feedback as transaction - how many?
p14$feedback <- as.character(p14$feedback)
fb <- subset(p14, p14$feedback != "\n    Feedbacks:\n    No feedbacks found.\n")
# number of listings drops from 7986 listings to 1430. 

levels(fb$cat)
#  [1] "Counterfeits"       "Data"               "Drug paraphernalia" "Drugs"              "Forgeries"         
#  [6] "Information"        "Listings"           "Services"           "Tobacco"            "Weapons"

levels(fb$subcat)
# [1] "Accessories"         "Accounts"            "Ammunition"          "Benzos"              "Cannabis"           
# [6] "Clothing"            "Containers"          "Disassociatives"     "eBooks"              "Ecstasy"            
# [11] "Ecstasy-MDMA"        "Electronics"         "Guides"              "Hacking"             "Lethal firearms"    
# [16] "Melee"               "Methylone"           "Money"               "Non-lethal firearms" "Opioids"            
# [21] "Other"               "Pipes"               "Pirated"             "Prescription"        "Psychedelics"       
# [26] "RCs"                 "Smoked"              "Software"            "Steroids"            "Stimulants"         
# [31] "Watches"             "Weight loss" 

levels(fb$subsubcat)
# [1] "2C"         "5-MeO"      "Cocaine"    "DMT"        "Edibles"    "GBL"        "GHB"        "Hash"       "Ketamine"  
# [10] "LSD"        "MDA"        "MDMA"       "Mescaline"  "Meth"       "Mushrooms"  "MXE"        "NB"         "Other"     
# [19] "Others"     "Pills"      "Salvia"     "Speed"      "Spores"     "Synthetics" "Weed" 


# explore categories ---------------------------------------------------------

library(ggplot2)
library(RColorBrewer)
library(vcd)
library(extrafont)
library(colorRamps)
library(grDevices)

display.brewer.all()
font_import()
fonts()

# color
rich12equal = c("#000040", "#000093", "#0020E9", "#0076FF", 
                "#00B8C2", "#04E466", "#49FB25", "#E7FD09", 
                "#FEEA02", "#FFC200", "#FF8500", "#FF3300")

rich10equal = c("#000041", "#0000A9", "#0049FF", "#00A4DE", 
                "#03E070", "#2fa502", "#F6F905", "#FFD701", 
                "#FF9500", "#FF3300")

heat <- heat.colors(32)

# categorical counts
cat <- ggplot(p14, aes(x = cat, fill = cat)) + 
  geom_bar(stat = "count", color = "black", alpha = 0.75) 

cat <- cat + 
  scale_fill_manual(values = rich10equal) +
  theme_minimal(base_size = 14, base_family = "FranklinGothicSSK") +
  theme(plot.margin = unit(c(2, 2, 0.5, 2), "cm")) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1.25,
                                   size = 14, face = "bold")) +
  theme(plot.title = element_text(size = 18, face = "bold")) +
  labs(title = "Agora Marketplace: Main Categories",
       x = "", y  = "count")

cat

# subcategory counts
length(levels(p14$subcat)) # 32

subcat <- ggplot(p14, aes(x = subcat, fill = subcat)) +
  geom_bar(stat = "count", color = "black", alpha = 0.75) +
  theme_minimal(base_size = 12, base_family = "FranklinGothicSSK") +
  theme(plot.margin = unit(c(2, 2, 0.5, 2), "cm"))

subcat <- subcat +
  scale_fill_manual(values = heat) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1.25,
                                   size = 14, face = "bold")) +
  theme(axis.text.y = element_text(size = 14, face = "bold")) +
  theme(plot.title = element_text(size = 18, face = "bold")) +
  theme(legend.position = "none") +
  labs(title = "Agora Marketplace: subcategories",
       x = "", y  = "count")
  
subcat
# revise NAs in subcategory - fill in with category value

# subsubcategory counts
length(levels(p14$subsubcat)) # 25

terr25 <- terrain.colors(25)

subsub <- ggplot(na.omit(p14), aes(x = subsubcat, fill = subsubcat)) +
  geom_bar(stat = "count", color = "black", fill = 0.75) +
  theme_minimal(base_size = 14, base_family = "FranklinGothicSSK") +
  theme(plot.margin = unit(c(2, 2, 0.5, 2), "cm"))

subsub + theme(axis.text.x = element_text(angle = 45, hjust = 1.1, vjust = 1.25,
                                   size = 14, face = "bold")) +
  theme(axis.text.y = element_text(size = 14, face = "bold")) +
  theme(plot.title = element_text(size = 18, face = "bold")) +
  theme(legend.position = "none") +
  labs(title = "Agora Marketplace: sub-subcategories",
       x = "", y  = "count")

subsub


# feedback/transaction category counts

fbCat <- ggplot(fb, aes(x = cat, fill = cat)) + 
  geom_bar(stat = "count", color = "black", alpha = 1) 

fbCat <- fbCat + 
  scale_fill_brewer(palette = "Reds", direction = -1) +
  theme_minimal(base_size = 14, base_family = "FranklinGothicSSK") +
  theme(plot.margin = unit(c(2, 2, 0.5, 2), "cm")) +
  theme(legend.position = "none") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1.25,
                                   size = 14, face = "bold")) +
  theme(plot.title = element_text(size = 18, face = "bold")) +
  labs(title = "Agora Marketplace: categories with feedback/transactions ",
       x = "", y  = "count")

fbCat

# feedback/transaction subcat counts
fb.subcat <- ggplot(fb, aes(x = subcat, fill = subcat)) +
  geom_bar(stat = "count", color = "black", alpha = 1) +
  theme_minimal(base_size = 12, base_family = "FranklinGothicSSK") +
  theme(plot.margin = unit(c(2, 2, 0.5, 2), "cm"))

fb.subcat <- fb.subcat +
  scale_fill_manual(values = heat) +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1.25,
                                   size = 14, face = "bold")) +
  theme(axis.text.y = element_text(size = 14, face = "bold")) +
  theme(plot.title = element_text(size = 18, face = "bold")) +
  theme(legend.position = "none") +
  labs(title = "Agora Marketplace: subcategories with feedback/transactions",
       x = "", y  = "count")

fb.subcat

# feedback/transactions sub-subcategory counts
fb.subsub <- ggplot(na.omit(p14), aes(x = subsubcat, fill = subsubcat)) +
  geom_bar(stat = "count", color = "black", fill = 0.75) +
  theme_minimal(base_size = 14, base_family = "FranklinGothicSSK") +
  theme(plot.margin = unit(c(2, 2, 0.5, 2), "cm"))

fb.subsub + theme(axis.text.x = element_text(angle = 45, hjust = 1.1, vjust = 1.25,
                                          size = 14, face = "bold")) +
  theme(axis.text.y = element_text(size = 14, face = "bold")) +
  theme(plot.title = element_text(size = 18, face = "bold")) +
  theme(legend.position = "none") +
  labs(title = "Agora Marketplace: sub-subcategories with feedback/transactions",
       x = "", y  = "count")


# cat by subcat
par(mar = c(12, 12, 8, 8), family = "FranklinGothicSSK", las = 2)
plot(p14$cat, p14$subcat, xlab = "", ylab = "", 
     main = "AgMarketplace: categories by subcategories")

#main
par(mar = c(12, 12, 12, 8), las = 1)
plot(p14$cat, main = "AgMarketplace: listing categories", horiz = TRUE)

# sub
par(mar = c(12, 12, 12, 8), las = 1)
plot(p14$subcat, main = "AgMarketplace: listing subcategories", horiz = TRUE)

# subsub
par(mar = c(12, 12, 12, 8), las = 1)
plot(p14$subsubcat, main = "AgMarketplace: listing sub-subcategories", horiz = TRUE)

# categories by location

p14$from <- stripWhitespace(as.character(p14$from))
levels(as.factor(p14$from))
p14$from <- as.factor(p14$from)

# subcat by ship_from
par(mar = c(12, 18, 8, 8), family = "FranklinGothicSSK", las = 2)
plot(p14$subcat, p14$from, main = "AgMarket: subcategory by location",
     xlab = "", ylab = "")

# cat by ship_from
par(mar = c(12, 18, 8, 8), family = "FranklinGothicSSK", las = 2)
plot(p14$cat, p14$from, main = "AgMarket: listing category by location",
     xlab = "", ylab = "")

# subsubcat by location
par(mar = c(12, 18, 8, 8), family = "FranklinGothicSSK", las = 2)
plot(p14$subsubcat, p14$from, main = "AgMarket: sub-subcategory by location",
     xlab = "", ylab = "")

library(vcd)
assoc( ~ subcat + subsubcat, data = fb, shade = TRUE)
