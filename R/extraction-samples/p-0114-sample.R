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

display.brewer.all()
font_import()
fonts()


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

# cleanse categoricals -----------------------------------------------------------------
library(tm)

rownames(fb) <- NULL

# initial check
levels(fb$from) # 69 messy levels
levels(fb$to) # 96 messy levels

# location ships from -------------------------------------
fb$from <- as.character(fb$from)
fb$from <- stripWhitespace(fb$from)
levels(as.factor(fb$from))

# replace blanks
fb[1, 10]
fb$from[fb$from == " "] <- "none"

# combine duplicate misspellings
fb$from[fb$from == " undeclared "] <- " Undeclared "
fb$from[fb$from == " Undeclared;) "] <- " Undeclared "
fb$from[fb$from == " Undelcared;) "] <- " Undeclared "
fb$from[fb$from == " United States "] <- " USA "

# location ships to ---------------------------------------
fb$to <- as.character(fb$to)
fb$to <- stripWhitespace(fb$to)
levels(as.factor(fb$to))

# remove blanks
fb$to[is.na(fb$to)] <- "none"

# combine duplicates
fb$to[fb$to == "australia " ] <- "Australia "
fb$to[fb$to == "DOMESTIC ONLY " ] <- "Domestic Only "
fb$to[fb$to == "europe " ] <- "Europe "
fb$to[fb$to == "European Union " ] <- "Europe "
fb$to[fb$to == "everywhere but australia " ] <- "Everywhere except Australia "
fb$to[fb$to == "everywhere but Australia " ] <- "Everywhere except Australia "
fb$to[fb$to == "everywhere except Australia " ] <- "Everywhere except Australia "
fb$to[fb$to == "Everywhere but Australia " ] <- "Everywhere except Australia "
fb$to[fb$to == "Worldwide except Australia " ] <- "Everywhere except Australia "
fb$to[fb$to == "Worldwide except australia, finland " ] <- "Everyplace except Australia and Finland "

fb$to[fb$to == "Kind and Gentle people "] <- "Kind Hearts and Gentle people "
fb$to[fb$to == "Kind hearts and Gentle people "] <- "Kind Hearts and Gentle people "
fb$to[fb$to == "Knd Hearts and Gentle people "] <- "Kind Hearts and Gentle people "

fb$to[fb$to == "sweden "] <- "Sweden "

fb$to[fb$to == "U.S.A. "] <- "USA"
fb$to[fb$to == "United States "] <- "USA"
fb$to[fb$to == "United States. "] <- "USA"
fb$to[fb$to == " USA "] <- "USA"
fb$to[fb$to == "US " ] <- "USA"
fb$to[fb$to == "usa " ] <- "USA"
fb$to[fb$to == "USA " ] <- "USA"
fb$to[fb$to == "USA only "] <- "USA"
fb$to[fb$to == "USA Only "] <- "USA"
fb$to[fb$to == "Americana " ] <- "USA"

fb$to[fb$to == "Wolrdwide "] <- "Worldwide"
fb$to[fb$to == "World Wide "] <- "Worldwide"
fb$to[fb$to == "world "] <- "Worldwide"
fb$to[fb$to == "World "] <- "Worldwide"
fb$to[fb$to == "WORLD WIDE "] <- "Worldwide"
fb$to[fb$to == "worldwide "] <- "Worldwide"
fb$to[fb$to == "Worldwide "] <- "Worldwide"

fb$to[fb$to == "WorldWide "] <- "Worldwide"
fb$to[fb$to == "WORLDWIDE " ] <- "Worldwide"
fb$to[fb$to == "Worldwide/ Check my profile "] <- "Worldwide"
fb$to[fb$to == "WORLWIDE "] <- "Worldwide"

fb$to[fb$to == "EU, USA, Cancada "] <- "EU, USA, Canada "

levels(as.factor(fb$to))


# final check
fb$from <- as.factor(fb$from)
levels(fb$from) # 28
fb$to <- as.factor(fb$to)
levels(fb$to) # 25

write.csv(fb, file = "p-2014-01-fb-clean.csv", row.names = F)

# mosaic plots ----------------------------------------------------------------
library(vcd)
mosaic( ~ cat + from, data = fb, shade = TRUE)

# category by location
par(mar = c(12, 14, 8, 8), family = "FranklinGothicSSK", las = 2)
plot(fb$cat, fb$from, xlab = "", ylab = "", 
     main = "AgMarketplace: Category by Location")

# subcategory by location
par(mar = c(12, 14, 8, 8), family = "FranklinGothicSSK", las = 2)
plot(fb$subcat, fb$from, xlab = "", ylab = "", 
     main = "AgMarketplace: Category by Location")

fb$subcat <- as.character(fb$subcat)
fb$subcat[fb$subcat == "Ecstasy-MDMA"] <- "Ecstasy"
levels(as.factor(fb$subcat))
fb$subcat <- as.factor(fb$subcat)

plot(fb$subcat, fb$from, xlab = "", ylab = "", 
     main = "AgMarketplace: SubCategory by Location")

# subsubcategory by location
plot(fb$subsubcat, fb$from, xlab= "", ylab = "",
     main = "AgMarketplace: SubSubCategory by Location")

