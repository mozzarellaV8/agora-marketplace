# Agora Marketplace Analysis
# scrape - categories
# extracting data from a category page

library(rvest)
library(magrittr)
library(tm)
library(XML)

# set directory ---------------------------------------------------------------
catdir <- "~/GitHub/agora/2014-01-01/cat/PTixSEoFoF_a_1.html"

# parse html
cat <- read_html(catdir)

# category ------------------------------------------------
catcat <- cat %>%
  html_nodes(".topnav-element") %>%
  html_text()
catcat
# [1] "Drugs" "Other"
# main category followed by more specific category

# subcategory list ----------------------------------------
catdrill <- cat %>%
  html_nodes(".leftmenu-subelements a") %>%
  html_text(trim = T)

catdrill <- stripWhitespace(catdrill)
table(catdrill)
catdrill
# [1] "Benzos (50+)" "Cannabis (100+)" "Disassociatives (9)"
# [6] "Other (100+)"        "Prescription (20+)"  "Psychedelics (70+)"
# create table as data frame later

# main catgories list -------------------------------------
maincat <- cat %>%
  html_nodes(".leftmenu-element a") %>%
  html_text()
maincat
#  [1] "Drugs"                 "Benzos (50+)"          "Cannabis (100+)"
#  [6] "Opioids (30+)"         "Other (100+)"          "Prescription (20+)"
# main categories from index/front page

# category product list table -------------------------------------------------

# this is not as clean as creating a dataframe 
# from individual nodes of #product-list class.
# product name and description preview bleed together.

catTable <- cat %>%
  html_nodes("table.products-list") %>%
  html_table(header = T, trim = T)

catTable <- as.data.frame(catTable)
catTable$Var.1 <- NULL
colnames(catTable) <- c("Name", "Price", "Shipping", "Vendor")

# category product list individual tags ---------------------------------------

# Header ----------------------------------------
# might be easier without this
catHeader <- cat %>%
  html_nodes(".products-list-header") %>%
  html_text(trim = T)
catHeader <- stripWhitespace(catHeader)

# Name ------------------------------------------
catName <- cat %>%
  html_nodes(".column-name a") %>%
  html_text(trim = T)

catName
# [1] "5000g FDU-PB22"                                "500g 5F-SDB-006"                              
# [3] "25g AB-PINACA"                                 "25g 5F-AB-PINACA" 

# Description Preview ---------------------------
catDescription <- cat %>%
  html_nodes(".description-preview") %>%
  html_text(trim = T)
catDescription
# [1] "CAS: 432023-23-2 Formula: C26H18FNO2 Molecular weight: 
# 395.4 Compound purity: > 99.7% Related substance: Total Impurities(%) ≤ 0.3% 
# Appearance: White powder IUPAC: naphthalen- 1- yl 1- (4- fluo ..." 

# Price -----------------------------------------
catPrice <- cat %>%
  html_nodes(".products-list td") %>%
  html_text()
catPrice
# [1] "15.19056015 BTC"

# or - draw it from html_table
catPrice <- catTable$Price
catPrice
# [1] "15.19056015 BTC" "2.11582802 BTC"  "0.35263800 BTC"
# [8] "1.49871151 BTC"  "0.22921470 BTC"  "0.89922690 BTC"

# Shipping --------------------------------------
catShip <- cat %>%
  html_nodes(".column-name~ td+ td") %>%
  html_nodes(xpath = "//td[((count(preceding-sibling::*) + 1) = 4)]") %>%
  html_text(trim = T)
catShip <- stripWhitespace(catShip)

# this solution to the parent in char vector may not be 
# general enough for all pages - CHECK.
catShip <- catShip[3:32]

# Vendor ----------------------------------------
catVendor <- cat %>%
  html_nodes("a.gen-user-link") %>%
  html_attr("href")
catVendor <- gsub("/vendor/", "", catVendor)
catVendor
# [1] "jackiestovalls" "jackiestovalls"

# Vendor Rating ---------------------------------
catVR <- cat %>%
  html_nodes(".gen-user-ratings") %>%
  html_text()
catVR
#  [1] " [0 deals]" " [0 deals]"

# data frame of extracted info ------------------------------------------------

library(tidyr)

catList <- data.frame(vendor = catVendor, rating = catVR, name = catName, 
                      description = catDescription, price = catPrice, 
                      shipping = catShip)

catList <- separate(catList, shipping, into = c("ship_from", "ship_to"))


