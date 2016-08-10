# scrape - vendor
# harvesting information from vendor pages

library(rvest)
library(magrittr)
library(tm)
library(XML)
library(tidyr)

# vendor directory - single page ---------------------------------------------------

# set path to scraped files
vendDir <- "~/GitHub/agora/2014-01-01/vendor/3dames.html"

# parse html
vend01 <- read_html(vendDir)

# vendor name ---------------------------------------------
vendorvendor <- vend01 %>%
  html_nodes("strong") %>%
  html_text()

vendorvendor
# [1] "3dames"

# vendor bio ----------------------------------------------
bio <- vend01 %>%
  html_nodes(".vendorbio-description") %>%
  html_text(trim = T)

head(bio)
# [1] "New shop on Agora-INTRODUCTIONI have been providing 
# trusted service and high quality products on SR since the ......
# goes on through the PGP key

# PGP key -------------------------------------------------
pgp <- vend01 %>%
  html_nodes(".pgptoken") %>%
  html_text()

pgp
# [1] "-----BEGIN PGP PUBLIC KEY BLOCK-----mQINBFKXLp0BEAC6oQhlrwObUL ...

# feedback ------------------------------------------------

vendor_fb <- vend01 %>%
  html_nodes(".embedded-feedback-list") %>%
  html_text(trim = T)

# line breaks in html create extra blank space
vendor_fb <- stripWhitespace(vendor_fb)
vendor_fb
# [1] "Feedbacks: No feedbacks found."

# products of [this vendor] -------------------------------

# this is not as clean as creating a dataframe 
# from individual nodes of #product-list class

vendorProducts <- vend01 %>%
  html_nodes("table.products-list") %>%
  html_table(header = T)

vendorProducts <- as.data.frame(vendorProducts)
vendorProducts$Var.1 <- NULL
colnames(vendorProducts) <- c("Name", "Price", "Shipping")

vendorProducts <- separate(vendorProducts, Shipping, 
                           into  = c("Ship_From", "Ship_To"))

# product list - name, description, price, shipping ---------------------------

# name
vpListName <- vend01 %>%
  html_nodes("#product-list a") %>%
  html_text()

vpListName
#  [1] "Lorazepam (Ativan) 1 mg x 50 (free shipping)" 
# ....

# description preview
vpListDP <- vend01 %>%
  html_nodes(".description-preview") %>%
  html_text()

vpListDP
# [1] "Top-quality branded generic equivalent of Ativan. 100% correct dosage...

# price
vpListPrice <- vend01 %>%
  html_nodes("#product-list td") %>%
  extract2(7) %>%
  html_text()

vpListPrice
# [1] "0.05989857 BTC"

# ship from
vpListShipping <- vend01 %>%
  html_nodes(".column-name~ td+ td") %>%
  html_text(trim = T)

vpListShipping <- stripWhitespace(vpListShipping)
vpListShipping

# data frame of product listings ----------------------------------------------
vendorListings <- data.frame(vendor = vendorvendor, name = vpListName, 
                             description = vpListDP, price = vpListPrice,
                             shipping = vpListShipping)

vendorListings <- separate(vendorListings, shipping,
                           into = c("ship_from", "ship_to"))

write.table(vendorListings, 
            file = "~/GitHub/agora-data/AgScrape/3damesListings.csv",
            sep = ",", row.names = F)

ag_3dames <- read.csv("~/GitHub/agora-data/AgScrape/3damesListings.csv")
