# scrape-clean v1
# getting the information out
# with rvest

library(rvest)
library(tm)

# p directory - single page ---------------------------------------------------

# set path to scraped files
datDir02 <- "~/GitHub/agora/2014-01-01/p/a37plcuKol"

# test
agora02 <- read_html(datDir02)

# extract product name/listing headline -------------------
title02 <- agora02 %>%
  html_nodes("title") %>%
  html_text()

title02
# [1] "10 regular seeds - Skunk Kush - pain relief"

# extract description -------------------------------------

# selector for html_nodes() is not specific enough. 
# it's possible to harvest the whole listing 
# then separate out with regex later.
# but there are no css selectors for the text description
# might be an xml tag. 

description02 <- agora02 %>%
  html_nodes("#single-product") %>%
  html_text()

description

# extract price -------------------------------------------
price02 <- agora02 %>%
  html_nodes(".product-page-price") %>%
  html_text()

price02
# [1] "0.05353963 BTC"

# vendor --------------------------------------------------
vendor02 <- agora02 %>%
  html_nodes("a.gen-user-link ") %>%
  html_attr("href")

vendor02
# [1] "/vendor/budbrother"

# remove the dir path in front of vendor name
vendor02 <- gsub("/vendor/", "", vendor02)
vendor02
# [1] "budbrother"

# ratings -------------------------------------------------
rating02 <- agora02 %>%
  html_nodes(".gen-user-ratings") %>%
  html_text()

rating02
# [1] " [0 deals]"

# ships from ----------------------------------------------
ship_from02 <- agora02 %>%
  html_nodes(".product-page-ships") %>%
  html_text()

ship_from02
# [1] "\r\n      Ships from  Australia\r\n    \r\n    "

# clean up info
ship_from02 <- gsub("Ships from", "", ship_from02)
ship_from02 <- stripWhitespace(ship_from02)
ship_from02
# [1] " Australia "

# data frame ------------------------------------------------------------------
agora <- data.frame(title = title, price = price, description = description, 
                    vendor = vendor, rating = rating, ship_from = ship_from)
