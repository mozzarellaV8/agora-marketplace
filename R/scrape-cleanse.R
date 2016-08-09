# scrape-clean v1
# getting the information out
# with rvest

library("rvest")

# p directory - single page ---------------------------------------------------

# set path to scraped files
datDir <- "~/GitHub/agora/2014-01-01/p/a1BkWEipyk"

# test
agora01 <- read_html(datDir)

# extract product name/listing headline -------------------
title <- agora01 %>%
  html_nodes("title") %>%
  html_text()

title
# [1] "Secret of Methamphetamine Manufacture 8th Edition"

titleH1 <- agora01 %>%
  html_nodes("#single-product h1") %>%
  html_text()
titleH1
# [1] "Secret of Methamphetamine Manufacture 8th Edition"

# extract description -------------------------------------

# selector for html_nodes() is not specific enough. 
# it's possible to harvest the whole listing 
# then separate out with regex later.
# but there are no css selectors for the text description
# might be an xml tag. 

description <- agora01 %>%
  html_nodes("#single-product") %>%
  html_text()

description

# extract price -------------------------------------------
price <- agora01 %>%
  html_nodes(".product-page-price") %>%
  html_text()

price
# [1] "0.00266844 BTC"

# vendor --------------------------------------------------
vendor <- agora01 %>%
  html_nodes("a.gen-user-link ") %>%
  html_attr("href")

vendor
# [1] "/vendor/passman"

# remove the dir path in front of vendor name
vendor <- gsub("/vendor/", "", vendor)
vendor

# ratings -------------------------------------------------
rating <- agora01 %>%
  html_nodes(".gen-user-ratings") %>%
  html_text()
 
rating
# [1] "  5.0/5, 15~25 deals"

# ships from ----------------------------------------------
ship_from <- agora01 %>%
  html_nodes(".product-page-ships") %>%
  html_text()

ship_from

# data frame ------------------------------------------------------------------

agora <- data.frame(title = title, price = price, description = description, 
                    vendor = vendor, rating = rating)
