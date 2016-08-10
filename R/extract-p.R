# Agora Marketplace Analysis
# scrape - p directory pages
# getting the information out
# with rvest

library(rvest)
library(tm)
library(XML)
library(magrittr)

# p directory - single page ---------------------------------------------------

# set path to scraped files
datDir <- "~/GitHub/agora/2014-01-01/p/a1BkWEipyk.html"

# this works
agora01 <- read_html(datDir)
# this doesn't
ag01 <- htmlParse(datDir, ignoreBlanks = T, isHTML = T, trim = T)

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

# feedback ------------------------------------------------
feedback <- agora01 %>%
  html_nodes(".embedded-feedback-list") %>%
  html_text()

feedback <- stripWhitespace(feedback)
feedback
# [1] " Feedbacks: No feedbacks found. "

# data frame ------------------------------------------------------------------

agora <- data.frame(price = price, title = title, description = description, 
                    vendor = vendor, rating = rating, ship_from = ship_from,
                    feedback = feedback)

