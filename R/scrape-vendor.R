# scrape - vendor
# harvesting information from vendor pages

library(rvest)
library(magrittr)
library(tm)
library(XML)

# p directory - single page ---------------------------------------------------

# set path to scraped files
datDir <- "~/GitHub/agora/2014-01-01/p/a1BkWEipyk.html"

# test
agora01 <- read_html(datDir)
