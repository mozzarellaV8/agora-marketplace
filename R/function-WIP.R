# rvest function
library(rvest)

datDir <- "~/GitHub/x/2014-01-01/p/"
x <- read_html(datDir)

# extract relevant info -------------------------------------------------------

pReader <- function(x) {

datDir <- "~/GitHub/x/2014-01-01/p/"
x <- read_html(datDir)
title <- x %>%
  html_nodes("title") %>%
  html_text()
description <- x %>%
  html_nodes("#single-product") %>%
  html_text()
price <- x %>%
  html_nodes(".product-page-price") %>%
  html_text()
vendor <- x %>%
  html_nodes("a.gen-user-link ") %>%
  html_attr("href")
vendor <- gsub("/vendor/", "", vendor)
rating <- x %>%
  html_nodes(".gen-user-ratings") %>%
  html_text()
ship_from <- x %>%
  html_nodes(".product-page-ships") %>%
  html_text()

result <- data.frame(title = title, price = price, description = description, 
                     vendor = vendor, rating = rating)

}