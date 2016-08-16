# Agora Marketplace Analysis
# vendor feedback extraction by month

# load ------------------------------------------------------------------------

library(rvest)
library(magrittr)
library(tm)
library(tidyr)

# set directory

rm(list = ls())

pDir <- "~/GitHub/ag-Product/2014-01-01"
setwd(pDir)

# 1082 files
pList <- list.files(path = pDir, pattern = ".html", all.files = T, recursive = T)
products2014_01_01 <- data.frame()

system.time(
  for (i in 1:length(pList)) {
    pLog <- read_html(pList[i])
    
    prod <- data.frame()
    
    vendor <- pLog %>%
      html_nodes("a.gen-user-link ") %>% 
      html_attr("href")
    vendor <- gsub("/vendor/", "", vendor)
    
    prod$date <- sub(" *\\__.*", "", pDir[i])
    
    prod$product <- pLog %>%
      html_nodes("title") %>%
      html_text()
    
    prod$category <- pLog %>%
      html_nodes(".topnav-element a") %>%
      extract2(1) %>%
      html_text()
    
    prod$subcat <- pLog %>%
      html_nodes(".topnav-element a") %>%
      extract2(2) %>%
      html_text()
    
    prod$price <- pLog %>%
      html_nodes(".product-page-price") %>%
      html_text()
    prod$price <- gsub(" BTC", "", price)
    
    prod$description <- pLog %>%
      html_nodes("#single-product") %>%
      html_text()
    prod$description <- stripWhitespace(prod$description)
    
    prod$rating <- pLog %>%
      html_nodes(".gen-user-ratings") %>%
      html_text(trim = T)
    
    prod$feedback <- pLog%>%
      html_nodes(".embedded-feedback-list") %>%
      html_text()
    prod$feedback <- stripWhitespace(prod$feedback)
    
    prod$shipping <- pLog %>%
      html_nodes(".product-page-ships") %>%
      html_text()
    
    products2014_01_01 <- rbind(products2014_01_01, prod)
  }
)
