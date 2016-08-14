# Agora Marketplace Analysis
# extract vendor names and listings

# load ------------------------------------------------------------------------

rm(list = ls())
# motherfucker, updated Rstudio and my installed packages are all gone.
# maybe that's a good thing.

pkg <- c("rvest", "magrittr", "tm", "tidyr", "ggplot2", "dplyr", "plyr")
install.packages(pkg)

library(rvest)
library(magrittr)
library(tm)
library(tidyr)

mainVendDir <- "~/GitHub/ag-Vendor/2014-02"
setwd(mainVendDir)

vlist <- list.files(path = mainVendDir, pattern = ".html", all.files = T, recursive = T)
# 16013 elements
head(vlist)
# [1] "2014-04-06/_drugs.inc_.html"
tail(vlist)
# [1] "2014-09-30/Weedland.html" 

# vendor products via table ---------------------------------------------------

vendorall <- data.frame()

for (i in 1:length(vlist)) {
  log <- read_html(vlist[i])
  
  pTab <- log %>%
    html_nodes("table.products-list") %>%
    html_table(header = T)
  
  pTab <- as.data.frame(pTab)
  
  pTab$date <- sub(" *\\/.*", "", vlist[i])
  
  for (j in 1:length(pTab)) {
    pTab$vendor <- paste(vlist[i])
  }
  
  vendorall <- rbind(vendorall, pTab)
}

# clean extracted dataframe ---------------------------------------------------

vendorall$Var.1 <- NULL
vendorall <- vendorall[c(4, 5, 1, 2, 3)]

vendorall$vendor <- gsub("/", "", vendorall$vendor)
vendorall$vendor <- gsub(".html", "", vendorall$vendor)
vendorall$vendor <- as.factor(vendorall$vendor)

colnames(vendorall) <- c("date", "vendor", "name", "price", "shipping")

vendorall <- separate(vendorall, shipping, into = c("from", "to"))
vendorall$from <- as.factor(vendorall$from)
vendorall$to <- as.factor(vendorall$to)

vendorall$name <- stripWhitespace(vendorall$name)
vendorall$name <- gsub(",", " ", vendorall$name)
vendorall$name <- as.factor(vendorall$name)

vendorall$price <- gsub(" BTC", "", vendorall$price)
vendorall$price <- as.double(vendorall$price)

write.csv(vendorall, file = "vendorall-2014-01.csv", row.names = F)

test <- read.csv("vendorall-2014-01.csv")
