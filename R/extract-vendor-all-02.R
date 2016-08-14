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

# create list of files to extract data from -----------------------------------

vlist <- list.files(path = mainVendDir, pattern = ".html", all.files = T, recursive = T)
# 16011 elements
head(vlist)
# [1] "2014-04-06/_drugs.inc_.html"
tail(vlist)
# [1] "2014-09-30/Weedland.html" 

# inspect list for date discrepancies
vlistTest <- as.data.frame(vlist)

# test on single page ---------------------------------------------------------
vlist[108]
# [1] "2014-04-06/_drugs.inc_.html"

# read in html
test <- read_html(vlist[108])

# extract table of vendor's listings
vname <- test %>%
  html_nodes("table.products-list") %>%
  html_table(header = T)

# create data frame, add date of listing and vendor name as columns
vname <- as.data.frame(vname)
vname$date <- paste(sub(" *\\/.*", "", vlist[108]))

vname$vendor <- test %>%
  html_nodes("#middlestuff strong") %>%
  extract2(1) %>%
  html_text()


# vendor products via table ---------------------------------------------------

vendorall <- data.frame(stringsAsFactors = F)

for (i in 1:length(vlist)) {
  log <- read_html(vlist[i])
  
  pTab <- log %>%
    html_nodes("table.products-list") %>%
    html_table(header = T)
  
  pTab <- as.data.frame(pTab)
  pTab$date <- sub(" *\\/.*", "", vlist[i])
  
  pTab$vendor <- log %>%
    html_nodes("#middlestuff strong") %>%
    extract2(1) %>%
    html_text()
  
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
