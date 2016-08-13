# Agora Marketplace Analysis
# extract vendor names and listings

# load ------------------------------------------------------------------------

library(rvest)
library(magrittr)
library(tm)
library(tidyr)

mainVendDir <- "~/GitHub/ag-Vendor/2014-01-01"
setwd(mainVendDir)

vendlist <- list.files(path = mainVendDir, pattern = ".html", all.files = T)

# Date and vendors Name ------------------------------------------------------

# this method is OK. But below 'product list table' method is a little more
# efficient and provides more data.

# create blank frame
vendors <- data.frame(stringsAsFactors = F)
rm(cat)

# extract main vendors
for (i in 1:length(vendlist)) {
  
  log <- read_html(vendlist[i])
  
  cat <- log %>%
    html_nodes("#product-list a") %>%
    html_text()
  
  cat <- data.frame(cat)
  
  for (j in 1:length(cat)) {
    cat$vendname <- paste(vendlist[i])
  }
  
  vendors <- rbind(vendors, cat)
}

vendors$vendname <- gsub(".html", "", vendors$vendname)
vendors <- vendors[c(2, 1)]
colnames(vendors) <- c("vendor", "product")

write.table(vendors, file = "2014-01-01-vendors.csv", sep = ",", row.names = F)

# product list table  ---------------------------------------------------------

plt <- data.frame()

for (i in 1:length(vendlist)) {
  log <- read_html(vendlist[i])
  
  pTab <- log %>%
    html_nodes("table.products-list") %>%
    html_table(header = T)
  
  pTab <- as.data.frame(pTab)
  
  for (j in 1:length(pTab)) {
    pTab$vendor <- paste(vendlist[i])
  }
  
  plt <- rbind(plt, pTab)
}

plt$Var.1 <- NULL
plt <- plt[c(4, 1, 2, 3)]
plt$vendor <- gsub(".html", "", plt$vendor)
colnames(plt) <- c("vendor", "name", "price", "shipping")

plt <- separate(plt, shipping, into = c("from", "to"))

plt$name <- stripWhitespace(plt$name)
plt$name <- gsub(",", " ", plt$name)

plt$vendor <- as.factor(plt$vendor)
plt$name <- as.factor(plt$name)
plt$from <- as.factor(plt$from)
plt$to <- as.factor(plt$to)

plt$price <- gsub(" BTC", "", plt$price)
plt$price <- as.double(plt$price)

write.csv(plt, file = "plt.csv", row.names = F)

test <- read.csv("plt.csv")
