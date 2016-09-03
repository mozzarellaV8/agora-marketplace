# Agora Marketplace Analysis
# product extraction functions
# single folder loops

library(rvest)
library(tm)
library(XML)
library(magrittr)


# single folder trial ---------------------------------------------------------

# swtich to data.table object
sessionInfo()
singleDir <- list.files("~/GitHub/ag-Product/2014-01-01", pattern = ".html", 
                        all.files = T, recursive = T)
length(unique(singleDir))
# [1] 1076

# Date and Product Name -------------------------------------------------------
setwd("~/GitHub/ag-Product/2014-01-01")
nameframe <- data.frame(stringsAsFactors = F)

system.time(
for (i in 1:length(singleDir)) {
  log <- read_html(singleDir[i])
  name <- log %>%
    html_nodes("title") %>%
    html_text()
  nameframe[i, 1] <- "2014-01-01"
  nameframe[i, 2] <- name
}
)

#    user  system elapsed 
#   6.028   0.107   6.161 

colnames(nameframe) <- c("date", "product.name")
write.table(nameframe, file = "~/GitHub/agora-data/AgScrape/2014-01-01-pName.csv",
            sep = ",", row.names = F)

# description frame -----------------------------------------------------------
singleDir <- list.files("~/GitHub/ag-Product/2014-01-01", pattern = ".html", 
                        all.files = T, recursive = T)
length(unique(singleDir))
# [1] 1076

# empty data.frame to store descriptions
dframe <- data.frame(stringsAsFactors = F)

system.time(
  for (i in 1:length(singleDir)) {
    log <- read_html(singleDir[i])
    description <- log %>%
      html_nodes("#single-product.nofirstmargin") %>%
      html_text()
    dframe[i, 1] <- description
  }
)
#    user  system elapsed 
#  10.799   0.137  10.975

colnames(dframe) <- "description"
write.table(dframe, file = "~/GitHub/agora-data/AgScrape/2014-01-01-pDescript.csv",
            sep = ",", row.names = F)

pFrame <- cbind(nameframe, dframe)
# write.table(nameframe, file = "~/GitHub/agora-data/AgScrape/2014-01-01-pFrame.csv",
#             sep = ",", row.names = F)

# price frame -----------------------------------------------------------------

price <- data.frame(stringsAsFactors = F)

system.time(
  for (i in 1:length(singleDir)) {
    log <- read_html(singleDir[i])
    pr <- log %>%
      html_nodes(".product-page-price") %>%
      html_text()
    price[i, 1] <- pr
  }
)
#    user  system elapsed 
#  10.089   0.088  10.221

colnames(price) <- "BTC"
write.table(price, file = "~/GitHub/agora-data/AgScrape/2014-01-01-pBTC.csv",
            sep = ",", row.names = F)

pFrame <- cbind(nameframe, dframe, price)
write.table(pFrame, file = "~/GitHub/agora-data/AgScrape/2014-01-01-pFrame.csv",
             sep = ",", row.names = F)


# vendor name -----------------------------------------------------------------

vendor <- data.frame(stringsAsFactors = F)

system.time(
  for (i in 1:length(singleDir)) {
    log <- read_html(singleDir[i])
    vn <- log %>%
      html_nodes("a.gen-user-link ") %>%
      html_attr("href")
    vn <- gsub("/vendor/", "", vn)
    vendor[i, 1] <- vn
  }
)
#   user  system elapsed 
# 10.819   0.092  10.938

colnames(vendor) <- "vendor"
write.table(vendor, file = "~/GitHub/agora-data/AgScrape/2014-01-01-pVendor.csv",
            sep = ",", row.names = F)

vendor <- read.csv("~/GitHub/agora-data/AgScrape/2014-01-01-pVendor.csv")

pFrame <- cbind(nameframe, dframe, price, vendor)

# vendor rating ---------------------------------------------------------------

# skip this.
rating <- data.frame(stringsAsFactors = F)

for (i in 1:length(singleDir)) {
  log <- read_html(singleDir[i])
  rtg <- log %>%
    html_nodes(".gen-user-ratings") %>%
    html_text()
  rating <- rbind(rating, rtg)
}

# ship_from ------------------------------------------------------------------- 

shipping <- data.frame(stringsAsFactors = F)

for (i in 1:length(singleDir)) {
  log <- read_html(singleDir[i])
  shp <- log %>%
    html_nodes(".product-page-ships") %>%
    html_text()
  shipping[i, 1] <- shp
}

write.table(shipping, file = "~/GitHub/agora-data/AgScrape/pShip.csv",
            sep = ",", row.names = F)

library(tidyr)
shipping <- stripWhitespace(shipping$V1)
shipping <- as.data.frame(shipping)
shipping$shipping <- as.character(shipping$shipping)
shipping[1, 1]
# " "

shippingV2 <- separate(shipping, shipping, into = c("ship.from", "ship.to"),
                       sep = grep("Ships", shipping$shipping))
# this looks cool to print, but not to work with:
write.table(shippingV2, file = "~/GitHub/agora-data/AgScrape/pShipV2.csv",
            sep = ",", row.names = F)

shippingV3 <- separate(shipping, shipping, into = c("ship_from", "ship_to"),
                       sep = "to")
shippingV3$ship_from <- gsub("Ships ", "", shippingV3$ship_from)
shippingV3$ship_from <- gsub("from ", "", shippingV3$ship_from)
shippingV3$ship_from[shippingV3$ship_from == " "] <- NA

# this is the one - if it wrote correctly:
write.table(shippingV3, file = "~/GitHub/agora-data/AgScrape/pShipV3.csv",
            sep = ",", row.names = F)


# final dataframe -------------------------------------------------------------
pFrame <- cbind(nameframe, price, vendor, shippingV3)
pFrame$BTC <- gsub(" BTC", "", pFrame$BTC)
pFrame$BTC <- as.double(pFrame$BTC)

write.table(pFrame, file = "~/GitHub/agora-data/AgScrape/pFrame.csv",
            sep = ",", row.names = F)
