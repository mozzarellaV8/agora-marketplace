# Agora Marketplace Analysis
# vendor feedback extraction by month

# load ------------------------------------------------------------------------

library(rvest)
library(magrittr)
library(tm)
library(tidyr)

# set directory

rm(list = ls())

pDir <- "~/GitHub/ag-Product/2014-03-01"
setwd(pDir)

# Vendor and Date extraction --------------------------------------------------
pList <- list.files(path = pDir, pattern = ".html", all.files = T, recursive = T)
products0314 <- data.frame()

system.time(
  for (i in 1:length(pList)) {
    pLog <- read_html(pList[i])
    
    pTab <- pLog %>%
      html_nodes("a.gen-user-link ") %>% 
      html_attr("href")
    
    pTab <- as.data.frame(pTab)
    pTab$date <- sub(" *\\__.*", "", pList[i])
    
    pTab$product <- pLog %>%
      html_nodes("title") %>%
      html_text()
    
    pTab$price <- pLog %>%
      html_nodes(".product-page-price") %>%
      html_text()
    
    pTab$cat <- pLog %>%
      html_nodes(".topnav-element a") %>%
      extract2(1) %>%
      html_text()
    
    pTab$feedback <- pLog%>%
      html_nodes(".embedded-feedback-list") %>%
      html_text()
    
    pTab$shipping <- pLog %>%
      html_nodes(".product-page-ships") %>%
      html_text()
    
    pTab$List <- pList[i]
    
    products0314 <- rbind(products0314, pTab)
  }
)

#    user  system elapsed 
# 272.863   0.907 273.840

# clean extracted data --------------------------------------------------------

products0314 <- products0314[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(products0314) <- c("list", "date", "vendor", "product", "price", "cat", "feedback", "shipping")

products0314$vendor <- gsub("/vendor/", "", products0314$vendor)
products0314$vendor <- gsub("#", "", products0314$vendor)

products0314 <- separate(products0314, shipping, c("from", "to"), sep = "To: ")
products0314$from <- gsub("From: ", "", products0314$from)
products0314$from[products0314$from == ""] <- NA # figure this out.

products0314$price <- gsub(" BTC", "", products0314$price)
products0314$price <- as.double(products0314$price)

write.csv(products0314, file = "products0314-sample.csv", row.names = F)
products0314 <- read.csv("products0314-sample.csv")

levels(as.factor(products0314$cat))

# extract subcategories -------------------------------------------------------

# subset out products without subcategories
p0314s <- products0314
p0314s$cat <- as.character(p0314s$cat)
p0314s <- subset(products0314, products0314$cat != "Listings" & products0314$cat != "Electronics")


# subcategory extraction
pList2 <- p0314s$list

subcat <- data.frame()
for (i in 1:length(pList2)) {
  pLog2 <- read_html(pList2[i])
  
  pTab2 <- pLog2 %>%
    html_nodes(".topnav-element a") %>%
    extract2(2) %>%
    html_text()
  
  pTab2 <- as.data.frame(pTab2)
  pTab2$list <- pList2[i]
  subcat <- rbind(subcat, pTab2)
}

# p0314s <- cbind(p0314s, subcat)
# p0314s <- p0314s[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
# colnames(p0314s) <- c("list", "date", "vendor", "product", "price", 
#                       "cat", "subcat", "feedback", "from", "to")

# write.csv(p0314s, file = "p0314s.csv", row.names = F)

# rbind "Listings" and "Electronics" back onto p0314s

test0314 <- dplyr::left_join(products0314, subcat, by = "list")
is.na(test0314$pTab2)

test0314 <- test0314[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
colnames(test0314) <- c("list", "date", "vendor", "product", "price", 
                        "cat", "subcat", "feedback", "from", "to")

products0314 <- test0314
write.csv(products0314, file = "p-2014-03-01.csv", row.names = F)
test <- read.csv("p-2014-03-01.csv")

