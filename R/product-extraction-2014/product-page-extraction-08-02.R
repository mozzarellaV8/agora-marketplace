# Agora Marketplace Analysis
# vendor feedback extraction by month
# 2014-08-02

# load ------------------------------------------------------------------------

library(rvest)
library(magrittr)
library(tm)
library(tidyr)
library(dplyr)

# set directory

rm(list = ls())

pDir <- "~/GitHub/ag-Product/2014-08-02"
setwd(pDir)

# Vendor and Date extraction --------------------------------------------------

# 9456
pList <- list.files(path = pDir, pattern = ".html", all.files = T, recursive = T)
p0814.02 <- data.frame()

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
    
    pTab$list <- pList[i]
    p0814.02 <- rbind(p0814.02, pTab)
  }
)

pList[356] # kXzmGhXLe8 - no info - blank page
pList[7993] # xS0R0SAEP1 - incomplete crawl

#    user  system elapsed 
# 417.815   1.992 420.357

# safety
write.csv(p0814.02, file = "p-0814-02-raw.csv", row.names = F)

# clean extracted data
p0814.02 <- p0814.02[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(p0814.02) <- c("list", "date", "vendor", "product", 
                        "price", "cat", "feedback", "shipping")

p0814.02$vendor <- gsub("/vendor/", "", p0814.02$vendor)
p0814.02$vendor <- gsub("#", "", p0814.02$vendor)

p0814.02$shipping <- as.character(p0814.02$shipping)
p0814.02$shipping <- stripWhitespace(p0814.02$shipping)
p0814.02$shipping[p0814.02$shipping == " "] <- NA
is.na(p0814.02$shipping)

p0814.02 <- separate(p0814.02, shipping, c("from", "to"), sep = "To: ")
p0814.02$from <- gsub("From: ", "", p0814.02$from)

levels(as.factor(p0814.02$from)) # 62
levels(as.factor(p0814.02$to)) # 256

p0814.02$price <- gsub(" BTC", "", p0814.02$price)
p0814.02$price <- as.double(p0814.02$price)

write.csv(p0814.02, file = "p0814.02-c1.csv", row.names = F)

# extract subcategories -------------------------------------------------------

# subset out variables w/o a subcategory
levels(as.factor(p0814.02$cat))

p0814.02$cat <- as.character(p0814.02$cat)
p0814 <- subset(p0814.02,  p0814.02$cat != "Listings" & p0814.02$cat != "Jewelry"
                & p0814.02$cat != "Electronics" & p0814.02$cat != "Other")

# 9456 > 9222
pList2 <- as.character(p0814$list)
subcat <- data.frame(stringsAsFactors = F)

system.time(
  for (i in 1:length(pList2)) {
    pLog2 <- read_html(pList2[i])
    
    pTab2 <- pLog2 %>%
      html_nodes(".topnav-element a") %>%
      extract2(2) %>%
      html_text()
    
    pTab2 <- as.data.frame(pTab2)
    pTab2$list <- pList2[i]
    subcat <- rbind(subcat, pTab2)
  })

#     user  system elapsed 
#   98.980   1.191 100.200  

# bind subcategories
bind0814_02 <- dplyr::left_join(p0814.02, subcat, by = "list")
is.na(bind0814_02$pTab2)

bind0814_02 <- bind0814_02[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
colnames(bind0814_02) <- c("list", "date", "vendor", "product", "price", 
                           "cat", "subcat", "feedback", "from", "to")


# safety
write.csv(bind0814_02, file = "p-2014-08-02.csv", row.names = F)

# extract subsubcategories ----------------------------------------------------

# subset subsubcategories
levels(bind0814_02$subcat)
bind0814_02$subcat <- as.character(bind0814_02$subcat)

# 9456 > 9222 > 7096 > 4932
drugs0814.02 <- subset(bind0814_02, bind0814_02$cat == "Drugs")
drugs0814.02 <- subset(drugs0814.02, drugs0814.02$subcat != "Other" & 
                         drugs0814.02$subcat != "Weight loss" &
                         drugs0814.02$subcat != "Benzos" &
                         drugs0814.02$subcat != "Prescription" &
                         drugs0814.02$subcat != "RCs" &
                         drugs0814.02$subcat != "Steroids" &
                         drugs0814.02$subcat != "Methylone" &
                         drugs0814.02$subcat != "Opioids" &
                         drugs0814.02$subcat != "Ecstasy-MDMA" &
                         drugs0814.02$subcat != "Barbiturates")

# extract subsubcategories
levels(as.factor(drugs0814.02$subcat))

pList3 <- drugs0814.02$list
subcat2 <- data.frame()

system.time(
  for (i in 1:length(pList3)) {
    pLog3 <- read_html(pList3[i])
    
    pTab3 <- pLog3 %>%
      html_nodes(".topnav-element a") %>%
      extract2(3) %>%
      html_text()
    
    pTab3 <- as.data.frame(pTab3)
    pTab3$list <- pList3[i]
    subcat2 <- rbind(subcat2, pTab3)
  })

#    user  system elapsed 
#  51.084   0.421  51.497   

# bind sub-subcategories
bind0814_02b <- dplyr::left_join(bind0814_02, subcat2, by = "list")
is.na(bind0814_02b$pTab3)

bind0814_02b  <- bind0814_02b[c(1, 2, 3, 4, 5, 6, 7, 11, 8, 9, 10)]
colnames(bind0814_02b ) <- c("list", "date", "vendor", "product", "price", 
                             "cat", "subcat", "subsubcat", "feedback", "from", "to")

# final extracted data pre-arules/contigency table transformations
write.csv(bind0814_02b, file = "products-2014-08-02.csv", row.names = F)
test <- read.csv("products-2014-08-02.csv")

