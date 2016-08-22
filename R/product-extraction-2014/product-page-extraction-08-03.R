# Agora Marketplace Analysis
# vendor feedback extraction by month
# 2014-08-03

# load ------------------------------------------------------------------------

library(rvest)
library(magrittr)
library(tm)
library(tidyr)
library(dplyr)

rm(list = ls())

# set directory
pDir <- "~/GitHub/ag-Product/2014-08-03"
setwd(pDir)

# Vendor and Date extraction --------------------------------------------------

# 10842
pList <- list.files(path = pDir, pattern = ".html", all.files = T, recursive = T)
p0814.03 <- data.frame()

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
    p0814.03 <- rbind(p0814.03, pTab)
  }
)

#    user  system elapsed 
# 492.111   2.753 496.098 

pList[9436] # ti1TbBaNNV - incomplete crawl - imputed with previous listing

# safety
write.csv(p0814.03, file = "p-0814-03-raw.csv", row.names = F)
test_p0814.03 <- read.csv("p-0814-03-raw.csv")

# clean extracted data
p0814.03 <- p0814.03[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(p0814.03) <- c("list", "date", "vendor", "product", 
                        "price", "cat", "feedback", "shipping")

p0814.03$vendor <- gsub("/vendor/", "", p0814.03$vendor)
p0814.03$vendor <- gsub("#", "", p0814.03$vendor)

p0814.03$shipping <- as.character(p0814.03$shipping)
p0814.03$shipping <- stripWhitespace(p0814.03$shipping)
p0814.03$shipping[p0814.03$shipping == " "] <- NA
is.na(p0814.03$shipping)

p0814.03 <- separate(p0814.03, shipping, c("from", "to"), sep = "To: ")
p0814.03$from <- gsub("From: ", "", p0814.03$from)

levels(as.factor(p0814.03$from)) # 51
levels(as.factor(p0814.03$to)) # 204

p0814.03$price <- gsub(" BTC", "", p0814.03$price)
p0814.03$price <- as.double(p0814.03$price)

write.csv(p0814.03, file = "p0814.03-c1.csv", row.names = F)

# extract subcategories -------------------------------------------------------

# subset out variables w/o a subcategory
levels(as.factor(p0814.03$cat))

p0814.03$cat <- as.character(p0814.03$cat)
p0814 <- subset(p0814.03,  p0814.03$cat != "Listings" & p0814.03$cat != "Jewelry"
                & p0814.03$cat != "Electronics" & p0814.03$cat != "Other")

# 10842 > 10071
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
#  108.625   1.482 110.119   

# bind subcategories
bind0814_03 <- dplyr::left_join(p0814.03, subcat, by = "list")
is.na(bind0814_03$pTab2)

bind0814_03 <- bind0814_03[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
colnames(bind0814_03) <- c("list", "date", "vendor", "product", "price", 
                           "cat", "subcat", "feedback", "from", "to")

p0814.03 <- bind0814_03

# safety
write.csv(p0814.03, file = "p-2014-08-03.csv", row.names = F)

# extract subsubcategories ----------------------------------------------------

# subset subsubcategories
levels(p0814.03$subcat)
p0814.03$subcat <- as.character(p0814.03$subcat)

# 10842 > 10071 > 5088 > 2988
drugs0814.03 <- subset(p0814.03, p0814.03$cat == "Drugs")
drugs0814.03 <- subset(drugs0814.03, drugs0814.03$subcat != "Other" & 
                         drugs0814.03$subcat != "Weight loss" &
                         drugs0814.03$subcat != "Benzos" &
                         drugs0814.03$subcat != "Prescription" &
                         drugs0814.03$subcat != "RCs" &
                         drugs0814.03$subcat != "Steroids" &
                         drugs0814.03$subcat != "Methylone" &
                         drugs0814.03$subcat != "Opioids" &
                         drugs0814.03$subcat != "Ecstasy-MDMA" &
                         drugs0814.03$subcat != "Barbiturates")

# extract subsubcategories
levels(as.factor(drugs0814.03$subcat))

pList3 <- drugs0814.03$list
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
#  34.386   0.237  34.996   

# bind sub-subcategories
bind0814_03b <- dplyr::left_join(p0814.03, subcat2, by = "list")
is.na(bind0814_03b$pTab3)

bind0814_03b  <- bind0814_03b [c(1, 2, 3, 4, 5, 6, 7, 11, 8, 9, 10)]
colnames(bind0814_03b) <- c("list", "date", "vendor", "product", "price", 
                             "cat", "subcat", "subsubcat", "feedback", "from", "to")

p0814.03 <- bind0814_03b

# final extracted data pre-arules/contigency table transformations
write.csv(p0814.03, file = "products-2014-08-03.csv", row.names = F)
test <- read.csv("products-2014-08-03.csv")

