# Agora Marketplace Analysis
# Product info extraction
# 2015-01-10

# load ------------------------------------------------------------------------

library(rvest)
library(magrittr)
library(tm)
library(tidyr)
library(dplyr)

# Vendor and Date extraction --------------------------------------------------

rm(list = ls())
pDir <- "~/GitHub/ag-Product/2015-01-10"
setwd(pDir)

# 21111
pList <- list.files(path = pDir, pattern = ".html", all.files = T, recursive = T)
p0115.10 <- data.frame()

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
    p0115.10 <- rbind(p0115.10, pTab)
  }
)

#        user  system elapsed 
#  1054.514   12.029 1071.745

pList[902] # d4AMWY4Mlk - aod-9604
pList[9223] # DjZrE3h0Xj - skuff THC concentrate
pList[16559] # tpSWqUeUzo - Hia Haschich
pList[18580] # xoRrmfEoM3 - how to cash out stolen cc's

# safety
write.csv(p0115.10, file = "p-0115-10-raw.csv", row.names = F)
p0115.10 <- read.csv("p-0115-10-raw.csv")

# clean extracted data
p0115.10 <- p0115.10[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(p0115.10) <- c("list", "date", "vendor", "product", 
                        "price", "cat", "feedback", "shipping")

p0115.10$vendor <- gsub("/vendor/", "", p0115.10$vendor)
p0115.10$vendor <- gsub("/user/", "", p0115.10$vendor)
p0115.10$vendor <- gsub("#", "", p0115.10$vendor)

p0115.10$shipping <- as.character(p0115.10$shipping)
p0115.10$shipping <- stripWhitespace(p0115.10$shipping)
p0115.10$shipping[p0115.10$shipping == " "] <- NA
is.na(p0115.10$shipping)

p0115.10 <- separate(p0115.10, shipping, c("from", "to"), sep = "To: ")
p0115.10$from <- gsub("From: ", "", p0115.10$from)

levels(as.factor(p0115.10$from)) # 58
levels(as.factor(p0115.10$to)) # 337

p0115.10$price <- gsub(" BTC", "", p0115.10$price)
p0115.10$price <- as.double(p0115.10$price)

write.csv(p0115.10, file = "p0115.10-c1.csv", row.names = F)

# extract subcategories -------------------------------------------------------

# subset out variables w/o a subcategory
levels(as.factor(p0115.10$cat))

p0115.10$cat <- as.character(p0115.10$cat)
p0115 <- subset(p0115.10,  p0115.10$cat != "Listings" & p0115.10$cat != "Jewelry"
                & p0115.10$cat != "Electronics" & p0115.10$cat != "Other")

# 21107 > 20523 > 
pList2 <- as.character(p0115$list)
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
#  247.173   8.854 256.499 

# bind subcategories
bind0115_10 <- dplyr::left_join(p0115.10, subcat, by = "list")
is.na(bind0115_10$pTab2)

bind0115_10 <- bind0115_10[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
colnames(bind0115_10) <- c("list", "date", "vendor", "product", "price", 
                           "cat", "subcat", "feedback", "from", "to")

p0115.10 <- bind0115_10

# safety
write.csv(p0115.10, file = "p-2015-01-10.csv", row.names = F)

# extract subsubcategories ----------------------------------------------------

# subset subsubcategories
levels(p0115.10$subcat)
p0115.10$subcat <- as.character(p0115.10$subcat)

# 21107 > 20523 > 15599 > 11166 
drugs0115.10 <- subset(p0115.10, p0115.10$cat == "Drugs")
drugs0115.10 <- subset(drugs0115.10, drugs0115.10$subcat != "Other" & 
                         drugs0115.10$subcat != "Weight loss" &
                         drugs0115.10$subcat != "Benzos" &
                         drugs0115.10$subcat != "Prescription" &
                         drugs0115.10$subcat != "RCs" &
                         drugs0115.10$subcat != "Steroids" &
                         drugs0115.10$subcat != "Methylone" &
                         drugs0115.10$subcat != "Ecstasy-MDMA" &
                         drugs0115.10$subcat != "Barbiturates")

# extract subsubcategories
levels(as.factor(drugs0115.10$subcat))

pList3 <- drugs0115.10$list
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

#      user  system elapsed 
#   128.406   1.942 130.353  

# bind sub-subcategories
bind0115_10b <- dplyr::left_join(p0115.10, subcat2, by = "list")
is.na(bind0115_10b$pTab3)

bind0115_10b  <- bind0115_10b [c(1, 2, 3, 4, 5, 6, 7, 11, 8, 9, 10)]
colnames(bind0115_10b) <- c("list", "date", "vendor", "product", "price", 
                            "cat", "subcat", "subsubcat", "feedback", "from", "to")

p0115.10 <- bind0115_10b

# final extracted data pre-arules/contigency table transformations
write.csv(p0115.10, file = "products-2015-01-10.csv", row.names = F)
test <- read.csv("products-2015-01-10.csv")
