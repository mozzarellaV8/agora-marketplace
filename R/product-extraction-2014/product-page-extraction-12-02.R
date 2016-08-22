# Agora Marketplace Analysis
# Product info extraction
# 2014-12-02

# load ------------------------------------------------------------------------

library(rvest)
library(magrittr)
library(tm)
library(tidyr)
library(dplyr)

# Vendor and Date extraction --------------------------------------------------

rm(list = ls())
pDir <- "~/GitHub/ag-Product/2014-12-02"
setwd(pDir)

# 22304
pList <- list.files(path = pDir, pattern = ".html", all.files = T, recursive = T)
p1214.02 <- data.frame()

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
    p1214.02 <- rbind(p1214.02, pTab)
  }
)

#       user  system elapsed 
# 1081.672   20.570 1110.399

pList[16332] # x107yWzlYU - blank
pList[18359] # BJMeGNjhPM - sharpie pen - incomplete, imputed
pList[20070] # mfBVdJT90h - forensics - incomplete, imputed

# safety
write.csv(p1214.02, file = "p-1214-02-raw.csv", row.names = F)
p1214.02 <- read.csv("p-1214-02-raw.csv")

# clean extracted data
p1214.02 <- p1214.02[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(p1214.02) <- c("list", "date", "vendor", "product", 
                        "price", "cat", "feedback", "shipping")

p1214.02$vendor <- gsub("/vendor/", "", p1214.02$vendor)
p1214.02$vendor <- gsub("/user/", "", p1214.02$vendor)
p1214.02$vendor <- gsub("#", "", p1214.02$vendor)

p1214.02$shipping <- as.character(p1214.02$shipping)
p1214.02$shipping <- stripWhitespace(p1214.02$shipping)
p1214.02$shipping[p1214.02$shipping == " "] <- NA
is.na(p1214.02$shipping)

p1214.02 <- separate(p1214.02, shipping, c("from", "to"), sep = "To: ")
p1214.02$from <- gsub("From: ", "", p1214.02$from)

levels(as.factor(p1214.02$from)) # 67
levels(as.factor(p1214.02$to)) # 336

p1214.02$price <- gsub(" BTC", "", p1214.02$price)
p1214.02$price <- as.double(p1214.02$price)

write.csv(p1214.02, file = "p1214.02-c1.csv", row.names = F)

# extract subcategories -------------------------------------------------------

# subset out variables w/o a subcategory
levels(as.factor(p1214.02$cat))

p1214.02$cat <- as.character(p1214.02$cat)
p1014 <- subset(p1214.02,  p1214.02$cat != "Listings" & p1214.02$cat != "Jewelry"
                & p1214.02$cat != "Electronics" & p1214.02$cat != "Other")

# 22303 > 21310 
pList2 <- as.character(p1014$list)
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
#  263.009  10.020 273.113 

# bind subcategories
bind1214_02 <- dplyr::left_join(p1214.02, subcat, by = "list")
is.na(bind1214_02$pTab2)

bind1214_02 <- bind1214_02[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
colnames(bind1214_02) <- c("list", "date", "vendor", "product", "price", 
                           "cat", "subcat", "feedback", "from", "to")

p1214.02 <- bind1214_02

# safety
write.csv(p1214.02, file = "p-2014-12-02.csv", row.names = F)

# extract subsubcategories ----------------------------------------------------

# subset subsubcategories
levels(p1214.02$subcat)
p1214.02$subcat <- as.character(p1214.02$subcat)

# 22303 > 21310 > 14881 > 10546
drugs1214.02 <- subset(p1214.02, p1214.02$cat == "Drugs")
drugs1214.02 <- subset(drugs1214.02, drugs1214.02$subcat != "Other" & 
                         drugs1214.02$subcat != "Weight loss" &
                         drugs1214.02$subcat != "Benzos" &
                         drugs1214.02$subcat != "Prescription" &
                         drugs1214.02$subcat != "RCs" &
                         drugs1214.02$subcat != "Steroids" &
                         drugs1214.02$subcat != "Methylone" &
                         drugs1214.02$subcat != "Opioids" &
                         drugs1214.02$subcat != "Ecstasy-MDMA" &
                         drugs1214.02$subcat != "Barbiturates")

# extract subsubcategories
levels(as.factor(drugs1214.02$subcat))

pList3 <- drugs1214.02$list
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

#     user  system elapsed 
#  118.933   2.479 121.496

# bind sub-subcategories
bind1214_02b <- dplyr::left_join(p1214.02, subcat2, by = "list")
is.na(bind1214_02b$pTab3)

bind1214_02b  <- bind1214_02b [c(1, 2, 3, 4, 5, 6, 7, 11, 8, 9, 10)]
colnames(bind1214_02b) <- c("list", "date", "vendor", "product", "price", 
                            "cat", "subcat", "subsubcat", "feedback", "from", "to")

p1214.02 <- bind1214_02b

# final extracted data pre-arules/contigency table transformations
write.csv(p1214.02, file = "products-2014-12-02.csv", row.names = F)
test <- read.csv("products-2014-12-02.csv")

