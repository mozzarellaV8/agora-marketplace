# Agora Marketplace Analysis
# Product info extraction
# 2014-11-07

# load ------------------------------------------------------------------------

library(rvest)
library(magrittr)
library(tm)
library(tidyr)
library(dplyr)

rm(list = ls())

# set directory
pDir <- "~/GitHub/ag-Product/2014-11-08"
setwd(pDir)

# Vendor and Date extraction --------------------------------------------------

# 19273
pList <- list.files(path = pDir, pattern = ".html", all.files = T, recursive = T)
p1114.08 <- data.frame()

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
    p1114.08 <- rbind(p1114.08, pTab)
  }
)

#    user  system elapsed 
# 963.850  14.387 999.497

pList[6460] # iRNSGxo4EZ - weed - agent orange
pList[7616] # pxuagKU9MB -  butylone
pList[8140] # rqN1n5Efph - blank
pList[8822] # TecrURUmMv - 

# safety
write.csv(p1114.08, file = "p-1114-08-raw.csv", row.names = F)
p1114.08 <- read.csv("p-1114-08-raw.csv")

# clean extracted data
p1114.08 <- p1114.08[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(p1114.08) <- c("list", "date", "vendor", "product", 
                        "price", "cat", "feedback", "shipping")

p1114.08$vendor <- gsub("/vendor/", "", p1114.08$vendor)
p1114.08$vendor <- gsub("/user/", "", p1114.08$vendor)
p1114.08$vendor <- gsub("#", "", p1114.08$vendor)

p1114.08$shipping <- as.character(p1114.08$shipping)
p1114.08$shipping <- stripWhitespace(p1114.08$shipping)
p1114.08$shipping[p1114.08$shipping == " "] <- NA
is.na(p1114.08$shipping)

p1114.08 <- separate(p1114.08, shipping, c("from", "to"), sep = "To: ")
p1114.08$from <- gsub("From: ", "", p1114.08$from)

levels(as.factor(p1114.08$from)) # 61
levels(as.factor(p1114.08$to)) # 354

p1114.08$price <- gsub(" BTC", "", p1114.08$price)
p1114.08$price <- as.double(p1114.08$price)

write.csv(p1114.08, file = "p1114.08-c1.csv", row.names = F)

# extract subcategories -------------------------------------------------------

# subset out variables w/o a subcategory
levels(as.factor(p1114.08$cat))

p1114.08$cat <- as.character(p1114.08$cat)
p1014 <- subset(p1114.08,  p1114.08$cat != "Listings" & p1114.08$cat != "Jewelry"
                & p1114.08$cat != "Electronics" & p1114.08$cat != "Other")

# 19270 > 18560 
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
#  109.039   1.486 110.806 

# bind subcategories
bind1114_08 <- dplyr::left_join(p1114.08, subcat, by = "list")
is.na(bind1114_08$pTab2)

bind1114_08 <- bind1114_08[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
colnames(bind1114_08) <- c("list", "date", "vendor", "product", "price", 
                           "cat", "subcat", "feedback", "from", "to")

bind1114_08 <- bind1114_08[c(1, 2, 3, 4, 5, 6, 8, 9, 7, 10)]
colnames(bind1114_08) <- c("list", "date", "vendor", "product", "price", 
                           "cat", "from", "subcat", "feedback", "to")
bind1114_08 <- bind1114_08[c(1, 2, 3, 4, 5, 6, 8, 9, 7, 10)]

p1114.08 <- bind1114_08

# safety
write.csv(p1114.08, file = "p-2014-11-08.csv", row.names = F)

# extract subsubcategories ----------------------------------------------------

# subset subsubcategories
levels(as.factor(p1114.08$subcat))
p1114.08$subcat <- as.character(p1114.08$subcat)

# 19270 > 18560 > 13687 > 9102
drugs1114.08 <- subset(p1114.08, p1114.08$cat == "Drugs")
drugs1114.08 <- subset(drugs1114.08, drugs1114.08$subcat != "Other" & 
                         drugs1114.08$subcat != "Weight loss" &
                         drugs1114.08$subcat != "Benzos" &
                         drugs1114.08$subcat != "Prescription" &
                         drugs1114.08$subcat != "RCs" &
                         drugs1114.08$subcat != "Steroids" &
                         drugs1114.08$subcat != "Methylone" &
                         drugs1114.08$subcat != "Opioids" &
                         drugs1114.08$subcat != "Ecstasy-MDMA" &
                         drugs1114.08$subcat != "Barbiturates")

# extract subsubcategories
levels(as.factor(drugs1114.08$subcat))

pList3 <- drugs1114.08$list
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
#  105.577   1.670 107.435 

# bind sub-subcategories
bind1114_08b <- dplyr::left_join(p1114.08, subcat2, by = "list")
is.na(bind1114_08b$pTab3)

bind1114_08b  <- bind1114_08b [c(1, 2, 3, 4, 5, 6, 7, 11, 8, 9, 10)]
colnames(bind1114_08b) <- c("list", "date", "vendor", "product", "price", 
                            "cat", "subcat", "subsubcat", "feedback", "from", "to")

bind1114_08b  <- bind1114_08b [c(1, 2, 3, 4, 5, 6, 7, 8, 11, 9, 10)]
colnames(bind1114_08b) <- c("list", "date", "vendor", "product", "price", 
                            "cat", "subcat", "subsubcat", "feedback", "from", "to")

p1114.08 <- bind1114_08b

# final extracted data pre-arules/contigency table transformations
write.csv(p1114.08, file = "products-2014-11-08.csv", row.names = F)
test <- read.csv("products-2014-11-08.csv")

