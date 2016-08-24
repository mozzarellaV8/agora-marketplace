# Agora Marketplace Analysis
# Product info extraction
# 2014-12-06

# load ------------------------------------------------------------------------

library(rvest)
library(magrittr)
library(tm)
library(tidyr)
library(dplyr)

# Vendor and Date extraction --------------------------------------------------

rm(list = ls())
pDir <- "~/GitHub/ag-Product/2014/2014-12-07"
setwd(pDir)

# 21780
pList <- list.files(path = pDir, pattern = ".html", all.files = T, recursive = T)
p1214.07 <- data.frame()

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
    p1214.07 <- rbind(p1214.07, pTab)
  }
)

#         user  system elapsed 
#   1159.748   14.918 1187.175 

pList[11956] # YkfCV51sAJ - 1000 xanax bars
pList[11956] # yKhw7aWXLN - literally same number - 'shake/trimmings'
pList[13640] # CvFR7LZMdA - hand-rubbed
pList[13640] # cVlceNbiB - mickey mouse XTC

# safety
write.csv(p1214.07, file = "p-1214-07-raw.csv", row.names = F)
p1214.07 <- read.csv("p-1214-07-raw.csv")

# clean extracted data
p1214.07 <- p1214.07[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(p1214.07) <- c("list", "date", "vendor", "product", 
                        "price", "cat", "feedback", "shipping")

p1214.07$vendor <- gsub("/vendor/", "", p1214.07$vendor)
p1214.07$vendor <- gsub("/user/", "", p1214.07$vendor)
p1214.07$vendor <- gsub("#", "", p1214.07$vendor)

p1214.07$shipping <- as.character(p1214.07$shipping)
p1214.07$shipping <- stripWhitespace(p1214.07$shipping)
p1214.07$shipping[p1214.07$shipping == " "] <- NA
is.na(p1214.07$shipping)

p1214.07 <- separate(p1214.07, shipping, c("from", "to"), sep = "To: ")
p1214.07$from <- gsub("From: ", "", p1214.07$from)

levels(as.factor(p1214.07$from)) # 58
levels(as.factor(p1214.07$to)) # 337

p1214.07$price <- gsub(" BTC", "", p1214.07$price)
p1214.07$price <- as.double(p1214.07$price)

write.csv(p1214.07, file = "p1214.07-c1.csv", row.names = F)

# extract subcategories -------------------------------------------------------

# subset out variables w/o a subcategory
levels(as.factor(p1214.07$cat))

p1214.07$cat <- as.character(p1214.07$cat)
p1214 <- subset(p1214.07,  p1214.07$cat != "Listings" & p1214.07$cat != "Jewelry"
                & p1214.07$cat != "Electronics" & p1214.07$cat != "Other")

# 21776 > 20918
pList2 <- as.character(p1214$list)
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
#  262.135   9.498 276.256 

# bind subcategories
bind1214_07 <- dplyr::left_join(p1214.07, subcat, by = "list")
is.na(bind1214_07$pTab2)

bind1214_07 <- bind1214_07[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
colnames(bind1214_07) <- c("list", "date", "vendor", "product", "price", 
                           "cat", "subcat", "feedback", "from", "to")

p1214.07 <- bind1214_07

# safety
write.csv(p1214.07, file = "p-2014-12-07.csv", row.names = F)

# extract subsubcategories ----------------------------------------------------

# subset subsubcategories
levels(p1214.07$subcat)
p1214.07$subcat <- as.character(p1214.07$subcat)

# 21776 > 20918 > 15638 > 10576
drugs1214.07 <- subset(p1214.07, p1214.07$cat == "Drugs")
drugs1214.07 <- subset(drugs1214.07, drugs1214.07$subcat != "Other" & 
                         drugs1214.07$subcat != "Weight loss" &
                         drugs1214.07$subcat != "Benzos" &
                         drugs1214.07$subcat != "Prescription" &
                         drugs1214.07$subcat != "RCs" &
                         drugs1214.07$subcat != "Steroids" &
                         drugs1214.07$subcat != "Methylone" &
                         drugs1214.07$subcat != "Opioids" &
                         drugs1214.07$subcat != "Ecstasy-MDMA" &
                         drugs1214.07$subcat != "Barbiturates")

# extract subsubcategories
levels(as.factor(drugs1214.07$subcat))

pList3 <- drugs1214.07$list
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
#  121.405   1.424 122.995

# bind sub-subcategories
bind1214_07b <- dplyr::left_join(p1214.07, subcat2, by = "list")
is.na(bind1214_07b$pTab3)

bind1214_07b  <- bind1214_07b [c(1, 2, 3, 4, 5, 6, 7, 11, 8, 9, 10)]
colnames(bind1214_07b) <- c("list", "date", "vendor", "product", "price", 
                            "cat", "subcat", "subsubcat", "feedback", "from", "to")

p1214.07 <- bind1214_07b
p1214.07$vendor <- gsub("%7E", "", p1214.07$vendor)

# final extracted data pre-arules/contigency table transformations
write.csv(p1214.07, file = "products-2014-12-07.csv", row.names = F)
test <- read.csv("products-2014-12-07.csv")

