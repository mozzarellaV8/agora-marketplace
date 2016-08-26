# Agora Marketplace Analysis
# Product info extraction
# 2015-06-03

# load ------------------------------------------------------------------------

library(rvest)
library(magrittr)
library(tm)
library(tidyr)
library(dplyr)

# Vendor and Date extraction --------------------------------------------------

rm(list = ls())
pDir <- "~/GitHub/ag-Product/2015-06-03"
setwd(pDir)

# 22211
pList <- list.files(path = pDir, pattern = ".html", all.files = T, recursive = T)
p0615.03 <- data.frame()

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
    p0615.03 <- rbind(p0615.03, pTab)
  }
)

#        user  system elapsed 
#  1121.786   21.314 1185.201

# safety
write.csv(p0615.03, file = "p-0615-03-raw.csv", row.names = F)

# clean extracted data --------------------------------------------------------
p0615.03 <- p0615.03[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(p0615.03) <- c("list", "date", "vendor", "product", 
                        "price", "cat", "feedback", "shipping")

p0615.03$vendor <- gsub("/vendor/", "", p0615.03$vendor)
p0615.03$vendor <- gsub("/user/", "", p0615.03$vendor)
p0615.03$vendor <- gsub("#", "", p0615.03$vendor)
p0615.03$vendor <- gsub("%7E", "", p0615.03$vendor)

p0615.03$shipping <- as.character(p0615.03$shipping)
p0615.03$shipping <- stripWhitespace(p0615.03$shipping)
p0615.03$shipping[p0615.03$shipping == " "] <- NA
is.na(p0615.03$shipping)

p0615.03 <- separate(p0615.03, shipping, c("from", "to"), sep = "To: ")
p0615.03$from <- gsub("From: ", "", p0615.03$from)

levels(as.factor(p0615.03$from)) # 49
levels(as.factor(p0615.03$to)) # 300

p0615.03$price <- gsub(" BTC", "", p0615.03$price)
p0615.03$price <- as.double(p0615.03$price)

write.csv(p0615.03, file = "p0615.03-c1.csv", row.names = F)

# extract subcategories -------------------------------------------------------

# subset out variables w/o a subcategory
levels(as.factor(p0615.03$cat))

p0615.03$cat <- as.character(p0615.03$cat)
p0115 <- subset(p0615.03,  p0615.03$cat != "Listings" & p0615.03$cat != "Jewelry"
                & p0615.03$cat != "Electronics" & p0615.03$cat != "Other")

# 22211 > 21512
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
#  271.130  10.882 283.107 

# bind subcategories
bind0615_03 <- dplyr::left_join(p0615.03, subcat, by = "list")
is.na(bind0615_03$pTab2)

bind0615_03 <- bind0615_03[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
colnames(bind0615_03) <- c("list", "date", "vendor", "product", "price", 
                           "cat", "subcat", "feedback", "from", "to")

p0615.03 <- bind0615_03

# safety
write.csv(p0615.03, file = "p-2015-06-03.csv", row.names = F)

# extract subsubcategories ----------------------------------------------------

# subset subsubcategories
levels(p0615.03$subcat)
p0615.03$subcat <- as.character(p0615.03$subcat)

# 22211 > 21512 > 18009 > 9000
drugs0615.03 <- subset(p0615.03, p0615.03$cat == "Drugs")
drugs0615.03 <- subset(drugs0615.03, drugs0615.03$subcat != "Other" & 
                         drugs0615.03$subcat != "Weight loss" &
                         drugs0615.03$subcat != "Benzos" &
                         drugs0615.03$subcat != "Prescription" &
                         drugs0615.03$subcat != "RCs" &
                         drugs0615.03$subcat != "Steroids" &
                         drugs0615.03$subcat != "Methylone" &
                         drugs0615.03$subcat != "Ecstasy-MDMA" &
                         drugs0615.03$subcat != "Barbiturates")

# extract subsubcategories
levels(as.factor(drugs0615.03$subcat))

pList3 <- drugs0615.03$list
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
#   96.235   1.760  98.010  

# bind sub-subcategories
bind0615_03b <- dplyr::left_join(p0615.03, subcat2, by = "list")
is.na(bind0615_03b$pTab3)

bind0615_03b  <- bind0615_03b [c(1, 2, 3, 4, 5, 6, 7, 11, 8, 9, 10)]
colnames(bind0615_03b) <- c("list", "date", "vendor", "product", "price", 
                            "cat", "subcat", "subsubcat", "feedback", "from", "to")

p0615.03 <- bind0615_03b

# final extracted data 
write.csv(p0615.03, file = "products-2015-06-03.csv", row.names = F)
test <- read.csv("products-2015-06-03.csv")
