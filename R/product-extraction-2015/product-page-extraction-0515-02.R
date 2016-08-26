# Agora Marketplace Analysis
# Product info extraction
# 2015-05-02

# load ------------------------------------------------------------------------

library(rvest)
library(magrittr)
library(tm)
library(tidyr)
library(dplyr)

# Vendor and Date extraction --------------------------------------------------

rm(list = ls())
pDir <- "~/GitHub/ag-Product/2015-05-02"
setwd(pDir)

# 22211
pList <- list.files(path = pDir, pattern = ".html", all.files = T, recursive = T)
p0515.02 <- data.frame()

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
    p0515.02 <- rbind(p0515.02, pTab)
  }
)

#        user  system elapsed 
#  1121.786   21.314 1185.201

# safety
write.csv(p0515.02, file = "p-0515-02-raw.csv", row.names = F)

# clean extracted data -------------------------------------------------------

p0515.02 <- p0515.02[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(p0515.02) <- c("list", "date", "vendor", "product", 
                        "price", "cat", "feedback", "shipping")

p0515.02$vendor <- gsub("/vendor/", "", p0515.02$vendor)
p0515.02$vendor <- gsub("/user/", "", p0515.02$vendor)
p0515.02$vendor <- gsub("#", "", p0515.02$vendor)

p0515.02$shipping <- as.character(p0515.02$shipping)
p0515.02$shipping <- stripWhitespace(p0515.02$shipping)
p0515.02$shipping[p0515.02$shipping == " "] <- NA
is.na(p0515.02$shipping)

p0515.02 <- separate(p0515.02, shipping, c("from", "to"), sep = "To: ")
p0515.02$from <- gsub("From: ", "", p0515.02$from)

levels(as.factor(p0515.02$from)) # 49
levels(as.factor(p0515.02$to)) # 300

p0515.02$price <- gsub(" BTC", "", p0515.02$price)
p0515.02$price <- as.double(p0515.02$price)

write.csv(p0515.02, file = "p0515.02-c1.csv", row.names = F)

# extract subcategories -------------------------------------------------------

# subset out variables w/o a subcategory
levels(as.factor(p0515.02$cat))

p0515.02$cat <- as.character(p0515.02$cat)
p0115 <- subset(p0515.02,  p0515.02$cat != "Listings" & p0515.02$cat != "Jewelry"
                & p0515.02$cat != "Electronics" & p0515.02$cat != "Other")

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
bind0512_02 <- dplyr::left_join(p0515.02, subcat, by = "list")
is.na(bind0512_02$pTab2)

bind0512_02 <- bind0512_02[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
colnames(bind0512_02) <- c("list", "date", "vendor", "product", "price", 
                           "cat", "subcat", "feedback", "from", "to")

p0515.02 <- bind0512_02

# safety
write.csv(p0515.02, file = "p-2015-05-02.csv", row.names = F)

# extract subsubcategories ----------------------------------------------------

# subset subsubcategories
levels(p0515.02$subcat)
p0515.02$subcat <- as.character(p0515.02$subcat)

# 22211 > 21512 > 18009 > 9000
drugs0515.02 <- subset(p0515.02, p0515.02$cat == "Drugs")
drugs0515.02 <- subset(drugs0515.02, drugs0515.02$subcat != "Other" & 
                         drugs0515.02$subcat != "Weight loss" &
                         drugs0515.02$subcat != "Benzos" &
                         drugs0515.02$subcat != "Prescription" &
                         drugs0515.02$subcat != "RCs" &
                         drugs0515.02$subcat != "Steroids" &
                         drugs0515.02$subcat != "Methylone" &
                         drugs0515.02$subcat != "Opioids" &
                         drugs0515.02$subcat != "Ecstasy-MDMA" &
                         drugs0515.02$subcat != "Barbiturates")

# extract subsubcategories
levels(as.factor(drugs0515.02$subcat))

pList3 <- drugs0515.02$list
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
bind0515_02b <- dplyr::left_join(p0515.02, subcat2, by = "list")
is.na(bind0515_02b$pTab3)

bind0515_02b  <- bind0515_02b [c(1, 2, 3, 4, 5, 6, 7, 11, 8, 9, 10)]
colnames(bind0515_02b) <- c("list", "date", "vendor", "product", "price", 
                            "cat", "subcat", "subsubcat", "feedback", "from", "to")

p0515.02 <- bind0515_02b

# final extracted data pre-arules/contigency table transformations
write.csv(p0515.02, file = "products-2015-05-02.csv", row.names = F)
test <- read.csv("products-2015-05-02.csv")
