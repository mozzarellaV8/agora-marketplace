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
pDir <- "~/GitHub/ag-Product/2014-11-09"
setwd(pDir)

# Vendor and Date extraction --------------------------------------------------

# 14184
pList <- list.files(path = pDir, pattern = ".html", all.files = T, recursive = T)
p1114.09 <- data.frame()

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
    p1114.09 <- rbind(p1114.09, pTab)
  }
)

#    user  system elapsed 
# 682.100   8.611 693.704

pList[9951] # p9m734lJMp - weed - agent orange
pList[11693] # UCgDM7Rt2G - blank

# safety
write.csv(p1114.09, file = "p-1114-09-raw.csv", row.names = F)
p1114.09 <- read.csv("p-1114-09-raw.csv")

# clean extracted data
p1114.09 <- p1114.09[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(p1114.09) <- c("list", "date", "vendor", "product", 
                        "price", "cat", "feedback", "shipping")

p1114.09$vendor <- gsub("/vendor/", "", p1114.09$vendor)
p1114.09$vendor <- gsub("/user/", "", p1114.09$vendor)
p1114.09$vendor <- gsub("#", "", p1114.09$vendor)

p1114.09$shipping <- as.character(p1114.09$shipping)
p1114.09$shipping <- stripWhitespace(p1114.09$shipping)
p1114.09$shipping[p1114.09$shipping == " "] <- NA
is.na(p1114.09$shipping)

p1114.09 <- separate(p1114.09, shipping, c("from", "to"), sep = "To: ")
p1114.09$from <- gsub("From: ", "", p1114.09$from)

levels(as.factor(p1114.09$from)) # 58
levels(as.factor(p1114.09$to)) # 245

p1114.09$price <- gsub(" BTC", "", p1114.09$price)
p1114.09$price <- as.double(p1114.09$price)

write.csv(p1114.09, file = "p1114.09-c1.csv", row.names = F)

# extract subcategories -------------------------------------------------------

# subset out variables w/o a subcategory
levels(as.factor(p1114.09$cat))

p1114.09$cat <- as.character(p1114.09$cat)
p1014 <- subset(p1114.09,  p1114.09$cat != "Listings" & p1114.09$cat != "Jewelry"
                & p1114.09$cat != "Electronics" & p1114.09$cat != "Other")

# 14182 > 13481
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
#  163.415   2.622 166.469 

# bind subcategories
bind1114_09 <- dplyr::left_join(p1114.09, subcat, by = "list")
is.na(bind1114_09$pTab2)

bind1114_09 <- bind1114_09[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
colnames(bind1114_09) <- c("list", "date", "vendor", "product", "price", 
                           "cat", "subcat", "feedback", "from", "to")

p1114.09 <- bind1114_09

# safety
write.csv(p1114.09, file = "p-2014-11-09.csv", row.names = F)

# extract subsubcategories ----------------------------------------------------

# subset subsubcategories
levels(p1114.09$subcat)
p1114.09$subcat <- as.character(p1114.09$subcat)

# 14182 > 13481 > 8664 > 4544
drugs1114.09 <- subset(p1114.09, p1114.09$cat == "Drugs")
drugs1114.09 <- subset(drugs1114.09, drugs1114.09$subcat != "Other" & 
                         drugs1114.09$subcat != "Weight loss" &
                         drugs1114.09$subcat != "Benzos" &
                         drugs1114.09$subcat != "Prescription" &
                         drugs1114.09$subcat != "RCs" &
                         drugs1114.09$subcat != "Steroids" &
                         drugs1114.09$subcat != "Methylone" &
                         drugs1114.09$subcat != "Opioids" &
                         drugs1114.09$subcat != "Ecstasy-MDMA" &
                         drugs1114.09$subcat != "Barbiturates")

# extract subsubcategories
levels(as.factor(drugs1114.09$subcat))

pList3 <- drugs1114.09$list
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
#  54.265   0.589  55.042

# bind sub-subcategories
bind1114_09b <- dplyr::left_join(p1114.09, subcat2, by = "list")
is.na(bind1114_09b$pTab3)

bind1114_09b  <- bind1114_09b [c(1, 2, 3, 4, 5, 6, 7, 11, 8, 9, 10)]
colnames(bind1114_09b) <- c("list", "date", "vendor", "product", "price", 
                            "cat", "subcat", "subsubcat", "feedback", "from", "to")

p1114.09 <- bind1114_09b

# final extracted data pre-arules/contigency table transformations
write.csv(p1114.09, file = "products-2014-11-09.csv", row.names = F)
test <- read.csv("products-2014-11-09.csv")

