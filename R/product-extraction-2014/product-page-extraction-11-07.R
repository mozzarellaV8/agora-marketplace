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
pDir <- "~/GitHub/ag-Product/2014-11-07"
setwd(pDir)

# Vendor and Date extraction --------------------------------------------------

# 15563
pList <- list.files(path = pDir, pattern = ".html", all.files = T, recursive = T)
p1114.07 <- data.frame()

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
    p1114.07 <- rbind(p1114.07, pTab)
  }
)

#    user  system elapsed 
# 619.633   5.992 627.837

pList[6460] # iRNSGxo4EZ - weed - agent orange
pList[10964] # v7YXelXTuG - tag heuer watch/jewelry
pList[11553] # wUXZp20uZa - Danabol DS

# safety
write.csv(p1114.07, file = "p-1114-07-raw.csv", row.names = F)
p1114.07 <- read.csv("p-1114-07-raw.csv")

# clean extracted data
p1114.07 <- p1114.07[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(p1114.07) <- c("list", "date", "vendor", "product", 
                        "price", "cat", "feedback", "shipping")

p1114.07$vendor <- gsub("/vendor/", "", p1114.07$vendor)
p1114.07$vendor <- gsub("/user/", "", p1114.07$vendor)
p1114.07$vendor <- gsub("#", "", p1114.07$vendor)

p1114.07$shipping <- as.character(p1114.07$shipping)
p1114.07$shipping <- stripWhitespace(p1114.07$shipping)
p1114.07$shipping[p1114.07$shipping == " "] <- NA
is.na(p1114.07$shipping)

p1114.07 <- separate(p1114.07, shipping, c("from", "to"), sep = "To: ")
p1114.07$from <- gsub("From: ", "", p1114.07$from)

levels(as.factor(p1114.07$from)) # 47
levels(as.factor(p1114.07$to)) # 198

p1114.07$price <- gsub(" BTC", "", p1114.07$price)
p1114.07$price <- as.double(p1114.07$price)

write.csv(p1114.07, file = "p1114.07-c1.csv", row.names = F)

# extract subcategories -------------------------------------------------------

# subset out variables w/o a subcategory
levels(as.factor(p1114.07$cat))

p1114.07$cat <- as.character(p1114.07$cat)
p1014 <- subset(p1114.07,  p1114.07$cat != "Listings" & p1114.07$cat != "Jewelry"
                & p1114.07$cat != "Electronics" & p1114.07$cat != "Other")

# 12518 > 11611
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

#      user  system elapsed 
#   138.499   2.255 140.891 

# bind subcategories
bind1114_07 <- dplyr::left_join(p1114.07, subcat, by = "list")
is.na(bind1114_07$pTab2)

bind1114_07 <- bind1114_07[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
colnames(bind1114_07) <- c("list", "date", "vendor", "product", "price", 
                           "cat", "subcat", "feedback", "from", "to")

p1114.07 <- bind1114_07

# safety
write.csv(p1114.07, file = "p-2014-11-07.csv", row.names = F)

# extract subsubcategories ----------------------------------------------------

# subset subsubcategories
levels(p1114.07$subcat)
p1114.07$subcat <- as.character(p1114.07$subcat)

# 12518 > 11611 > 7319 > 3220
drugs1114.07 <- subset(p1114.07, p1114.07$cat == "Drugs")
drugs1114.07 <- subset(drugs1114.07, drugs1114.07$subcat != "Other" & 
                         drugs1114.07$subcat != "Weight loss" &
                         drugs1114.07$subcat != "Benzos" &
                         drugs1114.07$subcat != "Prescription" &
                         drugs1114.07$subcat != "RCs" &
                         drugs1114.07$subcat != "Steroids" &
                         drugs1114.07$subcat != "Methylone" &
                         drugs1114.07$subcat != "Opioids" &
                         drugs1114.07$subcat != "Ecstasy-MDMA" &
                         drugs1114.07$subcat != "Barbiturates")

# extract subsubcategories
levels(as.factor(drugs1114.07$subcat))

pList3 <- drugs1114.07$list
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
#  36.453   0.408  36.870

# bind sub-subcategories
bind1114_07b <- dplyr::left_join(p1114.07, subcat2, by = "list")
is.na(bind1114_07b$pTab3)

bind1114_07b  <- bind1114_07b [c(1, 2, 3, 4, 5, 6, 7, 11, 8, 9, 10)]
colnames(bind1114_07b) <- c("list", "date", "vendor", "product", "price", 
                            "cat", "subcat", "subsubcat", "feedback", "from", "to")

p1114.07 <- bind1114_07b

# final extracted data pre-arules/contigency table transformations
write.csv(p1114.07, file = "products-2014-11-07.csv", row.names = F)
test <- read.csv("products-2014-11-07.csv")

