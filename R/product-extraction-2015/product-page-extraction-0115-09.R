# Agora Marketplace Analysis
# Product info extraction
# 2015-01-09

# load ------------------------------------------------------------------------

library(rvest)
library(magrittr)
library(tm)
library(tidyr)
library(dplyr)

# Vendor and Date extraction --------------------------------------------------

rm(list = ls())
pDir <- "~/GitHub/ag-Product/2015-01-09"
setwd(pDir)

# 20099
pList <- list.files(path = pDir, pattern = ".html", all.files = T, recursive = T)
p0115.09 <- data.frame()

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
    p0115.09 <- rbind(p0115.09, pTab)
  }
)

#     user  system elapsed 
#  936.867  11.700 951.619

pList[302] # cEaYgBqLXx - roche valium
pList[10990] # t20gJ7h9sV - anapolon
pList[15091] # E0DkrjPSMp - custom mdma france
pList[18172] # sDo30tL3S0 - high white xtal lsd blotters

# safety
write.csv(p0115.09, file = "p-0115-09-raw.csv", row.names = F)
p0115.09 <- read.csv("p-0115-09-raw.csv")

# clean extracted data
p0115.09 <- p0115.09[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(p0115.09) <- c("list", "date", "vendor", "product", 
                        "price", "cat", "feedback", "shipping")

p0115.09$vendor <- gsub("/vendor/", "", p0115.09$vendor)
p0115.09$vendor <- gsub("/user/", "", p0115.09$vendor)
p0115.09$vendor <- gsub("#", "", p0115.09$vendor)

p0115.09$shipping <- as.character(p0115.09$shipping)
p0115.09$shipping <- stripWhitespace(p0115.09$shipping)
p0115.09$shipping[p0115.09$shipping == " "] <- NA
is.na(p0115.09$shipping)

p0115.09 <- separate(p0115.09, shipping, c("from", "to"), sep = "To: ")
p0115.09$from <- gsub("From: ", "", p0115.09$from)

levels(as.factor(p0115.09$from)) # 56
levels(as.factor(p0115.09$to)) # 324

p0115.09$price <- gsub(" BTC", "", p0115.09$price)
p0115.09$price <- as.double(p0115.09$price)

write.csv(p0115.09, file = "p0115.09-c1.csv", row.names = F)

# extract subcategories -------------------------------------------------------

# subset out variables w/o a subcategory
levels(as.factor(p0115.09$cat))

p0115.09$cat <- as.character(p0115.09$cat)
p0115 <- subset(p0115.09,  p0115.09$cat != "Listings" & p0115.09$cat != "Jewelry"
                & p0115.09$cat != "Electronics" & p0115.09$cat != "Other")

# 20095 > 19504
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
#  231.454   7.893 239.660 

# bind subcategories
bind0115_09 <- dplyr::left_join(p0115.09, subcat, by = "list")
is.na(bind0115_09$pTab2)

bind0115_09 <- bind0115_09[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
colnames(bind0115_09) <- c("list", "date", "vendor", "product", "price", 
                           "cat", "subcat", "feedback", "from", "to")

p0115.09 <- bind0115_09

# safety
write.csv(p0115.09, file = "p-2015-01-09.csv", row.names = F)

# extract subsubcategories ----------------------------------------------------

# subset subsubcategories
levels(p0115.09$subcat)
p0115.09$subcat <- as.character(p0115.09$subcat)

# 20095 > 19504 > 14688 > 10633
drugs0115.09 <- subset(p0115.09, p0115.09$cat == "Drugs")
drugs0115.09 <- subset(drugs0115.09, drugs0115.09$subcat != "Other" & 
                         drugs0115.09$subcat != "Weight loss" &
                         drugs0115.09$subcat != "Benzos" &
                         drugs0115.09$subcat != "Prescription" &
                         drugs0115.09$subcat != "RCs" &
                         drugs0115.09$subcat != "Steroids" &
                         drugs0115.09$subcat != "Methylone" &
                         drugs0115.09$subcat != "Ecstasy-MDMA" &
                         drugs0115.09$subcat != "Barbiturates")

# extract subsubcategories
levels(as.factor(drugs0115.09$subcat))

pList3 <- drugs0115.09$list
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
#   126.597   1.549 128.373  

# bind sub-subcategories
bind0115_09b <- dplyr::left_join(p0115.09, subcat2, by = "list")
is.na(bind0115_09b$pTab3)

bind0115_09b  <- bind0115_09b [c(1, 2, 3, 4, 5, 6, 7, 11, 8, 9, 10)]
colnames(bind0115_09b) <- c("list", "date", "vendor", "product", "price", 
                            "cat", "subcat", "subsubcat", "feedback", "from", "to")

p0115.09 <- bind0115_09b

# final extracted data pre-arules/contigency table transformations
write.csv(p0115.09, file = "products-2015-01-09.csv", row.names = F)
test <- read.csv("products-2015-01-09.csv")
