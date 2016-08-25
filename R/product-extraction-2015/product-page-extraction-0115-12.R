# Agora Marketplace Analysis
# Product info extraction
# 2015-01-12

# load ------------------------------------------------------------------------

library(rvest)
library(magrittr)
library(tm)
library(tidyr)
library(dplyr)
library(data.table)

# Vendor and Date extraction --------------------------------------------------

rm(list = ls())
pDir <- "~/GitHub/ag-Product/2015-01-12"
setwd(pDir)

# 16553
pList <- list.files(path = pDir, pattern = ".html", all.files = T, recursive = T)
p0115.12 <- data.frame()

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
    p0115.12 <- rbind(p0115.12, pTab)
  }
)

#     user  system elapsed 
#  797.218   8.900 817.251

pList[9547] # gPGSGFKA0g - FUB-PB22

# safety
write.csv(p0115.12, file = "p-0115-12-raw.csv", row.names = F)
# p0115.12 <- read.csv("p-0115-12-raw.csv")

# clean extracted data
p0115.12 <- p0115.12[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(p0115.12) <- c("list", "date", "vendor", "product", 
                        "price", "cat", "feedback", "shipping")

p0115.12$vendor <- gsub("/vendor/", "", p0115.12$vendor)
p0115.12$vendor <- gsub("/user/", "", p0115.12$vendor)
p0115.12$vendor <- gsub("#", "", p0115.12$vendor)

p0115.12$shipping <- as.character(p0115.12$shipping)
p0115.12$shipping <- stripWhitespace(p0115.12$shipping)
p0115.12$shipping[p0115.12$shipping == " "] <- NA
is.na(p0115.12$shipping)

p0115.12 <- separate(p0115.12, shipping, c("from", "to"), sep = "To: ")
p0115.12$from <- gsub("From: ", "", p0115.12$from)

levels(as.factor(p0115.12$from)) # 47
levels(as.factor(p0115.12$to)) # 298

p0115.12$price <- gsub(" BTC", "", p0115.12$price)
p0115.12$price <- as.double(p0115.12$price)

write.csv(p0115.12, file = "p0115.12-c1.csv", row.names = F)

# extract subcategories -------------------------------------------------------

# subset out variables w/o a subcategory
levels(as.factor(p0115.12$cat))

p0115.12$cat <- as.character(p0115.12$cat)
p0115 <- subset(p0115.12,  p0115.12$cat != "Listings" & p0115.12$cat != "Jewelry"
                & p0115.12$cat != "Electronics" & p0115.12$cat != "Other")

# 16532 > 15990
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
#  188.174   4.934 193.078 

# bind subcategories
bind0115_12 <- dplyr::left_join(p0115.12, subcat, by = "list")
is.na(bind0115_12$pTab2)

bind0115_12 <- bind0115_12[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
colnames(bind0115_12) <- c("list", "date", "vendor", "product", "price", 
                           "cat", "subcat", "feedback", "from", "to")

p0115.12 <- bind0115_12

# safety
write.csv(p0115.12, file = "p-2015-01-12.csv", row.names = F)

# extract subsubcategories ----------------------------------------------------

p0115.12 <- fread("~/GitHub/ag-product-safety-2015/p-2015-01-12.csv")

# subset subsubcategories
levels(as.factor(p0115.12$subcat))
p0115.12$subcat <- as.character(p0115.12$subcat)

# 16532 > 15990 > 12980 > 8884
drugs0115.12 <- subset(p0115.12, p0115.12$cat == "Drugs")
drugs0115.12 <- subset(drugs0115.12, drugs0115.12$subcat != "Other" & 
                         drugs0115.12$subcat != "Weight loss" &
                         drugs0115.12$subcat != "Benzos" &
                         drugs0115.12$subcat != "Prescription" &
                         drugs0115.12$subcat != "RCs" &
                         drugs0115.12$subcat != "Steroids" &
                         drugs0115.12$subcat != "Methylone" &
                         drugs0115.12$subcat != "Ecstasy-MDMA" &
                         drugs0115.12$subcat != "Barbiturates")

# extract subsubcategories
levels(as.factor(drugs0115.12$subcat))

pList3 <- drugs0115.12$list
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
#   107.150   1.715 113.972  

# bind sub-subcategories
p0115.12 <- as.data.frame(p0115.12)
bind0115_12b <- dplyr::left_join(p0115.12, subcat2, by = "list")

bind0115_12b  <- bind0115_12b [c(1, 2, 3, 4, 5, 6, 7, 11, 8, 9, 10)]
colnames(bind0115_12b) <- c("list", "date", "vendor", "product", "price", 
                            "cat", "subcat", "subsubcat", "feedback", "from", "to")

p0115.12 <- bind0115_12b

# final extracted data pre-arules/contigency table transformations
write.csv(p0115.12, file = "products-2015-01-12.csv", row.names = F)
test <- fread("products-2015-01-12.csv")
