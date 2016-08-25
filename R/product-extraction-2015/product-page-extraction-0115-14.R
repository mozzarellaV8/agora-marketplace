# Agora Marketplace Analysis
# Product info extraction
# 2015-01-14

# load ------------------------------------------------------------------------

library(rvest)
library(magrittr)
library(tm)
library(tidyr)
library(dplyr)

# Vendor and Date extraction --------------------------------------------------

rm(list = ls())
pDir <- "~/GitHub/ag-Product/2015-01-14"
setwd(pDir)

# 14831
pList <- list.files(path = pDir, pattern = ".html", all.files = T, recursive = T)
p0115.14 <- data.frame()

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
    p0115.14 <- rbind(p0115.14, pTab)
  }
)

#    user  system elapsed 
#  704.661   7.572 729.006

# safety
write.csv(p0115.14, file = "p-0115-14-raw.csv", row.names = F)
p0115.14 <- read.csv("p-0115-14-raw.csv")

# clean extracted data
p0115.14 <- p0115.14[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(p0115.14) <- c("list", "date", "vendor", "product", 
                        "price", "cat", "feedback", "shipping")

p0115.14$vendor <- gsub("/vendor/", "", p0115.14$vendor)
p0115.14$vendor <- gsub("/user/", "", p0115.14$vendor)
p0115.14$vendor <- gsub("#", "", p0115.14$vendor)

p0115.14$shipping <- as.character(p0115.14$shipping)
p0115.14$shipping <- stripWhitespace(p0115.14$shipping)
p0115.14$shipping[p0115.14$shipping == " "] <- NA
is.na(p0115.14$shipping)

p0115.14 <- separate(p0115.14, shipping, c("from", "to"), sep = "To: ")
p0115.14$from <- gsub("From: ", "", p0115.14$from)

levels(as.factor(p0115.14$from)) # 51
levels(as.factor(p0115.14$to)) # 268

p0115.14$price <- gsub(" BTC", "", p0115.14$price)
p0115.14$price <- as.double(p0115.14$price)

write.csv(p0115.14, file = "p0115.14-c1.csv", row.names = F)

# extract subcategories -------------------------------------------------------

# subset out variables w/o a subcategory
levels(as.factor(p0115.14$cat))

p0115.14$cat <- as.character(p0115.14$cat)
p0115 <- subset(p0115.14,  p0115.14$cat != "Listings" & p0115.14$cat != "Jewelry"
                & p0115.14$cat != "Electronics" & p0115.14$cat != "Other")

# 14791 > 14265
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
#  164.208   4.504 168.803 

# bind subcategories
bind0115_14 <- dplyr::left_join(p0115.14, subcat, by = "list")
is.na(bind0115_14$pTab2)

bind0115_14 <- bind0115_14[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
colnames(bind0115_14) <- c("list", "date", "vendor", "product", "price", 
                           "cat", "subcat", "feedback", "from", "to")

p0115.14 <- bind0115_14

# safety
write.csv(p0115.14, file = "p-2015-01-14.csv", row.names = F)

# extract subsubcategories ----------------------------------------------------

p0115.14 <- fread("~/GitHub/ag-product-safety-2015/p-2015-01-14.csv")

# subset subsubcategories
levels(as.factor(p0115.14$subcat))
p0115.14$subcat <- as.character(p0115.14$subcat)

# 14791 > 14265 > 11358 > 7467
drugs0115.14 <- subset(p0115.14, p0115.14$cat == "Drugs")
drugs0115.14 <- subset(drugs0115.14, drugs0115.14$subcat != "Other" & 
                         drugs0115.14$subcat != "Weight loss" &
                         drugs0115.14$subcat != "Benzos" &
                         drugs0115.14$subcat != "Prescription" &
                         drugs0115.14$subcat != "RCs" &
                         drugs0115.14$subcat != "Steroids" &
                         drugs0115.14$subcat != "Methylone" &
                         drugs0115.14$subcat != "Ecstasy-MDMA" &
                         drugs0115.14$subcat != "Barbiturates")

# extract subsubcategories
levels(as.factor(drugs0115.14$subcat))

pList3 <- drugs0115.14$list
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
#   91.415   1.751  97.610  

# bind sub-subcategories
p0115.14 <- as.data.frame(p0115.14)
bind0115_14b <- dplyr::left_join(p0115.14, subcat2, by = "list")
is.na(bind0115_14b$pTab3)

bind0115_14b  <- bind0115_14b [c(1, 2, 3, 4, 5, 6, 7, 11, 8, 9, 10)]
colnames(bind0115_14b) <- c("list", "date", "vendor", "product", "price", 
                            "cat", "subcat", "subsubcat", "feedback", "from", "to")

p0115.14 <- bind0115_14b

# final extracted data pre-arules/contigency table transformations
write.csv(p0115.14, file = "products-2015-01-14.csv", row.names = F)
test <- read.csv("products-2015-01-14.csv")
