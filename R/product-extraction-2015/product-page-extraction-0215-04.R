# Agora Marketplace Analysis
# Product info extraction
# 2015-02-04

# load ------------------------------------------------------------------------

library(rvest)
library(magrittr)
library(tm)
library(tidyr)
library(dplyr)

# Vendor and Date extraction --------------------------------------------------

rm(list = ls())
pDir <- "~/GitHub/ag-Product/2015-02-04"
setwd(pDir)

# 13922
pList <- list.files(path = pDir, pattern = ".html", all.files = T, recursive = T)
p0215.04 <- data.frame()

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
    p0215.04 <- rbind(p0215.04, pTab)
  }
)

#     user  system elapsed 
#  684.435   5.448 702.218

pList[1132] # LecmdyCvRt - Etizolam
pList[6630] # khjsofLx6T - 50g methylone

# safety
write.csv(p0215.04, file = "p-0215-04-raw.csv", row.names = F)
p0215.04 <- read.csv("p-0215-04-raw.csv")

# clean extracted data
p0215.04 <- p0215.04[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(p0215.04) <- c("list", "date", "vendor", "product", 
                        "price", "cat", "feedback", "shipping")

p0215.04$vendor <- gsub("/vendor/", "", p0215.04$vendor)
p0215.04$vendor <- gsub("/user/", "", p0215.04$vendor)
p0215.04$vendor <- gsub("#", "", p0215.04$vendor)
p0215.04$vendor <- gsub("%7E", "", p0215.04$vendor)

p0215.04$shipping <- as.character(p0215.04$shipping)
p0215.04$shipping <- stripWhitespace(p0215.04$shipping)
p0215.04$shipping[p0215.04$shipping == " "] <- NA
is.na(p0215.04$shipping)

p0215.04 <- separate(p0215.04, shipping, c("from", "to"), sep = "To: ")
p0215.04$from <- gsub("From: ", "", p0215.04$from)

levels(as.factor(p0215.04$from)) # 52
levels(as.factor(p0215.04$to)) # 225

p0215.04$price <- gsub(" BTC", "", p0215.04$price)
p0215.04$price <- as.double(p0215.04$price)

write.csv(p0215.04, file = "p0215.04-c1.csv", row.names = F)

# extract subcategories -------------------------------------------------------

# subset out variables w/o a subcategory
levels(as.factor(p0215.04$cat))

p0215.04$cat <- as.character(p0215.04$cat)
p0215 <- subset(p0215.04,  p0215.04$cat != "Listings" & p0215.04$cat != "Jewelry"
                & p0215.04$cat != "Electronics" & p0215.04$cat != "Other")

# 13920 > 13413
pList2 <- as.character(p0215$list)
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
#  164.699   2.132 167.250 

# bind subcategories
bind0215_04 <- dplyr::left_join(p0215.04, subcat, by = "list")
is.na(bind0215_04$pTab2)

bind0215_04 <- bind0215_04[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
colnames(bind0215_04) <- c("list", "date", "vendor", "product", "price", 
                           "cat", "subcat", "feedback", "from", "to")

p0215.04 <- bind0215_04

# safety
write.csv(p0215.04, file = "p-2015-02-04.csv", row.names = F)

# extract subsubcategories ----------------------------------------------------

library(data.table)
p0215.04 <- fread("~/GitHub/ag-product-safety-2015/p-2015-02-04.csv")

# subset subsubcategories
levels(p0215.04$subcat)
p0215.04$subcat <- as.character(p0215.04$subcat)

# 13920 > 13413 > 8098 > 3928
drugs0215.04 <- subset(p0215.04, p0215.04$cat == "Drugs")
drugs0215.04 <- subset(drugs0215.04, drugs0215.04$subcat != "Other" & 
                         drugs0215.04$subcat != "Weight loss" &
                         drugs0215.04$subcat != "Benzos" &
                         drugs0215.04$subcat != "Prescription" &
                         drugs0215.04$subcat != "RCs" &
                         drugs0215.04$subcat != "Steroids" &
                         drugs0215.04$subcat != "Methylone" &
                         drugs0215.04$subcat != "Ecstasy-MDMA" &
                         drugs0215.04$subcat != "Barbiturates")

# extract subsubcategories
levels(as.factor(drugs0215.04$subcat))

pList3 <- drugs0215.04$list
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
#   48.092   0.800  51.324  

# bind sub-subcategories
p0215.04 <- as.data.frame(p0215.04)
bind0215_04b <- dplyr::left_join(p0215.04, subcat2, by = "list")
length(is.na(bind0215_04b$pTab3))

bind0215_04b  <- bind0215_04b [c(1, 2, 3, 4, 5, 6, 7, 11, 8, 9, 10)]
colnames(bind0215_04b) <- c("list", "date", "vendor", "product", "price", 
                            "cat", "subcat", "subsubcat", "feedback", "from", "to")

p0215.04 <- bind0215_04b

# final extracted data pre-arules/contigency table transformations
write.csv(p0215.04, file = "products-2015-02-04.csv", row.names = F)
test <- read.csv("products-2015-02-04.csv")
