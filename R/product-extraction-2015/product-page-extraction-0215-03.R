# Agora Marketplace Analysis
# Product info extraction
# 2015-02-02

# load ------------------------------------------------------------------------

library(rvest)
library(magrittr)
library(tm)
library(tidyr)
library(dplyr)

# Vendor and Date extraction --------------------------------------------------

rm(list = ls())
pDir <- "~/GitHub/ag-Product/2015-02-03"
setwd(pDir)

# 14816
pList <- list.files(path = pDir, pattern = ".html", all.files = T, recursive = T)
p0215.03 <- data.frame()

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
    p0215.03 <- rbind(p0215.03, pTab)
  }
)

#     user  system elapsed 
#  711.288  10.466 727.162

pList[2657] # lm8yR8xPUB - blank
pList[10371] # MNWjDgP4eP _ blank
pList[11944] # rXFEb9srs4 - karamelo hasch

# safety
write.csv(p0215.03, file = "p-0215-03-raw.csv", row.names = F)
p0215.03 <- read.csv("p-0215-03-raw.csv")

# clean extracted data
p0215.03 <- p0215.03[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(p0215.03) <- c("list", "date", "vendor", "product", 
                        "price", "cat", "feedback", "shipping")

p0215.03$vendor <- gsub("/vendor/", "", p0215.03$vendor)
p0215.03$vendor <- gsub("/user/", "", p0215.03$vendor)
p0215.03$vendor <- gsub("#", "", p0215.03$vendor)
p0215.03$vendor <- gsub("%7E", "", p0215.03$vendor)

p0215.03$shipping <- as.character(p0215.03$shipping)
p0215.03$shipping <- stripWhitespace(p0215.03$shipping)
p0215.03$shipping[p0215.03$shipping == " "] <- NA
is.na(p0215.03$shipping)

p0215.03 <- separate(p0215.03, shipping, c("from", "to"), sep = "To: ")
p0215.03$from <- gsub("From: ", "", p0215.03$from)

levels(as.factor(p0215.03$from)) # 51
levels(as.factor(p0215.03$to)) # 262

p0215.03$price <- gsub(" BTC", "", p0215.03$price)
p0215.03$price <- as.double(p0215.03$price)

write.csv(p0215.03, file = "p0215.03-c1.csv", row.names = F)

# extract subcategories -------------------------------------------------------

# subset out variables w/o a subcategory
levels(as.factor(p0215.03$cat))

p0215.03$cat <- as.character(p0215.03$cat)
p0115 <- subset(p0215.03,  p0215.03$cat != "Listings" & p0215.03$cat != "Jewelry"
                & p0215.03$cat != "Electronics" & p0215.03$cat != "Other")

# 14813 > 14236
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
#  164.513   4.185 168.969 

# bind subcategories
bind0215_03 <- dplyr::left_join(p0215.03, subcat, by = "list")
is.na(bind0215_03$pTab2)

bind0215_03 <- bind0215_03[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
colnames(bind0215_03) <- c("list", "date", "vendor", "product", "price", 
                           "cat", "subcat", "feedback", "from", "to")

p0215.03 <- bind0215_03

# safety
write.csv(p0215.03, file = "p-2015-02-03.csv", row.names = F)

# extract subsubcategories ----------------------------------------------------

# subset subsubcategories
levels(p0215.03$subcat)
p0215.03$subcat <- as.character(p0215.03$subcat)

# 14813 > 14236 > 11030 > 6894
drugs0215.03 <- subset(p0215.03, p0215.03$cat == "Drugs")
drugs0215.03 <- subset(drugs0215.03, drugs0215.03$subcat != "Other" & 
                         drugs0215.03$subcat != "Weight loss" &
                         drugs0215.03$subcat != "Benzos" &
                         drugs0215.03$subcat != "Prescription" &
                         drugs0215.03$subcat != "RCs" &
                         drugs0215.03$subcat != "Steroids" &
                         drugs0215.03$subcat != "Methylone" &
                         drugs0215.03$subcat != "Ecstasy-MDMA" &
                         drugs0215.03$subcat != "Barbiturates")

# extract subsubcategories
levels(as.factor(drugs0215.03$subcat))

pList3 <- drugs0215.03$list
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
#   82.772   1.117  84.202  

# bind sub-subcategories
bind0215_03b <- dplyr::left_join(p0215.03, subcat2, by = "list")
is.na(bind0215_03b$pTab3)

bind0215_03b  <- bind0215_03b [c(1, 2, 3, 4, 5, 6, 7, 11, 8, 9, 10)]
colnames(bind0215_03b) <- c("list", "date", "vendor", "product", "price", 
                            "cat", "subcat", "subsubcat", "feedback", "from", "to")

p0215.03 <- bind0215_03b

# final extracted data pre-arules/contigency table transformations
write.csv(p0215.03, file = "products-2015-02-03.csv", row.names = F)
test <- read.csv("products-2015-02-03.csv")
