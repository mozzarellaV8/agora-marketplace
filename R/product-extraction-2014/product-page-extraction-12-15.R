# Agora Marketplace Analysis
# Product info extraction
# 2014-12-10

# load ------------------------------------------------------------------------

library(rvest)
library(magrittr)
library(tm)
library(tidyr)
library(dplyr)

# Vendor and Date extraction --------------------------------------------------

rm(list = ls())
pDir <- "~/GitHub/ag-Product/2014-12-15"
setwd(pDir)

# 37960
pList <- list.files(path = pDir, pattern = ".html", all.files = T, recursive = T)
p1214.15 <- data.frame()

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
    p1214.15 <- rbind(p1214.15, pTab)
  }
)

#       user  system elapsed 
# 2007.591   65.845 2091.902

pList[557] # AtE1CVAncT - imputed from completed crawl page
pList[1030] # BoYwDZUh3b - imputed from completed crawl page
pList[12865] # YvPCaiY1hd - blank - speed paste 12-19
pList[17140] # dDpzK8gC83 - cigs - imputed from completed crawl page
pList[31028] # Y3Aiwrcxgc - intense MDMA
pList[33601] # f5YpC6f6B0 - imputed from completed crawl page
pList[35801] # qcJCexGL14 - blank
pList[36014] # Rm0NbGKewf - blank
pList[37562] # YCz0C73ahG - blank

# safety
write.csv(p1214.15, file = "p-1214-15-raw.csv", row.names = F)
p1214.15 <- read.csv("p-1214-15-raw.csv")

# clean extracted data
p1214.15 <- p1214.15[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(p1214.15) <- c("list", "date", "vendor", "product", 
                        "price", "cat", "feedback", "shipping")

p1214.15$vendor <- gsub("/vendor/", "", p1214.15$vendor)
p1214.15$vendor <- gsub("/user/", "", p1214.15$vendor)
p1214.15$vendor <- gsub("#", "", p1214.15$vendor)

p1214.15$shipping <- as.character(p1214.15$shipping)
p1214.15$shipping <- stripWhitespace(p1214.15$shipping)
p1214.15$shipping[p1214.15$shipping == " "] <- NA
is.na(p1214.15$shipping)

p1214.15 <- separate(p1214.15, shipping, c("from", "to"), sep = "To: ")
p1214.15$from <- gsub("From: ", "", p1214.15$from)

levels(as.factor(p1214.15$from)) # 55
levels(as.factor(p1214.15$to)) # 319

p1214.15$price <- gsub(" BTC", "", p1214.15$price)
p1214.15$price <- as.double(p1214.15$price)

write.csv(p1214.15, file = "p1214.15-c1.csv", row.names = F)

# extract subcategories -------------------------------------------------------

# subset out variables w/o a subcategory
levels(as.factor(p1214.15$cat))

p1214.15$cat <- as.character(p1214.15$cat)
p1214 <- subset(p1214.15,  p1214.15$cat != "Listings" & p1214.15$cat != "Jewelry"
                & p1214.15$cat != "Electronics" & p1214.15$cat != "Other")

#  37955 > 36433
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
#  528.182  41.077 594.208  

# bind subcategories
bind1214_15 <- dplyr::left_join(p1214.15, subcat, by = "list")
is.na(bind1214_15$pTab2)

bind1214_15 <- bind1214_15[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
colnames(bind1214_15) <- c("list", "date", "vendor", "product", "price", 
                           "cat", "subcat", "feedback", "from", "to")

p1214.15 <- bind1214_15

# safety
write.csv(p1214.15, file = "p-2014-12-15.csv", row.names = F)

# extract subsubcategories ----------------------------------------------------

# subset subsubcategories
levels(p1214.15$subcat)
p1214.15$subcat <- as.character(p1214.15$subcat)

# 37955 > 36433 > 25856 > 16636
drugs1214.15 <- subset(p1214.15, p1214.15$cat == "Drugs")
drugs1214.15 <- subset(drugs1214.15, drugs1214.15$subcat != "Other" & 
                         drugs1214.15$subcat != "Weight loss" &
                         drugs1214.15$subcat != "Benzos" &
                         drugs1214.15$subcat != "Prescription" &
                         drugs1214.15$subcat != "RCs" &
                         drugs1214.15$subcat != "Steroids" &
                         drugs1214.15$subcat != "Methylone" &
                         drugs1214.15$subcat != "Opioids" &
                         drugs1214.15$subcat != "Ecstasy-MDMA" &
                         drugs1214.15$subcat != "Barbiturates")

# extract subsubcategories
levels(as.factor(drugs1214.15$subcat))

pList3 <- drugs1214.15$list
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
#  207.879   6.633 217.106

# bind sub-subcategories
bind1214_15b <- dplyr::left_join(p1214.15, subcat2, by = "list")
is.na(bind1214_15b$pTab3)

bind1214_15b  <- bind1214_15b [c(1, 2, 3, 4, 5, 6, 7, 11, 8, 9, 10)]
colnames(bind1214_15b) <- c("list", "date", "vendor", "product", "price", 
                            "cat", "subcat", "subsubcat", "feedback", "from", "to")

p1214.15 <- bind1214_15b

# final extracted data pre-arules/contigency table transformations
write.csv(p1214.15, file = "products-2014-12-15.csv", row.names = F)
test <- read.csv("products-2014-12-15.csv")

