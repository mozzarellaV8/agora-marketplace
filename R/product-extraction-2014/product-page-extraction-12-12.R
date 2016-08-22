# Agora Marketplace Analysis
# Product info extraction
# 2014-12-12

# load ------------------------------------------------------------------------

library(rvest)
library(magrittr)
library(tm)
library(tidyr)
library(dplyr)

# Vendor and Date extraction --------------------------------------------------

rm(list = ls())
pDir <- "~/GitHub/ag-Product/2014-12-12"
setwd(pDir)

# 21165
pList <- list.files(path = pDir, pattern = ".html", all.files = T, recursive = T)
p1214.12 <- data.frame()

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
    p1214.12 <- rbind(p1214.12, pTab)
  }
)

#         user  system elapsed 
#   1031.761   13.353 1050.694 

pList[3343] # aviJZ6koWk - blank
pList[7077] # g3wLe7fMpk - Adderall
pList[12411] # Pc39Ebhl4T - Heroin SE Asia
pList[16080] # uyeL2G5yC4 - xanax
pList[16624] # vpoi7ij7dV - special order Gimm
pList[17394] # wPoWxeqBCk - speed amphtamin paste
pList[18104] # XPWfnqlK6r - Dutch gold bars - gold

# safety
write.csv(p1214.12, file = "p-1214-12-raw.csv", row.names = F)
p1214.12 <- read.csv("p-1214-12-raw.csv")

# clean extracted data
p1214.12 <- p1214.12[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(p1214.12) <- c("list", "date", "vendor", "product", 
                        "price", "cat", "feedback", "shipping")

p1214.12$vendor <- gsub("/vendor/", "", p1214.12$vendor)
p1214.12$vendor <- gsub("/user/", "", p1214.12$vendor)
p1214.12$vendor <- gsub("#", "", p1214.12$vendor)

p1214.12$shipping <- as.character(p1214.12$shipping)
p1214.12$shipping <- stripWhitespace(p1214.12$shipping)
p1214.12$shipping[p1214.12$shipping == " "] <- NA
is.na(p1214.12$shipping)

p1214.12 <- separate(p1214.12, shipping, c("from", "to"), sep = "To: ")
p1214.12$from <- gsub("From: ", "", p1214.12$from)

levels(as.factor(p1214.12$from)) # 58
levels(as.factor(p1214.12$to)) # 314

p1214.12$price <- gsub(" BTC", "", p1214.12$price)
p1214.12$price <- as.double(p1214.12$price)

write.csv(p1214.12, file = "p1214.12-c1.csv", row.names = F)

# extract subcategories -------------------------------------------------------

# subset out variables w/o a subcategory
levels(as.factor(p1214.12$cat))

p1214.12$cat <- as.character(p1214.12$cat)
p1214 <- subset(p1214.12,  p1214.12$cat != "Listings" & p1214.12$cat != "Jewelry"
                & p1214.12$cat != "Electronics" & p1214.12$cat != "Other")

# 21162 > 20352
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
#  249.611   8.700 258.262  

# bind subcategories
bind1214_12 <- dplyr::left_join(p1214.12, subcat, by = "list")
is.na(bind1214_12$pTab2)

bind1214_12 <- bind1214_12[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
colnames(bind1214_12) <- c("list", "date", "vendor", "product", "price", 
                           "cat", "subcat", "feedback", "from", "to")

p1214.12 <- bind1214_12

# safety
write.csv(p1214.12, file = "p-2014-12-12.csv", row.names = F)

# extract subsubcategories ----------------------------------------------------

# subset subsubcategories
levels(p1214.12$subcat)
p1214.12$subcat <- as.character(p1214.12$subcat)

# 21162 > 20352 > 14947 >  10112
drugs1214.12 <- subset(p1214.12, p1214.12$cat == "Drugs")
drugs1214.12 <- subset(drugs1214.12, drugs1214.12$subcat != "Other" & 
                         drugs1214.12$subcat != "Weight loss" &
                         drugs1214.12$subcat != "Benzos" &
                         drugs1214.12$subcat != "Prescription" &
                         drugs1214.12$subcat != "RCs" &
                         drugs1214.12$subcat != "Steroids" &
                         drugs1214.12$subcat != "Methylone" &
                         drugs1214.12$subcat != "Opioids" &
                         drugs1214.12$subcat != "Ecstasy-MDMA" &
                         drugs1214.12$subcat != "Barbiturates")

# extract subsubcategories
levels(as.factor(drugs1214.12$subcat))

pList3 <- drugs1214.12$list
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
#  116.918   1.787 118.703

# bind sub-subcategories
bind1214_12b <- dplyr::left_join(p1214.12, subcat2, by = "list")
is.na(bind1214_12b$pTab3)

bind1214_12b  <- bind1214_12b [c(1, 2, 3, 4, 5, 6, 7, 11, 8, 9, 10)]
colnames(bind1214_12b) <- c("list", "date", "vendor", "product", "price", 
                            "cat", "subcat", "subsubcat", "feedback", "from", "to")

p1214.12 <- bind1214_12b

# final extracted data pre-arules/contigency table transformations
write.csv(p1214.12, file = "products-2014-12-12.csv", row.names = F)
test <- read.csv("products-2014-12-12.csv")

