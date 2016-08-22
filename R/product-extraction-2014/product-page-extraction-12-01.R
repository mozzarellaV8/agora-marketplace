# Agora Marketplace Analysis
# Product info extraction
# 2014-12-01

# load ------------------------------------------------------------------------

library(rvest)
library(magrittr)
library(tm)
library(tidyr)
library(dplyr)

rm(list = ls())

# set directory
pDir <- "~/GitHub/ag-Product/2014-12-01"
setwd(pDir)

# Vendor and Date extraction --------------------------------------------------

# 17736
pList <- list.files(path = pDir, pattern = ".html", all.files = T, recursive = T)
p1214.01 <- data.frame()

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
    p1214.01 <- rbind(p1214.01, pTab)
  }
)

#       user  system elapsed 
#    836.474   8.552 892.335

pList[2910] # RWKLW37mCz - Girl Scout Cookies - (weed)

# safety
write.csv(p1214.01, file = "p-1214-01-raw.csv", row.names = F)
p1214.01 <- read.csv("p-1214-01-raw.csv")

# clean extracted data
p1214.01 <- p1214.01[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(p1214.01) <- c("list", "date", "vendor", "product", 
                        "price", "cat", "feedback", "shipping")

p1214.01$vendor <- gsub("/vendor/", "", p1214.01$vendor)
p1214.01$vendor <- gsub("/user/", "", p1214.01$vendor)
p1214.01$vendor <- gsub("#", "", p1214.01$vendor)

p1214.01$shipping <- as.character(p1214.01$shipping)
p1214.01$shipping <- stripWhitespace(p1214.01$shipping)
p1214.01$shipping[p1214.01$shipping == " "] <- NA
is.na(p1214.01$shipping)

p1214.01 <- separate(p1214.01, shipping, c("from", "to"), sep = "To: ")
p1214.01$from <- gsub("From: ", "", p1214.01$from)

levels(as.factor(p1214.01$from)) # 61
levels(as.factor(p1214.01$to)) # 293

p1214.01$price <- gsub(" BTC", "", p1214.01$price)
p1214.01$price <- as.double(p1214.01$price)

write.csv(p1214.01, file = "p1214.01-c1.csv", row.names = F)

# extract subcategories -------------------------------------------------------

# subset out variables w/o a subcategory
levels(as.factor(p1214.01$cat))

p1214.01$cat <- as.character(p1214.01$cat)
p1014 <- subset(p1214.01,  p1214.01$cat != "Listings" & p1214.01$cat != "Jewelry"
                & p1214.01$cat != "Electronics" & p1214.01$cat != "Other")

# 17735 > 16854 > 11730
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
#  189.513   6.307 195.781 

# bind subcategories
bind1214_01 <- dplyr::left_join(p1214.01, subcat, by = "list")
is.na(bind1214_01$pTab2)

bind1214_01 <- bind1214_01[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
colnames(bind1214_01) <- c("list", "date", "vendor", "product", "price", 
                           "cat", "subcat", "feedback", "from", "to")

p1214.01 <- bind1214_01

# safety
write.csv(p1214.01, file = "p-2014-12-01.csv", row.names = F)

# extract subsubcategories ----------------------------------------------------

# subset subsubcategories
levels(p1214.01$subcat)
p1214.01$subcat <- as.character(p1214.01$subcat)

# 17735 > 16854 > 11730 > 7355
drugs1214.01 <- subset(p1214.01, p1214.01$cat == "Drugs")
drugs1214.01 <- subset(drugs1214.01, drugs1214.01$subcat != "Other" & 
                         drugs1214.01$subcat != "Weight loss" &
                         drugs1214.01$subcat != "Benzos" &
                         drugs1214.01$subcat != "Prescription" &
                         drugs1214.01$subcat != "RCs" &
                         drugs1214.01$subcat != "Steroids" &
                         drugs1214.01$subcat != "Methylone" &
                         drugs1214.01$subcat != "Opioids" &
                         drugs1214.01$subcat != "Ecstasy-MDMA" &
                         drugs1214.01$subcat != "Barbiturates")

# extract subsubcategories
levels(as.factor(drugs1214.01$subcat))

pList3 <- drugs1214.01$list
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
#  77.825   1.098  78.921

# bind sub-subcategories
bind1214_01b <- dplyr::left_join(p1214.01, subcat2, by = "list")
is.na(bind1214_01b$pTab3)

bind1214_01b  <- bind1214_01b [c(1, 2, 3, 4, 5, 6, 7, 11, 8, 9, 10)]
colnames(bind1214_01b) <- c("list", "date", "vendor", "product", "price", 
                            "cat", "subcat", "subsubcat", "feedback", "from", "to")

p1214.01 <- bind1214_01b

# final extracted data pre-arules/contigency table transformations
write.csv(p1214.01, file = "products-2014-12-01.csv", row.names = F)
test <- read.csv("products-2014-12-01.csv")

