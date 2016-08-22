# Agora Marketplace Analysis
# Product info extraction
# 2014-12-13

# load ------------------------------------------------------------------------

library(rvest)
library(magrittr)
library(tm)
library(tidyr)
library(dplyr)

# Vendor and Date extraction --------------------------------------------------

rm(list = ls())
pDir <- "~/GitHub/ag-Product/2014-12-13"
setwd(pDir)

# 13362
pList <- list.files(path = pDir, pattern = ".html", all.files = T, recursive = T)
p1214.13 <- data.frame()

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
    p1214.13 <- rbind(p1214.13, pTab)
  }
)

#       user  system elapsed 
#    634.182   6.794 686.313 

# safety
write.csv(p1214.13, file = "p-1214-13-raw.csv", row.names = F)
p1214.13 <- read.csv("p-1214-13-raw.csv")

# clean extracted data
p1214.13 <- p1214.13[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(p1214.13) <- c("list", "date", "vendor", "product", 
                        "price", "cat", "feedback", "shipping")

p1214.13$vendor <- gsub("/vendor/", "", p1214.13$vendor)
p1214.13$vendor <- gsub("/user/", "", p1214.13$vendor)
p1214.13$vendor <- gsub("#", "", p1214.13$vendor)

p1214.13$shipping <- as.character(p1214.13$shipping)
p1214.13$shipping <- stripWhitespace(p1214.13$shipping)
p1214.13$shipping[p1214.13$shipping == " "] <- NA
is.na(p1214.13$shipping)

p1214.13 <- separate(p1214.13, shipping, c("from", "to"), sep = "To: ")
p1214.13$from <- gsub("From: ", "", p1214.13$from)

levels(as.factor(p1214.13$from)) # 48
levels(as.factor(p1214.13$to)) # 199

p1214.13$price <- gsub(" BTC", "", p1214.13$price)
p1214.13$price <- as.double(p1214.13$price)

write.csv(p1214.13, file = "p1214.13-c1.csv", row.names = F)

# extract subcategories -------------------------------------------------------

# subset out variables w/o a subcategory
levels(as.factor(p1214.13$cat))

p1214.13$cat <- as.character(p1214.13$cat)
p1214 <- subset(p1214.13,  p1214.13$cat != "Listings" & p1214.13$cat != "Jewelry"
                & p1214.13$cat != "Electronics" & p1214.13$cat != "Other")

# 13362 > 12626
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
#  150.045   2.751 153.236

# bind subcategories
bind1214_13 <- dplyr::left_join(p1214.13, subcat, by = "list")
is.na(bind1214_13$pTab2)

bind1214_13 <- bind1214_13[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
colnames(bind1214_13) <- c("list", "date", "vendor", "product", "price", 
                           "cat", "subcat", "feedback", "from", "to")

p1214.13 <- bind1214_13
p1214.13$vendor <- gsub("%7E", "", p1214.13$vendor)

# safety
write.csv(p1214.13, file = "p-2014-12-13.csv", row.names = F)

# extract subsubcategories ----------------------------------------------------

# subset subsubcategories
levels(p1214.13$subcat)
p1214.13$subcat <- as.character(p1214.13$subcat)

# 13362 > 12626 > 7382 > 3552
drugs1214.13 <- subset(p1214.13, p1214.13$cat == "Drugs")
drugs1214.13 <- subset(drugs1214.13, drugs1214.13$subcat != "Other" & 
                         drugs1214.13$subcat != "Weight loss" &
                         drugs1214.13$subcat != "Benzos" &
                         drugs1214.13$subcat != "Prescription" &
                         drugs1214.13$subcat != "RCs" &
                         drugs1214.13$subcat != "Steroids" &
                         drugs1214.13$subcat != "Methylone" &
                         drugs1214.13$subcat != "Opioids" &
                         drugs1214.13$subcat != "Ecstasy-MDMA" &
                         drugs1214.13$subcat != "Barbiturates")

# extract subsubcategories
levels(as.factor(drugs1214.13$subcat))

pList3 <- drugs1214.13$list
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
#   41.059   0.448  41.654 

# bind sub-subcategories
bind1214_13b <- dplyr::left_join(p1214.13, subcat2, by = "list")
is.na(bind1214_13b$pTab3)

bind1214_13b  <- bind1214_13b [c(1, 2, 3, 4, 5, 6, 7, 11, 8, 9, 10)]
colnames(bind1214_13b) <- c("list", "date", "vendor", "product", "price", 
                            "cat", "subcat", "subsubcat", "feedback", "from", "to")

p1214.13 <- bind1214_13b

# final extracted data pre-arules/contigency table transformations
write.csv(p1214.13, file = "products-2014-12-13.csv", row.names = F)
test <- read.csv("products-2014-12-13.csv")

