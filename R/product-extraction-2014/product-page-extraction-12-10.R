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
pDir <- "~/GitHub/ag-Product/2014-12-10"
setwd(pDir)

# 19503
pList <- list.files(path = pDir, pattern = ".html", all.files = T, recursive = T)
p1214.10 <- data.frame()

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
    p1214.10 <- rbind(p1214.10, pTab)
  }
)

#       user  system elapsed 
#  976.577   12.065 1006.099

pList[7481] # cCs6iZqi6f - amnesia haze

# safety
write.csv(p1214.10, file = "p-1214-10-raw.csv", row.names = F)
p1214.10 <- read.csv("p-1214-10-raw.csv")

# clean extracted data
p1214.10 <- p1214.10[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(p1214.10) <- c("list", "date", "vendor", "product", 
                        "price", "cat", "feedback", "shipping")

p1214.10$vendor <- gsub("/vendor/", "", p1214.10$vendor)
p1214.10$vendor <- gsub("/user/", "", p1214.10$vendor)
p1214.10$vendor <- gsub("#", "", p1214.10$vendor)

p1214.10$shipping <- as.character(p1214.10$shipping)
p1214.10$shipping <- stripWhitespace(p1214.10$shipping)
p1214.10$shipping[p1214.10$shipping == " "] <- NA
is.na(p1214.10$shipping)

p1214.10 <- separate(p1214.10, shipping, c("from", "to"), sep = "To: ")
p1214.10$from <- gsub("From: ", "", p1214.10$from)

levels(as.factor(p1214.10$from)) # 52
levels(as.factor(p1214.10$to)) # 293

p1214.10$price <- gsub(" BTC", "", p1214.10$price)
p1214.10$price <- as.double(p1214.10$price)

write.csv(p1214.10, file = "p1214.10-c1.csv", row.names = F)

# extract subcategories -------------------------------------------------------

# subset out variables w/o a subcategory
levels(as.factor(p1214.10$cat))

p1214.10$cat <- as.character(p1214.10$cat)
p1214 <- subset(p1214.10,  p1214.10$cat != "Listings" & p1214.10$cat != "Jewelry"
                & p1214.10$cat != "Electronics" & p1214.10$cat != "Other")

# 19502 > 18740
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
#  235.452   5.525 241.648 

# bind subcategories
bind1214_10 <- dplyr::left_join(p1214.10, subcat, by = "list")
is.na(bind1214_10$pTab2)

bind1214_10 <- bind1214_10[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
colnames(bind1214_10) <- c("list", "date", "vendor", "product", "price", 
                           "cat", "subcat", "feedback", "from", "to")

p1214.10 <- bind1214_10

# safety
write.csv(p1214.10, file = "p-2014-12-10.csv", row.names = F)

# extract subsubcategories ----------------------------------------------------

# subset subsubcategories
levels(p1214.10$subcat)
p1214.10$subcat <- as.character(p1214.10$subcat)

# 19502 > 18740 > 13357 > 8710
drugs1214.10 <- subset(p1214.10, p1214.10$cat == "Drugs")
drugs1214.10 <- subset(drugs1214.10, drugs1214.10$subcat != "Other" & 
                         drugs1214.10$subcat != "Weight loss" &
                         drugs1214.10$subcat != "Benzos" &
                         drugs1214.10$subcat != "Prescription" &
                         drugs1214.10$subcat != "RCs" &
                         drugs1214.10$subcat != "Steroids" &
                         drugs1214.10$subcat != "Methylone" &
                         drugs1214.10$subcat != "Opioids" &
                         drugs1214.10$subcat != "Ecstasy-MDMA" &
                         drugs1214.10$subcat != "Barbiturates")

# extract subsubcategories
levels(as.factor(drugs1214.10$subcat))

pList3 <- drugs1214.10$list
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
#  105.711   1.106 107.118

# bind sub-subcategories
bind1214_10b <- dplyr::left_join(p1214.10, subcat2, by = "list")
is.na(bind1214_10b$pTab3)

bind1214_10b  <- bind1214_10b [c(1, 2, 3, 4, 5, 6, 7, 11, 8, 9, 10)]
colnames(bind1214_10b) <- c("list", "date", "vendor", "product", "price", 
                            "cat", "subcat", "subsubcat", "feedback", "from", "to")

p1214.10 <- bind1214_10b

# final extracted data pre-arules/contigency table transformations
write.csv(p1214.10, file = "products-2014-12-10.csv", row.names = F)
test <- read.csv("products-2014-12-10.csv")

