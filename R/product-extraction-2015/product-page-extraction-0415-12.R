# Agora Marketplace Analysis
# Product info extraction
# 2015-04-12

# load ------------------------------------------------------------------------

library(rvest)
library(magrittr)
library(tm)
library(tidyr)
library(dplyr)

# Vendor and Date extraction --------------------------------------------------

rm(list = ls())
pDir <- "~/GitHub/ag-Product/2015-04-12"
setwd(pDir)

# 
pList <- list.files(path = pDir, pattern = ".html", all.files = T, recursive = T)
p0415.12 <- data.frame()

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
    p0415.12 <- rbind(p0415.12, pTab)
  }
)

#        user  system elapsed 
#  1121.786   21.314 1185.201

# safety
write.csv(p0415.12, file = "p-0415-12-raw.csv", row.names = F)

# clean extracted data --------------------------------------------------------

p0415.12 <- p0415.12[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(p0415.12) <- c("list", "date", "vendor", "product", 
                        "price", "cat", "feedback", "shipping")

p0415.12$vendor <- gsub("/vendor/", "", p0415.12$vendor)
p0415.12$vendor <- gsub("/user/", "", p0415.12$vendor)
p0415.12$vendor <- gsub("#", "", p0415.12$vendor)
p0415.12$vendor <- gsub("%7E", "", p0415.12$vendor)

p0415.12$shipping <- as.character(p0415.12$shipping)
p0415.12$shipping <- stripWhitespace(p0415.12$shipping)
p0415.12$shipping[p0415.12$shipping == " "] <- NA
is.na(p0415.12$shipping)

p0415.12 <- separate(p0415.12, shipping, c("from", "to"), sep = "To: ")
p0415.12$from <- gsub("From: ", "", p0415.12$from)

levels(as.factor(p0415.12$from)) # 49
levels(as.factor(p0415.12$to)) # 300

p0415.12$price <- gsub(" BTC", "", p0415.12$price)
p0415.12$price <- as.double(p0415.12$price)

write.csv(p0415.12, file = "p0415.12-c2.csv", row.names = F)

# extract subcategories -------------------------------------------------------

# subset out variables w/o a subcategory
levels(as.factor(p0415.12$cat))

p0415.12$cat <- as.character(p0415.12$cat)
p0115 <- subset(p0415.12,  p0415.12$cat != "Listings" & p0415.12$cat != "Jewelry"
                & p0415.12$cat != "Electronics" & p0415.12$cat != "Other")

# 
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
#  271.130  10.882 283.107 

# bind subcategories
bind0415_12 <- dplyr::left_join(p0415.12, subcat, by = "list")
is.na(bind0415_12$pTab2)

bind0415_12 <- bind0415_12[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
colnames(bind0415_12) <- c("list", "date", "vendor", "product", "price", 
                           "cat", "subcat", "feedback", "from", "to")

p0415.12 <- bind0415_12

# safety
write.csv(p0415.12, file = "p-2015-04-12.csv", row.names = F)

# extract subsubcategories ----------------------------------------------------

# subset subsubcategories
levels(p0415.12$subcat)
p0415.12$subcat <- as.character(p0415.12$subcat)

# 
drugs0415.12 <- subset(p0415.12, p0415.12$cat == "Drugs")
drugs0415.12 <- subset(drugs0415.12, drugs0415.12$subcat != "Other" & 
                         drugs0415.12$subcat != "Weight loss" &
                         drugs0415.12$subcat != "Benzos" &
                         drugs0415.12$subcat != "Prescription" &
                         drugs0415.12$subcat != "RCs" &
                         drugs0415.12$subcat != "Steroids" &
                         drugs0415.12$subcat != "Methylone" &
                         drugs0415.12$subcat != "Ecstasy-MDMA" &
                         drugs0415.12$subcat != "Barbiturates")

# extract subsubcategories
levels(as.factor(drugs0415.12$subcat))
pList3 <- drugs0415.12$list
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
#   96.235   1.760  98.010  

# bind sub-subcategories
bind0415_12b <- dplyr::left_join(p0415.12, subcat2, by = "list")
is.na(bind0415_12b$pTab3)

bind0415_12b  <- bind0415_12b [c(1, 2, 3, 4, 5, 6, 7, 11, 8, 9, 10)]
colnames(bind0415_12b) <- c("list", "date", "vendor", "product", "price", 
                            "cat", "subcat", "subsubcat", "feedback", "from", "to")

p0415.12 <- bind0415_12b

# final extracted data pre-arules
write.csv(p0415.12, file = "products-2015-04-12.csv", row.names = F)
test <- read.csv("products-2015-04-12.csv")
