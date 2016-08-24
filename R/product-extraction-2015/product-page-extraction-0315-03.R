# Agora Marketplace Analysis
# Product info extraction
# 2015-03-03

# load ------------------------------------------------------------------------

library(rvest)
library(magrittr)
library(tm)
library(tidyr)
library(dplyr)

# Vendor and Date extraction --------------------------------------------------

rm(list = ls())
pDir <- "~/GitHub/ag-Product/2015-03-03"
setwd(pDir)

# 
pList <- list.files(path = pDir, pattern = ".html", all.files = T, recursive = T)
p0315.03 <- data.frame()

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
    p0315.03 <- rbind(p0315.03, pTab)
  }
)

#        user  system elapsed 
#  1121.786   21.314 1185.201

# safety
write.csv(p0315.03, file = "p-0315-03-raw.csv", row.names = F)

# clean extracted data
p0315.03 <- p0315.03[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(p0315.03) <- c("list", "date", "vendor", "product", 
                        "price", "cat", "feedback", "shipping")

p0315.03$vendor <- gsub("/vendor/", "", p0315.03$vendor)
p0315.03$vendor <- gsub("/user/", "", p0315.03$vendor)
p0315.03$vendor <- gsub("#", "", p0315.03$vendor)

p0315.03$shipping <- as.character(p0315.03$shipping)
p0315.03$shipping <- stripWhitespace(p0315.03$shipping)
p0315.03$shipping[p0315.03$shipping == " "] <- NA
is.na(p0315.03$shipping)

p0315.03 <- separate(p0315.03, shipping, c("from", "to"), sep = "To: ")
p0315.03$from <- gsub("From: ", "", p0315.03$from)

levels(as.factor(p0315.03$from)) # 49
levels(as.factor(p0315.03$to)) # 300

p0315.03$price <- gsub(" BTC", "", p0315.03$price)
p0315.03$price <- as.double(p0315.03$price)

write.csv(p0315.03, file = "p0315.03-c1.csv", row.names = F)

# extract subcategories -------------------------------------------------------

# subset out variables w/o a subcategory
levels(as.factor(p0315.03$cat))

p0315.03$cat <- as.character(p0315.03$cat)
p0115 <- subset(p0315.03,  p0315.03$cat != "Listings" & p0315.03$cat != "Jewelry"
                & p0315.03$cat != "Electronics" & p0315.03$cat != "Other")

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
bind0215_01 <- dplyr::left_join(p0315.03, subcat, by = "list")
is.na(bind0215_01$pTab2)

bind0215_01 <- bind0215_01[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
colnames(bind0215_01) <- c("list", "date", "vendor", "product", "price", 
                           "cat", "subcat", "feedback", "from", "to")

p0315.03 <- bind0215_01

# safety
write.csv(p0315.03, file = "p-2015-03-03.csv", row.names = F)

# extract subsubcategories ----------------------------------------------------

# subset subsubcategories
levels(p0315.03$subcat)
p0315.03$subcat <- as.character(p0315.03$subcat)

# 22211 > 21512 > 18009 > 9000
drugs0315.03 <- subset(p0315.03, p0315.03$cat == "Drugs")
drugs0315.03 <- subset(drugs0315.03, drugs0315.03$subcat != "Other" & 
                         drugs0315.03$subcat != "Weight loss" &
                         drugs0315.03$subcat != "Benzos" &
                         drugs0315.03$subcat != "Prescription" &
                         drugs0315.03$subcat != "RCs" &
                         drugs0315.03$subcat != "Steroids" &
                         drugs0315.03$subcat != "Methylone" &
                         drugs0315.03$subcat != "Opioids" &
                         drugs0315.03$subcat != "Ecstasy-MDMA" &
                         drugs0315.03$subcat != "Barbiturates")

# extract subsubcategories
levels(as.factor(drugs0315.03$subcat))

pList3 <- drugs0315.03$list
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
bind0315_03b <- dplyr::left_join(p0315.03, subcat2, by = "list")
is.na(bind0315_03b$pTab3)

bind0315_03b  <- bind0315_03b [c(1, 2, 3, 4, 5, 6, 7, 11, 8, 9, 10)]
colnames(bind0315_03b) <- c("list", "date", "vendor", "product", "price", 
                            "cat", "subcat", "subsubcat", "feedback", "from", "to")

p0315.03 <- bind0315_03b

# final extracted data pre-arules/contigency table transformations
write.csv(p0315.03, file = "products-2015-03-03.csv", row.names = F)
test <- read.csv("products-2015-03-03.csv")
