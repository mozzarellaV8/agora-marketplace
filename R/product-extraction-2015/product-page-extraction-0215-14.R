# Agora Marketplace Analysis
# product page data extraction
# 2015-02-14

# load ------------------------------------------------------------------------

library(rvest)
library(magrittr)
library(tm)
library(tidyr)
library(dplyr)

# Vendor and Date extraction --------------------------------------------------

rm(list = ls())
pDir <- "~/GitHub/ag-Product/2015-02-14"
setwd(pDir)

# 14846
pList <- list.files(path = pDir, pattern = ".html", all.files = T, recursive = T)
p0215.14 <- data.frame()

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
    p0215.14 <- rbind(p0215.14, pTab)
  }
)

#        user  system elapsed 
#  1121.786   21.314 1185.201

# safety
write.csv(p0215.14, file = "p-0215-14-raw.csv", row.names = F)

# clean extracted data
p0215.14 <- p0215.14[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(p0215.14) <- c("list", "date", "vendor", "product", 
                        "price", "cat", "feedback", "shipping")

p0215.14$vendor <- gsub("/vendor/", "", p0215.14$vendor)
p0215.14$vendor <- gsub("/user/", "", p0215.14$vendor)
p0215.14$vendor <- gsub("#", "", p0215.14$vendor)

p0215.14$shipping <- as.character(p0215.14$shipping)
p0215.14$shipping <- stripWhitespace(p0215.14$shipping)
p0215.14$shipping[p0215.14$shipping == " "] <- NA
is.na(p0215.14$shipping)

p0215.14 <- separate(p0215.14, shipping, c("from", "to"), sep = "To: ")
p0215.14$from <- gsub("From: ", "", p0215.14$from)

levels(as.factor(p0215.14$from)) # 49
levels(as.factor(p0215.14$to)) # 300

p0215.14$price <- gsub(" BTC", "", p0215.14$price)
p0215.14$price <- as.double(p0215.14$price)

write.csv(p0215.14, file = "p0215.14-c1.csv", row.names = F)

# extract subcategories -------------------------------------------------------

# subset out variables w/o a subcategory
levels(as.factor(p0215.14$cat))

p0215.14$cat <- as.character(p0215.14$cat)
p0115 <- subset(p0215.14,  p0215.14$cat != "Listings" & p0215.14$cat != "Jewelry"
                & p0215.14$cat != "Electronics" & p0215.14$cat != "Other")

# 22211 > 21512
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
bind0215_14 <- dplyr::left_join(p0215.14, subcat, by = "list")
is.na(bind0215_14$pTab2)

bind0215_14 <- bind0215_14[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
colnames(bind0215_14) <- c("list", "date", "vendor", "product", "price", 
                           "cat", "subcat", "feedback", "from", "to")

p0215.14 <- bind0215_14

# safety
write.csv(p0215.14, file = "p-2015-02-14.csv", row.names = F)

# extract subsubcategories ----------------------------------------------------

# subset subsubcategories
levels(p0215.14$subcat)
p0215.14$subcat <- as.character(p0215.14$subcat)

# 22211 > 21512 > 18009 > 9000
drugs0215.14 <- subset(p0215.14, p0215.14$cat == "Drugs")
drugs0215.14 <- subset(drugs0215.14, drugs0215.14$subcat != "Other" & 
                         drugs0215.14$subcat != "Weight loss" &
                         drugs0215.14$subcat != "Benzos" &
                         drugs0215.14$subcat != "Prescription" &
                         drugs0215.14$subcat != "RCs" &
                         drugs0215.14$subcat != "Steroids" &
                         drugs0215.14$subcat != "Methylone" &
                         drugs0215.14$subcat != "Ecstasy-MDMA" &
                         drugs0215.14$subcat != "Barbiturates")

# extract subsubcategories
levels(as.factor(drugs0215.14$subcat))

pList3 <- drugs0215.14$list
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
bind0215_14b <- dplyr::left_join(p0215.14, subcat2, by = "list")
is.na(bind0215_14b$pTab3)

bind0215_14b  <- bind0215_14b [c(1, 2, 3, 4, 5, 6, 7, 11, 8, 9, 10)]
colnames(bind0215_14b) <- c("list", "date", "vendor", "product", "price", 
                            "cat", "subcat", "subsubcat", "feedback", "from", "to")

p0215.14 <- bind0215_14b

# final extracted data pre-arules/contigency table transformations
write.csv(p0215.14, file = "products-2015-02-14.csv", row.names = F)
test <- read.csv("products-2015-02-14.csv")
