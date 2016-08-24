# Agora Marketplace Analysis
# Product info extraction
# 2015-07-01

# load ------------------------------------------------------------------------

library(rvest)
library(magrittr)
library(tm)
library(tidyr)
library(dplyr)

# Vendor and Date extraction --------------------------------------------------

rm(list = ls())
pDir <- "~/GitHub/ag-Product/2015-07-01"
setwd(pDir)

# 22211
pList <- list.files(path = pDir, pattern = ".html", all.files = T, recursive = T)
p0715.01 <- data.frame()

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
    p0715.01 <- rbind(p0715.01, pTab)
  }
)

#        user  system elapsed 
#  1121.786   21.314 1185.201

# safety
write.csv(p0715.01, file = "p-0715-01-raw.csv", row.names = F)
p0715.01 <- read.csv("p-0715-01-raw.csv")

# clean extracted data
p0715.01 <- p0715.01[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(p0715.01) <- c("list", "date", "vendor", "product", 
                        "price", "cat", "feedback", "shipping")

p0715.01$vendor <- gsub("/vendor/", "", p0715.01$vendor)
p0715.01$vendor <- gsub("/user/", "", p0715.01$vendor)
p0715.01$vendor <- gsub("#", "", p0715.01$vendor)

p0715.01$shipping <- as.character(p0715.01$shipping)
p0715.01$shipping <- stripWhitespace(p0715.01$shipping)
p0715.01$shipping[p0715.01$shipping == " "] <- NA
is.na(p0715.01$shipping)

p0715.01 <- separate(p0715.01, shipping, c("from", "to"), sep = "To: ")
p0715.01$from <- gsub("From: ", "", p0715.01$from)

levels(as.factor(p0715.01$from)) # 49
levels(as.factor(p0715.01$to)) # 300

p0715.01$price <- gsub(" BTC", "", p0715.01$price)
p0715.01$price <- as.double(p0715.01$price)

write.csv(p0715.01, file = "p0715.01-c1.csv", row.names = F)

# extract subcategories -------------------------------------------------------

# subset out variables w/o a subcategory
levels(as.factor(p0715.01$cat))

p0715.01$cat <- as.character(p0715.01$cat)
p0115 <- subset(p0715.01,  p0715.01$cat != "Listings" & p0715.01$cat != "Jewelry"
                & p0715.01$cat != "Electronics" & p0715.01$cat != "Other")

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
bind0215_01 <- dplyr::left_join(p0715.01, subcat, by = "list")
is.na(bind0215_01$pTab2)

bind0215_01 <- bind0215_01[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
colnames(bind0215_01) <- c("list", "date", "vendor", "product", "price", 
                           "cat", "subcat", "feedback", "from", "to")

p0715.01 <- bind0215_01

# safety
write.csv(p0715.01, file = "p-2015-07-01.csv", row.names = F)

# extract subsubcategories ----------------------------------------------------

# subset subsubcategories
levels(p0715.01$subcat)
p0715.01$subcat <- as.character(p0715.01$subcat)

# 22211 > 21512 > 18009 > 9000
drugs0715.01 <- subset(p0715.01, p0715.01$cat == "Drugs")
drugs0715.01 <- subset(drugs0715.01, drugs0715.01$subcat != "Other" & 
                         drugs0715.01$subcat != "Weight loss" &
                         drugs0715.01$subcat != "Benzos" &
                         drugs0715.01$subcat != "Prescription" &
                         drugs0715.01$subcat != "RCs" &
                         drugs0715.01$subcat != "Steroids" &
                         drugs0715.01$subcat != "Methylone" &
                         drugs0715.01$subcat != "Opioids" &
                         drugs0715.01$subcat != "Ecstasy-MDMA" &
                         drugs0715.01$subcat != "Barbiturates")

# extract subsubcategories
levels(as.factor(drugs0715.01$subcat))

pList3 <- drugs0715.01$list
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
bind0715_01b <- dplyr::left_join(p0715.01, subcat2, by = "list")
is.na(bind0715_01b$pTab3)

bind0715_01b  <- bind0715_01b [c(1, 2, 3, 4, 5, 6, 7, 11, 8, 9, 10)]
colnames(bind0715_01b) <- c("list", "date", "vendor", "product", "price", 
                            "cat", "subcat", "subsubcat", "feedback", "from", "to")

p0715.01 <- bind0715_01b

# final extracted data pre-arules/contigency table transformations
write.csv(p0715.01, file = "products-2015-07-01.csv", row.names = F)
test <- read.csv("products-2015-07-01.csv")
