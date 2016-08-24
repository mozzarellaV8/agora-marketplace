# Agora Marketplace Analysis
# Product info extraction
# 2015-01-06

# load ------------------------------------------------------------------------

library(rvest)
library(magrittr)
library(tm)
library(tidyr)
library(dplyr)

# Vendor and Date extraction --------------------------------------------------

rm(list = ls())
pDir <- "~/GitHub/ag-Product/2015-01-06"
setwd(pDir)

# 22211
pList <- list.files(path = pDir, pattern = ".html", all.files = T, recursive = T)
p0115.06 <- data.frame()

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
    p0115.06 <- rbind(p0115.06, pTab)
  }
)

#        user  system elapsed 
#  1121.786   21.314 1185.201

# safety
write.csv(p0115.06, file = "p-0115-06-raw.csv", row.names = F)
p0115.06 <- read.csv("p-0115-06-raw.csv")

# clean extracted data
p0115.06 <- p0115.06[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(p0115.06) <- c("list", "date", "vendor", "product", 
                        "price", "cat", "feedback", "shipping")

p0115.06$vendor <- gsub("/vendor/", "", p0115.06$vendor)
p0115.06$vendor <- gsub("/user/", "", p0115.06$vendor)
p0115.06$vendor <- gsub("#", "", p0115.06$vendor)

p0115.06$shipping <- as.character(p0115.06$shipping)
p0115.06$shipping <- stripWhitespace(p0115.06$shipping)
p0115.06$shipping[p0115.06$shipping == " "] <- NA
is.na(p0115.06$shipping)

p0115.06 <- separate(p0115.06, shipping, c("from", "to"), sep = "To: ")
p0115.06$from <- gsub("From: ", "", p0115.06$from)

levels(as.factor(p0115.06$from)) # 49
levels(as.factor(p0115.06$to)) # 300

p0115.06$price <- gsub(" BTC", "", p0115.06$price)
p0115.06$price <- as.double(p0115.06$price)

write.csv(p0115.06, file = "p0115.06-c1.csv", row.names = F)

# extract subcategories -------------------------------------------------------

# subset out variables w/o a subcategory
levels(as.factor(p0115.06$cat))

p0115.06$cat <- as.character(p0115.06$cat)
p0115 <- subset(p0115.06,  p0115.06$cat != "Listings" & p0115.06$cat != "Jewelry"
                & p0115.06$cat != "Electronics" & p0115.06$cat != "Other")

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
bind0115_06 <- dplyr::left_join(p0115.06, subcat, by = "list")
is.na(bind0115_06$pTab2)

bind0115_06 <- bind0115_06[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
colnames(bind0115_06) <- c("list", "date", "vendor", "product", "price", 
                           "cat", "subcat", "feedback", "from", "to")

p0115.06 <- bind0115_06

# safety
write.csv(p0115.06, file = "p-2015-01-06.csv", row.names = F)

# extract subsubcategories ----------------------------------------------------

# subset subsubcategories
levels(p0115.06$subcat)
p0115.06$subcat <- as.character(p0115.06$subcat)

# 22211 > 21512 > 18009 > 9000
drugs0115.06 <- subset(p0115.06, p0115.06$cat == "Drugs")
drugs0115.06 <- subset(drugs0115.06, drugs0115.06$subcat != "Other" & 
                         drugs0115.06$subcat != "Weight loss" &
                         drugs0115.06$subcat != "Benzos" &
                         drugs0115.06$subcat != "Prescription" &
                         drugs0115.06$subcat != "RCs" &
                         drugs0115.06$subcat != "Steroids" &
                         drugs0115.06$subcat != "Methylone" &
                         drugs0115.06$subcat != "Opioids" &
                         drugs0115.06$subcat != "Ecstasy-MDMA" &
                         drugs0115.06$subcat != "Barbiturates")

# extract subsubcategories
levels(as.factor(drugs0115.06$subcat))

pList3 <- drugs0115.06$list
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
bind0115_06b <- dplyr::left_join(p0115.06, subcat2, by = "list")
is.na(bind0115_06b$pTab3)

bind0115_06b  <- bind0115_06b [c(1, 2, 3, 4, 5, 6, 7, 11, 8, 9, 10)]
colnames(bind0115_06b) <- c("list", "date", "vendor", "product", "price", 
                            "cat", "subcat", "subsubcat", "feedback", "from", "to")

p0115.06 <- bind0115_06b

# final extracted data pre-arules/contigency table transformations
write.csv(p0115.06, file = "products-2015-01-06.csv", row.names = F)
test <- read.csv("products-2015-01-06.csv")
