# Agora Marketplace Analysis
# Product info extraction
# 2014-11-14

# load ------------------------------------------------------------------------

library(rvest)
library(magrittr)
library(tm)
library(tidyr)
library(dplyr)

rm(list = ls())

# set directory
pDir <- "~/GitHub/ag-Product/2014-11-14"
setwd(pDir)

# Vendor and Date extraction --------------------------------------------------

# 14769
pList <- list.files(path = pDir, pattern = ".html", all.files = T, recursive = T)
p1114.14 <- data.frame()

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
    p1114.14 <- rbind(p1114.14, pTab)
  }
)

#    user  system elapsed 
# 533.082   4.969 562.240

# safety
write.csv(p1114.14, file = "p-1114-14-raw.csv", row.names = F)
p1114.14 <- read.csv("p-1114-14-raw.csv")

# clean extracted data
p1114.14 <- p1114.14[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(p1114.14) <- c("list", "date", "vendor", "product", 
                        "price", "cat", "feedback", "shipping")

p1114.14$vendor <- gsub("/vendor/", "", p1114.14$vendor)
p1114.14$vendor <- gsub("/user/", "", p1114.14$vendor)
p1114.14$vendor <- gsub("#", "", p1114.14$vendor)

p1114.14$shipping <- as.character(p1114.14$shipping)
p1114.14$shipping <- stripWhitespace(p1114.14$shipping)
p1114.14$shipping[p1114.14$shipping == " "] <- NA
is.na(p1114.14$shipping)

p1114.14 <- separate(p1114.14, shipping, c("from", "to"), sep = "To: ")
p1114.14$from <- gsub("From: ", "", p1114.14$from)

levels(as.factor(p1114.14$from)) # 44
levels(as.factor(p1114.14$to)) # 186

p1114.14$price <- gsub(" BTC", "", p1114.14$price)
p1114.14$price <- as.double(p1114.14$price)

write.csv(p1114.14, file = "p1114.14-c1.csv", row.names = F)

# extract subcategories -------------------------------------------------------

# subset out variables w/o a subcategory
levels(as.factor(p1114.14$cat))

p1114.14$cat <- as.character(p1114.14$cat)
p1014 <- subset(p1114.14,  p1114.14$cat != "Listings" & p1114.14$cat != "Jewelry"
                & p1114.14$cat != "Electronics" & p1114.14$cat != "Other")

# 11020 > 10260
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
#  127.235   1.611 129.206 

# bind subcategories
bind1114_14 <- dplyr::left_join(p1114.14, subcat, by = "list")
is.na(bind1114_14$pTab2)

bind1114_14 <- bind1114_14[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
colnames(bind1114_14) <- c("list", "date", "vendor", "product", "price", 
                           "cat", "subcat", "feedback", "from", "to")

p1114.14 <- bind1114_14

# safety
write.csv(p1114.14, file = "p-2014-11-14.csv", row.names = F)

# extract subsubcategories ----------------------------------------------------

# subset subsubcategories
levels(p1114.14$subcat)
p1114.14$subcat <- as.character(p1114.14$subcat)

# 11020 > 10260 > 5808 > 2103
drugs1114.14 <- subset(p1114.14, p1114.14$cat == "Drugs")
drugs1114.14 <- subset(drugs1114.14, drugs1114.14$subcat != "Other" & 
                         drugs1114.14$subcat != "Weight loss" &
                         drugs1114.14$subcat != "Benzos" &
                         drugs1114.14$subcat != "Prescription" &
                         drugs1114.14$subcat != "RCs" &
                         drugs1114.14$subcat != "Steroids" &
                         drugs1114.14$subcat != "Methylone" &
                         drugs1114.14$subcat != "Opioids" &
                         drugs1114.14$subcat != "Ecstasy-MDMA" &
                         drugs1114.14$subcat != "Barbiturates")

# extract subsubcategories
levels(as.factor(drugs1114.14$subcat))

pList3 <- drugs1114.14$list
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
#  23.583   0.263  23.880

# bind sub-subcategories
bind1114_14b <- dplyr::left_join(p1114.14, subcat2, by = "list")
is.na(bind1114_14b$pTab3)

bind1114_14b  <- bind1114_14b [c(1, 2, 3, 4, 5, 6, 7, 11, 8, 9, 10)]
colnames(bind1114_14b) <- c("list", "date", "vendor", "product", "price", 
                            "cat", "subcat", "subsubcat", "feedback", "from", "to")

p1114.14 <- bind1114_14b

# final extracted data pre-arules/contigency table transformations
write.csv(p1114.14, file = "products-2014-11-14.csv", row.names = F)
test <- read.csv("products-2014-11-14.csv")

