# Agora Marketplace Analysis
# vendor feedback extraction by month
# 2014-10-11

# load ------------------------------------------------------------------------

library(rvest)
library(magrittr)
library(tm)
library(tidyr)
library(dplyr)

rm(list = ls())

# set directory
pDir <- "~/GitHub/ag-Product/2014-10-11"
setwd(pDir)

# Vendor and Date extraction --------------------------------------------------

# 14797
pList <- list.files(path = pDir, pattern = ".html", all.files = T, recursive = T)
p1014.11 <- data.frame()

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
    p1014.11 <- rbind(p1014.11, pTab)
  }
)

#    user  system elapsed 
# 688.065   7.351 695.989  

pList[5453] # w2hxgpnS3B
pList[14264]

# safety
write.csv(p1014.11, file = "p-1014-11-raw.csv", row.names = F)
p1014.11 <- read.csv("p-1014-11-raw.csv")

# clean extracted data
p1014.11 <- p1014.11[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(p1014.11) <- c("list", "date", "vendor", "product", 
                        "price", "cat", "feedback", "shipping")

p1014.11$vendor <- gsub("/vendor/", "", p1014.11$vendor)
p1014.11$vendor <- gsub("#", "", p1014.11$vendor)

p1014.11$shipping <- as.character(p1014.11$shipping)
p1014.11$shipping <- stripWhitespace(p1014.11$shipping)
p1014.11$shipping[p1014.11$shipping == " "] <- NA
is.na(p1014.11$shipping)

p1014.11 <- separate(p1014.11, shipping, c("from", "to"), sep = "To: ")
p1014.11$from <- gsub("From: ", "", p1014.11$from)

levels(as.factor(p1014.11$from)) # 52
levels(as.factor(p1014.11$to)) # 299

p1014.11$price <- gsub(" BTC", "", p1014.11$price)
p1014.11$price <- as.double(p1014.11$price)

write.csv(p1014.11, file = "p1014.11-c1.csv", row.names = F)

# extract subcategories -------------------------------------------------------

# subset out variables w/o a subcategory
levels(as.factor(p1014.11$cat))

p1014.11$cat <- as.character(p1014.11$cat)
p1014 <- subset(p1014.11,  p1014.11$cat != "Listings" & p1014.11$cat != "Jewelry"
                & p1014.11$cat != "Electronics" & p1014.11$cat != "Other")

# 14795 > 14356
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
#  171.545   3.058 174.969

# bind subcategories
bind1014_11 <- dplyr::left_join(p1014.11, subcat, by = "list")
is.na(bind1014_11$pTab2)

bind1014_11 <- bind1014_11[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
colnames(bind1014_11) <- c("list", "date", "vendor", "product", "price", 
                           "cat", "subcat", "feedback", "from", "to")

p1014.11 <- bind1014_11

# safety
write.csv(p1014.11, file = "p-2014-10-11.csv", row.names = F)

# extract subsubcategories ----------------------------------------------------

# subset subsubcategories
levels(p1014.11$subcat)
p1014.11$subcat <- as.character(p1014.11$subcat)

# 14795 > 14356 > 10428 > 6649
drugs1014.11 <- subset(p1014.11, p1014.11$cat == "Drugs")
drugs1014.11 <- subset(drugs1014.11, drugs1014.11$subcat != "Other" & 
                         drugs1014.11$subcat != "Weight loss" &
                         drugs1014.11$subcat != "Benzos" &
                         drugs1014.11$subcat != "Prescription" &
                         drugs1014.11$subcat != "RCs" &
                         drugs1014.11$subcat != "Steroids" &
                         drugs1014.11$subcat != "Methylone" &
                         drugs1014.11$subcat != "Opioids" &
                         drugs1014.11$subcat != "Ecstasy-MDMA" &
                         drugs1014.11$subcat != "Barbiturates")

# extract subsubcategories
levels(as.factor(drugs1014.11$subcat))

pList3 <- drugs1014.11$list
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
#  80.653   0.949  81.887 

# bind sub-subcategories
bind1014_11b <- dplyr::left_join(p1014.11, subcat2, by = "list")
is.na(bind1014_11b$pTab3)

bind1014_11b  <- bind1014_11b [c(1, 2, 3, 4, 5, 6, 7, 11, 8, 9, 10)]
colnames(bind1014_11b) <- c("list", "date", "vendor", "product", "price", 
                            "cat", "subcat", "subsubcat", "feedback", "from", "to")

p1014.11 <- bind1014_11b

# final extracted data pre-arules/contigency table transformations
write.csv(p1014.11, file = "products-2014-10-11.csv", row.names = F)
test <- read.csv("products-2014-10-11.csv")

