# Agora Marketplace Analysis
# vendor feedback extraction by month
# 2014-10-09

# load ------------------------------------------------------------------------

library(rvest)
library(magrittr)
library(tm)
library(tidyr)
library(dplyr)

rm(list = ls())

# set directory
pDir <- "~/GitHub/ag-Product/2014-10-09"
setwd(pDir)

# Vendor and Date extraction --------------------------------------------------

# 14944
pList <- list.files(path = pDir, pattern = ".html", all.files = T, recursive = T)
p1014.09 <- data.frame()

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
    p1014.09 <- rbind(p1014.09, pTab)
  }
)

#    user  system elapsed 
# 757.833   9.027 801.329  

# safety
write.csv(p1014.09, file = "p-1014-09-raw.csv", row.names = F)
p1014.09 <- read.csv("p-1014-09-raw.csv")

# clean extracted data
p1014.09 <- p1014.09[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(p1014.09) <- c("list", "date", "vendor", "product", 
                        "price", "cat", "feedback", "shipping")

p1014.09$vendor <- gsub("/vendor/", "", p1014.09$vendor)
p1014.09$vendor <- gsub("#", "", p1014.09$vendor)

p1014.09$shipping <- as.character(p1014.09$shipping)
p1014.09$shipping <- stripWhitespace(p1014.09$shipping)
p1014.09$shipping[p1014.09$shipping == " "] <- NA
is.na(p1014.09$shipping)

p1014.09 <- separate(p1014.09, shipping, c("from", "to"), sep = "To: ")
p1014.09$from <- gsub("From: ", "", p1014.09$from)

levels(as.factor(p1014.09$from)) # 58
levels(as.factor(p1014.09$to)) # 321

p1014.09$price <- gsub(" BTC", "", p1014.09$price)
p1014.09$price <- as.double(p1014.09$price)

write.csv(p1014.09, file = "p1014.09-c1.csv", row.names = F)

# extract subcategories -------------------------------------------------------

# subset out variables w/o a subcategory
levels(as.factor(p1014.09$cat))

p1014.09$cat <- as.character(p1014.09$cat)
p1014 <- subset(p1014.09,  p1014.09$cat != "Listings" & p1014.09$cat != "Jewelry"
                & p1014.09$cat != "Electronics" & p1014.09$cat != "Other")

# 14944 > 14541
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
#  176.454   3.493 180.625

# bind subcategories
bind1014_09 <- dplyr::left_join(p1014.09, subcat, by = "list")
is.na(bind1014_09$pTab2)

bind1014_09 <- bind1014_09[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
colnames(bind1014_09) <- c("list", "date", "vendor", "product", "price", 
                           "cat", "subcat", "feedback", "from", "to")

p1014.09 <- bind1014_09

# safety
write.csv(p1014.09, file = "p-2014-10-09.csv", row.names = F)

# extract subsubcategories ----------------------------------------------------

# subset subsubcategories
levels(p1014.09$subcat)
p1014.09$subcat <- as.character(p1014.09$subcat)

# 14944 > 14541 > 11005 > 7367
drugs1014.09 <- subset(p1014.09, p1014.09$cat == "Drugs")
drugs1014.09 <- subset(drugs1014.09, drugs1014.09$subcat != "Other" & 
                         drugs1014.09$subcat != "Weight loss" &
                         drugs1014.09$subcat != "Benzos" &
                         drugs1014.09$subcat != "Prescription" &
                         drugs1014.09$subcat != "RCs" &
                         drugs1014.09$subcat != "Steroids" &
                         drugs1014.09$subcat != "Methylone" &
                         drugs1014.09$subcat != "Opioids" &
                         drugs1014.09$subcat != "Ecstasy-MDMA" &
                         drugs1014.09$subcat != "Barbiturates")

# extract subsubcategories
levels(as.factor(drugs1014.09$subcat))

pList3 <- drugs1014.09$list
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
#  82.050   1.086  83.133

# bind sub-subcategories
bind1014_09b <- dplyr::left_join(p1014.09, subcat2, by = "list")
is.na(bind1014_09b$pTab3)

bind1014_09b  <- bind1014_09b [c(1, 2, 3, 4, 5, 6, 7, 11, 8, 9, 10)]
colnames(bind1014_09b) <- c("list", "date", "vendor", "product", "price", 
                            "cat", "subcat", "subsubcat", "feedback", "from", "to")

p1014.09 <- bind1014_09b

# final extracted data pre-arules/contigency table transformations
write.csv(p1014.09, file = "products-2014-10-09.csv", row.names = F)
test <- read.csv("products-2014-10-09.csv")

