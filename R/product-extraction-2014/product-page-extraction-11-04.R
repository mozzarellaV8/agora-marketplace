# Agora Marketplace Analysis
# Product info extraction
# 2014-11-04

# load ------------------------------------------------------------------------

library(rvest)
library(magrittr)
library(tm)
library(tidyr)
library(dplyr)

rm(list = ls())

# set directory
pDir <- "~/GitHub/ag-Product/2014-11-04"
setwd(pDir)

# Vendor and Date extraction --------------------------------------------------

# 16999
pList <- list.files(path = pDir, pattern = ".html", all.files = T, recursive = T)
p1114.04 <- data.frame()

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
    p1114.04 <- rbind(p1114.04, pTab)
  }
)

#    user  system elapsed 
# 870.624  10.197 918.782

pList[9576] # KjzChryzcy - vyvanse and adderall save $2230

# safety
write.csv(p1114.04, file = "p-1114-04-raw.csv", row.names = F)
p1114.04 <- read.csv("p-1114-04-raw.csv")

# clean extracted data
p1114.04 <- p1114.04[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(p1114.04) <- c("list", "date", "vendor", "product", 
                        "price", "cat", "feedback", "shipping")

p1114.04$vendor <- gsub("/vendor/", "", p1114.04$vendor)
p1114.04$vendor <- gsub("#", "", p1114.04$vendor)

p1114.04$shipping <- as.character(p1114.04$shipping)
p1114.04$shipping <- stripWhitespace(p1114.04$shipping)
p1114.04$shipping[p1114.04$shipping == " "] <- NA
is.na(p1114.04$shipping)

p1114.04 <- separate(p1114.04, shipping, c("from", "to"), sep = "To: ")
p1114.04$from <- gsub("From: ", "", p1114.04$from)

levels(as.factor(p1114.04$from)) # 55
levels(as.factor(p1114.04$to)) # 332

p1114.04$price <- gsub(" BTC", "", p1114.04$price)
p1114.04$price <- as.double(p1114.04$price)

write.csv(p1114.04, file = "p1114.04-c1.csv", row.names = F)

# extract subcategories -------------------------------------------------------

# subset out variables w/o a subcategory
levels(as.factor(p1114.04$cat))

p1114.04$cat <- as.character(p1114.04$cat)
p1014 <- subset(p1114.04,  p1114.04$cat != "Listings" & p1114.04$cat != "Jewelry"
                & p1114.04$cat != "Electronics" & p1114.04$cat != "Other")

# 16999 > 16547
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

#      user  system elapsed 
#   216.361   4.733 222.171

# bind subcategories
bind1114_04 <- dplyr::left_join(p1114.04, subcat, by = "list")
is.na(bind1114_04$pTab2)

bind1114_04 <- bind1114_04[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
colnames(bind1114_04) <- c("list", "date", "vendor", "product", "price", 
                           "cat", "subcat", "feedback", "from", "to")

p1114.04 <- bind1114_04

# safety
write.csv(p1114.04, file = "p-2014-11-04.csv", row.names = F)

# extract subsubcategories ----------------------------------------------------

# subset subsubcategories
levels(p1114.04$subcat)
p1114.04$subcat <- as.character(p1114.04$subcat)

# 16999 > 16547 > 13503 > 8933
drugs1114.04 <- subset(p1114.04, p1114.04$cat == "Drugs")
drugs1114.04 <- subset(drugs1114.04, drugs1114.04$subcat != "Other" & 
                         drugs1114.04$subcat != "Weight loss" &
                         drugs1114.04$subcat != "Benzos" &
                         drugs1114.04$subcat != "Prescription" &
                         drugs1114.04$subcat != "RCs" &
                         drugs1114.04$subcat != "Steroids" &
                         drugs1114.04$subcat != "Methylone" &
                         drugs1114.04$subcat != "Opioids" &
                         drugs1114.04$subcat != "Ecstasy-MDMA" &
                         drugs1114.04$subcat != "Barbiturates")

# extract subsubcategories
levels(as.factor(drugs1114.04$subcat))

pList3 <- drugs1114.04$list
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
# 105.656   1.463 107.387

# bind sub-subcategories
bind1114_04b <- dplyr::left_join(p1114.04, subcat2, by = "list")
is.na(bind1114_04b$pTab3)

bind1114_04b  <- bind1114_04b [c(1, 2, 3, 4, 5, 6, 7, 11, 8, 9, 10)]
colnames(bind1114_04b) <- c("list", "date", "vendor", "product", "price", 
                            "cat", "subcat", "subsubcat", "feedback", "from", "to")

p1114.04 <- bind1114_04b

# final extracted data pre-arules/contigency table transformations
write.csv(p1114.04, file = "products-2014-11-04.csv", row.names = F)
test <- read.csv("products-2014-11-04.csv")

