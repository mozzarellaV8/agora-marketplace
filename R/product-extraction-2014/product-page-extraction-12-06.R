# Agora Marketplace Analysis
# Product info extraction
# 2014-12-06

# load ------------------------------------------------------------------------

library(rvest)
library(magrittr)
library(tm)
library(tidyr)
library(dplyr)

# Vendor and Date extraction --------------------------------------------------

rm(list = ls())
pDir <- "~/GitHub/ag-Product/2014-12-06"
setwd(pDir)

# 18828
pList <- list.files(path = pDir, pattern = ".html", all.files = T, recursive = T)
p1214.06 <- data.frame()

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
    p1214.06 <- rbind(p1214.06, pTab)
  }
)

#       user  system elapsed 
#    871.848  13.987 894.341

pList[8695] # r3WFSdv0Uw - Cannabis gem candies - incomplete
pList[14388] # b8BYrjTEz5 - blank

# safety
write.csv(p1214.06, file = "p-1214-06-raw.csv", row.names = F)
p1214.06 <- read.csv("p-1214-06-raw.csv")

# clean extracted data
p1214.06 <- p1214.06[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(p1214.06) <- c("list", "date", "vendor", "product", 
                        "price", "cat", "feedback", "shipping")

p1214.06$vendor <- gsub("/vendor/", "", p1214.06$vendor)
p1214.06$vendor <- gsub("/user/", "", p1214.06$vendor)
p1214.06$vendor <- gsub("#", "", p1214.06$vendor)

p1214.06$shipping <- as.character(p1214.06$shipping)
p1214.06$shipping <- stripWhitespace(p1214.06$shipping)
p1214.06$shipping[p1214.06$shipping == " "] <- NA
is.na(p1214.06$shipping)

p1214.06 <- separate(p1214.06, shipping, c("from", "to"), sep = "To: ")
p1214.06$from <- gsub("From: ", "", p1214.06$from)

levels(as.factor(p1214.06$from)) # 51
levels(as.factor(p1214.06$to)) # 318

p1214.06$price <- gsub(" BTC", "", p1214.06$price)
p1214.06$price <- as.double(p1214.06$price)

write.csv(p1214.06, file = "p1214.06-c1.csv", row.names = F)

# extract subcategories -------------------------------------------------------

# subset out variables w/o a subcategory
levels(as.factor(p1214.06$cat))

p1214.06$cat <- as.character(p1214.06$cat)
p1214 <- subset(p1214.06,  p1214.06$cat != "Listings" & p1214.06$cat != "Jewelry"
                & p1214.06$cat != "Electronics" & p1214.06$cat != "Other")

# 18827 > 18136
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
#  206.575   6.877 213.420 

# bind subcategories
bind1214_06 <- dplyr::left_join(p1214.06, subcat, by = "list")
is.na(bind1214_06$pTab2)

bind1214_06 <- bind1214_06[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
colnames(bind1214_06) <- c("list", "date", "vendor", "product", "price", 
                           "cat", "subcat", "feedback", "from", "to")

p1214.06 <- bind1214_06

# safety
write.csv(p1214.06, file = "p-2014-12-06.csv", row.names = F)

# extract subsubcategories ----------------------------------------------------

# subset subsubcategories
levels(p1214.06$subcat)
p1214.06$subcat <- as.character(p1214.06$subcat)

# 18827 > 18136 > 13530 > 9124
drugs1214.06 <- subset(p1214.06, p1214.06$cat == "Drugs")
drugs1214.06 <- subset(drugs1214.06, drugs1214.06$subcat != "Other" & 
                         drugs1214.06$subcat != "Weight loss" &
                         drugs1214.06$subcat != "Benzos" &
                         drugs1214.06$subcat != "Prescription" &
                         drugs1214.06$subcat != "RCs" &
                         drugs1214.06$subcat != "Steroids" &
                         drugs1214.06$subcat != "Methylone" &
                         drugs1214.06$subcat != "Opioids" &
                         drugs1214.06$subcat != "Ecstasy-MDMA" &
                         drugs1214.06$subcat != "Barbiturates")

# extract subsubcategories
levels(as.factor(drugs1214.06$subcat))

pList3 <- drugs1214.06$list
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
#   97.598   1.634  99.228

# bind sub-subcategories
bind1214_06b <- dplyr::left_join(p1214.06, subcat2, by = "list")
is.na(bind1214_06b$pTab3)

bind1214_06b  <- bind1214_06b [c(1, 2, 3, 4, 5, 6, 7, 11, 8, 9, 10)]
colnames(bind1214_06b) <- c("list", "date", "vendor", "product", "price", 
                            "cat", "subcat", "subsubcat", "feedback", "from", "to")

p1214.06 <- bind1214_06b

# final extracted data pre-arules/contigency table transformations
write.csv(p1214.06, file = "products-2014-12-06.csv", row.names = F)
test <- read.csv("products-2014-12-06.csv")

