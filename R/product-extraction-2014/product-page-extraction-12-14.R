# Agora Marketplace Analysis
# Product info extraction
# 2014-12-08

# load ------------------------------------------------------------------------

library(rvest)
library(magrittr)
library(tm)
library(tidyr)
library(dplyr)

# Vendor and Date extraction --------------------------------------------------

rm(list = ls())
pDir <- "~/GitHub/ag-Product/2014-12-14"
setwd(pDir)

# 12331
pList <- list.files(path = pDir, pattern = ".html", all.files = T, recursive = T)
p1214.14 <- data.frame()

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
    p1214.14 <- rbind(p1214.14, pTab)
  }
)

#       user  system elapsed 
#    594.797   5.818 643.770

pList[8695] # r3WFSdv0Uw - Cannabis gem candies - incomplete
pList[11037] # -twerking princess nBome

# safety
write.csv(p1214.14, file = "p-1214-14-raw.csv", row.names = F)
p1214.14 <- read.csv("p-1214-14-raw.csv")

# clean extracted data
p1214.14 <- p1214.14[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(p1214.14) <- c("list", "date", "vendor", "product", 
                        "price", "cat", "feedback", "shipping")

p1214.14$vendor <- gsub("/vendor/", "", p1214.14$vendor)
p1214.14$vendor <- gsub("/user/", "", p1214.14$vendor)
p1214.14$vendor <- gsub("#", "", p1214.14$vendor)

p1214.14$shipping <- as.character(p1214.14$shipping)
p1214.14$shipping <- stripWhitespace(p1214.14$shipping)
p1214.14$shipping[p1214.14$shipping == " "] <- NA
is.na(p1214.14$shipping)

p1214.14 <- separate(p1214.14, shipping, c("from", "to"), sep = "To: ")
p1214.14$from <- gsub("From: ", "", p1214.14$from)

levels(as.factor(p1214.14$from)) # 49
levels(as.factor(p1214.14$to)) # 171

p1214.14$price <- gsub(" BTC", "", p1214.14$price)
p1214.14$price <- as.double(p1214.14$price)

write.csv(p1214.14, file = "p1214.14-c1.csv", row.names = F)

# extract subcategories -------------------------------------------------------

# subset out variables w/o a subcategory
levels(as.factor(p1214.14$cat))

p1214.14$cat <- as.character(p1214.14$cat)
p1214 <- subset(p1214.14,  p1214.14$cat != "Listings" & p1214.14$cat != "Jewelry"
                & p1214.14$cat != "Electronics" & p1214.14$cat != "Other")

# 12331 > 11597
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
#  135.250   2.195 137.702  

# bind subcategories
bind1214_14 <- dplyr::left_join(p1214.14, subcat, by = "list")
is.na(bind1214_14$pTab2)

bind1214_14 <- bind1214_14[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
colnames(bind1214_14) <- c("list", "date", "vendor", "product", "price", 
                           "cat", "subcat", "feedback", "from", "to")

p1214.14 <- bind1214_14

# safety
write.csv(p1214.14, file = "p-2014-12-14.csv", row.names = F)

# extract subsubcategories ----------------------------------------------------

# subset subsubcategories
levels(p1214.14$subcat)
p1214.14$subcat <- as.character(p1214.14$subcat)

# 12331 > 11597 > 6399 > 2510
drugs1214.14 <- subset(p1214.14, p1214.14$cat == "Drugs")
drugs1214.14 <- subset(drugs1214.14, drugs1214.14$subcat != "Other" & 
                         drugs1214.14$subcat != "Weight loss" &
                         drugs1214.14$subcat != "Benzos" &
                         drugs1214.14$subcat != "Prescription" &
                         drugs1214.14$subcat != "RCs" &
                         drugs1214.14$subcat != "Steroids" &
                         drugs1214.14$subcat != "Methylone" &
                         drugs1214.14$subcat != "Opioids" &
                         drugs1214.14$subcat != "Ecstasy-MDMA" &
                         drugs1214.14$subcat != "Barbiturates")

# extract subsubcategories
levels(as.factor(drugs1214.14$subcat))

pList3 <- drugs1214.14$list
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
#   27.811   0.298  28.161

# bind sub-subcategories
bind1214_14b <- dplyr::left_join(p1214.14, subcat2, by = "list")
is.na(bind1214_14b$pTab3)

bind1214_14b  <- bind1214_14b [c(1, 2, 3, 4, 5, 6, 7, 11, 8, 9, 10)]
colnames(bind1214_14b) <- c("list", "date", "vendor", "product", "price", 
                            "cat", "subcat", "subsubcat", "feedback", "from", "to")

p1214.14 <- bind1214_14b
p1214.14$vendor <- gsub("%7E", "", p1214.14$vendor)

# final extracted data pre-arules/contigency table transformations
write.csv(p1214.14, file = "products-2014-12-14.csv", row.names = F)
test <- read.csv("products-2014-12-14.csv")

