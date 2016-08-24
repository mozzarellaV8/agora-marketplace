# Agora Marketplace Analysis
# Product info extraction
# 2015-02-04

# load ------------------------------------------------------------------------

library(rvest)
library(magrittr)
library(tm)
library(tidyr)
library(dplyr)

# Vendor and Date extraction --------------------------------------------------

rm(list = ls())
pDir <- "~/GitHub/ag-Product/2015-02-06"
setwd(pDir)

# 16768
pList <- list.files(path = pDir, pattern = ".html", all.files = T, recursive = T)
p0215.06 <- data.frame()

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
    p0215.02 <- rbind(p0215.02, pTab)
  }
)

#     user  system elapsed 
#  837.176   9.966 857.955

pList[7107] # imputed - same vendor
pList[9904] # NLTEX7X3vg - xanax blotters 6bar

# safety
write.csv(p0215.02, file = "p-0215-06-raw.csv", row.names = F)

# clean extracted data
p0215.02 <- p0215.02[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(p0215.02) <- c("list", "date", "vendor", "product", 
                        "price", "cat", "feedback", "shipping")

p0215.02$vendor <- gsub("/vendor/", "", p0215.02$vendor)
p0215.02$vendor <- gsub("/user/", "", p0215.02$vendor)
p0215.02$vendor <- gsub("#", "", p0215.02$vendor)
p0215.02$vendor <- gsub("%7E", "", p0215.02$vendor)

p0215.02$shipping <- as.character(p0215.02$shipping)
p0215.02$shipping <- stripWhitespace(p0215.02$shipping)
p0215.02$shipping[p0215.02$shipping == " "] <- NA
is.na(p0215.02$shipping)

p0215.02 <- separate(p0215.02, shipping, c("from", "to"), sep = "To: ")
p0215.02$from <- gsub("From: ", "", p0215.02$from)

levels(as.factor(p0215.02$from)) # 53
levels(as.factor(p0215.02$to)) # 274

p0215.02$price <- gsub(" BTC", "", p0215.02$price)
p0215.02$price <- as.double(p0215.02$price)

write.csv(p0215.02, file = "p0215.06-c1.csv", row.names = F)

# extract subcategories -------------------------------------------------------

# subset out variables w/o a subcategory
levels(as.factor(p0215.02$cat))

p0215.02$cat <- as.character(p0215.02$cat)
p0215 <- subset(p0215.02,  p0215.02$cat != "Listings" & p0215.02$cat != "Jewelry"
                & p0215.02$cat != "Electronics" & p0215.02$cat != "Other")

# 16767 > 16189
pList2 <- as.character(p0215$list)
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
#  200.294   3.740 204.296 

# bind subcategories
bind0215_06 <- dplyr::left_join(p0215.02, subcat, by = "list")
is.na(bind0215_06$pTab2)

bind0215_06 <- bind0215_06[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
colnames(bind0215_06) <- c("list", "date", "vendor", "product", "price", 
                           "cat", "subcat", "feedback", "from", "to")

p0215.02 <- bind0215_06

# safety
write.csv(p0215.02, file = "p-2015-02-06.csv", row.names = F)

# extract subsubcategories ----------------------------------------------------

# subset subsubcategories
levels(p0215.02$subcat)
p0215.02$subcat <- as.character(p0215.02$subcat)

# 16767 > 16189 > 12706 > 8421
drugs0215.06 <- subset(p0215.02, p0215.02$cat == "Drugs")
drugs0215.06 <- subset(drugs0215.06, drugs0215.06$subcat != "Other" & 
                         drugs0215.06$subcat != "Weight loss" &
                         drugs0215.06$subcat != "Benzos" &
                         drugs0215.06$subcat != "Prescription" &
                         drugs0215.06$subcat != "RCs" &
                         drugs0215.06$subcat != "Steroids" &
                         drugs0215.06$subcat != "Methylone" &
                         drugs0215.06$subcat != "Ecstasy-MDMA" &
                         drugs0215.06$subcat != "Barbiturates")

# extract subsubcategories
levels(as.factor(drugs0215.06$subcat))

pList3 <- drugs0215.06$list
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
#   97.615   0.882  98.680  

# bind sub-subcategories
bind0215_06b <- dplyr::left_join(p0215.02, subcat2, by = "list")
is.na(bind0215_06b$pTab3)

bind0215_06b  <- bind0215_06b [c(1, 2, 3, 4, 5, 6, 7, 11, 8, 9, 10)]
colnames(bind0215_06b) <- c("list", "date", "vendor", "product", "price", 
                            "cat", "subcat", "subsubcat", "feedback", "from", "to")

p0215.06 <- bind0215_06b

# final extracted data pre-arules/contigency table transformations
write.csv(p0215.06, file = "products-2015-02-06.csv", row.names = F)
test <- read.csv("products-2015-02-06.csv")
