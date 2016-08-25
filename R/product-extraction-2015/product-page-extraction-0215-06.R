# Agora Marketplace Analysis
# Product info extraction
# 2015-02-06

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

# 16767
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
    p01215.06 <- rbind(p0215.06, pTab)
  }
)

#     user  system elapsed 
#  837.176   9.966 857.955

pList[7107] # imputed - same vendor
pList[9904] # NLTEX7X3vg - xanax blotters 6bar

# safety
write.csv(p01215.06, file = "p-0215-06-raw.csv", row.names = F)

# clean extracted data
p01215.06 <- p01215.06[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(p01215.06) <- c("list", "date", "vendor", "product", 
                        "price", "cat", "feedback", "shipping")

p01215.06$vendor <- gsub("/vendor/", "", p01215.06$vendor)
p01215.06$vendor <- gsub("/user/", "", p01215.06$vendor)
p01215.06$vendor <- gsub("#", "", p01215.06$vendor)
p01215.06$vendor <- gsub("%7E", "", p01215.06$vendor)

p01215.06$shipping <- as.character(p01215.06$shipping)
p01215.06$shipping <- stripWhitespace(p01215.06$shipping)
p01215.06$shipping[p01215.06$shipping == " "] <- NA
is.na(p01215.06$shipping)

p01215.06 <- separate(p01215.06, shipping, c("from", "to"), sep = "To: ")
p01215.06$from <- gsub("From: ", "", p01215.06$from)

levels(as.factor(p01215.06$from)) # 53
levels(as.factor(p01215.06$to)) # 274

p01215.06$price <- gsub(" BTC", "", p01215.06$price)
p01215.06$price <- as.double(p01215.06$price)

write.csv(p01215.06, file = "p0215.06-c1.csv", row.names = F)

# extract subcategories -------------------------------------------------------

# subset out variables w/o a subcategory
levels(as.factor(p01215.06$cat))

p01215.06$cat <- as.character(p01215.06$cat)
p0215 <- subset(p01215.06,  p01215.06$cat != "Listings" & p01215.06$cat != "Jewelry"
                & p01215.06$cat != "Electronics" & p01215.06$cat != "Other")

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
bind0215_06 <- dplyr::left_join(p01215.06, subcat, by = "list")
is.na(bind0215_06$pTab2)

bind0215_06 <- bind0215_06[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
colnames(bind0215_06) <- c("list", "date", "vendor", "product", "price", 
                           "cat", "subcat", "feedback", "from", "to")

p01215.06 <- bind0215_06

# safety
write.csv(p01215.06, file = "p-2015-02-06.csv", row.names = F)

# extract subsubcategories ----------------------------------------------------

# subset subsubcategories
levels(p01215.06$subcat)
p01215.06$subcat <- as.character(p01215.06$subcat)

# 16767 > 16189 > 12706 > 8421
drugs0215.06 <- subset(p01215.06, p01215.06$cat == "Drugs")
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
bind0215_06b <- dplyr::left_join(p01215.06, subcat2, by = "list")
is.na(bind0215_06b$pTab3)

bind0215_06b  <- bind0215_06b [c(1, 2, 3, 4, 5, 6, 7, 11, 8, 9, 10)]
colnames(bind0215_06b) <- c("list", "date", "vendor", "product", "price", 
                            "cat", "subcat", "subsubcat", "feedback", "from", "to")

p0215.06 <- bind0215_06b

# final extracted data pre-arules/contigency table transformations
write.csv(p0215.06, file = "products-2015-02-06.csv", row.names = F)
test <- read.csv("products-2015-02-06.csv")
