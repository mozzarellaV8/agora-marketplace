# Agora Marketplace Analysis
# Product info extraction
# 2014-11-10

# load ------------------------------------------------------------------------

library(rvest)
library(magrittr)
library(tm)
library(tidyr)
library(dplyr)

rm(list = ls())

# set directory
pDir <- "~/GitHub/ag-Product/2014-11-10"
setwd(pDir)

# Vendor and Date extraction --------------------------------------------------

# 19661
pList <- list.files(path = pDir, pattern = ".html", all.files = T, recursive = T)
p1114.10 <- data.frame()

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
    p1114.10 <- rbind(p1114.10, pTab)
  }
)

#    user  system elapsed 
# 960.957  16.936 990.184

pList[6075] # AeXnfCWi0K - outdoor bluedream - (weed)
pList[8540] # Ena1NXsThC - 84% mdma

# safety
write.csv(p1114.10, file = "p-1114-10-raw.csv", row.names = F)
p1114.10 <- read.csv("p-1114-10-raw.csv")

p1114.10 <- p1114.07
rm(p1114.07)

# clean extracted data
p1114.10 <- p1114.10[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(p1114.10) <- c("list", "date", "vendor", "product", 
                        "price", "cat", "feedback", "shipping")

p1114.10$vendor <- gsub("/vendor/", "", p1114.10$vendor)
p1114.10$vendor <- gsub("/user/", "", p1114.10$vendor)
p1114.10$vendor <- gsub("#", "", p1114.10$vendor)

p1114.10$shipping <- as.character(p1114.10$shipping)
p1114.10$shipping <- stripWhitespace(p1114.10$shipping)
p1114.10$shipping[p1114.10$shipping == " "] <- NA
is.na(p1114.10$shipping)

p1114.10 <- separate(p1114.10, shipping, c("from", "to"), sep = "To: ")
p1114.10$from <- gsub("From: ", "", p1114.10$from)

levels(as.factor(p1114.10$from)) # 61
levels(as.factor(p1114.10$to)) # 335

p1114.10$price <- gsub(" BTC", "", p1114.10$price)
p1114.10$price <- as.double(p1114.10$price)

write.csv(p1114.10, file = "p1114.10-c1.csv", row.names = F)

# extract subcategories -------------------------------------------------------

# subset out variables w/o a subcategory
levels(as.factor(p1114.10$cat))

p1114.10$cat <- as.character(p1114.10$cat)
p1014 <- subset(p1114.10,  p1114.10$cat != "Listings" & p1114.10$cat != "Jewelry"
                & p1114.10$cat != "Electronics" & p1114.10$cat != "Other")

# 19659 > 18931
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
#   249.362   7.281 257.210 

# bind subcategories
bind1114_10 <- dplyr::left_join(p1114.10, subcat, by = "list")
is.na(bind1114_10$pTab2)

bind1114_10 <- bind1114_10[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
colnames(bind1114_10) <- c("list", "date", "vendor", "product", "price", 
                           "cat", "subcat", "feedback", "from", "to")

p1114.10 <- bind1114_10

# safety
write.csv(p1114.10, file = "p-2014-11-10.csv", row.names = F)

# extract subsubcategories ----------------------------------------------------

# subset subsubcategories
levels(p1114.10$subcat)
p1114.10$subcat <- as.character(p1114.10$subcat)

# 19659 > 18931 > 13973 > 9335
drugs1114.10 <- subset(p1114.10, p1114.10$cat == "Drugs")
drugs1114.10 <- subset(drugs1114.10, drugs1114.10$subcat != "Other" & 
                         drugs1114.10$subcat != "Weight loss" &
                         drugs1114.10$subcat != "Benzos" &
                         drugs1114.10$subcat != "Prescription" &
                         drugs1114.10$subcat != "RCs" &
                         drugs1114.10$subcat != "Steroids" &
                         drugs1114.10$subcat != "Methylone" &
                         drugs1114.10$subcat != "Opioids" &
                         drugs1114.10$subcat != "Ecstasy-MDMA" &
                         drugs1114.10$subcat != "Barbiturates")

# extract subsubcategories
levels(as.factor(drugs1114.10$subcat))

pList3 <- drugs1114.10$list
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
#  113.696   1.459 115.503

# bind sub-subcategories
bind1114_10b <- dplyr::left_join(p1114.10, subcat2, by = "list")
is.na(bind1114_10b$pTab3)

bind1114_10b  <- bind1114_10b [c(1, 2, 3, 4, 5, 6, 7, 11, 8, 9, 10)]
colnames(bind1114_10b) <- c("list", "date", "vendor", "product", "price", 
                            "cat", "subcat", "subsubcat", "feedback", "from", "to")

p1114.10 <- bind1114_10b

# final extracted data pre-arules/contigency table transformations
write.csv(p1114.10, file = "products-2014-11-10.csv", row.names = F)
test <- read.csv("products-2014-11-10.csv")

