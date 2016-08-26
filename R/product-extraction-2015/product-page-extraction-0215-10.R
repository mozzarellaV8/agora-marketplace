# Agora Marketplace Analysis
# product page data extraction
# 2015-02-10

# load ------------------------------------------------------------------------

library(rvest)
library(magrittr)
library(tm)
library(tidyr)
library(dplyr)

# Vendor and Date extraction --------------------------------------------------

rm(list = ls())
pDir <- "~/GitHub/ag-Product/2015-02-10"
setwd(pDir)

# 17672
pList <- list.files(path = pDir, pattern = ".html", all.files = T, recursive = T)
p0215.10 <- data.frame()

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
    p0215.10 <- rbind(p0215.10, pTab)
  }
)

#        user  system elapsed 
#  909.122  11.819 940.899

pList[7039] # irSbtMyEts - 4-HO-MET imputed 1/4g

# safety
write.csv(p0215.10, file = "p-0215-10-raw.csv", row.names = F)

# clean extracted data
p0215.10 <- p0215.10[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(p0215.10) <- c("list", "date", "vendor", "product", 
                        "price", "cat", "feedback", "shipping")

p0215.10$vendor <- gsub("/vendor/", "", p0215.10$vendor)
p0215.10$vendor <- gsub("/user/", "", p0215.10$vendor)
p0215.10$vendor <- gsub("#", "", p0215.10$vendor)
p0215.10$vendor <- gsub("%7E", "", p0215.10$vendor)

p0215.10$shipping <- as.character(p0215.10$shipping)
p0215.10$shipping <- stripWhitespace(p0215.10$shipping)
p0215.10$shipping[p0215.10$shipping == " "] <- NA
is.na(p0215.10$shipping)

p0215.10 <- separate(p0215.10, shipping, c("from", "to"), sep = "To: ")
p0215.10$from <- gsub("From: ", "", p0215.10$from)

levels(as.factor(p0215.10$from)) # 56
levels(as.factor(p0215.10$to)) # 300

# get a few repeat levels out
p0215.10$from <- gsub("\\suk(.*)", "UK", p0215.10$from)
p0215.10$from <- gsub("\\sUK\\s", "UK", p0215.10$from)
p0215.10$from <- gsub("\\sWorldwide(*.)", "Worldwide", p0215.10$from)
p0215.10$from <- gsub("\\sworldwide(*.)", "Worldwide", p0215.10$from)

p0215.10$price <- gsub(" BTC", "", p0215.10$price)
p0215.10$price <- as.double(p0215.10$price)

write.csv(p0215.10, file = "p0215.10-c1.csv", row.names = F)

# extract subcategories -------------------------------------------------------

# subset out variables w/o a subcategory
levels(as.factor(p0215.10$cat))

p0215.10$cat <- as.character(p0215.10$cat)
p0115 <- subset(p0215.10,  p0215.10$cat != "Listings" & p0215.10$cat != "Jewelry"
                & p0215.10$cat != "Electronics" & p0215.10$cat != "Other")

# 17672 > 17071
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
#  221.057   6.583 228.066 

# bind subcategories
bind0215_10 <- dplyr::left_join(p0215.10, subcat, by = "list")
is.na(bind0215_10$pTab2)

bind0215_10 <- bind0215_10[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
colnames(bind0215_10) <- c("list", "date", "vendor", "product", "price", 
                           "cat", "subcat", "feedback", "from", "to")

p0215.10 <- bind0215_10

# safety
write.csv(p0215.10, file = "p-2015-02-10.csv", row.names = F)

# extract subsubcategories ----------------------------------------------------

# subset subsubcategories
levels(p0215.10$subcat)
p0215.10$subcat <- as.character(p0215.10$subcat)

# 17672 > 17071 > 13172 > 9061
drugs0215.10 <- subset(p0215.10, p0215.10$cat == "Drugs")
drugs0215.10 <- subset(drugs0215.10, drugs0215.10$subcat != "Other" & 
                         drugs0215.10$subcat != "Weight loss" &
                         drugs0215.10$subcat != "Benzos" &
                         drugs0215.10$subcat != "Prescription" &
                         drugs0215.10$subcat != "RCs" &
                         drugs0215.10$subcat != "Steroids" &
                         drugs0215.10$subcat != "Methylone" &
                         drugs0215.10$subcat != "Ecstasy-MDMA" &
                         drugs0215.10$subcat != "Barbiturates")

# extract subsubcategories
levels(as.factor(drugs0215.10$subcat))

pList3 <- drugs0215.10$list
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
#  113.670   1.606 115.521  

# bind sub-subcategories
bind0215_10b <- dplyr::left_join(p0215.10, subcat2, by = "list")
is.na(bind0215_10b$pTab3)

bind0215_10b  <- bind0215_10b [c(1, 2, 3, 4, 5, 6, 7, 11, 8, 9, 10)]
colnames(bind0215_10b) <- c("list", "date", "vendor", "product", "price", 
                            "cat", "subcat", "subsubcat", "feedback", "from", "to")

p0215.10 <- bind0215_10b

# final extracted data pre-arules/contigency table transformations
write.csv(p0215.10, file = "products-2015-02-10.csv", row.names = F)
test <- read.csv("products-2015-02-10.csv")
