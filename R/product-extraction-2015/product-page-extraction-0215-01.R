# Agora Marketplace Analysis
# Product info extraction
# 2015-02-01

# load ------------------------------------------------------------------------

library(rvest)
library(magrittr)
library(tm)
library(tidyr)
library(dplyr)

# Vendor and Date extraction --------------------------------------------------

rm(list = ls())
pDir <- "~/GitHub/ag-Product/2015-02-01"
setwd(pDir)

# 21767
pList <- list.files(path = pDir, pattern = ".html", all.files = T, recursive = T)
p0215.01 <- data.frame()

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
    p0215.01 <- rbind(p0215.01, pTab)
  }
)

#        user  system elapsed 
#  1074.087   19.163 1095.849

pList[21384] # Y1TeVmnF69 - white domino mdma - free half G weed

# safety
write.csv(p0215.01, file = "p-0215-01-raw.csv", row.names = F)
p0215.01 <- read.csv("p-0215-01-raw.csv")

# clean extracted data
p0215.01 <- p0215.01[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(p0215.01) <- c("list", "date", "vendor", "product", 
                        "price", "cat", "feedback", "shipping")

p0215.01$vendor <- gsub("/vendor/", "", p0215.01$vendor)
p0215.01$vendor <- gsub("/user/", "", p0215.01$vendor)
p0215.01$vendor <- gsub("#", "", p0215.01$vendor)
p0215.01$vendor <- gsub("%7E", "", p0215.01$vendor)

p0215.01$shipping <- as.character(p0215.01$shipping)
p0215.01$shipping <- stripWhitespace(p0215.01$shipping)
p0215.01$shipping[p0215.01$shipping == " "] <- NA
is.na(p0215.01$shipping)

p0215.01 <- separate(p0215.01, shipping, c("from", "to"), sep = "To: ")
p0215.01$from <- gsub("From: ", "", p0215.01$from)

levels(as.factor(p0215.01$from)) # 60
levels(as.factor(p0215.01$to)) # 360

p0215.01$price <- gsub(" BTC", "", p0215.01$price)
p0215.01$price <- as.double(p0215.01$price)

write.csv(p0215.01, file = "p0215.01-c1.csv", row.names = F)

# extract subcategories -------------------------------------------------------

# subset out variables w/o a subcategory
levels(as.factor(p0215.01$cat))

p0215.01$cat <- as.character(p0215.01$cat)
p0115 <- subset(p0215.01,  p0215.01$cat != "Listings" & p0215.01$cat != "Jewelry"
                & p0215.01$cat != "Electronics" & p0215.01$cat != "Other")

# 21766 > 21160
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
#  272.213  12.256 285.657 

# bind subcategories
bind0215_01 <- dplyr::left_join(p0215.01, subcat, by = "list")
is.na(bind0215_01$pTab2)

bind0215_01 <- bind0215_01[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
colnames(bind0215_01) <- c("list", "date", "vendor", "product", "price", 
                           "cat", "subcat", "feedback", "from", "to")

p0215.01 <- bind0215_01

# safety
write.csv(p0215.01, file = "p-2015-02-01.csv", row.names = F)

# extract subsubcategories ----------------------------------------------------

# subset subsubcategories
levels(p0215.01$subcat)
p0215.01$subcat <- as.character(p0215.01$subcat)

# 21766 > 21160 > 16307 > 11772
drugs0215.01 <- subset(p0215.01, p0215.01$cat == "Drugs")
drugs0215.01 <- subset(drugs0215.01, drugs0215.01$subcat != "Other" & 
                         drugs0215.01$subcat != "Weight loss" &
                         drugs0215.01$subcat != "Benzos" &
                         drugs0215.01$subcat != "Prescription" &
                         drugs0215.01$subcat != "RCs" &
                         drugs0215.01$subcat != "Steroids" &
                         drugs0215.01$subcat != "Methylone" &
                         drugs0215.01$subcat != "Ecstasy-MDMA" &
                         drugs0215.01$subcat != "Barbiturates")

# extract subsubcategories
levels(as.factor(drugs0215.01$subcat))

pList3 <- drugs0215.01$list
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
#  145.397   3.219 148.738  

# bind sub-subcategories
p0215.01 <- read.csv("p-2015-02-01.csv", stringsAsFactors = F)

bind0215_01b <- dplyr::left_join(p0215.01, subcat2, by = "list")
is.na(bind0215_01b$pTab3)

bind0215_01b  <- bind0215_01b [c(1, 2, 3, 4, 5, 6, 7, 11, 8, 9, 10)]
colnames(bind0215_01b) <- c("list", "date", "vendor", "product", "price", 
                            "cat", "subcat", "subsubcat", "feedback", "from", "to")

p0215.01 <- bind0215_01b

# final extracted data pre-arules/contigency table transformations
write.csv(p0215.01, file = "products-2015-02-01.csv", row.names = F)
test <- read.csv("products-2015-02-01.csv")
