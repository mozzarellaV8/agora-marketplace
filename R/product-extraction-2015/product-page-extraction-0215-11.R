# Agora Marketplace Analysis
# product page data extraction
# 2015-02-11

# load ------------------------------------------------------------------------

library(rvest)
library(magrittr)
library(tm)
library(tidyr)
library(dplyr)

# Vendor and Date extraction --------------------------------------------------

rm(list = ls())
pDir <- "~/GitHub/ag-Product/2015-02-11"
setwd(pDir)

# 12696
pList <- list.files(path = pDir, pattern = ".html", all.files = T, recursive = T)
p0215.11 <- data.frame()

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
    p0215.11 <- rbind(p0215.11, pTab)
  }
)

#        user  system elapsed 
#     591.843   4.609 602.584 

pList[8043] # LRJTZ4iyPY - 28G mexican fireball ice.

# safety
write.csv(p0215.11, file = "p-0215-11-raw.csv", row.names = F)

# clean extracted data
p0215.11 <- p0215.11[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(p0215.11) <- c("list", "date", "vendor", "product", 
                        "price", "cat", "feedback", "shipping")

p0215.11$vendor <- gsub("/vendor/", "", p0215.11$vendor)
p0215.11$vendor <- gsub("/user/", "", p0215.11$vendor)
p0215.11$vendor <- gsub("#", "", p0215.11$vendor)
p0215.11$vendor <- gsub("%7E", "", p0215.11$vendor)

p0215.11$shipping <- as.character(p0215.11$shipping)
p0215.11$shipping <- stripWhitespace(p0215.11$shipping)
p0215.11$shipping[p0215.11$shipping == " "] <- NA
is.na(p0215.11$shipping)

p0215.11 <- separate(p0215.11, shipping, c("from", "to"), sep = "To: ")
p0215.11$from <- gsub("From: ", "", p0215.11$from)

levels(as.factor(p0215.11$from)) # 50 > 46
levels(as.factor(p0215.11$to)) # 300

p0215.11$from <- gsub("\\suk(.*)", "UK", p0215.11$from)
p0215.11$from <- gsub("\\sUK\\s", "UK", p0215.11$from)
p0215.11$from <- gsub("\\sUK, Asia\\s", "UK, Asia(.*)", p0215.11$from)
p0215.11$from <- gsub("\\sUntied Kingdom(.*)", "UK", p0215.11$from)
p0215.11$from <- gsub("\\sUnited Kingdom\\s", "UK", p0215.11$from)

p0215.11$from <- gsub("\\sWorldwide(*.)", "Worldwide", p0215.11$from)
p0215.11$from <- gsub("\\sworldwide(*.)", "Worldwide", p0215.11$from)
p0215.11$from <- gsub("\\sWORLDWIDE(*.)", "Worldwide", p0215.11$from)

p0215.11$from <- gsub("\\storland\\s", "Torland", p0215.11$from)
p0215.11$from <- gsub("\\sTorland\\s", "Torland", p0215.11$from)

p0215.11$price <- gsub(" BTC", "", p0215.11$price)
p0215.11$price <- as.double(p0215.11$price)

write.csv(p0215.11, file = "p0215.11-c1.csv", row.names = F)

# extract subcategories -------------------------------------------------------

# subset out variables w/o a subcategory
levels(as.factor(p0215.11$cat))

p0215.11$cat <- as.character(p0215.11$cat)
p0115 <- subset(p0215.11,  p0215.11$cat != "Listings" & p0215.11$cat != "Jewelry"
                & p0215.11$cat != "Electronics" & p0215.11$cat != "Other")

# 12695 > 12296
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
#  271.130  10.882 283.107 

# bind subcategories
bind0215_11 <- dplyr::left_join(p0215.11, subcat, by = "list")
is.na(bind0215_11$pTab2)

bind0215_11 <- bind0215_11[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
colnames(bind0215_11) <- c("list", "date", "vendor", "product", "price", 
                           "cat", "subcat", "feedback", "from", "to")

p0215.11 <- bind0215_11

# safety
write.csv(p0215.11, file = "p-2015-02-11.csv", row.names = F)

# extract subsubcategories ----------------------------------------------------

# subset subsubcategories
levels(p0215.11$subcat)
p0215.11$subcat <- as.character(p0215.11$subcat)

# 12695 > 5856
drugs0215.11 <- subset(p0215.11, p0215.11$cat == "Drugs")
drugs0215.11 <- subset(drugs0215.11, drugs0215.11$subcat != "Other" & 
                         drugs0215.11$subcat != "Weight loss" &
                         drugs0215.11$subcat != "Benzos" &
                         drugs0215.11$subcat != "Prescription" &
                         drugs0215.11$subcat != "RCs" &
                         drugs0215.11$subcat != "Steroids" &
                         drugs0215.11$subcat != "Methylone" &
                         drugs0215.11$subcat != "Ecstasy-MDMA" &
                         drugs0215.11$subcat != "Barbiturates")

# extract subsubcategories
levels(as.factor(drugs0215.11$subcat))

pList3 <- drugs0215.11$list
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
#   62.503   0.695  63.296 

# bind sub-subcategories
bind0215_11b <- dplyr::left_join(p0215.11, subcat2, by = "list")
is.na(bind0215_11b$pTab3)

bind0215_11b  <- bind0215_11b [c(1, 2, 3, 4, 5, 6, 7, 11, 8, 9, 10)]
colnames(bind0215_11b) <- c("list", "date", "vendor", "product", "price", 
                            "cat", "subcat", "subsubcat", "feedback", "from", "to")

p0215.11 <- bind0215_11b

# final extracted data pre-arules/contigency table transformations
write.csv(p0215.11, file = "products-2015-02-11.csv", row.names = F)
test <- read.csv("products-2015-02-11.csv")
