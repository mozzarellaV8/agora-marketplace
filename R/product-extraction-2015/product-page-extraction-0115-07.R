# Agora Marketplace Analysis
# Product info extraction
# 2015-01-07

# load ------------------------------------------------------------------------

library(rvest)
library(magrittr)
library(tm)
library(tidyr)
library(dplyr)

# Vendor and Date extraction --------------------------------------------------

rm(list = ls())
pDir <- "~/GitHub/ag-Product/2015-01-07"
setwd(pDir)

# 213187
pList <- list.files(path = pDir, pattern = ".html", all.files = T, recursive = T)
p0115.07 <- data.frame()

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
    p0115.07 <- rbind(p0115.07, pTab)
  }
)

#        user  system elapsed 
#  1090.078   15.594 1152.473

# safety
write.csv(p0115.07, file = "p-0115-07-raw.csv", row.names = F)
p0115.07 <- read.csv("p-0115-07-raw.csv")

# clean extracted data
p0115.07 <- p0115.07[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(p0115.07) <- c("list", "date", "vendor", "product", 
                        "price", "cat", "feedback", "shipping")

p0115.07$vendor <- gsub("/vendor/", "", p0115.07$vendor)
p0115.07$vendor <- gsub("/user/", "", p0115.07$vendor)
p0115.07$vendor <- gsub("#", "", p0115.07$vendor)

p0115.07$shipping <- as.character(p0115.07$shipping)
p0115.07$shipping <- stripWhitespace(p0115.07$shipping)
p0115.07$shipping[p0115.07$shipping == " "] <- NA
is.na(p0115.07$shipping)

p0115.07 <- separate(p0115.07, shipping, c("from", "to"), sep = "To: ")
p0115.07$from <- gsub("From: ", "", p0115.07$from)

levels(as.factor(p0115.07$from)) # 49
levels(as.factor(p0115.07$to)) # 300

p0115.07$price <- gsub(" BTC", "", p0115.07$price)
p0115.07$price <- as.double(p0115.07$price)

write.csv(p0115.07, file = "p0115.07-c1.csv", row.names = F)

# extract subcategories -------------------------------------------------------

# subset out variables w/o a subcategory
levels(as.factor(p0115.07$cat))

p0115.07$cat <- as.character(p0115.07$cat)
p0115 <- subset(p0115.07,  p0115.07$cat != "Listings" & p0115.07$cat != "Jewelry"
                & p0115.07$cat != "Electronics" & p0115.07$cat != "Other")

# 21318 > 20693
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
#  264.470   9.624 274.446

# bind subcategories
bind0115_07 <- dplyr::left_join(p0115.07, subcat, by = "list")
is.na(bind0115_07$pTab2)

bind0115_07 <- bind0115_07[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
colnames(bind0115_07) <- c("list", "date", "vendor", "product", "price", 
                           "cat", "subcat", "feedback", "from", "to")

p0115.07 <- bind0115_07

# safety
write.csv(p0115.07, file = "p-2015-01-07.csv", row.names = F)

# extract subsubcategories ----------------------------------------------------

# subset subsubcategories
levels(p0115.07$subcat)
p0115.07$subcat <- as.character(p0115.07$subcat)

# ADDED OPIODS BACK IN
# 21318 > 20693 > 15912 > 11634
drugs0115.07 <- subset(p0115.07, p0115.07$cat == "Drugs")
drugs0115.07 <- subset(drugs0115.07, drugs0115.07$subcat != "Other" & 
                         drugs0115.07$subcat != "Weight loss" &
                         drugs0115.07$subcat != "Benzos" &
                         drugs0115.07$subcat != "Prescription" &
                         drugs0115.07$subcat != "RCs" &
                         drugs0115.07$subcat != "Steroids" &
                         drugs0115.07$subcat != "Methylone" &
                         drugs0115.07$subcat != "Ecstasy-MDMA" &
                         drugs0115.07$subcat != "Barbiturates")

# extract subsubcategories
levels(as.factor(drugs0115.07$subcat))

pList3 <- drugs0115.07$list
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
#  143.200   2.165 146.261  

# bind sub-subcategories
bind0115_07b <- dplyr::left_join(p0115.07, subcat2, by = "list")
is.na(bind0115_07b$pTab3)

bind0115_07b  <- bind0115_07b [c(1, 2, 3, 4, 5, 6, 7, 11, 8, 9, 10)]
colnames(bind0115_07b) <- c("list", "date", "vendor", "product", "price", 
                            "cat", "subcat", "subsubcat", "feedback", "from", "to")

p0115.07 <- bind0115_07b

# final extracted data pre-arules/contigency table transformations
write.csv(p0115.07, file = "products-2015-01-07.csv", row.names = F)
test <- read.csv("products-2015-01-07.csv")
