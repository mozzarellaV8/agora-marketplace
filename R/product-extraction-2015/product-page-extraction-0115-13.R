# Agora Marketplace Analysis
# Product info extraction
# 2015-01-13

# load ------------------------------------------------------------------------

library(rvest)
library(magrittr)
library(tm)
library(tidyr)
library(dplyr)

# Vendor and Date extraction --------------------------------------------------

rm(list = ls())
pDir <- "~/GitHub/ag-Product/2015-01-13"
setwd(pDir)

# 22221
pList <- list.files(path = pDir, pattern = ".html", all.files = T, recursive = T)
p0115.13 <- data.frame()

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
    p0115.13 <- rbind(p0115.13, pTab)
  }
)

#        user  system elapsed 
#  1088.836   21.489 1111.946

pList[6507] # AX07xohAo7 - blank
pList[9755] # GpABZWuehP - amesia ice-o-later
pList[13298] # od19g0lxvp - blank
pList[17382] # WdJNnzBshD - blank
pList[21813] # WJmzYp7Knv - 500 real facebook likes

# safety
write.csv(p0115.13, file = "p-0115-13-raw.csv", row.names = F)
p0115.13 <- read.csv("p-0115-13-raw.csv")

# clean extracted data
p0115.13 <- p0115.13[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(p0115.13) <- c("list", "date", "vendor", "product", 
                        "price", "cat", "feedback", "shipping")

p0115.13$vendor <- gsub("/vendor/", "", p0115.13$vendor)
p0115.13$vendor <- gsub("/user/", "", p0115.13$vendor)
p0115.13$vendor <- gsub("#", "", p0115.13$vendor)
p0115.13$vendor <- gsub("%7E", "", p0115.13$vendor)

p0115.13$shipping <- as.character(p0115.13$shipping)
p0115.13$shipping <- stripWhitespace(p0115.13$shipping)
p0115.13$shipping[p0115.13$shipping == " "] <- NA
is.na(p0115.13$shipping)

p0115.13 <- separate(p0115.13, shipping, c("from", "to"), sep = "To: ")
p0115.13$from <- gsub("From: ", "", p0115.13$from)

levels(as.factor(p0115.13$from)) # 56
levels(as.factor(p0115.13$to)) # 367

p0115.13$price <- gsub(" BTC", "", p0115.13$price)
p0115.13$price <- as.double(p0115.13$price)

write.csv(p0115.13, file = "p0115.13-c1.csv", row.names = F)

# extract subcategories -------------------------------------------------------

# subset out variables w/o a subcategory
levels(as.factor(p0115.13$cat))

p0115.13$cat <- as.character(p0115.13$cat)
p0115 <- subset(p0115.13,  p0115.13$cat != "Listings" & p0115.13$cat != "Jewelry"
                & p0115.13$cat != "Electronics" & p0115.13$cat != "Other")

# 22216 > 21574
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
#  270.818  11.573 282.791 

# bind subcategories
bind0115_13 <- dplyr::left_join(p0115.13, subcat, by = "list")
is.na(bind0115_13$pTab2)

bind0115_13 <- bind0115_13[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
colnames(bind0115_13) <- c("list", "date", "vendor", "product", "price", 
                           "cat", "subcat", "feedback", "from", "to")

p0115.13 <- bind0115_13

# safety
write.csv(p0115.13, file = "p-2015-01-13.csv", row.names = F)

# extract subsubcategories ----------------------------------------------------

library(data.table)
p0115.13 <- fread("~/GitHub/ag-product-safety-2015/p-2015-01-13.csv")

# subset subsubcategories
levels(as.factor(p0115.13$subcat))
p0115.13$subcat <- as.character(p0115.13$subcat)

# 22216 > 21574 > 16437 > 12015
drugs0115.13 <- subset(p0115.13, p0115.13$cat == "Drugs")
drugs0115.13 <- subset(drugs0115.13, drugs0115.13$subcat != "Other" & 
                         drugs0115.13$subcat != "Weight loss" &
                         drugs0115.13$subcat != "Benzos" &
                         drugs0115.13$subcat != "Prescription" &
                         drugs0115.13$subcat != "RCs" &
                         drugs0115.13$subcat != "Steroids" &
                         drugs0115.13$subcat != "Methylone" &
                         drugs0115.13$subcat != "Ecstasy-MDMA" &
                         drugs0115.13$subcat != "Barbiturates")

# extract subsubcategories
levels(as.factor(drugs0115.13$subcat))

pList3 <- drugs0115.13$list
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
#  152.714   3.805 163.582  

# bind sub-subcategories
p0115.13 <- as.data.frame(p0115.13)
bind0115_13b <- dplyr::left_join(p0115.13, subcat2, by = "list")

bind0115_13b  <- bind0115_13b [c(1, 2, 3, 4, 5, 6, 7, 11, 8, 9, 10)]
colnames(bind0115_13b) <- c("list", "date", "vendor", "product", "price", 
                            "cat", "subcat", "subsubcat", "feedback", "from", "to")

p0115.13 <- bind0115_13b

# final extracted data pre-arules/contigency table transformations
write.csv(p0115.13, file = "products-2015-01-13.csv", row.names = F)
test <- fread("products-2015-01-13.csv")
