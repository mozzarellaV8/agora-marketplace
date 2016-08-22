# Agora Marketplace Analysis
# vendor feedback extraction by month
# 2014-09-04

# load ------------------------------------------------------------------------

library(rvest)
library(magrittr)
library(tm)
library(tidyr)
library(dplyr)

rm(list = ls())

# set directory
pDir <- "~/GitHub/ag-Product/2014-09-04"
setwd(pDir)

# Vendor and Date extraction --------------------------------------------------

# 8598 > 8597
pList <- list.files(path = pDir, pattern = ".html", all.files = T, recursive = T)
p0914.04 <- data.frame()

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
    p0914.04 <- rbind(p0914.04, pTab)
  }
)

#    user  system elapsed 
# 387.085   2.171 391.760  

pList[7440] # tacTL5Apah - blank page

# safety
write.csv(p0914.04, file = "p-0914-04-raw.csv", row.names = F)
p0914.04 <- read.csv("p-0914-04-raw.csv")

# clean extracted data
p0914.04 <- p0914.04[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(p0914.04) <- c("list", "date", "vendor", "product", 
                        "price", "cat", "feedback", "shipping")

p0914.04$vendor <- gsub("/vendor/", "", p0914.04$vendor)
p0914.04$vendor <- gsub("#", "", p0914.04$vendor)

p0914.04$shipping <- as.character(p0914.04$shipping)
p0914.04$shipping <- stripWhitespace(p0914.04$shipping)
p0914.04$shipping[p0914.04$shipping == " "] <- NA
is.na(p0914.04$shipping)

p0914.04 <- separate(p0914.04, shipping, c("from", "to"), sep = "To: ")
p0914.04$from <- gsub("From: ", "", p0914.04$from)

levels(as.factor(p0914.04$from)) # 47
levels(as.factor(p0914.04$to)) # 171

p0914.04$price <- gsub(" BTC", "", p0914.04$price)
p0914.04$price <- as.double(p0914.04$price)

write.csv(p0914.04, file = "p0914.04-c1.csv", row.names = F)

# extract subcategories -------------------------------------------------------

# subset out variables w/o a subcategory
levels(as.factor(p0914.04$cat))

p0914.04$cat <- as.character(p0914.04$cat)
p0914 <- subset(p0914.04,  p0914.04$cat != "Listings" & p0914.04$cat != "Jewelry"
                & p0914.04$cat != "Electronics" & p0914.04$cat != "Other")

# 8597 > 8259
pList2 <- as.character(p0914$list)
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
#   91.472   1.088  92.544   

# bind subcategories
bind0914_04 <- dplyr::left_join(p0914.04, subcat, by = "list")
is.na(bind0914_04$pTab2)

bind0914_04 <- bind0914_04[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
colnames(bind0914_04) <- c("list", "date", "vendor", "product", "price", 
                           "cat", "subcat", "feedback", "from", "to")

p0914.04 <- bind0914_04

# safety
write.csv(p0914.04, file = "p-2014-09-04.csv", row.names = F)

# extract subsubcategories ----------------------------------------------------

# subset subsubcategories
levels(p0914.04$subcat)
p0914.04$subcat <- as.character(p0914.04$subcat)

# 8597 > 8259 > 6081 > 4970
drugs0914.04 <- subset(p0914.04, p0914.04$cat == "Drugs")
drugs0914.04 <- subset(drugs0914.04, drugs0914.04$subcat != "Other" & 
                         drugs0914.04$subcat != "Weight loss" &
                         drugs0914.04$subcat != "Benzos" &
                         drugs0914.04$subcat != "Prescription" &
                         drugs0914.04$subcat != "RCs" &
                         drugs0914.04$subcat != "Steroids" &
                         drugs0914.04$subcat != "Methylone" &
                         drugs0914.04$subcat != "Opioids" &
                         drugs0914.04$subcat != "Ecstasy-MDMA" &
                         drugs0914.04$subcat != "Barbiturates")

# extract subsubcategories
levels(as.factor(drugs0914.04$subcat))

pList3 <- drugs0914.04$list
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

#    user  system elapsed 
#  57.135   0.428  57.595   

# bind sub-subcategories
bind0914_04b <- dplyr::left_join(p0914.04, subcat2, by = "list")
is.na(bind0914_04b$pTab3)

bind0914_04b  <- bind0914_04b [c(1, 2, 3, 4, 5, 6, 7, 11, 8, 9, 10)]
colnames(bind0914_04b) <- c("list", "date", "vendor", "product", "price", 
                            "cat", "subcat", "subsubcat", "feedback", "from", "to")

p0914.04 <- bind0914_04b

# final extracted data pre-arules/contigency table transformations
write.csv(p0914.04, file = "products-2014-09-04.csv", row.names = F)
test <- read.csv("products-2014-09-04.csv")

