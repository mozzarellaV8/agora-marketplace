# Agora Marketplace Analysis
# vendor feedback extraction by month
# 2014-09-05

# load ------------------------------------------------------------------------

library(rvest)
library(magrittr)
library(tm)
library(tidyr)
library(dplyr)

rm(list = ls())

# set directory
pDir <- "~/GitHub/ag-Product/2014-09-05"
setwd(pDir)

# Vendor and Date extraction --------------------------------------------------

# 12113 > 12112 > 12111
pList <- list.files(path = pDir, pattern = ".html", all.files = T, recursive = T)
p0914.05 <- data.frame()

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
    p0914.05 <- rbind(p0914.05, pTab)
  }
)

#    user  system elapsed 
# 591.043   4.550 606.858 

pList[754] # BMi4W8yNxW - incomplete crawl - not enough info
pList[6086] # NcffZy6uNr - incomplete crawl - no feedback

# safety
write.csv(p0914.05, file = "p-0914-05-raw.csv", row.names = F)
p0914.05 <- read.csv("p-0914-05-raw.csv")

# clean extracted data
p0914.05 <- p0914.05[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(p0914.05) <- c("list", "date", "vendor", "product", 
                        "price", "cat", "feedback", "shipping")

p0914.05$vendor <- gsub("/vendor/", "", p0914.05$vendor)
p0914.05$vendor <- gsub("#", "", p0914.05$vendor)

p0914.05$shipping <- as.character(p0914.05$shipping)
p0914.05$shipping <- stripWhitespace(p0914.05$shipping)
p0914.05$shipping[p0914.05$shipping == " "] <- NA
is.na(p0914.05$shipping)

p0914.05 <- separate(p0914.05, shipping, c("from", "to"), sep = "To: ")
p0914.05$from <- gsub("From: ", "", p0914.05$from)

levels(as.factor(p0914.05$from)) # 49
levels(as.factor(p0914.05$to)) # 290

p0914.05$price <- gsub(" BTC", "", p0914.05$price)
p0914.05$price <- as.double(p0914.05$price)

write.csv(p0914.05, file = "p0914.05-c1.csv", row.names = F)

# extract subcategories -------------------------------------------------------

# subset out variables w/o a subcategory
levels(as.factor(p0914.05$cat))

p0914.05$cat <- as.character(p0914.05$cat)
p0914 <- subset(p0914.05,  p0914.05$cat != "Listings" & p0914.05$cat != "Jewelry"
                & p0914.05$cat != "Electronics" & p0914.05$cat != "Other")

# 12111 > 11976
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
#  142.577   2.390 145.278   

# bind subcategories
bind0914_05 <- dplyr::left_join(p0914.05, subcat, by = "list")
is.na(bind0914_05$pTab2)

bind0914_05 <- bind0914_05[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
colnames(bind0914_05) <- c("list", "date", "vendor", "product", "price", 
                           "cat", "subcat", "feedback", "from", "to")

p0914.05 <- bind0914_05

# safety
write.csv(p0914.05, file = "p-2014-09-05.csv", row.names = F)

# extract subsubcategories ----------------------------------------------------

# subset subsubcategories
levels(p0914.05$subcat)
p0914.05$subcat <- as.character(p0914.05$subcat)

# 12111 > 11976 > 10862 > 9016
drugs0914.05 <- subset(p0914.05, p0914.05$cat == "Drugs")
drugs0914.05 <- subset(drugs0914.05, drugs0914.05$subcat != "Other" & 
                         drugs0914.05$subcat != "Weight loss" &
                         drugs0914.05$subcat != "Benzos" &
                         drugs0914.05$subcat != "Prescription" &
                         drugs0914.05$subcat != "RCs" &
                         drugs0914.05$subcat != "Steroids" &
                         drugs0914.05$subcat != "Methylone" &
                         drugs0914.05$subcat != "Opioids" &
                         drugs0914.05$subcat != "Ecstasy-MDMA" &
                         drugs0914.05$subcat != "Barbiturates")

# extract subsubcategories
levels(as.factor(drugs0914.05$subcat))

pList3 <- drugs0914.05$list
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
# 112.124   1.629 114.435   

# bind sub-subcategories
bind0914_05b <- dplyr::left_join(p0914.05, subcat2, by = "list")
is.na(bind0914_05b$pTab3)

bind0914_05b  <- bind0914_05b [c(1, 2, 3, 4, 5, 6, 7, 11, 8, 9, 10)]
colnames(bind0914_05b) <- c("list", "date", "vendor", "product", "price", 
                            "cat", "subcat", "subsubcat", "feedback", "from", "to")

p0914.05 <- bind0914_05b

# final extracted data pre-arules/contigency table transformations
write.csv(p0914.05, file = "products-2014-09-05.csv", row.names = F)
test <- read.csv("products-2014-09-05.csv")

