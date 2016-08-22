# Agora Marketplace Analysis
# vendor feedback extraction by month
# 2014-11-01

# load ------------------------------------------------------------------------

library(rvest)
library(magrittr)
library(tm)
library(tidyr)
library(dplyr)

rm(list = ls())

# set directory
pDir <- "~/GitHub/ag-Product/2014-11-01"
setwd(pDir)

# Vendor and Date extraction --------------------------------------------------

# 16945
pList <- list.files(path = pDir, pattern = ".html", all.files = T, recursive = T)
p1114.01 <- data.frame()

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
    p1114.01 <- rbind(p1114.01, pTab)
  }
)

#    user  system elapsed 
# 833.297  10.014 845.235

pList[988] # urine
pList[4464] # aRJicwzz11 - 1g super Potent DMT
pList[5690] # E27w4FzAvB - 10g Eitzolam
pList[10167] # sFDTXFNGV9 - 'pussy riot' nBome
pList[14995] # pqqjlXBSzn - captain kirk book.

# safety
write.csv(p1114.01, file = "p-1114-01-raw.csv", row.names = F)
p1114.01 <- read.csv("p-1114-01-raw.csv")

# clean extracted data
p1114.01 <- p1114.01[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(p1114.01) <- c("list", "date", "vendor", "product", 
                        "price", "cat", "feedback", "shipping")

p1114.01$vendor <- gsub("/vendor/", "", p1114.01$vendor)
p1114.01$vendor <- gsub("#", "", p1114.01$vendor)

p1114.01$shipping <- as.character(p1114.01$shipping)
p1114.01$shipping <- stripWhitespace(p1114.01$shipping)
p1114.01$shipping[p1114.01$shipping == " "] <- NA
is.na(p1114.01$shipping)

p1114.01 <- separate(p1114.01, shipping, c("from", "to"), sep = "To: ")
p1114.01$from <- gsub("From: ", "", p1114.01$from)

levels(as.factor(p1114.01$from)) # 56
levels(as.factor(p1114.01$to)) # 334

p1114.01$price <- gsub(" BTC", "", p1114.01$price)
p1114.01$price <- as.double(p1114.01$price)

write.csv(p1114.01, file = "p1114.01-c1.csv", row.names = F)

# extract subcategories -------------------------------------------------------

# subset out variables w/o a subcategory
levels(as.factor(p1114.01$cat))

p1114.01$cat <- as.character(p1114.01$cat)
p1014 <- subset(p1114.01,  p1114.01$cat != "Listings" & p1114.01$cat != "Jewelry"
                & p1114.01$cat != "Electronics" & p1114.01$cat != "Other")

# 16490 > 16027
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
#   181.459   4.486 186.009 

# bind subcategories
bind1114_01 <- dplyr::left_join(p1114.01, subcat, by = "list")
is.na(bind1114_01$pTab2)

bind1114_01 <- bind1114_01[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
colnames(bind1114_01) <- c("list", "date", "vendor", "product", "price", 
                           "cat", "subcat", "feedback", "from", "to")

p1114.01 <- bind1114_01

# safety
write.csv(p1114.01, file = "p-2014-11-01.csv", row.names = F)

# extract subsubcategories ----------------------------------------------------

# subset subsubcategories
levels(p1114.01$subcat)
p1114.01$subcat <- as.character(p1114.01$subcat)

# 16490 > 16027 > 12053 > 8051
drugs1114.01 <- subset(p1114.01, p1114.01$cat == "Drugs")
drugs1114.01 <- subset(drugs1114.01, drugs1114.01$subcat != "Other" & 
                         drugs1114.01$subcat != "Weight loss" &
                         drugs1114.01$subcat != "Benzos" &
                         drugs1114.01$subcat != "Prescription" &
                         drugs1114.01$subcat != "RCs" &
                         drugs1114.01$subcat != "Steroids" &
                         drugs1114.01$subcat != "Methylone" &
                         drugs1114.01$subcat != "Opioids" &
                         drugs1114.01$subcat != "Ecstasy-MDMA" &
                         drugs1114.01$subcat != "Barbiturates")

# extract subsubcategories
levels(as.factor(drugs1114.01$subcat))

pList3 <- drugs1114.01$list
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
#  92.372   1.300  93.883

# bind sub-subcategories
bind1114_01b <- dplyr::left_join(p1114.01, subcat2, by = "list")
is.na(bind1114_01b$pTab3)

bind1114_01b  <- bind1114_01b [c(1, 2, 3, 4, 5, 6, 7, 11, 8, 9, 10)]
colnames(bind1114_01b) <- c("list", "date", "vendor", "product", "price", 
                            "cat", "subcat", "subsubcat", "feedback", "from", "to")

p1114.01 <- bind1114_01b

# final extracted data pre-arules/contigency table transformations
write.csv(p1114.01, file = "products-2014-11-01.csv", row.names = F)
test <- read.csv("products-2014-11-01.csv")

