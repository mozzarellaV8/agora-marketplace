# Agora Marketplace Analysis
# vendor feedback extraction by month
# 2014-10-05

# load ------------------------------------------------------------------------

library(rvest)
library(magrittr)
library(tm)
library(tidyr)
library(dplyr)

rm(list = ls())

# set directory
pDir <- "~/GitHub/ag-Product/2014-10-05"
setwd(pDir)

# Vendor and Date extraction --------------------------------------------------

# 9759
pList <- list.files(path = pDir, pattern = ".html", all.files = T, recursive = T)
p1014.05 <- data.frame()

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
    p1014.05 <- rbind(p1014.05, pTab)
  }
)

#    user  system elapsed 
# 443.859   3.061 453.290 

pList[6571] # s2vU5doqfP - nBomes - incomplete
pList[6593] # s63sZ7eBlS - Weed - incomplete

# safety
write.csv(p1014.05, file = "p-1014-05-raw.csv", row.names = F)
p1014.05 <- read.csv("p-1014-05-raw.csv")

# clean extracted data
p1014.05 <- p1014.05[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(p1014.05) <- c("list", "date", "vendor", "product", 
                        "price", "cat", "feedback", "shipping")

p1014.05$vendor <- gsub("/vendor/", "", p1014.05$vendor)
p1014.05$vendor <- gsub("#", "", p1014.05$vendor)

p1014.05$shipping <- as.character(p1014.05$shipping)
p1014.05$shipping <- stripWhitespace(p1014.05$shipping)
p1014.05$shipping[p1014.05$shipping == " "] <- NA
is.na(p1014.05$shipping)

p1014.05 <- separate(p1014.05, shipping, c("from", "to"), sep = "To: ")
p1014.05$from <- gsub("From: ", "", p1014.05$from)

levels(as.factor(p1014.05$from)) # 50
levels(as.factor(p1014.05$to)) # 185

p1014.05$price <- gsub(" BTC", "", p1014.05$price)
p1014.05$price <- as.double(p1014.05$price)

write.csv(p1014.05, file = "p1014.05-c1.csv", row.names = F)

# extract subcategories -------------------------------------------------------

# subset out variables w/o a subcategory
levels(as.factor(p1014.05$cat))

p1014.05$cat <- as.character(p1014.05$cat)
p1014 <- subset(p1014.05,  p1014.05$cat != "Listings" & p1014.05$cat != "Jewelry"
                & p1014.05$cat != "Electronics" & p1014.05$cat != "Other")

# 9757 > 9728 
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

#     user  system elapsed 
#  111.710   1.330 113.388 

# bind subcategories
bind1014_05 <- dplyr::left_join(p1014.05, subcat, by = "list")
is.na(bind1014_05$pTab2)

bind1014_05 <- bind1014_05[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
colnames(bind1014_05) <- c("list", "date", "vendor", "product", "price", 
                           "cat", "subcat", "feedback", "from", "to")

p1014.05 <- bind1014_05

# safety
write.csv(p1014.05, file = "p-2014-10-05.csv", row.names = F)

# extract subsubcategories ----------------------------------------------------

# subset subsubcategories
levels(p1014.05$subcat)
p1014.05$subcat <- as.character(p1014.05$subcat)

# 9757 > 9728 > 9625 > 6468
drugs1014.05 <- subset(p1014.05, p1014.05$cat == "Drugs")
drugs1014.05 <- subset(drugs1014.05, drugs1014.05$subcat != "Other" & 
                         drugs1014.05$subcat != "Weight loss" &
                         drugs1014.05$subcat != "Benzos" &
                         drugs1014.05$subcat != "Prescription" &
                         drugs1014.05$subcat != "RCs" &
                         drugs1014.05$subcat != "Steroids" &
                         drugs1014.05$subcat != "Methylone" &
                         drugs1014.05$subcat != "Opioids" &
                         drugs1014.05$subcat != "Ecstasy-MDMA" &
                         drugs1014.05$subcat != "Barbiturates")

# extract subsubcategories
levels(as.factor(drugs1014.05$subcat))

pList3 <- drugs1014.05$list
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
#  40.939   0.341  41.282

pList3[4874] # pXkzBvwXfC - impute - ecstacy


# bind sub-subcategories
bind1014_05b <- dplyr::left_join(p1014.05, subcat2, by = "list")
is.na(bind1014_05b$pTab3)

bind1014_05b  <- bind1014_05b [c(1, 2, 3, 4, 5, 6, 7, 11, 8, 9, 10)]
colnames(bind1014_05b) <- c("list", "date", "vendor", "product", "price", 
                            "cat", "subcat", "subsubcat", "feedback", "from", "to")

p1014.05 <- bind1014_05b

# final extracted data pre-arules/contigency table transformations
write.csv(p1014.05, file = "products-2014-10-05.csv", row.names = F)
test <- read.csv("products-2014-10-05.csv")

