# Agora Marketplace Analysis
# vendor feedback extraction by month
# 2014-09-01

# load ------------------------------------------------------------------------

library(rvest)
library(magrittr)
library(tm)
library(tidyr)
library(dplyr)

rm(list = ls())

# set directory
pDir <- "~/GitHub/ag-Product/2014-09-01"
setwd(pDir)

# Vendor and Date extraction --------------------------------------------------

# 19767 > 19436
pList <- list.files(path = pDir, pattern = ".html", all.files = T, recursive = T)
p0914.01 <- data.frame()

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
    p0914.01 <- rbind(p0914.01, pTab)
  }
)

#    user  system elapsed 
# 928.232   9.751 950.040 

pList[156] # AufG8AjJmR
pList[10627] # Kc3fPNxFmo - incomplete crawl

# safety
write.csv(p0914.01, file = "p-0914-01-raw.csv", row.names = F)
p0914.01 <- read.csv("p-0914-01-raw.csv")

# clean extracted data
p0914.01 <- p0914.01[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(p0914.01) <- c("list", "date", "vendor", "product", 
                        "price", "cat", "feedback", "shipping")

p0914.01$vendor <- gsub("/vendor/", "", p0914.01$vendor)
p0914.01$vendor <- gsub("#", "", p0914.01$vendor)

p0914.01$shipping <- as.character(p0914.01$shipping)
p0914.01$shipping <- stripWhitespace(p0914.01$shipping)
p0914.01$shipping[p0914.01$shipping == " "] <- NA
is.na(p0914.01$shipping)

p0914.01 <- separate(p0914.01, shipping, c("from", "to"), sep = "To: ")
p0914.01$from <- gsub("From: ", "", p0914.01$from)

levels(as.factor(p0914.01$from)) # 60
levels(as.factor(p0914.01$to)) # 308

p0914.01$price <- gsub(" BTC", "", p0914.01$price)
p0914.01$price <- as.double(p0914.01$price)

write.csv(p0914.01, file = "p0914.01-c1.csv", row.names = F)

# extract subcategories -------------------------------------------------------

# subset out variables w/o a subcategory
levels(as.factor(p0914.01$cat))

p0914.01$cat <- as.character(p0914.01$cat)
p0914 <- subset(p0914.01,  p0914.01$cat != "Listings" & p0914.01$cat != "Jewelry"
                & p0914.01$cat != "Electronics" & p0914.01$cat != "Other")

# 19435 > 18707
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
#  224.029   6.294 230.359   

# bind subcategories
bind0914_01 <- dplyr::left_join(p0914.01, subcat, by = "list")
is.na(bind0914_01$pTab2)

bind0914_01 <- bind0914_01[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
colnames(bind0914_01) <- c("list", "date", "vendor", "product", "price", 
                           "cat", "subcat", "feedback", "from", "to")

p0914.01 <- bind0914_01

# safety
write.csv(p0914.01, file = "p-2014-09-01.csv", row.names = F)

# extract subsubcategories ----------------------------------------------------

# subset subsubcategories
levels(p0914.01$subcat)
p0914.01$subcat <- as.character(p0914.01$subcat)

# 19435 > 18707 > 13098 > 8214
drugs0914.01 <- subset(p0914.01, p0914.01$cat == "Drugs")
drugs0914.01 <- subset(drugs0914.01, drugs0914.01$subcat != "Other" & 
                         drugs0914.01$subcat != "Weight loss" &
                         drugs0914.01$subcat != "Benzos" &
                         drugs0914.01$subcat != "Prescription" &
                         drugs0914.01$subcat != "RCs" &
                         drugs0914.01$subcat != "Steroids" &
                         drugs0914.01$subcat != "Methylone" &
                         drugs0914.01$subcat != "Opioids" &
                         drugs0914.01$subcat != "Ecstasy-MDMA" &
                         drugs0914.01$subcat != "Barbiturates")

# extract subsubcategories
levels(as.factor(drugs0914.01$subcat))

pList3 <- drugs0914.01$list
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
#  89.768   0.996  90.749   

pList3[5539] # pXkzBvwXfC - impute - Ecstasy
pList3[6548] # tvVJErlrzG - impute - Ecstasy

# bind sub-subcategories
bind0914_01b <- dplyr::left_join(p0914.01, subcat2, by = "list")
is.na(bind0914_01b$pTab3)

bind0914_01b  <- bind0914_01b [c(1, 2, 3, 4, 5, 6, 7, 11, 8, 9, 10)]
colnames(bind0914_01b) <- c("list", "date", "vendor", "product", "price", 
                            "cat", "subcat", "subsubcat", "feedback", "from", "to")

p0914.01 <- bind0914_01b

# final extracted data pre-arules/contigency table transformations
write.csv(p0914.01, file = "products-2014-09-01.csv", row.names = F)
test <- read.csv("products-2014-09-01.csv")

