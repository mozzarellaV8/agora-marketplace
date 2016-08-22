# Agora Marketplace Analysis
# Product info extraction
# 2015-01-03

# load ------------------------------------------------------------------------

library(rvest)
library(magrittr)
library(tm)
library(tidyr)
library(dplyr)

# Vendor and Date extraction --------------------------------------------------

rm(list = ls())
pDir <- "~/GitHub/ag-Product/2015-01-03"
setwd(pDir)

# 19509 > 18609
pList <- list.files(path = pDir, pattern = ".html", all.files = T, recursive = T)
p0115.03<- data.frame()

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
    p0115.03<- rbind(p0115.03, pTab)
  }
)

#        user  system elapsed 
#     911.393  15.608 934.084 

pList[268]
pList[4937] # jsMo2ksbFB - pineapple trainwreck
pList[6640] # Mwg17zSu71 - cannabis lollipop
pList[8449] # rAESgn40aa - rolex oyster
pList[12170] # XyCkPViMK1 - pv8
pList[14166] # e5mi9cDfUe - opium latex

# safety
write.csv(p0115.03, file = "p-0115-03-raw.csv", row.names = F)
p0115.03<- read.csv("p-0115-03-raw.csv")

# clean extracted data
p0115.03<- p0115.03[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(p0115.03) <- c("list", "date", "vendor", "product", 
                        "price", "cat", "feedback", "shipping")

p0115.03$vendor <- gsub("/vendor/", "", p0115.03$vendor)
p0115.03$vendor <- gsub("/user/", "", p0115.03$vendor)
p0115.03$vendor <- gsub("#", "", p0115.03$vendor)

p0115.03$shipping <- as.character(p0115.03$shipping)
p0115.03$shipping <- stripWhitespace(p0115.03$shipping)
p0115.03$shipping[p0115.03$shipping == " "] <- NA
is.na(p0115.03$shipping)

p0115.03<- separate(p0115.03, shipping, c("from", "to"), sep = "To: ")
p0115.03$from <- gsub("From: ", "", p0115.03$from)

levels(as.factor(p0115.03$from)) # 51
levels(as.factor(p0115.03$to)) # 328

p0115.03$price <- gsub(" BTC", "", p0115.03$price)
p0115.03$price <- as.double(p0115.03$price)

write.csv(p0115.03, file = "p0115.03-c1.csv", row.names = F)

# extract subcategories -------------------------------------------------------

# subset out variables w/o a subcategory
levels(as.factor(p0115.03$cat))

p0115.03$cat <- as.character(p0115.03$cat)
p0115 <- subset(p0115.03,  p0115.03$cat != "Listings" & p0115.03$cat != "Jewelry"
                & p0115.03$cat != "Electronics" & p0115.03$cat != "Other")

# 18599 > 18129
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
#  212.646   6.548 219.304 

# bind subcategories
bind0115_03 <- dplyr::left_join(p0115.03, subcat, by = "list")
is.na(bind0115_03$pTab2)

bind0115_03 <- bind0115_03[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
colnames(bind0115_03) <- c("list", "date", "vendor", "product", "price", 
                           "cat", "subcat", "feedback", "from", "to")

p0115.03<- bind0115_03

# safety
write.csv(p0115.03, file = "p-2015-01-03.csv", row.names = F)

# extract subsubcategories ----------------------------------------------------

# subset subsubcategories
p0115.03 <- read.csv("p-2015-01-03.csv", stringsAsFactors = F)
levels(p0115.03$subcat)
p0115.03$subcat <- as.character(p0115.03$subcat)

# 18599 > 18129 > 13811 > 9787
drugs0115.03 <- subset(p0115.03, p0115.03$cat == "Drugs")
drugs0115.03 <- subset(drugs0115.03, drugs0115.03$subcat != "Other" & 
                         drugs0115.03$subcat != "Weight loss" &
                         drugs0115.03$subcat != "Benzos" &
                         drugs0115.03$subcat != "Prescription" &
                         drugs0115.03$subcat != "RCs" &
                         drugs0115.03$subcat != "Steroids" &
                         drugs0115.03$subcat != "Methylone" &
                         drugs0115.03$subcat != "Opioids" &
                         drugs0115.03$subcat != "Ecstasy-MDMA" &
                         drugs0115.03$subcat != "Barbiturates")

# extract subsubcategories
levels(as.factor(drugs0115.03$subcat))

pList3 <- as.character(drugs0115.03$list)
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
#  116.640   1.907 119.447

# bind sub-subcategories
bind0115_03b <- dplyr::left_join(p0115.03, subcat2, by = "list")
is.na(bind0115_03b$pTab3)

bind0115_03b  <- bind0115_03b [c(1, 2, 3, 4, 5, 6, 7, 11, 8, 9, 10)]
colnames(bind0115_03b) <- c("list", "date", "vendor", "product", "price", 
                            "cat", "subcat", "subsubcat", "feedback", "from", "to")

p0115.03<- bind0115_03b

# final extracted data pre-arules/contigency table transformations
write.csv(p0115.03, file = "products-2015-01-03.csv", row.names = F)
test <- read.csv("products-2015-01-03.csv")

