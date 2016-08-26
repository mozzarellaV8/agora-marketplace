# Agora Marketplace Analysis
# Product info extraction
# 2015-02-07

# load ------------------------------------------------------------------------

library(rvest)
library(magrittr)
library(tm)
library(tidyr)
library(dplyr)

# Vendor and Date extraction --------------------------------------------------

rm(list = ls())
pDir <- "~/GitHub/ag-Product/2015-02-07"
setwd(pDir)

# 20303 > 20302 > 20301
pList <- list.files(path = pDir, pattern = ".html", all.files = T, recursive = T)
p0215.07 <- data.frame()

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
    p0215.07 <- rbind(p0215.07, pTab)
  }
)

#        user  system elapsed 
#  1038.086   17.504 1068.394 

pList[2446] # k9a4TvNFoC - 20x Cialis
pList[5092] # USDFzTk4lp - Dealing with Difficult People - 24 lessons
pList[11824] # mtC6xfCdfT - Oxygesic 80 mg
pList[12002] # n8cS8yCTAl - How to Win Friends and Influence People (audio)

# safety
write.csv(p0215.07, file = "p-0215-07-raw.csv", row.names = F)

# clean extracted data --------------------------------------------------------

p0215.07 <- p0215.07[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(p0215.07) <- c("list", "date", "vendor", "product", 
                        "price", "cat", "feedback", "shipping")

p0215.07$vendor <- gsub("/vendor/", "", p0215.07$vendor)
p0215.07$vendor <- gsub("/user/", "", p0215.07$vendor)
p0215.07$vendor <- gsub("#", "", p0215.07$vendor)
p0215.07$vendor <- gsub("%7E", "", p0215.07$vendor)

p0215.07$shipping <- as.character(p0215.07$shipping)
p0215.07$shipping <- stripWhitespace(p0215.07$shipping)
p0215.07$shipping[p0215.07$shipping == " "] <- NA
is.na(p0215.07$shipping)

p0215.07 <- separate(p0215.07, shipping, c("from", "to"), sep = "To: ")
p0215.07$from <- gsub("From: ", "", p0215.07$from)

levels(as.factor(p0215.07$from)) # 54
levels(as.factor(p0215.07$to)) # 329

p0215.07$price <- gsub(" BTC", "", p0215.07$price)
p0215.07$price <- as.double(p0215.07$price)

write.csv(p0215.07, file = "p0215.07-c1.csv", row.names = F)

# extract subcategories -------------------------------------------------------

# subset out variables w/o a subcategory
levels(as.factor(p0215.07$cat))

p0215.07$cat <- as.character(p0215.07$cat)
p0115 <- subset(p0215.07,  p0215.07$cat != "Listings" & p0215.07$cat != "Jewelry"
                & p0215.07$cat != "Electronics" & p0215.07$cat != "Other")

# 20299 > 19631
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
#  252.588  10.535 263.481 

# bind subcategories
bind0215_07 <- dplyr::left_join(p0215.07, subcat, by = "list")
is.na(bind0215_07$pTab2)

bind0215_07 <- bind0215_07[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
colnames(bind0215_07) <- c("list", "date", "vendor", "product", "price", 
                           "cat", "subcat", "feedback", "from", "to")

p0215.07 <- bind0215_07

# safety
write.csv(p0215.07, file = "p-2015-02-07.csv", row.names = F)

# extract subsubcategories ----------------------------------------------------

# subset subsubcategories
levels(p0215.07$subcat)
p0215.07$subcat <- as.character(p0215.07$subcat)

# 20299 > 19631 > 15976 > 12163
drugs0215.07 <- subset(p0215.07, p0215.07$cat == "Drugs")
drugs0215.07 <- subset(drugs0215.07, drugs0215.07$subcat != "Other" & 
                         drugs0215.07$subcat != "Weight loss" &
                         drugs0215.07$subcat != "Benzos" &
                         drugs0215.07$subcat != "Prescription" &
                         drugs0215.07$subcat != "RCs" &
                         drugs0215.07$subcat != "Steroids" &
                         drugs0215.07$subcat != "Methylone" &
                         drugs0215.07$subcat != "Ecstasy-MDMA" &
                         drugs0215.07$subcat != "Barbiturates")

# extract subsubcategories
levels(as.factor(drugs0215.07$subcat))

pList3 <- drugs0215.07$list
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
#  148.568   3.129 151.900 

# bind sub-subcategories
bind0215_07b <- dplyr::left_join(p0215.07, subcat2, by = "list")
is.na(bind0215_07b$pTab3)

bind0215_07b  <- bind0215_07b [c(1, 2, 3, 4, 5, 6, 7, 11, 8, 9, 10)]
colnames(bind0215_07b) <- c("list", "date", "vendor", "product", "price", 
                            "cat", "subcat", "subsubcat", "feedback", "from", "to")

p0215.07 <- bind0215_07b

# final extracted data pre-arules/contigency table transformations
write.csv(p0215.07, file = "products-2015-02-07.csv", row.names = F)
test <- read.csv("products-2015-02-07.csv")
