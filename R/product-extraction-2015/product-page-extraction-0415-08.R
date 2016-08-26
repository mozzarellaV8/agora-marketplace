# Agora Marketplace Analysis
# Product info extraction
# 2015-04-08

# load ------------------------------------------------------------------------

library(rvest)
library(magrittr)
library(tm)
library(tidyr)
library(dplyr)

# Vendor and Date extraction --------------------------------------------------

rm(list = ls())
pDir <- "~/GitHub/ag-Product/2015-04-08"
setwd(pDir)

# 
pList <- list.files(path = pDir, pattern = ".html", all.files = T, recursive = T)
p0415.08 <- data.frame()

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
    p0415.08 <- rbind(p0415.08, pTab)
  }
)

#        user  system elapsed 
#  1121.786   21.314 1185.201

# safety
write.csv(p0415.08, file = "p-0415-08-raw.csv", row.names = F)

# clean extracted data --------------------------------------------------------

p0415.08 <- p0415.08[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(p0415.08) <- c("list", "date", "vendor", "product", 
                        "price", "cat", "feedback", "shipping")

p0415.08$vendor <- gsub("/vendor/", "", p0415.08$vendor)
p0415.08$vendor <- gsub("/user/", "", p0415.08$vendor)
p0415.08$vendor <- gsub("#", "", p0415.08$vendor)
p0415.08$vendor <- gsub("%7E", "", p0415.08$vendor)

p0415.08$shipping <- as.character(p0415.08$shipping)
p0415.08$shipping <- stripWhitespace(p0415.08$shipping)
p0415.08$shipping[p0415.08$shipping == " "] <- NA
is.na(p0415.08$shipping)

p0415.08 <- separate(p0415.08, shipping, c("from", "to"), sep = "To: ")
p0415.08$from <- gsub("From: ", "", p0415.08$from)

levels(as.factor(p0415.08$from)) # 49
levels(as.factor(p0415.08$to)) # 300

p0415.08$price <- gsub(" BTC", "", p0415.08$price)
p0415.08$price <- as.double(p0415.08$price)

write.csv(p0415.08, file = "p0415.08-c1.csv", row.names = F)

# extract subcategories -------------------------------------------------------

# subset out variables w/o a subcategory
levels(as.factor(p0415.08$cat))

p0415.08$cat <- as.character(p0415.08$cat)
p0115 <- subset(p0415.08,  p0415.08$cat != "Listings" & p0415.08$cat != "Jewelry"
                & p0415.08$cat != "Electronics" & p0415.08$cat != "Other")

# 
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
bind0415_08 <- dplyr::left_join(p0415.08, subcat, by = "list")
is.na(bind0415_08$pTab2)

bind0415_08 <- bind0415_08[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
colnames(bind0415_08) <- c("list", "date", "vendor", "product", "price", 
                           "cat", "subcat", "feedback", "from", "to")

p0415.08 <- bind0415_08

# safety
write.csv(p0415.08, file = "p-2015-04-08.csv", row.names = F)

# extract subsubcategories ----------------------------------------------------

# subset subsubcategories
levels(p0415.08$subcat)
p0415.08$subcat <- as.character(p0415.08$subcat)

# 
drugs0415.08 <- subset(p0415.08, p0415.08$cat == "Drugs")
drugs0415.08 <- subset(drugs0415.08, drugs0415.08$subcat != "Other" & 
                         drugs0415.08$subcat != "Weight loss" &
                         drugs0415.08$subcat != "Benzos" &
                         drugs0415.08$subcat != "Prescription" &
                         drugs0415.08$subcat != "RCs" &
                         drugs0415.08$subcat != "Steroids" &
                         drugs0415.08$subcat != "Methylone" &
                         drugs0415.08$subcat != "Ecstasy-MDMA" &
                         drugs0415.08$subcat != "Barbiturates")

# extract subsubcategories
levels(as.factor(drugs0415.08$subcat))
pList3 <- drugs0415.08$list
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
#   96.235   1.760  98.010  

# bind sub-subcategories
bind0415_08b <- dplyr::left_join(p0415.08, subcat2, by = "list")
is.na(bind0415_08b$pTab3)

bind0415_08b  <- bind0415_08b [c(1, 2, 3, 4, 5, 6, 7, 11, 8, 9, 10)]
colnames(bind0415_08b) <- c("list", "date", "vendor", "product", "price", 
                            "cat", "subcat", "subsubcat", "feedback", "from", "to")

p0415.08 <- bind0415_08b

# final extracted data pre-arules
write.csv(p0415.08, file = "products-2015-04-08.csv", row.names = F)
test <- read.csv("products-2015-04-08.csv")
