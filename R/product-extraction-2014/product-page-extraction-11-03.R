# Agora Marketplace Analysis
# Product info extraction
# 2014-11-03

# load ------------------------------------------------------------------------

library(rvest)
library(magrittr)
library(tm)
library(tidyr)
library(dplyr)

rm(list = ls())

# set directory
pDir <- "~/GitHub/ag-Product/2014-11-03"
setwd(pDir)

# Vendor and Date extraction --------------------------------------------------

# 18029
pList <- list.files(path = pDir, pattern = ".html", all.files = T, recursive = T)
p1114.03 <- data.frame()

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
    p1114.03 <- rbind(p1114.03, pTab)
  }
)

#    user  system elapsed 
# 921.449   8.795 945.549

pList[12915] # VuWUJ0Xqki - captainKirk no feedback

# safety
write.csv(p1114.03, file = "p-1114-03-raw.csv", row.names = F)
p1114.03 <- read.csv("p-1114-03-raw.csv")

# clean extracted data
p1114.03 <- p1114.03[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(p1114.03) <- c("list", "date", "vendor", "product", 
                        "price", "cat", "feedback", "shipping")

p1114.03$vendor <- gsub("/vendor/", "", p1114.03$vendor)
p1114.03$vendor <- gsub("#", "", p1114.03$vendor)

p1114.03$shipping <- as.character(p1114.03$shipping)
p1114.03$shipping <- stripWhitespace(p1114.03$shipping)
p1114.03$shipping[p1114.03$shipping == " "] <- NA
is.na(p1114.03$shipping)

p1114.03 <- separate(p1114.03, shipping, c("from", "to"), sep = "To: ")
p1114.03$from <- gsub("From: ", "", p1114.03$from)

levels(as.factor(p1114.03$from)) # 59
levels(as.factor(p1114.03$to)) # 346

p1114.03$price <- gsub(" BTC", "", p1114.03$price)
p1114.03$price <- as.double(p1114.03$price)

write.csv(p1114.03, file = "p1114.03-c1.csv", row.names = F)

# extract subcategories -------------------------------------------------------

# subset out variables w/o a subcategory
levels(as.factor(p1114.03$cat))

p1114.03$cat <- as.character(p1114.03$cat)
p1014 <- subset(p1114.03,  p1114.03$cat != "Listings" & p1114.03$cat != "Jewelry"
                & p1114.03$cat != "Electronics" & p1114.03$cat != "Other")

# 18028 > 17566
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
#  235.292   5.507 242.146 

# bind subcategories
bind1114_03 <- dplyr::left_join(p1114.03, subcat, by = "list")
is.na(bind1114_03$pTab2)

bind1114_03 <- bind1114_03[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
colnames(bind1114_03) <- c("list", "date", "vendor", "product", "price", 
                           "cat", "subcat", "feedback", "from", "to")

p1114.03 <- bind1114_03

# safety
write.csv(p1114.03, file = "p-2014-11-03.csv", row.names = F)

# extract subsubcategories ----------------------------------------------------

# subset subsubcategories
levels(p1114.03$subcat)
p1114.03$subcat <- as.character(p1114.03$subcat)

# 18028 > 17566 > 13715 > 9342
drugs1114.03 <- subset(p1114.03, p1114.03$cat == "Drugs")
drugs1114.03 <- subset(drugs1114.03, drugs1114.03$subcat != "Other" & 
                         drugs1114.03$subcat != "Weight loss" &
                         drugs1114.03$subcat != "Benzos" &
                         drugs1114.03$subcat != "Prescription" &
                         drugs1114.03$subcat != "RCs" &
                         drugs1114.03$subcat != "Steroids" &
                         drugs1114.03$subcat != "Methylone" &
                         drugs1114.03$subcat != "Opioids" &
                         drugs1114.03$subcat != "Ecstasy-MDMA" &
                         drugs1114.03$subcat != "Barbiturates")

# extract subsubcategories
levels(as.factor(drugs1114.03$subcat))

pList3 <- drugs1114.03$list
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
#  113.162   1.665 115.150

# bind sub-subcategories
bind1114_03b <- dplyr::left_join(p1114.03, subcat2, by = "list")
is.na(bind1114_03b$pTab3)

bind1114_03b  <- bind1114_03b [c(1, 2, 3, 4, 5, 6, 7, 11, 8, 9, 10)]
colnames(bind1114_03b) <- c("list", "date", "vendor", "product", "price", 
                            "cat", "subcat", "subsubcat", "feedback", "from", "to")

p1114.03 <- bind1114_03b
p1114.03$vendor <- gsub("/user/", "", p1114.03$vendor)

# final extracted data pre-arules/contigency table transformations
write.csv(p1114.03, file = "products-2014-11-03.csv", row.names = F)
test <- read.csv("products-2014-11-03.csv")

