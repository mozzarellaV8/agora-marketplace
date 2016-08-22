# Agora Marketplace Analysis
# Product info extraction
# 2014-11-06

# load ------------------------------------------------------------------------

library(rvest)
library(magrittr)
library(tm)
library(tidyr)
library(dplyr)

rm(list = ls())

# set directory
pDir <- "~/GitHub/ag-Product/2014-11-06"
setwd(pDir)

# Vendor and Date extraction --------------------------------------------------

# 18258
pList <- list.files(path = pDir, pattern = ".html", all.files = T, recursive = T)
p1114.06 <- data.frame()

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
    p1114.06 <- rbind(p1114.06, pTab)
  }
)

#    user  system elapsed 
# 932.611  10.687 945.395 

pList[12645] # ns9r5CYief - LSD white fluff
pList[17957] # ZFm1UxmD5r - custom no feedback

# safety
write.csv(p1114.06, file = "p-1114-06-raw.csv", row.names = F)
p1114.06 <- read.csv("p-1114-06-raw.csv")

# clean extracted data
p1114.06 <- p1114.06[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(p1114.06) <- c("list", "date", "vendor", "product", 
                        "price", "cat", "feedback", "shipping")

p1114.06$vendor <- gsub("/vendor/", "", p1114.06$vendor)
p1114.06$vendor <- gsub("#", "", p1114.06$vendor)

p1114.06$shipping <- as.character(p1114.06$shipping)
p1114.06$shipping <- stripWhitespace(p1114.06$shipping)
p1114.06$shipping[p1114.06$shipping == " "] <- NA
is.na(p1114.06$shipping)

p1114.06 <- separate(p1114.06, shipping, c("from", "to"), sep = "To: ")
p1114.06$from <- gsub("From: ", "", p1114.06$from)

levels(as.factor(p1114.06$from)) # 59
levels(as.factor(p1114.06$to)) # 349

p1114.06$price <- gsub(" BTC", "", p1114.06$price)
p1114.06$price <- as.double(p1114.06$price)

write.csv(p1114.06, file = "p1114.06-c1.csv", row.names = F)

# extract subcategories -------------------------------------------------------

# subset out variables w/o a subcategory
levels(as.factor(p1114.06$cat))

p1114.06$cat <- as.character(p1114.06$cat)
p1014 <- subset(p1114.06,  p1114.06$cat != "Listings" & p1114.06$cat != "Jewelry"
                & p1114.06$cat != "Electronics" & p1114.06$cat != "Other")

# 18256 > 17787
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
#  235.451   6.518 242.374 

# bind subcategories
bind1114_06 <- dplyr::left_join(p1114.06, subcat, by = "list")
is.na(bind1114_06$pTab2)

bind1114_06 <- bind1114_06[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
colnames(bind1114_06) <- c("list", "date", "vendor", "product", "price", 
                           "cat", "subcat", "feedback", "from", "to")

p1114.06 <- bind1114_06
p1114.06$vendor <- gsub("/user/", "", p1114.06$vendor)

# safety
write.csv(p1114.06, file = "p-2014-11-06.csv", row.names = F)

# extract subsubcategories ----------------------------------------------------

# subset subsubcategories
levels(p1114.06$subcat)
p1114.06$subcat <- as.character(p1114.06$subcat)

# 18256 > 17787 > 13882 > 9196
drugs1114.06 <- subset(p1114.06, p1114.06$cat == "Drugs")
drugs1114.06 <- subset(drugs1114.06, drugs1114.06$subcat != "Other" & 
                         drugs1114.06$subcat != "Weight loss" &
                         drugs1114.06$subcat != "Benzos" &
                         drugs1114.06$subcat != "Prescription" &
                         drugs1114.06$subcat != "RCs" &
                         drugs1114.06$subcat != "Steroids" &
                         drugs1114.06$subcat != "Methylone" &
                         drugs1114.06$subcat != "Opioids" &
                         drugs1114.06$subcat != "Ecstasy-MDMA" &
                         drugs1114.06$subcat != "Barbiturates")

# extract subsubcategories
levels(as.factor(drugs1114.06$subcat))

pList3 <- drugs1114.06$list
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
#  111.542   1.854 113.532

# bind sub-subcategories
bind1114_06b <- dplyr::left_join(p1114.06, subcat2, by = "list")
is.na(bind1114_06b$pTab3)

bind1114_06b  <- bind1114_06b [c(1, 2, 3, 4, 5, 6, 7, 11, 8, 9, 10)]
colnames(bind1114_06b) <- c("list", "date", "vendor", "product", "price", 
                            "cat", "subcat", "subsubcat", "feedback", "from", "to")

p1114.06 <- bind1114_06b

# final extracted data pre-arules/contigency table transformations
write.csv(p1114.06, file = "products-2014-11-06.csv", row.names = F)
test <- read.csv("products-2014-11-06.csv")

