# Agora Marketplace Analysis
# Product info extraction
# 2014-11-11

# load ------------------------------------------------------------------------

library(rvest)
library(magrittr)
library(tm)
library(tidyr)
library(dplyr)

rm(list = ls())

# set directory
pDir <- "~/GitHub/ag-Product/2014-11-11"
setwd(pDir)

# Vendor and Date extraction --------------------------------------------------

# 20226
pList <- list.files(path = pDir, pattern = ".html", all.files = T, recursive = T)
p1114.11 <- data.frame()

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
    p1114.11 <- rbind(p1114.11, pTab)
  }
)

#       user  system elapsed 
# 1052.669   14.250 1070.997 

pList[537] # aMb8VsVzkv - Xanax - alprazolam
pList[1064] # BJ1YExvGCz - blank
pList[13052] # XagiGcMlrY - mastabol - british dragon
pList[13739] # Yfq5P26jl9 - xanax blotters x10
pList[19847] # YbRwDCoPrX - blue twitter mdma

# safety
write.csv(p1114.11, file = "p-1114-11-raw.csv", row.names = F)
p1114.11 <- read.csv("p-1114-11-raw.csv")

# clean extracted data
p1114.11 <- p1114.11[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(p1114.11) <- c("list", "date", "vendor", "product", 
                        "price", "cat", "feedback", "shipping")

p1114.11$vendor <- gsub("/vendor/", "", p1114.11$vendor)
p1114.11$vendor <- gsub("#", "", p1114.11$vendor)

p1114.11$shipping <- as.character(p1114.11$shipping)
p1114.11$shipping <- stripWhitespace(p1114.11$shipping)
p1114.11$shipping[p1114.11$shipping == " "] <- NA
is.na(p1114.11$shipping)

p1114.11 <- separate(p1114.11, shipping, c("from", "to"), sep = "To: ")
p1114.11$from <- gsub("From: ", "", p1114.11$from)

levels(as.factor(p1114.11$from)) # 61
levels(as.factor(p1114.11$to)) # 345

p1114.11$price <- gsub(" BTC", "", p1114.11$price)
p1114.11$price <- as.double(p1114.11$price)

write.csv(p1114.11, file = "p1114.11-c1.csv", row.names = F)
p1114.11 <- read.csv("p1114.11-c1.csv")

p1114.11$boner <- grepl("viagra", p1114.11$product, ignore.case = T)
head(p1114.11$boner)
tail(p1114.11$boner)
p1114.11$boner <- NULL

# extract subcategories -------------------------------------------------------

# subset out variables w/o a subcategory
levels(as.factor(p1114.11$cat))

p1114.11$cat <- as.character(p1114.11$cat)
p1014 <- subset(p1114.11,  p1114.11$cat != "Listings" & p1114.11$cat != "Jewelry"
                & p1114.11$cat != "Electronics" & p1114.11$cat != "Other")

# 20221 > 19485
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
#  236.769   9.076 246.388  

# bind subcategories
bind1114_11 <- dplyr::left_join(p1114.11, subcat, by = "list")
is.na(bind1114_11$pTab2)

bind1114_11 <- bind1114_11[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
colnames(bind1114_11) <- c("list", "date", "vendor", "product", "price", 
                           "cat", "subcat", "feedback", "from", "to")

p1114.11 <- bind1114_11
p1114.11$vendor <- gsub("/user/", "", p1114.11$vendor)

# safety
write.csv(p1114.11, file = "p-2014-11-11.csv", row.names = F)

# extract subsubcategories ----------------------------------------------------

# subset subsubcategories
levels(p1114.11$subcat)
p1114.11$subcat <- as.character(p1114.11$subcat)

# 20221 > 19845 > 14489 > 9660
drugs1114.11 <- subset(p1114.11, p1114.11$cat == "Drugs")
drugs1114.11 <- subset(drugs1114.11, drugs1114.11$subcat != "Other" & 
                         drugs1114.11$subcat != "Weight loss" &
                         drugs1114.11$subcat != "Benzos" &
                         drugs1114.11$subcat != "Prescription" &
                         drugs1114.11$subcat != "RCs" &
                         drugs1114.11$subcat != "Steroids" &
                         drugs1114.11$subcat != "Methylone" &
                         drugs1114.11$subcat != "Opioids" &
                         drugs1114.11$subcat != "Ecstasy-MDMA" &
                         drugs1114.11$subcat != "Barbiturates")

# extract subsubcategories
levels(as.factor(drugs1114.11$subcat))

pList3 <- drugs1114.11$list
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
#  111.744   1.905 113.657

# bind sub-subcategories
bind1114_11b <- dplyr::left_join(p1114.11, subcat2, by = "list")
is.na(bind1114_11b$pTab3)

bind1114_11b  <- bind1114_11b [c(1, 2, 3, 4, 5, 6, 7, 11, 8, 9, 10)]
colnames(bind1114_11b) <- c("list", "date", "vendor", "product", "price", 
                            "cat", "subcat", "subsubcat", "feedback", "from", "to")

p1114.11 <- bind1114_11b

# final extracted data pre-arules/contigency table transformations
write.csv(p1114.11, file = "products-2014-11-11.csv", row.names = F)
test <- read.csv("products-2014-11-11.csv")

