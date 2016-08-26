# Agora Marketplace Analysis
# product page data extraction
# 2015-02-13

# load ------------------------------------------------------------------------

library(rvest)
library(magrittr)
library(tm)
library(tidyr)
library(dplyr)

# Vendor and Date extraction --------------------------------------------------

rm(list = ls())
pDir <- "~/GitHub/ag-Product/2015-02-13"
setwd(pDir)

# 19254
pList <- list.files(path = pDir, pattern = ".html", all.files = T, recursive = T)
p0215.13 <- data.frame()

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
    p0215.13 <- rbind(p0215.13, pTab)
  }
)

#     user  system elapsed 
#  941.041  14.875 970.520 

pList[10274] # Nz97Bv0tNe

# safety
write.csv(p0215.13, file = "p-0215-13-raw.csv", row.names = F)

# clean extracted data --------------------------------------------------------
p0215.13 <- p0215.13[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(p0215.13) <- c("list", "date", "vendor", "product", 
                        "price", "cat", "feedback", "shipping")

p0215.13$vendor <- gsub("/vendor/", "", p0215.13$vendor)
p0215.13$vendor <- gsub("/user/", "", p0215.13$vendor)
p0215.13$vendor <- gsub("#", "", p0215.13$vendor)
p0215.13$vendor <- gsub("%7E", "", p0215.13$vendor)

p0215.13$shipping <- as.character(p0215.13$shipping)
p0215.13$shipping <- stripWhitespace(p0215.13$shipping)
p0215.13$shipping[p0215.13$shipping == " "] <- NA
is.na(p0215.13$shipping)

p0215.13 <- separate(p0215.13, shipping, c("from", "to"), sep = "To: ")
p0215.13$from <- gsub("From: ", "", p0215.13$from)

levels(as.factor(p0215.13$from)) # 53
levels(as.factor(p0215.13$to)) # 300

p0215.13$from <- gsub("\\suk(.*)", "UK", p0215.13$from)
p0215.13$from <- gsub("\\sUK\\s", "UK", p0215.13$from)
p0215.13$from <- gsub("UK, Asia(.*)", "no info", p0215.13$from)
p0215.13$from <- gsub("\\sUntied Kingdom(.*)", "UK", p0215.13$from)
p0215.13$from <- gsub("\\sUnited Kingdom\\s", "UK", p0215.13$from)

p0215.13$from <- gsub("\\sWorldwide(*.)", "Worldwide", p0215.13$from)
p0215.13$from <- gsub("\\sworldwide(*.)", "Worldwide", p0215.13$from)
p0215.13$from <- gsub("\\sWORLDWIDE(*.)", "Worldwide", p0215.13$from)
p0215.13$from <- gsub("\\sWorld(*.)", "Worldwide", p0215.13$from)

p0215.13$from <- gsub("\\storland\\s", "Torland", p0215.13$from)
p0215.13$from <- gsub("\\sTorland\\s", "Torland", p0215.13$from)

p0215.13$price <- gsub(" BTC", "", p0215.13$price)
p0215.13$price <- as.double(p0215.13$price)

write.csv(p0215.13, file = "p0215.13-c1.csv", row.names = F)

# extract subcategories -------------------------------------------------------

# subset out variables w/o a subcategory
levels(as.factor(p0215.13$cat))

p0215.13$cat <- as.character(p0215.13$cat)
p0115 <- subset(p0215.13,  p0215.13$cat != "Listings" & p0215.13$cat != "Jewelry"
                & p0215.13$cat != "Electronics" & p0215.13$cat != "Other")

# 19253 > 18630
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
#  233.871   5.993 240.131 

# bind subcategories
bind0215_13 <- dplyr::left_join(p0215.13, subcat, by = "list")
is.na(bind0215_13$pTab2)

bind0215_13 <- bind0215_13[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
colnames(bind0215_13) <- c("list", "date", "vendor", "product", "price", 
                           "cat", "subcat", "feedback", "from", "to")

p0215.13 <- bind0215_13

# safety
write.csv(p0215.13, file = "p-2015-02-13.csv", row.names = F)

# extract subsubcategories ----------------------------------------------------

# subset subsubcategories
levels(p0215.13$subcat)
p0215.13$subcat <- as.character(p0215.13$subcat)

# 19253 > 18630 > 14746 > 10361
drugs0215.13 <- subset(p0215.13, p0215.13$cat == "Drugs")
drugs0215.13 <- subset(drugs0215.13, drugs0215.13$subcat != "Other" & 
                         drugs0215.13$subcat != "Weight loss" &
                         drugs0215.13$subcat != "Benzos" &
                         drugs0215.13$subcat != "Prescription" &
                         drugs0215.13$subcat != "RCs" &
                         drugs0215.13$subcat != "Steroids" &
                         drugs0215.13$subcat != "Methylone" &
                         drugs0215.13$subcat != "Ecstasy-MDMA" &
                         drugs0215.13$subcat != "Barbiturates")

# extract subsubcategories
levels(as.factor(drugs0215.13$subcat))

pList3 <- drugs0215.13$list
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
#  122.294   2.307 124.709  

# bind sub-subcategories
bind0215_13b <- dplyr::left_join(p0215.13, subcat2, by = "list")
is.na(bind0215_13b$pTab3)

bind0215_13b  <- bind0215_13b [c(1, 2, 3, 4, 5, 6, 7, 11, 8, 9, 10)]
colnames(bind0215_13b) <- c("list", "date", "vendor", "product", "price", 
                            "cat", "subcat", "subsubcat", "feedback", "from", "to")

p0215.13 <- bind0215_13b

# final extracted data pre-arules/contigency table transformations
write.csv(p0215.13, file = "products-2015-02-13.csv", row.names = F)
test <- read.csv("products-2015-02-13.csv")
