# Agora Marketplace Analysis
# Product info extraction
# 2015-01-04

# load ------------------------------------------------------------------------

library(rvest)
library(magrittr)
library(tm)
library(tidyr)
library(dplyr)

# Vendor and Date extraction --------------------------------------------------

rm(list = ls())
pDir <- "~/GitHub/ag-Product/2015-01-04"
setwd(pDir)

# 17797
pList <- list.files(path = pDir, pattern = ".html", all.files = T, recursive = T)
p0115.04 <- data.frame()

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
    p0115.04<- rbind(p0115.04, pTab)
  }
)

#        user  system elapsed 
#     921.492  10.375 942.406 

pList[2421] # A52L6tMeDS - salvia
pList[2421] # A52L6tMeDS
pList[11922] # jpTlohWDl3 - speed paste - pink

# safety
write.csv(p0115.04, file = "p-0115-04-raw.csv", row.names = F)
p0115.04<- read.csv("p-0115-04-raw.csv")

# clean extracted data
p0115.04<- p0115.04[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(p0115.04) <- c("list", "date", "vendor", "product", 
                        "price", "cat", "feedback", "shipping")

p0115.04$vendor <- gsub("/vendor/", "", p0115.04$vendor)
p0115.04$vendor <- gsub("/user/", "", p0115.04$vendor)
p0115.04$vendor <- gsub("#", "", p0115.04$vendor)

p0115.04$shipping <- as.character(p0115.04$shipping)
p0115.04$shipping <- stripWhitespace(p0115.04$shipping)
p0115.04$shipping[p0115.04$shipping == " "] <- NA
is.na(p0115.04$shipping)

p0115.04<- separate(p0115.04, shipping, c("from", "to"), sep = "To: ")
p0115.04$from <- gsub("From: ", "", p0115.04$from)

levels(as.factor(p0115.04$from)) # 48
levels(as.factor(p0115.04$to)) # 302

p0115.04$price <- gsub(" BTC", "", p0115.04$price)
p0115.04$price <- as.double(p0115.04$price)

write.csv(p0115.04, file = "p0115.04-c1.csv", row.names = F)

# extract subcategories -------------------------------------------------------

# subset out variables w/o a subcategory
levels(as.factor(p0115.04$cat))

p0115.04$cat <- as.character(p0115.04$cat)
p0115 <- subset(p0115.04,  p0115.04$cat != "Listings" & p0115.04$cat != "Jewelry"
                & p0115.04$cat != "Electronics" & p0115.04$cat != "Other")

# 17795 > 17179
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
#  223.177   5.318 229.047 

# bind subcategories
bind0115_04 <- dplyr::left_join(p0115.04, subcat, by = "list")
is.na(bind0115_04$pTab2)

bind0115_04 <- bind0115_04[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
colnames(bind0115_04) <- c("list", "date", "vendor", "product", "price", 
                           "cat", "subcat", "feedback", "from", "to")

p0115.04<- bind0115_04

# safety
write.csv(p0115.04, file = "p-2015-01-04.csv", row.names = F)

# extract subsubcategories ----------------------------------------------------

# subset subsubcategories
levels(p0115.04$subcat)
p0115.04$subcat <- as.character(p0115.04$subcat)

# 17795 > 17179 > 12663 > 7504
drugs0115.04 <- subset(p0115.04, p0115.04$cat == "Drugs")
drugs0115.04 <- subset(drugs0115.04, drugs0115.04$subcat != "Other" & 
                         drugs0115.04$subcat != "Weight loss" &
                         drugs0115.04$subcat != "Benzos" &
                         drugs0115.04$subcat != "Prescription" &
                         drugs0115.04$subcat != "RCs" &
                         drugs0115.04$subcat != "Steroids" &
                         drugs0115.04$subcat != "Methylone" &
                         drugs0115.04$subcat != "Opioids" &
                         drugs0115.04$subcat != "Ecstasy-MDMA" &
                         drugs0115.04$subcat != "Barbiturates")

# extract subsubcategories
levels(as.factor(drugs0115.04$subcat))

pList3 <- drugs0115.04$list
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
#   90.813   0.900  92.044

# bind sub-subcategories
bind0115_04b <- dplyr::left_join(p0115.04, subcat2, by = "list")
is.na(bind0115_04b$pTab3)

bind0115_04b  <- bind0115_04b [c(1, 2, 3, 4, 5, 6, 7, 11, 8, 9, 10)]
colnames(bind0115_04b) <- c("list", "date", "vendor", "product", "price", 
                            "cat", "subcat", "subsubcat", "feedback", "from", "to")

p0115.04<- bind0115_04b

# final extracted data pre-arules/contigency table transformations
write.csv(p0115.04, file = "products-2015-01-04.csv", row.names = F)
test <- read.csv("products-2015-01-04.csv")

