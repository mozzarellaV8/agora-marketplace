# Agora Marketplace Analysis
# vendor feedback extraction by month
# 2014-07-04

# load ------------------------------------------------------------------------

library(rvest)
library(magrittr)
library(tm)
library(tidyr)
library(dplyr)

# set directory

rm(list = ls())

pDir <- "~/GitHub/ag-Product/2014-07-04"
setwd(pDir)

# Vendor and Date extraction --------------------------------------------------

# 5927
pList <- list.files(path = pDir, pattern = ".html", all.files = T, recursive = T)
p0714.04 <- data.frame()

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
    p0714.04 <- rbind(p0714.04, pTab)
  }
)

pList[1978] # JDPyXjgE6b - not enough info - incomplete crawl

#    user  system elapsed 
# 261.146   1.280 267.891

# safety
write.csv(p0714.04, file = "p-0714-04-raw.csv", row.names = F)
test_p0714.04 <- read.csv("p-0714-04-raw.csv")

# clean extracted data
p0714.04 <- p0714.04[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(p0714.04) <- c("list", "date", "vendor", "product", 
                        "price", "cat", "feedback", "shipping")

p0714.04$vendor <- gsub("/vendor/", "", p0714.04$vendor)
p0714.04$vendor <- gsub("#", "", p0714.04$vendor)

p0714.04$shipping <- as.character(p0714.04$shipping)
p0714.04$shipping <- stripWhitespace(p0714.04$shipping)
p0714.04$shipping[p0714.04$shipping == " "] <- NA
is.na(p0714.04$shipping)

p0714.04 <- separate(p0714.04, shipping, c("from", "to"), sep = "To: ")
p0714.04$from <- gsub("From: ", "", p0714.04$from)

levels(as.factor(p0714.04$from)) # 48
levels(as.factor(p0714.04$to)) # 119

p0714.04$price <- gsub(" BTC", "", p0714.04$price)
p0714.04$price <- as.double(p0714.04$price)

write.csv(p0714.04, file = "p0714.04-c1.csv", row.names = F)

# extract subcategories -------------------------------------------------------

# subset out variables w/o a subcategory
levels(as.factor(p0714.04$cat))

p0714.04$cat <- as.character(p0714.04$cat)
p0714 <- subset(p0714.04,  p0714.04$cat != "Listings" & p0714.04$cat != "Jewelry"
                & p0714.04$cat != "Electronics" & p0714.04$cat != "Other")

# 5926 > 5833
pList2 <- as.character(p0714$list)
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
#  60.380   0.454  60.832   

# bind subcategories
bind0714_04 <- dplyr::left_join(p0714.04, subcat, by = "list")
is.na(bind0714_04$pTab2)

bind0714_04 <- bind0714_04[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
colnames(bind0714_04) <- c("list", "date", "vendor", "product", "price", 
                           "cat", "subcat", "feedback", "from", "to")

p0714.04 <- bind0714_04

# safety
write.csv(p0714.04, file = "p-2014-07-04.csv", row.names = F)

# extract subsubcategories ----------------------------------------------------

# subset subsubcategories
levels(p0714.04$subcat)
p0714.04$subcat <- as.character(p0714.04$subcat)

# 5926 > 5833 > 3988 > 1150
drugs0714.04 <- subset(p0714.04, p0714.04$cat == "Drugs")
drugs0714.04 <- subset(drugs0714.04, drugs0714.04$subcat != "Other" & 
                         drugs0714.04$subcat != "Weight loss" &
                         drugs0714.04$subcat != "Benzos" &
                         drugs0714.04$subcat != "Prescription" &
                         drugs0714.04$subcat != "RCs" &
                         drugs0714.04$subcat != "Steroids" &
                         drugs0714.04$subcat != "Methylone" &
                         drugs0714.04$subcat != "Opioids" &
                         drugs0714.04$subcat != "Ecstasy-MDMA" &
                         drugs0714.04$subcat != "Barbiturates")

# extract subsubcategories
levels(as.factor(drugs0714.04$subcat))

pList3 <- drugs0714.04$list
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
#  11.770   0.079  11.857   

# bind sub-subcategories
bind0714_04b <- dplyr::left_join(p0714.04, subcat2, by = "list")
is.na(bind0714_04b$pTab3)

bind0714_04b  <- bind0714_04b [c(1, 2, 3, 4, 5, 6, 7, 11, 8, 9, 10)]
colnames(bind0714_04b ) <- c("list", "date", "vendor", "product", "price", 
                             "cat", "subcat", "subsubcat", "feedback", "from", "to")

p0714.04 <- bind0714_04b

# final extracted data pre-arules/contigency table transformations
write.csv(p0714.04, file = "products-2014-07-04.csv", row.names = F)
test <- read.csv("products-2014-07-04.csv")

