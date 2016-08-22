# Agora Marketplace Analysis
# vendor feedback extraction by month
# 2014-10-13

# load ------------------------------------------------------------------------

library(rvest)
library(magrittr)
library(tm)
library(tidyr)
library(dplyr)

rm(list = ls())

# set directory
pDir <- "~/GitHub/ag-Product/2014-10-13"
setwd(pDir)

# Vendor and Date extraction --------------------------------------------------

# 20880
pList <- list.files(path = pDir, pattern = ".html", all.files = T, recursive = T)
p1014.13 <- data.frame()

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
    p1014.13 <- rbind(p1014.13, pTab)
  }
)

#       user  system elapsed 
# 1090.158   12.510 1104.890 

pList[6593] # s63sZ7eBlS - Weed - incomplete
pList[17872] # Wx0lURTjGE - BGrade Afghan Heroin - cheap
pList[19618] # aR0AUfWut9 - all blank

# safety
write.csv(p1014.13, file = "p-1014-13-raw.csv", row.names = F)
p1014.13 <- read.csv("p-1014-13-raw.csv")

# clean extracted data
p1014.13 <- p1014.13[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(p1014.13) <- c("list", "date", "vendor", "product", 
                        "price", "cat", "feedback", "shipping")

p1014.13$vendor <- gsub("/vendor/", "", p1014.13$vendor)
p1014.13$vendor <- gsub("#", "", p1014.13$vendor)

p1014.13$shipping <- as.character(p1014.13$shipping)
p1014.13$shipping <- stripWhitespace(p1014.13$shipping)
p1014.13$shipping[p1014.13$shipping == " "] <- NA
is.na(p1014.13$shipping)

p1014.13 <- separate(p1014.13, shipping, c("from", "to"), sep = "To: ")
p1014.13$from <- gsub("From: ", "", p1014.13$from)

levels(as.factor(p1014.13$from)) # 62
levels(as.factor(p1014.13$to)) # 406

p1014.13$price <- gsub(" BTC", "", p1014.13$price)
p1014.13$price <- as.double(p1014.13$price)

write.csv(p1014.13, file = "p1014.13-c1.csv", row.names = F)

# extract subcategories -------------------------------------------------------

# subset out variables w/o a subcategory
levels(as.factor(p1014.13$cat))

p1014.13$cat <- as.character(p1014.13$cat)
p1014 <- subset(p1014.13,  p1014.13$cat != "Listings" & p1014.13$cat != "Jewelry"
                & p1014.13$cat != "Electronics" & p1014.13$cat != "Other")

# 20878 > 20433
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
#  253.152   8.545 261.928

# bind subcategories
bind1014_13 <- dplyr::left_join(p1014.13, subcat, by = "list")
is.na(bind1014_13$pTab2)

bind1014_13 <- bind1014_13[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
colnames(bind1014_13) <- c("list", "date", "vendor", "product", "price", 
                           "cat", "subcat", "feedback", "from", "to")

p1014.13 <- bind1014_13

# safety
write.csv(p1014.13, file = "p-2014-10-13.csv", row.names = F)

# extract subsubcategories ----------------------------------------------------

# subset subsubcategories
levels(p1014.13$subcat)
p1014.13$subcat <- as.character(p1014.13$subcat)

# 20878 > 20433 > 16349 > 11596
drugs1014.13 <- subset(p1014.13, p1014.13$cat == "Drugs")
drugs1014.13 <- subset(drugs1014.13, drugs1014.13$subcat != "Other" & 
                         drugs1014.13$subcat != "Weight loss" &
                         drugs1014.13$subcat != "Benzos" &
                         drugs1014.13$subcat != "Prescription" &
                         drugs1014.13$subcat != "RCs" &
                         drugs1014.13$subcat != "Steroids" &
                         drugs1014.13$subcat != "Methylone" &
                         drugs1014.13$subcat != "Opioids" &
                         drugs1014.13$subcat != "Ecstasy-MDMA" &
                         drugs1014.13$subcat != "Barbiturates")

# extract subsubcategories
levels(as.factor(drugs1014.13$subcat))

pList3 <- drugs1014.13$list
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
#  142.005   1.578 143.977

# bind sub-subcategories
bind1014_13b <- dplyr::left_join(p1014.13, subcat2, by = "list")
is.na(bind1014_13b$pTab3)

bind1014_13b  <- bind1014_13b [c(1, 2, 3, 4, 5, 6, 7, 11, 8, 9, 10)]
colnames(bind1014_13b) <- c("list", "date", "vendor", "product", "price", 
                            "cat", "subcat", "subsubcat", "feedback", "from", "to")

p1014.13 <- bind1014_13b

# final extracted data pre-arules/contigency table transformations
write.csv(p1014.13, file = "products-2014-10-13.csv", row.names = F)
test <- read.csv("products-2014-10-13.csv")

