# Agora Marketplace Analysis
# vendor feedback extraction by month
# 2014-07-02

# load ------------------------------------------------------------------------

library(rvest)
library(magrittr)
library(tm)
library(tidyr)
library(dplyr)

# set directory

rm(list = ls())

pDir <- "~/GitHub/ag-Product/2014-07-02"
setwd(pDir)

# Vendor and Date extraction --------------------------------------------------

# 14365
pList <- list.files(path = pDir, pattern = ".html", all.files = T, recursive = T)
p0714.02 <- data.frame()

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
    p0714.02 <- rbind(p0714.02, pTab)
  }
)

pList[1841] # d98E7iEcgu- incomplete crawl - not enough info to impute
pList[4751] # j5DgA9i6ny - incomplete crawl - not enough info to impute
pList[8382] # PkV20ZrzMs - incomplete crawl - imputed price from div html tag 
pList[13209] # Y2meJF2snr - incomplete crawl - not enough info to impute

#    user  system elapsed 
# 739.418   6.579 750.883

# safety
write.csv(p0714.02, file = "p0714-02-raw.csv", row.names = F)
test_p0714.02 <- read.csv("p0714-02-raw.csv")

# clean extracted data
p0714.02 <- p0714.02[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(p0714.02) <- c("list", "date", "vendor", "product", 
                        "price", "cat", "feedback", "shipping")

p0714.02$vendor <- gsub("/vendor/", "", p0714.02$vendor)
p0714.02$vendor <- gsub("#", "", p0714.02$vendor)

p0714.02$shipping <- as.character(p0714.02$shipping)
p0714.02$shipping <- stripWhitespace(p0714.02$shipping)
p0714.02$shipping[p0714.02$shipping == " "] <- NA
is.na(p0714.02$shipping)

p0714.02 <- separate(p0714.02, shipping, c("from", "to"), sep = "To: ")
p0714.02$from <- gsub("From: ", "", p0714.02$from)

levels(as.factor(p0714.02$from)) # 66
levels(as.factor(p0714.02$to)) # 297

p0714.02$price <- gsub(" BTC", "", p0714.02$price)
p0714.02$price <- as.double(p0714.02$price)

write.csv(p0714.02, file = "p0714.02-c1.csv", row.names = F)

# extract subcategories -------------------------------------------------------

# subset out variables w/o a subcategory
levels(as.factor(p0714.02$cat))

p0714.02$cat <- as.character(p0714.02$cat)
p0714 <- subset(p0714.02,  p0714.02$cat != "Listings" & p0714.02$cat != "Jewelry"
                & p0714.02$cat != "Electronics" & p0714.02$cat != "Other")

# 14363 > 14038
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
#  160.614   2.854 163.597   


# bind subcategories
bind0714_02 <- dplyr::left_join(p0714.02, subcat, by = "list")
is.na(bind0714_02$pTab2)

bind0714_02 <- bind0714_02[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
colnames(bind0714_02) <- c("list", "date", "vendor", "product", "price", 
                           "cat", "subcat", "feedback", "from", "to")

p0714.02 <- bind0714_02

# safety
write.csv(p0714.02, file = "p-2014-07-02.csv", row.names = F)

# extract subsubcategories ----------------------------------------------------

# subset subsubcategories
levels(p0714.02$subcat)
p0714.02$subcat <- as.character(p0714.02$subcat)

# 14363 > 14038 > 10641 > 7176
drugs0714.02 <- subset(p0714.02, p0714.02$cat == "Drugs")
drugs0714.02 <- subset(drugs0714.02, drugs0714.02$subcat != "Other" & 
                         drugs0714.02$subcat != "Weight loss" &
                         drugs0714.02$subcat != "Benzos" &
                         drugs0714.02$subcat != "Prescription" &
                         drugs0714.02$subcat != "RCs" &
                         drugs0714.02$subcat != "Steroids" &
                         drugs0714.02$subcat != "Methylone" &
                         drugs0714.02$subcat != "Opioids" &
                         drugs0714.02$subcat != "Ecstasy-MDMA" &
                         drugs0714.02$subcat != "Barbiturates")

# extract subsubcategories
levels(as.factor(drugs0714.02$subcat))

pList3 <- drugs0714.02$list
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
#  78.580   0.732  79.372   

pList3[4306] # pXkzBvwXfC - impute ecstasy
pList3[5307] # tvVJErlrzG - impute ecstasy

# bind sub-subcategories
bind0714_02b <- dplyr::left_join(p0714.02, subcat2, by = "list")
is.na(bind0714_02b$pTab3)

bind0714_02b  <- bind0714_02b [c(1, 2, 3, 4, 5, 6, 7, 11, 8, 9, 10)]
colnames(bind0714_02b ) <- c("list", "date", "vendor", "product", "price", 
                             "cat", "subcat", "subsubcat", "feedback", "from", "to")

p0714.02 <- bind0714_02b

# final extracted data pre-arules/contigency table transformations
write.csv(p0714.02, file = "products-2014-07-02.csv", row.names = F)
test <- read.csv("products-2014-07-02.csv")

