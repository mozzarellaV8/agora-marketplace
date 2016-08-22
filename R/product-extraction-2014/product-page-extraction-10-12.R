# Agora Marketplace Analysis
# vendor feedback extraction by month
# 2014-10-12

# load ------------------------------------------------------------------------

library(rvest)
library(magrittr)
library(tm)
library(tidyr)
library(dplyr)

rm(list = ls())

# set directory
pDir <- "~/GitHub/ag-Product/2014-10-12"
setwd(pDir)

# Vendor and Date extraction --------------------------------------------------

# 17570
pList <- list.files(path = pDir, pattern = ".html", all.files = T, recursive = T)
p1014.12 <- data.frame()

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
    p1014.12 <- rbind(p1014.12, pTab)
  }
)

#    user  system elapsed 
# 915.496   9.206 930.585 

pList[162] # AMz9C73w7S - nbome
pList[4316] # sZ3ZGyD6of - 4-aco-DMT
pList[7345] # E27w4FzAvB - 10g Eitzolam - this one blank
pList[13330] # Yli2K4Ynsn - rivotril - clonazepam - imputed with vendor data.

# safety
write.csv(p1014.12, file = "p-1014-12-raw.csv", row.names = F)
p1014.12 <- read.csv("p-1014-12-raw.csv")

# clean extracted data
p1014.12 <- p1014.12[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(p1014.12) <- c("list", "date", "vendor", "product", 
                        "price", "cat", "feedback", "shipping")

p1014.12$vendor <- gsub("/vendor/", "", p1014.12$vendor)
p1014.12$vendor <- gsub("#", "", p1014.12$vendor)

p1014.12$shipping <- as.character(p1014.12$shipping)
p1014.12$shipping <- stripWhitespace(p1014.12$shipping)
p1014.12$shipping[p1014.12$shipping == " "] <- NA
is.na(p1014.12$shipping)

p1014.12 <- separate(p1014.12, shipping, c("from", "to"), sep = "To: ")
p1014.12$from <- gsub("From: ", "", p1014.12$from)

levels(as.factor(p1014.12$from)) # 56
levels(as.factor(p1014.12$to)) # 370

p1014.12$price <- gsub(" BTC", "", p1014.12$price)
p1014.12$price <- as.double(p1014.12$price)

write.csv(p1014.12, file = "p1014.12-c1.csv", row.names = F)

# extract subcategories -------------------------------------------------------

# subset out variables w/o a subcategory
levels(as.factor(p1014.12$cat))

p1014.12$cat <- as.character(p1014.12$cat)
p1014 <- subset(p1014.12,  p1014.12$cat != "Listings" & p1014.12$cat != "Jewelry"
                & p1014.12$cat != "Electronics" & p1014.12$cat != "Other")

# 17567 > 17076
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
#  222.055   4.642 226.804   

# bind subcategories
bind1014_12 <- dplyr::left_join(p1014.12, subcat, by = "list")
is.na(bind1014_12$pTab2)

bind1014_12 <- bind1014_12[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
colnames(bind1014_12) <- c("list", "date", "vendor", "product", "price", 
                           "cat", "subcat", "feedback", "from", "to")

p1014.12 <- bind1014_12

# safety
write.csv(p1014.12, file = "p-2014-10-12.csv", row.names = F)

# extract subsubcategories ----------------------------------------------------

# subset subsubcategories
levels(p1014.12$subcat)
p1014.12$subcat <- as.character(p1014.12$subcat)

# 17567 > 17076 > 13651 > 9503
drugs1014.12 <- subset(p1014.12, p1014.12$cat == "Drugs")
drugs1014.12 <- subset(drugs1014.12, drugs1014.12$subcat != "Other" & 
                         drugs1014.12$subcat != "Weight loss" &
                         drugs1014.12$subcat != "Benzos" &
                         drugs1014.12$subcat != "Prescription" &
                         drugs1014.12$subcat != "RCs" &
                         drugs1014.12$subcat != "Steroids" &
                         drugs1014.12$subcat != "Methylone" &
                         drugs1014.12$subcat != "Opioids" &
                         drugs1014.12$subcat != "Ecstasy-MDMA" &
                         drugs1014.12$subcat != "Barbiturates")

# extract subsubcategories
levels(as.factor(drugs1014.12$subcat))

pList3 <- drugs1014.12$list
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
#  108.004   1.363 109.632 

pList3[4731] # pXkzBvwXfC - ecstacy ##
pList3[8744] # tvVJErlrzG - ecstacy

# bind sub-subcategories
bind1014_12b <- dplyr::left_join(p1014.12, subcat2, by = "list")
is.na(bind1014_12b$pTab3)

bind1014_12b  <- bind1014_12b [c(1, 2, 3, 4, 5, 6, 7, 11, 8, 9, 10)]
colnames(bind1014_12b) <- c("list", "date", "vendor", "product", "price", 
                            "cat", "subcat", "subsubcat", "feedback", "from", "to")

p1014.12 <- bind1014_12b

# final extracted data pre-arules/contigency table transformations
write.csv(p1014.12, file = "products-2014-10-12.csv", row.names = F)
test <- read.csv("products-2014-10-12.csv")

