# Agora Marketplace Analysis
# Product info extraction
# 2014-12-08

# load ------------------------------------------------------------------------

library(rvest)
library(magrittr)
library(tm)
library(tidyr)
library(dplyr)

# Vendor and Date extraction --------------------------------------------------

rm(list = ls())
pDir <- "~/GitHub/ag-Product/2014-12-08"
setwd(pDir)

# 20874
pList <- list.files(path = pDir, pattern = ".html", all.files = T, recursive = T)
p1214.08 <- data.frame()

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
    p1214.08 <- rbind(p1214.08, pTab)
  }
)

#       user  system elapsed 
#    871.848  13.987 894.341

pList[8695] # r3WFSdv0Uw - Cannabis gem candies - incomplete
pList[11037] # -twerking princess nBome

# safety
write.csv(p1214.08, file = "p-1214-08-raw.csv", row.names = F)
p1214.08 <- read.csv("p-1214-08-raw.csv")

# clean extracted data
p1214.08 <- p1214.08[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(p1214.08) <- c("list", "date", "vendor", "product", 
                        "price", "cat", "feedback", "shipping")

p1214.08$vendor <- gsub("/vendor/", "", p1214.08$vendor)
p1214.08$vendor <- gsub("/user/", "", p1214.08$vendor)
p1214.08$vendor <- gsub("#", "", p1214.08$vendor)

p1214.08$shipping <- as.character(p1214.08$shipping)
p1214.08$shipping <- stripWhitespace(p1214.08$shipping)
p1214.08$shipping[p1214.08$shipping == " "] <- NA
is.na(p1214.08$shipping)

p1214.08 <- separate(p1214.08, shipping, c("from", "to"), sep = "To: ")
p1214.08$from <- gsub("From: ", "", p1214.08$from)

levels(as.factor(p1214.08$from)) # 51
levels(as.factor(p1214.08$to)) # 321

p1214.08$price <- gsub(" BTC", "", p1214.08$price)
p1214.08$price <- as.double(p1214.08$price)

write.csv(p1214.08, file = "p1214.08-c1.csv", row.names = F)

# extract subcategories -------------------------------------------------------

# subset out variables w/o a subcategory
levels(as.factor(p1214.08$cat))

p1214.08$cat <- as.character(p1214.08$cat)
p1214 <- subset(p1214.08,  p1214.08$cat != "Listings" & p1214.08$cat != "Jewelry"
                & p1214.08$cat != "Electronics" & p1214.08$cat != "Other")

# 20873 > 20103
pList2 <- as.character(p1214$list)
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
#  258.864  10.413 269.866 

# bind subcategories
bind1214_08 <- dplyr::left_join(p1214.08, subcat, by = "list")
is.na(bind1214_08$pTab2)

bind1214_08 <- bind1214_08[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
colnames(bind1214_08) <- c("list", "date", "vendor", "product", "price", 
                           "cat", "subcat", "feedback", "from", "to")

p1214.08 <- bind1214_08

# safety
write.csv(p1214.08, file = "p-2014-12-08.csv", row.names = F)

# extract subsubcategories ----------------------------------------------------

# subset subsubcategories
levels(p1214.08$subcat)
p1214.08$subcat <- as.character(p1214.08$subcat)

# 20873 > 20103 > 15118 > 10957
drugs1214.08 <- subset(p1214.08, p1214.08$cat == "Drugs")
drugs1214.08 <- subset(drugs1214.08, drugs1214.08$subcat != "Other" & 
                         drugs1214.08$subcat != "Weight loss" &
                         drugs1214.08$subcat != "Benzos" &
                         drugs1214.08$subcat != "Prescription" &
                         drugs1214.08$subcat != "RCs" &
                         drugs1214.08$subcat != "Steroids" &
                         drugs1214.08$subcat != "Methylone" &
                         drugs1214.08$subcat != "Opioids" &
                         drugs1214.08$subcat != "Ecstasy-MDMA" &
                         drugs1214.08$subcat != "Barbiturates")

# extract subsubcategories
levels(as.factor(drugs1214.08$subcat))

pList3 <- drugs1214.08$list
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
#  127.534   2.288 130.072

# bind sub-subcategories
bind1214_08b <- dplyr::left_join(p1214.08, subcat2, by = "list")
is.na(bind1214_08b$pTab3)

bind1214_08b  <- bind1214_08b [c(1, 2, 3, 4, 5, 6, 7, 11, 8, 9, 10)]
colnames(bind1214_08b) <- c("list", "date", "vendor", "product", "price", 
                            "cat", "subcat", "subsubcat", "feedback", "from", "to")

p1214.08 <- bind1214_08b

# final extracted data pre-arules/contigency table transformations
write.csv(p1214.08, file = "products-2014-12-08.csv", row.names = F)
test <- read.csv("products-2014-12-08.csv")

