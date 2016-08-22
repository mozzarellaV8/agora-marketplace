# Agora Marketplace Analysis
# vendor feedback extraction by month
# 2014-09-03

# load ------------------------------------------------------------------------

library(rvest)
library(magrittr)
library(tm)
library(tidyr)
library(dplyr)

rm(list = ls())

# set directory
pDir <- "~/GitHub/ag-Product/2014-09-03"
setwd(pDir)

# Vendor and Date extraction --------------------------------------------------

# 17672
pList <- list.files(path = pDir, pattern = ".html", all.files = T, recursive = T)
p0914.03 <- data.frame()

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
    p0914.03 <- rbind(p0914.03, pTab)
  }
)

#    user  system elapsed 
# 850.435   8.781 869.664

pList[12406] # SWlSZEdVfF - no feedback - removed

# safety
write.csv(p0914.03, file = "p-0914-03-raw.csv", row.names = F)
p0914.03 <- read.csv("p-0914-03-raw.csv")

# clean extracted data
p0914.03 <- p0914.03[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(p0914.03) <- c("list", "date", "vendor", "product", 
                        "price", "cat", "feedback", "shipping")

p0914.03$vendor <- gsub("/vendor/", "", p0914.03$vendor)
p0914.03$vendor <- gsub("#", "", p0914.03$vendor)

p0914.03$shipping <- as.character(p0914.03$shipping)
p0914.03$shipping <- stripWhitespace(p0914.03$shipping)
p0914.03$shipping[p0914.03$shipping == " "] <- NA
is.na(p0914.03$shipping)

p0914.03 <- separate(p0914.03, shipping, c("from", "to"), sep = "To: ")
p0914.03$from <- gsub("From: ", "", p0914.03$from)

levels(as.factor(p0914.03$from)) # 68
levels(as.factor(p0914.03$to)) # 354

p0914.03$price <- gsub(" BTC", "", p0914.03$price)
p0914.03$price <- as.double(p0914.03$price)

write.csv(p0914.03, file = "p0914.03-c1.csv", row.names = F)

# extract subcategories -------------------------------------------------------

# subset out variables w/o a subcategory
levels(as.factor(p0914.03$cat))

p0914.03$cat <- as.character(p0914.03$cat)
p0914 <- subset(p0914.03,  p0914.03$cat != "Listings" & p0914.03$cat != "Jewelry"
                & p0914.03$cat != "Electronics" & p0914.03$cat != "Other")

# 17671 > 17230
pList2 <- as.character(p0914$list)
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
#  195.985   6.088 202.034   

# bind subcategories
bind0914_03 <- dplyr::left_join(p0914.03, subcat, by = "list")
is.na(bind0914_03$pTab2)

bind0914_03 <- bind0914_03[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
colnames(bind0914_03) <- c("list", "date", "vendor", "product", "price", 
                           "cat", "subcat", "feedback", "from", "to")

p0914.03 <- bind0914_03

# safety
write.csv(p0914.03, file = "p-2014-09-03.csv", row.names = F)

# extract subsubcategories ----------------------------------------------------

# subset subsubcategories
levels(p0914.03$subcat)
p0914.03$subcat <- as.character(p0914.03$subcat)

# 17671 > 17230 > 13642 > 9395s
drugs0914.03 <- subset(p0914.03, p0914.03$cat == "Drugs")
drugs0914.03 <- subset(drugs0914.03, drugs0914.03$subcat != "Other" & 
                         drugs0914.03$subcat != "Weight loss" &
                         drugs0914.03$subcat != "Benzos" &
                         drugs0914.03$subcat != "Prescription" &
                         drugs0914.03$subcat != "RCs" &
                         drugs0914.03$subcat != "Steroids" &
                         drugs0914.03$subcat != "Methylone" &
                         drugs0914.03$subcat != "Opioids" &
                         drugs0914.03$subcat != "Ecstasy-MDMA" &
                         drugs0914.03$subcat != "Barbiturates")

# extract subsubcategories
levels(as.factor(drugs0914.03$subcat))

pList3 <- drugs0914.03$list
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
#  89.768   0.996  90.749   

pList3[5664] # pXkzBvwXfC - impute - Ecstasy
pList3[7018] # tvVJErlrzG

# bind sub-subcategories
bind0914_03b <- dplyr::left_join(p0914.03, subcat2, by = "list")
is.na(bind0914_03b$pTab3)

bind0914_03b  <- bind0914_03b [c(1, 2, 3, 4, 5, 6, 7, 11, 8, 9, 10)]
colnames(bind0914_03b) <- c("list", "date", "vendor", "product", "price", 
                            "cat", "subcat", "subsubcat", "feedback", "from", "to")

p0914.03 <- bind0914_03b

# final extracted data pre-arules/contigency table transformations
write.csv(p0914.03, file = "products-2014-09-03.csv", row.names = F)
library(data.table)
test <- read.csv("products-2014-09-03.csv")

