# Agora Marketplace Analysis
# vendor feedback extraction by month
# 2014-10-06

# load ------------------------------------------------------------------------

library(rvest)
library(magrittr)
library(tm)
library(tidyr)
library(dplyr)

rm(list = ls())

# set directory
pDir <- "~/GitHub/ag-Product/2014-10-06"
setwd(pDir)

# Vendor and Date extraction --------------------------------------------------

# 10673
pList <- list.files(path = pDir, pattern = ".html", all.files = T, recursive = T)
p1014.06 <- data.frame()

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
    p1014.06 <- rbind(p1014.06, pTab)
  }
)

#    user  system elapsed 
# 513.975   4.069 535.219 

pList[2121] # dc8dr7dAeU - 4-FMA - imputed from 10-02
pList[8543] # NUeitaqcZn - incomplete

# safety
write.csv(p1014.06, file = "p-1014-06-raw.csv", row.names = F)
p1014.06 <- read.csv("p-1014-06-raw.csv")

# clean extracted data
p1014.06 <- p1014.06[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(p1014.06) <- c("list", "date", "vendor", "product", 
                        "price", "cat", "feedback", "shipping")

p1014.06$vendor <- gsub("/vendor/", "", p1014.06$vendor)
p1014.06$vendor <- gsub("#", "", p1014.06$vendor)

p1014.06$shipping <- as.character(p1014.06$shipping)
p1014.06$shipping <- stripWhitespace(p1014.06$shipping)
p1014.06$shipping[p1014.06$shipping == " "] <- NA
is.na(p1014.06$shipping)

p1014.06 <- separate(p1014.06, shipping, c("from", "to"), sep = "To: ")
p1014.06$from <- gsub("From: ", "", p1014.06$from)

levels(as.factor(p1014.06$from)) # 51
levels(as.factor(p1014.06$to)) # 199

p1014.06$price <- gsub(" BTC", "", p1014.06$price)
p1014.06$price <- as.double(p1014.06$price)

write.csv(p1014.06, file = "p1014.06-c1.csv", row.names = F)

# extract subcategories -------------------------------------------------------

# subset out variables w/o a subcategory
levels(as.factor(p1014.06$cat))

p1014.06$cat <- as.character(p1014.06$cat)
p1014 <- subset(p1014.06,  p1014.06$cat != "Listings" & p1014.06$cat != "Jewelry"
                & p1014.06$cat != "Electronics" & p1014.06$cat != "Other")

# 10673 > 10236
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
#  117.434   1.994 119.504    

# bind subcategories
bind1014_06 <- dplyr::left_join(p1014.06, subcat, by = "list")
is.na(bind1014_06$pTab2)

bind1014_06 <- bind1014_06[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
colnames(bind1014_06) <- c("list", "date", "vendor", "product", "price", 
                           "cat", "subcat", "feedback", "from", "to")

p1014.06 <- bind1014_06

# safety
write.csv(p1014.06, file = "p-2014-10-06.csv", row.names = F)

# extract subsubcategories ----------------------------------------------------

# subset subsubcategories
levels(p1014.06$subcat)
p1014.06$subcat <- as.character(p1014.06$subcat)

# 10673 > 10236 > 7382 >  3647
drugs1014.06 <- subset(p1014.06, p1014.06$cat == "Drugs")
drugs1014.06 <- subset(drugs1014.06, drugs1014.06$subcat != "Other" & 
                         drugs1014.06$subcat != "Weight loss" &
                         drugs1014.06$subcat != "Benzos" &
                         drugs1014.06$subcat != "Prescription" &
                         drugs1014.06$subcat != "RCs" &
                         drugs1014.06$subcat != "Steroids" &
                         drugs1014.06$subcat != "Methylone" &
                         drugs1014.06$subcat != "Opioids" &
                         drugs1014.06$subcat != "Ecstasy-MDMA" &
                         drugs1014.06$subcat != "Barbiturates")

# extract subsubcategories
levels(as.factor(drugs1014.06$subcat))

pList3 <- drugs1014.06$list
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
#  38.047   0.312  38.354 

pList3[4731] # pXkzBvwXfC - ecstacy
pList3[5906] # tvVJErlrzG - ecstacy

# bind sub-subcategories
bind1014_06b <- dplyr::left_join(p1014.06, subcat2, by = "list")
is.na(bind1014_06b$pTab3)

bind1014_06b  <- bind1014_06b [c(1, 2, 3, 4, 5, 6, 7, 11, 8, 9, 10)]
colnames(bind1014_06b) <- c("list", "date", "vendor", "product", "price", 
                            "cat", "subcat", "subsubcat", "feedback", "from", "to")

p1014.06 <- bind1014_06b

# final extracted data pre-arules/contigency table transformations
write.csv(p1014.06, file = "products-2014-10-06.csv", row.names = F)
test <- read.csv("products-2014-10-06.csv")

