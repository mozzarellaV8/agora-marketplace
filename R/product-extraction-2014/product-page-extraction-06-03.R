# Agora Marketplace Analysis
# vendor feedback extraction by month
# 2014-06-02

# load ------------------------------------------------------------------------

library(rvest)
library(magrittr)
library(tm)
library(tidyr)
library(dplyr)

# set directory

rm(list = ls())

pDir <- "~/GitHub/ag-Product/2014-06-03"
setwd(pDir)

# Vendor and Date extraction --------------------------------------------------

# 9315
pList <- list.files(path = pDir, pattern = ".html", all.files = T, recursive = T)
p0614.03 <- data.frame()

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
    p0614.03 <- rbind(p0614.03, pTab)
  }
)

pList[7593] # ZBZuDzc7mk- incomplete crawl - not enough info to complete
pList[5232] # lmk8iV94XC - incomplete crawl - not enough info to impute

#    user  system elapsed 
# 474.516   2.993 481.114

# safety
write.csv(p0614.03, file = "p0614-03-raw.csv", row.names = F)
test_p0614.03 <- read.csv("p0614-03-raw.csv")

# clean extracted data
p0614.03 <- p0614.03[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(p0614.03) <- c("list", "date", "vendor", "product", 
                        "price", "cat", "feedback", "shipping")

p0614.03$vendor <- gsub("/vendor/", "", p0614.03$vendor)
p0614.03$vendor <- gsub("#", "", p0614.03$vendor)

p0614.03$shipping <- as.character(p0614.03$shipping)
p0614.03$shipping <- stripWhitespace(p0614.03$shipping)
p0614.03$shipping[p0614.03$shipping == " "] <- NA
is.na(p0614.03$shipping)

p0614.03 <- separate(p0614.03, shipping, c("from", "to"), sep = "To: ")
p0614.03$from <- gsub("From: ", "", p0614.03$from)

levels(as.factor(p0614.03$from)) # 50
levels(as.factor(p0614.03$to)) # 192

p0614.03$price <- gsub(" BTC", "", p0614.03$price)
p0614.03$price <- as.double(p0614.03$price)

write.csv(p0614.03, file = "p0614.03-c1.csv", row.names = F)

# extract subcategories -------------------------------------------------------

# subset out variables w/o a subcategory
levels(as.factor(p0614.03$cat))

p0614.03$cat <- as.character(p0614.03$cat)
p0614 <- subset(p0614.03,  p0614.03$cat != "Listings" & p0614.03$cat != "Jewelry"
                & p0614.03$cat != "Electronics" & p0614.03$cat != "Other")

# 9314 -> 8907
pList2 <- as.character(p0614$list)
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
#  103.324   1.006 104.689   


# bind subcategories
bind0614_03 <- dplyr::left_join(p0614.03, subcat, by = "list")
is.na(bind0614_03$pTab2)

bind0614_03 <- bind0614_03[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
colnames(bind0614_03) <- c("list", "date", "vendor", "product", "price", 
                           "cat", "subcat", "feedback", "from", "to")

p0614.03 <- bind0614_03

# safety
write.csv(p0614.03, file = "p-2014-06-03.csv", row.names = F)

# extract subsubcategories ----------------------------------------------------

# subset subsubcategories
levels(p0614.03$subcat)
p0614.03$subcat <- as.character(p0614.03$subcat)

# 9314 > 8907 > 5868 > 2533
drugs0614.03 <- subset(p0614.03, p0614.03$cat == "Drugs")
drugs0614.03 <- subset(drugs0614.03, drugs0614.03$subcat != "Other" & 
                         drugs0614.03$subcat != "Weight loss" &
                         drugs0614.03$subcat != "Benzos" &
                         drugs0614.03$subcat != "Prescription" &
                         drugs0614.03$subcat != "RCs" &
                         drugs0614.03$subcat != "Steroids" &
                         drugs0614.03$subcat != "Methylone" &
                         drugs0614.03$subcat != "Opioids" &
                         drugs0614.03$subcat != "Ecstasy-MDMA" &
                         drugs0614.03$subcat != "Barbiturates")

pList3[2809] # pXkzBvwXfC - ecstasy
pList3[3474] # tvVJErlrzG - ecstasy

# extract subsubcategories
levels(as.factor(drugs0614.03$subcat))

pList3 <- drugs0614.03$list
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
#  29.461   0.246  29.943   

# bind sub-subcategories
bind0614_03b <- dplyr::left_join(p0614.03, subcat2, by = "list")
is.na(bind0614_03b$pTab3)

bind0614_03b  <- bind0614_03b [c(1, 2, 3, 4, 5, 6, 7, 11, 8, 9, 10)]
colnames(bind0614_03b ) <- c("list", "date", "vendor", "product", "price", 
                             "cat", "subcat", "subsubcat", "feedback", "from", "to")

p0614.03 <- bind0614_03b

# final extracted data pre-arules/contigency table transformations
write.csv(p0614.03, file = "products-2014-06-03.csv", row.names = F)
test <- read.csv("products-2014-06-03.csv")

