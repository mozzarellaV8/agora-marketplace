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

pDir <- "~/GitHub/ag-Product/2014-06-02"
setwd(pDir)

# Vendor and Date extraction --------------------------------------------------

# 6311
pList <- list.files(path = pDir, pattern = ".html", all.files = T, recursive = T)
p0614.02 <- data.frame()

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
    p0614.02 <- rbind(p0614.02, pTab)
  }
)

pList[2537] # Mwj04n6KkK - incomplete crawl - not enough info to complete
pList[5232] # lmk8iV94XC - incomplete crawl - not enough info to impute

#    user  system elapsed 
# 286.874   1.158 289.573 

# safety
write.csv(p0614.02, file = "p0614-02-raw.csv", row.names = F)
test_p0614.02 <- read.csv("p0614-02-raw.csv")

# clean extracted data
p0614.02 <- p0614.02[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(p0614.02) <- c("list", "date", "vendor", "product", 
                        "price", "cat", "feedback", "shipping")

p0614.02$vendor <- gsub("/vendor/", "", p0614.02$vendor)
p0614.02$vendor <- gsub("#", "", p0614.02$vendor)

p0614.02$shipping <- as.character(p0614.02$shipping)
p0614.02$shipping <- stripWhitespace(p0614.02$shipping)
p0614.02$shipping[p0614.02$shipping == " "] <- NA
is.na(p0614.02$shipping)

p0614.02 <- separate(p0614.02, shipping, c("from", "to"), sep = "To: ")
p0614.02$from <- gsub("From: ", "", p0614.02$from)

levels(as.factor(p0614.02$from)) # 39
levels(as.factor(p0614.02$to)) # 149

p0614.02$price <- gsub(" BTC", "", p0614.02$price)
p0614.02$price <- as.double(p0614.02$price)

write.csv(p0614.02, file = "p0614.02-c1.csv", row.names = F)
test_p0614.02 <- read.csv("p0614.02-c1.csv")

# extract subcategories -------------------------------------------------------

# subset out variables w/o a subcategory
levels(as.factor(p0614.02$cat))

p0614.02$cat <- as.character(p0614.02$cat)
p0614 <- subset(p0614.02,  p0614.02$cat != "Listings" & p0614.02$cat != "Jewelry"
                & p0614.02$cat != "Electronics" & p0614.02$cat != "Other")

# 6310 -> 5843
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
#   63.538   0.543  64.097   


# bind subcategories
bind0614_02 <- dplyr::left_join(p0614.02, subcat, by = "list")
is.na(bind0614_02$pTab2)

bind0614_02 <- bind0614_02[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
colnames(bind0614_02) <- c("list", "date", "vendor", "product", "price", 
                           "cat", "subcat", "feedback", "from", "to")

p0614.02 <- bind0614_02

# safety
write.csv(p0614.02, file = "p-2014-06-02.csv", row.names = F)
test <- read.csv("p-2014-06-02.csv")

# extract subsubcategories ----------------------------------------------------

# subset subsubcategories
levels(p0614.02$subcat)
p0614.02$subcat <- as.character(p0614.02$subcat)

# 6310 > 5843 > 2770 >  1616
drugs0614.02 <- subset(p0614.02, p0614.02$cat == "Drugs")
drugs0614.02 <- subset(drugs0614.02, drugs0614.02$subcat != "Other" & 
                         drugs0614.02$subcat != "Weight loss" &
                         drugs0614.02$subcat != "Benzos" &
                         drugs0614.02$subcat != "Prescription" &
                         drugs0614.02$subcat != "RCs" &
                         drugs0614.02$subcat != "Steroids" &
                         drugs0614.02$subcat != "Methylone" &
                         drugs0614.02$subcat != "Opioids" &
                         drugs0614.02$subcat != "Ecstasy-MDMA" &
                         drugs0614.02$subcat != "Barbiturates")

pList3[2809] # pXkzBvwXfC - ecstasy
pList3[3474] # tvVJErlrzG - ecstasy

# extract subsubcategories
levels(as.factor(drugs0614.02$subcat))

pList3 <- drugs0614.02$list
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
#  16.966   0.126  17.189   

# bind sub-subcategories
bind0614_02b <- dplyr::left_join(p0614.02, subcat2, by = "list")
is.na(bind0614_02b$pTab3)

bind0614_02b  <- bind0614_02b [c(1, 2, 3, 4, 5, 6, 7, 11, 8, 9, 10)]
colnames(bind0614_02b ) <- c("list", "date", "vendor", "product", "price", 
                             "cat", "subcat", "subsubcat", "feedback", "from", "to")

p0614.02 <- bind0614_02b

# final extracted data pre-arules/contigency table transformations
write.csv(p0614.02, file = "products-2014-06-02.csv", row.names = F)
test <- read.csv("products-2014-06-02.csv")
