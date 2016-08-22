# Agora Marketplace Analysis
# vendor feedback extraction by month
# 2014-09-02

# load ------------------------------------------------------------------------

library(rvest)
library(magrittr)
library(tm)
library(tidyr)
library(dplyr)

rm(list = ls())

# set directory
pDir <- "~/GitHub/ag-Product/2014-09-02"
setwd(pDir)

# Vendor and Date extraction --------------------------------------------------

# 14854
pList <- list.files(path = pDir, pattern = ".html", all.files = T, recursive = T)
p0914.02 <- data.frame()

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
    p0914.02 <- rbind(p0914.02, pTab)
  }
)

#    user  system elapsed 
# 821.879   8.052 845.155
 
pList[1938] # DBa60hC8Eg - incomplete crawl - missing vendorname
pList[7828] # NS7bpLjhJL - incomplete crawl - most info missing
pList[10325] # SqzGfpmDm5 - incomplete crawl - no feedbacks

# safety
write.csv(p0914.02, file = "p-0914-02-raw.csv", row.names = F)
p0914.02 <- read.csv("p-0914-02-raw.csv")

# clean extracted data
p0914.02 <- p0914.02[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(p0914.02) <- c("list", "date", "vendor", "product", 
                        "price", "cat", "feedback", "shipping")

p0914.02$vendor <- gsub("/vendor/", "", p0914.02$vendor)
p0914.02$vendor <- gsub("#", "", p0914.02$vendor)

p0914.02$shipping <- as.character(p0914.02$shipping)
p0914.02$shipping <- stripWhitespace(p0914.02$shipping)
p0914.02$shipping[p0914.02$shipping == " "] <- NA
is.na(p0914.02$shipping)

p0914.02 <- separate(p0914.02, shipping, c("from", "to"), sep = "To: ")
p0914.02$from <- gsub("From: ", "", p0914.02$from)

levels(as.factor(p0914.02$from)) # 61
levels(as.factor(p0914.02$to)) # 304

p0914.02$price <- gsub(" BTC", "", p0914.02$price)
p0914.02$price <- as.double(p0914.02$price)

write.csv(p0914.02, file = "p0914.02-c1.csv", row.names = F)

# extract subcategories -------------------------------------------------------

# subset out variables w/o a subcategory
levels(as.factor(p0914.02$cat))

p0914.02$cat <- as.character(p0914.02$cat)
p0914 <- subset(p0914.02,  p0914.02$cat != "Listings" & p0914.02$cat != "Jewelry"
                & p0914.02$cat != "Electronics" & p0914.02$cat != "Other")

# 14852 > 14501
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
#  177.883   3.316 185.276   

# bind subcategories
bind0914_02 <- dplyr::left_join(p0914.02, subcat, by = "list")
is.na(bind0914_02$pTab2)

bind0914_02 <- bind0914_02[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
colnames(bind0914_02) <- c("list", "date", "vendor", "product", "price", 
                           "cat", "subcat", "feedback", "from", "to")

p0914.02 <- bind0914_02

# safety
write.csv(p0914.02, file = "p-2014-09-02.csv", row.names = F)

# extract subsubcategories ----------------------------------------------------

# subset subsubcategories
levels(p0914.02$subcat)
p0914.02$subcat <- as.character(p0914.02$subcat)

# 14852 > 14501 > 11489 > 7438
drugs0914.02 <- subset(p0914.02, p0914.02$cat == "Drugs")
drugs0914.02 <- subset(drugs0914.02, drugs0914.02$subcat != "Other" & 
                         drugs0914.02$subcat != "Weight loss" &
                         drugs0914.02$subcat != "Benzos" &
                         drugs0914.02$subcat != "Prescription" &
                         drugs0914.02$subcat != "RCs" &
                         drugs0914.02$subcat != "Steroids" &
                         drugs0914.02$subcat != "Methylone" &
                         drugs0914.02$subcat != "Opioids" &
                         drugs0914.02$subcat != "Ecstasy-MDMA" &
                         drugs0914.02$subcat != "Barbiturates")

# extract subsubcategories
levels(as.factor(drugs0914.02$subcat))

pList3 <- drugs0914.02$list
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
#  78.733   0.946  79.678   

# bind sub-subcategories
bind0914_02b <- dplyr::left_join(p0914.02, subcat2, by = "list")
is.na(bind0914_02b$pTab3)

bind0914_02b  <- bind0914_02b [c(1, 2, 3, 4, 5, 6, 7, 11, 8, 9, 10)]
colnames(bind0914_02b) <- c("list", "date", "vendor", "product", "price", 
                            "cat", "subcat", "subsubcat", "feedback", "from", "to")

p0914.02 <- bind0914_02b

# final extracted data pre-arules/contigency table transformations
write.csv(p0914.02, file = "products-2014-09-02.csv", row.names = F)
test <- read.csv("products-2014-09-02.csv")

