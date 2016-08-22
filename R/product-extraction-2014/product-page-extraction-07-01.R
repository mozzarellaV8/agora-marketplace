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

pDir <- "~/GitHub/ag-Product/2014-07-01"
setwd(pDir)

# Vendor and Date extraction --------------------------------------------------

# 10161
pList <- list.files(path = pDir, pattern = ".html", all.files = T, recursive = T)
p0714.01 <- data.frame()

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
    p0714.01 <- rbind(p0714.01, pTab)
  }
)

pList[7593] # ZBZuDzc7mk- incomplete crawl - not enough info to complete
pList[5232] # lmk8iV94XC - incomplete crawl - not enough info to impute

#    user  system elapsed 
# 477.257   3.481 488.018

# safety
write.csv(p0714.01, file = "p0714-01-raw.csv", row.names = F)
test_p0714.01 <- read.csv("p0714-01-raw.csv")

# clean extracted data
p0714.01 <- p0714.01[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(p0714.01) <- c("list", "date", "vendor", "product", 
                        "price", "cat", "feedback", "shipping")

p0714.01$vendor <- gsub("/vendor/", "", p0714.01$vendor)
p0714.01$vendor <- gsub("#", "", p0714.01$vendor)

p0714.01$shipping <- as.character(p0714.01$shipping)
p0714.01$shipping <- stripWhitespace(p0714.01$shipping)
p0714.01$shipping[p0714.01$shipping == " "] <- NA
is.na(p0714.01$shipping)

p0714.01 <- separate(p0714.01, shipping, c("from", "to"), sep = "To: ")
p0714.01$from <- gsub("From: ", "", p0714.01$from)

levels(as.factor(p0714.01$from)) # 59
levels(as.factor(p0714.01$to)) # 220

p0714.01$price <- gsub(" BTC", "", p0714.01$price)
p0714.01$price <- as.double(p0714.01$price)

write.csv(p0714.01, file = "p0714.01-c1.csv", row.names = F)

# extract subcategories -------------------------------------------------------

# subset out variables w/o a subcategory
levels(as.factor(p0714.01$cat))

p0714.01$cat <- as.character(p0714.01$cat)
p0714 <- subset(p0714.01,  p0714.01$cat != "Listings" & p0714.01$cat != "Jewelry"
                & p0714.01$cat != "Electronics" & p0714.01$cat != "Other")

# 10161 > 9750
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
#  115.519   1.279 117.100   


# bind subcategories
bind0714_01 <- dplyr::left_join(p0714.01, subcat, by = "list")
is.na(bind0714_01$pTab2)

bind0714_01 <- bind0714_01[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
colnames(bind0714_01) <- c("list", "date", "vendor", "product", "price", 
                           "cat", "subcat", "feedback", "from", "to")

p0714.01 <- bind0714_01

# safety
write.csv(p0714.01, file = "p-2014-07-01.csv", row.names = F)

# extract subsubcategories ----------------------------------------------------

# subset subsubcategories
levels(p0714.01$subcat)
p0714.01$subcat <- as.character(p0714.01$subcat)

# 10161 > 9750 > 6363 > 3300
drugs0714.01 <- subset(p0714.01, p0714.01$cat == "Drugs")
drugs0714.01 <- subset(drugs0714.01, drugs0714.01$subcat != "Other" & 
                         drugs0714.01$subcat != "Weight loss" &
                         drugs0714.01$subcat != "Benzos" &
                         drugs0714.01$subcat != "Prescription" &
                         drugs0714.01$subcat != "RCs" &
                         drugs0714.01$subcat != "Steroids" &
                         drugs0714.01$subcat != "Methylone" &
                         drugs0714.01$subcat != "Opioids" &
                         drugs0714.01$subcat != "Ecstasy-MDMA" &
                         drugs0714.01$subcat != "Barbiturates")

pList3[2809] # pXkzBvwXfC - ecstasy
pList3[3474] # tvVJErlrzG - ecstasy

# extract subsubcategories
levels(as.factor(drugs0714.01$subcat))

pList3 <- drugs0714.01$list
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
#  34.924   0.257  35.227   

# bind sub-subcategories
bind0714_01b <- dplyr::left_join(p0714.01, subcat2, by = "list")
is.na(bind0714_01b$pTab3)

bind0714_01b  <- bind0714_01b [c(1, 2, 3, 4, 5, 6, 7, 11, 8, 9, 10)]
colnames(bind0714_01b ) <- c("list", "date", "vendor", "product", "price", 
                             "cat", "subcat", "subsubcat", "feedback", "from", "to")

p0714.01 <- bind0714_01b

# final extracted data pre-arules/contigency table transformations
write.csv(p0714.01, file = "products-2014-07-01.csv", row.names = F)
test <- read.csv("products-2014-07-01.csv")

