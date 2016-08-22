# Agora Marketplace Analysis
# vendor feedback extraction by month
# 2014-05-04

# load ------------------------------------------------------------------------

library(rvest)
library(magrittr)
library(tm)
library(tidyr)
library(dplyr)

# set directory

rm(list = ls())

pDir <- "~/GitHub/ag-Product/2014-05-04"
setwd(pDir)

# Vendor and Date extraction --------------------------------------------------

# 10019
pList <- list.files(path = pDir, pattern = ".html", all.files = T, recursive = T)
p0514.04 <- data.frame()

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
    p0514.04 <- rbind(p0514.04, pTab)
  }
)

pList[7385] # tqNoJFr0i7 - incomplete crawl - not enough info to complete
pList[5003] # N1ZJDmnzmw - blank page


#    user  system elapsed 
# 443.614   2.253 448.013 

# safety
write.csv(p0514.04, file = "p0514-04-raw.csv", row.names = F)
test_p0514.04 <- read.csv("p0514-04-raw.csv")

# clean extracted data
p0514.04 <- p0514.04[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(p0514.04) <- c("list", "date", "vendor", "product", 
                        "price", "cat", "feedback", "shipping")

p0514.04$vendor <- gsub("/vendor/", "", p0514.04$vendor)
p0514.04$vendor <- gsub("#", "", p0514.04$vendor)

p0514.04$shipping <- as.character(p0514.04$shipping)
p0514.04$shipping <- stripWhitespace(p0514.04$shipping)
p0514.04$shipping[p0514.04$shipping == " "] <- NA
is.na(p0514.04$shipping)

p0514.04 <- separate(p0514.04, shipping, c("from", "to"), sep = "To: ")
p0514.04$from <- gsub("From: ", "", p0514.04$from)

levels(as.factor(p0514.04$from)) # 47
levels(as.factor(p0514.04$to)) # 221

p0514.04$price <- gsub(" BTC", "", p0514.04$price)
p0514.04$price <- as.double(p0514.04$price)

write.csv(p0514.04, file = "p0514.04-c2.csv", row.names = F)
test_p0514.04 <- read.csv("p0514.04-c2.csv")

# extract subcategories -------------------------------------------------------

# subset out variables w/o a subcategory
levels(as.factor(p0514.04$cat))

p0514.04$cat <- as.character(p0514.04$cat)
p0514 <- subset(p0514.04, p0514.04$cat != "Forgeries" & p0514.04$cat != "Listings" & p0514.04$cat != "Jewelry"
                & p0514.04$cat != "Electronics" & p0514.04$cat != "Other")

# 10017 -> 9621
pList2 <- as.character(p0514$list)
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
#  114.878   1.046 116.146   


# bind subcategories
bind0514_04 <- dplyr::left_join(p0514.04, subcat, by = "list")
is.na(bind0514_04$pTab2)

bind0514_04 <- bind0514_04[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
colnames(bind0514_04) <- c("list", "date", "vendor", "product", "price", 
                           "cat", "subcat", "feedback", "from", "to")

p0514.04 <- bind0514_04

# safety
write.csv(p0514.04, file = "p-2014-05-04.csv", row.names = F)
test <- read.csv("p-2014-05-04.csv")

# extract subsubcategories ----------------------------------------------------

# subset subsubcategories
levels(p0514.04$subcat)
p0514.04$subcat <- as.character(p0514.04$subcat)

# 10017 -> 9621 -> 7622 -> 4710
drugs0514.04 <- subset(p0514.04, p0514.04$cat == "Drugs")
drugs0514.04 <- subset(drugs0514.04, drugs0514.04$subcat != "Other" & 
                         drugs0514.04$subcat != "Weight loss" &
                         drugs0514.04$subcat != "Benzos" &
                         drugs0514.04$subcat != "Prescription" &
                         drugs0514.04$subcat != "RCs" &
                         drugs0514.04$subcat != "Steroids" &
                         drugs0514.04$subcat != "Methylone" &
                         drugs0514.04$subcat != "Opioids" &
                         drugs0514.04$subcat != "Ecstasy-MDMA" &
                         drugs0514.04$subcat != "Barbiturates")

pList3[2809] # pXkzBvwXfC - ecstasy
pList3[3474] # tvVJErlrzG - ecstasy

# extract subsubcategories
levels(as.factor(drugs0514.04$subcat))

pList3 <- drugs0514.04$list
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
#  48.814   0.398  49.254  

# bind sub-subcategories
bind0514_04b <- dplyr::left_join(p0514.04, subcat2, by = "list")
is.na(bind0514_04b$pTab3)

bind0514_04b  <- bind0514_04b [c(1, 2, 3, 4, 5, 6, 7, 11, 8, 9, 10)]
colnames(bind0514_04b ) <- c("list", "date", "vendor", "product", "price", 
                             "cat", "subcat", "subsubcat", "feedback", "from", "to")

p0514.04 <- bind0514_04b

# final extracted data pre-arules/contigency table transformations
write.csv(p0514.04, file = "products-2014-05-04.csv", row.names = F)
test <- read.csv("products-2014-05-04.csv")

# explore categories ---------------------------------------------------------

getwd()
setwd("~/GitHub/agora-marketplace")

p0514.04 <- read.csv("~/GitHub/agora-marketplace/data/product/products-2014-02-03.csv")
str(p0514.04)


# feedback as transaction - how many?
p0514.04$feedback <- as.character(p0514.04$feedback)
fb <- subset(p0514.04, p0514.04$feedback != "\n    Feedbacks:\n    No feedbacks found.\n")
# drops from 7986 listings to 1430. 

levels(fb$cat)
#  [1] "Counterfeits"       "Data"               "Drug paraphernalia" "Drugs"              "Forgeries"         
#  [6] "Information"        "Listings"           "Services"           "Tobacco"            "Weapons"

levels(fb$subcat)
# [1] "Accessories"         "Accounts"            "Ammunition"          "Benzos"              "Cannabis"           
# [6] "Clothing"            "Containers"          "Disassociatives"     "eBooks"              "Ecstasy"            
# [11] "Ecstasy-MDMA"        "Electronics"         "Guides"              "Hacking"             "Lethal firearms"    
# [16] "Melee"               "Methylone"           "Money"               "Non-lethal firearms" "Opioids"            
# [21] "Other"               "Pipes"               "Pirated"             "Prescription"        "Psychedelics"       
# [26] "RCs"                 "Smoked"              "Software"            "Steroids"            "Stimulants"         
# [31] "Watches"             "Weight loss" 

levels(fb$subsubcat)
# [1] "2C"         "5-MeO"      "Cocaine"    "DMT"        "Edibles"    "GBL"        "GHB"        "Hash"       "Ketamine"  
# [10] "LSD"        "MDA"        "MDMA"       "Mescaline"  "Meth"       "Mushrooms"  "MXE"        "NB"         "Other"     
# [19] "Others"     "Pills"      "Salvia"     "Speed"      "Spores"     "Synthetics" "Weed" 



# binarization for arules -----------------------------------------------------

