# Agora Marketplace Analysis
# vendor feedback extraction by month
# 2014-05-01

# load ------------------------------------------------------------------------

library(rvest)
library(magrittr)
library(tm)
library(tidyr)
library(dplyr)

# set directory

rm(list = ls())

pDir <- "~/GitHub/ag-Product/2014-05-02"
setwd(pDir)

# Vendor and Date extraction --------------------------------------------------

# 12604
pList <- list.files(path = pDir, pattern = ".html", all.files = T, recursive = T)
p0514.02 <- data.frame()

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
    p0514.02 <- rbind(p0514.02, pTab)
  }
)

pList[10504] # VYfjv8UURU
pList[644] # bAv2a5hDoY

#    user  system elapsed 
# 603.662   3.271 610.202 

# safety
write.csv(p0514.02, file = "p0514-02-raw.csv", row.names = F)
test_p0514.02 <- read.csv("p0514-02-raw.csv")

# clean extracted data
p0514.02 <- p0514.02[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(p0514.02) <- c("list", "date", "vendor", "product", 
                        "price", "cat", "feedback", "shipping")

p0514.02$vendor <- gsub("/vendor/", "", p0514.02$vendor)
p0514.02$vendor <- gsub("#", "", p0514.02$vendor)

p0514.02$shipping <- as.character(p0514.02$shipping)
p0514.02$shipping <- stripWhitespace(p0514.02$shipping)
p0514.02$shipping[p0514.02$shipping == " "] <- NA
is.na(p0514.02$shipping)

p0514.02 <- separate(p0514.02, shipping, c("from", "to"), sep = "To: ")
p0514.02$from <- gsub("From: ", "", p0514.02$from)

levels(as.factor(p0514.02$from)) # 52
levels(as.factor(p0514.02$to)) # 276

p0514.02$price <- gsub(" BTC", "", p0514.02$price)
p0514.02$price <- as.double(p0514.02$price)

write.csv(p0514.02, file = "p0514.02-c2.csv", row.names = F)
test_p0514.02 <- read.csv("p0514.02-c2.csv")

# extract subcategories -------------------------------------------------------

# subset out variables w/o a subcategory
levels(as.factor(p0514.02$cat))

p0514.02$cat <- as.character(p0514.02$cat)
p0514 <- subset(p0514.02, p0514.02$cat != "Forgeries" & p0514.02$cat != "Listings" & p0514.02$cat != "Jewelry"
                & p0514.02$cat != "Electronics" & p0514.02$cat != "Other")

# 12603 -> 12164
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
#  130.602   1.967 132.548  


# bind subcategories
bind0514_02 <- dplyr::left_join(p0514.02, subcat, by = "list")
is.na(bind0514_02$pTab2)

bind0514_02 <- bind0514_02[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
colnames(bind0514_02) <- c("list", "date", "vendor", "product", "price", 
                           "cat", "subcat", "feedback", "from", "to")

p0514.02 <- bind0514_02

# safety
write.csv(p0514.02, file = "p-2014-05-02.csv", row.names = F)
test <- read.csv("p-2014-05-02.csv")

# extract subsubcategories ----------------------------------------------------

# subset subsubcategories
levels(p0514.02$subcat)
p0514.02$subcat <- as.character(p0514.02$subcat)

# 12693 -> 12164 -> 9871 
drugs0514.02 <- subset(p0514.02, p0514.02$cat == "Drugs")
drugs0514.02 <- subset(drugs0514.02, drugs0514.02$subcat != "Other" & 
                         drugs0514.02$subcat != "Weight loss" &
                         drugs0514.02$subcat != "Benzos" &
                         drugs0514.02$subcat != "Prescription" &
                         drugs0514.02$subcat != "RCs" &
                         drugs0514.02$subcat != "Steroids" &
                         drugs0514.02$subcat != "Methylone" &
                         drugs0514.02$subcat != "Opioids" &
                         drugs0514.02$subcat != "Ecstasy-MDMA" &
                         drugs0514.02$subcat != "Barbiturates")

pList3[4130] # pXkzBvwXfC
pList3[5079] # tvVJErlrzG

# extract subsubcategories
levels(as.factor(drugs0514.02$subcat))

pList3 <- drugs0514.02$list
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
#  86.287   0.720  87.241    

# bind sub-subcategories
bind0514_02b <- dplyr::left_join(p0514.02, subcat2, by = "list")
is.na(bind0514_02b$pTab3)

bind0514_02b  <- bind0514_02b [c(1, 2, 3, 4, 5, 6, 7, 11, 8, 9, 10)]
colnames(bind0514_02b ) <- c("list", "date", "vendor", "product", "price", 
                             "cat", "subcat", "subsubcat", "feedback", "from", "to")

p0514.02 <- bind0514_02b

# final extracted data pre-arules/contigency table transformations
write.csv(p0514.02, file = "products-2014-05-02.csv", row.names = F)
test <- read.csv("products-2014-05-02.csv")

# explore categories ---------------------------------------------------------

getwd()
setwd("~/GitHub/agora-marketplace")

p0514.02 <- read.csv("~/GitHub/agora-marketplace/data/product/products-2014-02-03.csv")
str(p0514.02)


# feedback as transaction - how many?
p0514.02$feedback <- as.character(p0514.02$feedback)
fb <- subset(p0514.02, p0514.02$feedback != "\n    Feedbacks:\n    No feedbacks found.\n")
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

