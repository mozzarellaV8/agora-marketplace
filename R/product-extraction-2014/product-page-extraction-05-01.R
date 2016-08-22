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

pDir <- "~/GitHub/ag-Product/2014-05-01"
setwd(pDir)

# Vendor and Date extraction --------------------------------------------------

# 7489
pList <- list.files(path = pDir, pattern = ".html", all.files = T, recursive = T)
p0514.01 <- data.frame()

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
    p0514.01 <- rbind(p0514.01, pTab)
  }
)

pList[7407] # incomplete crawl: replaced
pList[5714]
pList[5151] # nwUdd7bZ3w
pList[1866] # VAv4Gb3oND
pList[243] # cTFRXjiKvD
pList[530] # fnCtZXdrmS
pList[761]

#    user  system elapsed 
# 320.438   1.030 321.594

# safety
write.csv(p0514.01, file = "p0514-01-raw.csv", row.names = F)
test_p0514.01 <- read.csv("p0514-01-raw.csv")

# clean extracted data
p0514.01 <- p0514.01[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(p0514.01) <- c("list", "date", "vendor", "product", 
                        "price", "cat", "feedback", "shipping")

p0514.01$vendor <- gsub("/vendor/", "", p0514.01$vendor)
p0514.01$vendor <- gsub("#", "", p0514.01$vendor)

p0514.01$shipping <- as.character(p0514.01$shipping)
p0514.01$shipping <- stripWhitespace(p0514.01$shipping)
p0514.01$shipping[p0514.01$shipping == " "] <- NA
is.na(p0514.01$shipping)

p0514.01 <- separate(p0514.01, shipping, c("from", "to"), sep = "To: ")
p0514.01$from <- gsub("From: ", "", p0514.01$from)

levels(as.factor(p0514.01$from)) # 44
levels(as.factor(p0514.01$to)) # 150

p0514.01$price <- gsub(" BTC", "", p0514.01$price)
p0514.01$price <- as.double(p0514.01$price)

write.csv(p0514.01, file = "p0514.01-c1.csv", row.names = F)
test_p0514.01 <- read.csv("p0514.01-c1.csv")

# extract subcategories -------------------------------------------------------

# subset out variables w/o a subcategory
levels(as.factor(p0514.01$cat))

p0514.01$cat <- as.character(p0514.01$cat)
p0514 <- subset(p0514.01, p0514.01$cat != "Forgeries" & p0514.01$cat != "Listings" & p0514.01$cat != "Jewelry"
                & p0514.01$cat != "Electronics" & p0514.01$cat != "Other")

# 7483 -> 6712
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
#   69.187   0.435  69.610  


# bind subcategories
bind0414_02 <- dplyr::left_join(p0514.01, subcat, by = "list")
is.na(bind0414_02$pTab2)

bind0414_02 <- bind0414_02[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
colnames(bind0414_02) <- c("list", "date", "vendor", "product", "price", 
                           "cat", "subcat", "feedback", "from", "to")

p0514.01 <- bind0414_02

# safety
write.csv(p0514.01, file = "p-2014-05-01.csv", row.names = F)
test <- read.csv("p-2014-05-01.csv")

# extract subsubcategories ----------------------------------------------------

# subset subsubcategories
levels(p0514.01$subcat)
p0514.01$subcat <- as.character(p0514.01$subcat)

# 7483 -> 6712 -> 3326 -> 2242
drugs0514.01 <- subset(p0514.01, p0514.01$cat == "Drugs")
drugs0514.01 <- subset(drugs0514.01, drugs0514.01$subcat != "Other" & 
                         drugs0514.01$subcat != "Weight loss" &
                         drugs0514.01$subcat != "Benzos" &
                         drugs0514.01$subcat != "Prescription" &
                         drugs0514.01$subcat != "RCs" &
                         drugs0514.01$subcat != "Steroids" &
                         drugs0514.01$subcat != "Methylone" &
                         drugs0514.01$subcat != "Opioids" &
                         drugs0514.01$subcat != "Ecstasy-MDMA" &
                         drugs0514.01$subcat != "Barbiturates")

pList3[2511] # pXkzBvwXfC

# extract subsubcategories
levels(as.factor(drugs0514.01$subcat))

pList3 <- drugs0514.01$list
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
#  24.677   0.131  24.804    

# bind sub-subcategories
bind0414_02b <- dplyr::left_join(p0514.01, subcat2, by = "list")
is.na(bind0414_02b$pTab3)

bind0414_02b  <- bind0414_02b [c(1, 2, 3, 4, 5, 6, 7, 11, 8, 9, 10)]
colnames(bind0414_02b ) <- c("list", "date", "vendor", "product", "price", 
                             "cat", "subcat", "subsubcat", "feedback", "from", "to")

p0514.01 <- bind0414_02b

# final extracted data pre-arules/contigency table transformations
write.csv(p0514.01, file = "products-2014-05-01.csv", row.names = F)
test <- read.csv("products-2014-05-01.csv")

# explore categories ---------------------------------------------------------

getwd()
setwd("~/GitHub/agora-marketplace")

p0514.01 <- read.csv("~/GitHub/agora-marketplace/data/product/products-2014-02-03.csv")
str(p0514.01)


# feedback as transaction - how many?
p0514.01$feedback <- as.character(p0514.01$feedback)
fb <- subset(p0514.01, p0514.01$feedback != "\n    Feedbacks:\n    No feedbacks found.\n")
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

