# Agora Marketplace Analysis
# vendor feedback extraction by month
# 2014-02-03

# load ------------------------------------------------------------------------

library(rvest)
library(magrittr)
library(tm)
library(tidyr)
library(dplyr)

# set directory

rm(list = ls())

pDir <- "~/GitHub/ag-Product/2014-02-03"
setwd(pDir)

# Vendor and Date extraction --------------------------------------------------

# 11664 items
pList <- list.files(path = pDir, pattern = ".html", all.files = T, recursive = T)
p0214.03 <- data.frame()

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
    p0214.03 <- rbind(p0214.03, pTab)
  }
)

#    user  system elapsed 
# 542.610   2.995 546.874 

# safety
write.csv(p0214.03, file = "p0214-03-raw.csv", row.names = F)
p0214.03 <- read.csv("p0214-03-raw.csv")

# clean extracted data
p0214.03 <- p0214.03[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(p0214.03) <- c("list", "date", "vendor", "product", 
                        "price", "cat", "feedback", "shipping")

p0214.03$vendor <- gsub("/vendor/", "", p0214.03$vendor)
p0214.03$vendor <- gsub("#", "", p0214.03$vendor)

p0214.03$shipping <- as.character(p0214.03$shipping)
p0214.03$shipping <- stripWhitespace(p0214.03$shipping)
p0214.03$shipping[p0214.03$shipping == " "] <- NA
is.na(p0214.03$shipping)

p0214.03 <- separate(p0214.03, shipping, c("from", "to"), sep = "To: ")
p0214.03$from <- gsub("From: ", "", p0214.03$from)

levels(as.factor(p0214.03$from)) # 52
levels(as.factor(p0214.03$to)) # 168

p0214.03$price <- gsub(" BTC", "", p0214.03$price)
p0214.03$price <- as.double(p0214.03$price)

write.csv(p0214.03, file = "p0214.03-c1.csv", row.names = F)
p0214.03 <- read.csv("p0214.03-c1.csv")

# extract subcategories -------------------------------------------------------

# subset out variables w/o a subcategory
levels(as.factor(p0214.03$cat))

p0214.03$cat <- as.character(p0214.03$cat)
p0214 <- subset(p0214.03, p0214.03$cat != "Forgeries" & p0214.03$cat != "Listings" & p0214.03$cat != "Jewelry"
                & p0214.03$cat != "Electronics")

# 7606 --> 7427 --> 7410
pList2 <- as.character(p0214$list)
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

#    user  system elapsed 
# 121.032   1.781 122.790

# bind subcategories
bind0214_02_03 <- dplyr::left_join(p0214.03, subcat, by = "list")
is.na(bind0214_02_03$pTab2)

bind0214_02_03 <- bind0214_02_03[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
colnames(bind0214_02_03) <- c("list", "date", "vendor", "product", "price", 
                              "cat", "subcat", "feedback", "from", "to")

p0214.03 <- bind0214_02_03

# safety
write.csv(p0214.03, file = "p-2014-02-03.csv", row.names = F)
test <- read.csv("p-2014-02-03.csv")

# extract subsubcategories ----------------------------------------------------

# subset subsubcategories
levels(p0214.03$subcat)
p0214.03$subcat <- as.character(p0214.03$subcat)

# 11664 --> 8819 --> 5620
drugs0214.03 <- subset(p0214.03, p0214.03$cat == "Drugs")
drugs0214.03 <- subset(drugs0214.03, drugs0214.03$subcat != "Other" & 
                         drugs0214.03$subcat != "Weight loss" &
                         drugs0214.03$subcat != "Benzos" &
                         drugs0214.03$subcat != "Prescription" &
                         drugs0214.03$subcat != "RCs" &
                         drugs0214.03$subcat != "Steroids" &
                         drugs0214.03$subcat != "Methylone" &
                         drugs0214.03$subcat != "Opioids" &
                         drugs0214.03$subcat != "Ecstasy-MDMA" &
                         drugs0214.03$subcat != "Ecstasy-NoSub")

pList3[3862] # pXkzBvwXfC
pList3[4493] # tvVJErlrzG

# extract subsubcategories
levels(as.factor(drugs0214.03$subcat))

pList3 <- drugs0214.03$list
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
#  59.712   0.543  60.292  

# bind sub-subcategories
bind0214_02_03b <- dplyr::left_join(p0214.03, subcat2, by = "list")
is.na(bind0214_02_03b$pTab3)

bind0214_02_03b  <- bind0214_02_03b [c(1, 2, 3, 4, 5, 6, 7, 11, 8, 9, 10)]
colnames(bind0214_02_03b ) <- c("list", "date", "vendor", "product", "price", 
                               "cat", "subcat", "subsubcat", "feedback", "from", "to")

p0214.03 <- bind0214_02_03b

# final extracted data pre-arules/contigency table transformations
write.csv(p0214.03, file = "products-2014-02-03.csv", row.names = F)
test <- read.csv("products-2014-02-03.csv")

# explore categories ---------------------------------------------------------

getwd()
setwd("~/GitHub/agora-marketplace")

p0214.03 <- read.csv("~/GitHub/agora-marketplace/data/product/products-2014-02-03.csv")
str(p0214.03)


# feedback as transaction - how many?
p0214.03$feedback <- as.character(p0214.03$feedback)
fb <- subset(p0214.03, p0214.03$feedback != "\n    Feedbacks:\n    No feedbacks found.\n")
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

