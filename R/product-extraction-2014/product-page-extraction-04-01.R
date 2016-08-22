# Agora Marketplace Analysis
# vendor feedback extraction by month
# 2014-04-01

# load ------------------------------------------------------------------------

library(rvest)
library(magrittr)
library(tm)
library(tidyr)
library(dplyr)

# set directory

rm(list = ls())

pDir <- "~/GitHub/ag-Product/2014-04-01"
setwd(pDir)

# Vendor and Date extraction --------------------------------------------------

# 10379
pList <- list.files(path = pDir, pattern = ".html", all.files = T, recursive = T)
p0414.01 <- data.frame()

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
    p0414.01 <- rbind(p0414.01, pTab)
  }
)

#    user  system elapsed 
# 502.647   3.333 515.820

pList[6660] # RhzpddZBv1

# safety
write.csv(p0414.01, file = "p0414-01-raw.csv", row.names = F)
p0414.01 <- read.csv("p0414-04-01-raw.csv")

# clean extracted data
p0414.01 <- p0414.01[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(p0414.01) <- c("list", "date", "vendor", "product", 
                        "price", "cat", "feedback", "shipping")

p0414.01$vendor <- gsub("/vendor/", "", p0414.01$vendor)
p0414.01$vendor <- gsub("#", "", p0414.01$vendor)

p0414.01$shipping <- as.character(p0414.01$shipping)
p0414.01$shipping <- stripWhitespace(p0414.01$shipping)
p0414.01$shipping[p0414.01$shipping == " "] <- NA
is.na(p0414.01$shipping)

p0414.01 <- separate(p0414.01, shipping, c("from", "to"), sep = "To: ")
p0414.01$from <- gsub("From: ", "", p0414.01$from)

levels(as.factor(p0414.01$from)) # 51
levels(as.factor(p0414.01$to)) # 224

p0414.01$price <- gsub(" BTC", "", p0414.01$price)
p0414.01$price <- as.double(p0414.01$price)

write.csv(p0414.01, file = "p0414.01-c1.csv", row.names = F)
p0414.01 <- read.csv("p0414.01-c1.csv")

# extract subcategories -------------------------------------------------------

# subset out variables w/o a subcategory
levels(as.factor(p0414.01$cat))

p0414.01$cat <- as.character(p0414.01$cat)
p0414 <- subset(p0414.01, p0414.01$cat != "Forgeries" & p0414.01$cat != "Listings" & p0414.01$cat != "Jewelry"
                & p0414.01$cat != "Electronics" & p0414.01$cat != "Other")

# 10378 --> 10113 --> 10097
pList2 <- as.character(p0414$list)
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
# 118.069   1.234 119.725  

pList2[839]

# bind subcategories
bind0414_01 <- dplyr::left_join(p0414.01, subcat, by = "list")
is.na(bind0414_01$pTab2)

bind0414_01 <- bind0414_01[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
colnames(bind0414_01) <- c("list", "date", "vendor", "product", "price", 
                           "cat", "subcat", "feedback", "from", "to")

p0414.01 <- bind0414_01

# safety
write.csv(p0414.01, file = "p-2014-04-01.csv", row.names = F)
test <- read.csv("p-2014-04-01.csv")

# extract subsubcategories ----------------------------------------------------

# subset subsubcategories
levels(p0414.01$subcat)
p0414.01$subcat <- as.character(p0414.01$subcat)

# 10097 -> 10378 -> 8172
drugs0414.01 <- subset(p0414.01, p0414.01$cat == "Drugs")
drugs0414.01 <- subset(drugs0414.01, drugs0414.01$subcat != "Other" & 
                         drugs0414.01$subcat != "Weight loss" &
                         drugs0414.01$subcat != "Benzos" &
                         drugs0414.01$subcat != "Prescription" &
                         drugs0414.01$subcat != "RCs" &
                         drugs0414.01$subcat != "Steroids" &
                         drugs0414.01$subcat != "Methylone" &
                         drugs0414.01$subcat != "Opioids" &
                         drugs0414.01$subcat != "Ecstasy-MDMA" &
                         drugs0414.01$subcat != "Ecstasy-NoSub")

pList3[2511] # pXkzBvwXfC
pList3[3108] # tvVJErlrzG

# extract subsubcategories
levels(as.factor(drugs0414.01$subcat))

pList3 <- drugs0414.01$list
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
#  62.307   0.455  62.863   

pList3[3443] # pXkzBvwXfC
pList3[4276] # tvVJErlrzG

# bind sub-subcategories
bind0414_01b <- dplyr::left_join(p0414.01, subcat2, by = "list")
is.na(bind0414_01b$pTab3)

bind0414_01b  <- bind0414_01b [c(1, 2, 3, 4, 5, 6, 7, 11, 8, 9, 10)]
colnames(bind0414_01b ) <- c("list", "date", "vendor", "product", "price", 
                             "cat", "subcat", "subsubcat", "feedback", "from", "to")

p0414.01 <- bind0414_01b

# final extracted data pre-arules/contigency table transformations
write.csv(p0414.01, file = "products-2014-04-01.csv", row.names = F)
test <- read.csv("products-2014-04-01.csv")

# explore categories ---------------------------------------------------------

getwd()
setwd("~/GitHub/agora-marketplace")

p0414.01 <- read.csv("~/GitHub/agora-marketplace/data/product/products-2014-02-03.csv")
str(p0414.01)


# feedback as transaction - how many?
p0414.01$feedback <- as.character(p0414.01$feedback)
fb <- subset(p0414.01, p0414.01$feedback != "\n    Feedbacks:\n    No feedbacks found.\n")
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

