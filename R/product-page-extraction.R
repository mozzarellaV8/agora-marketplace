# Agora Marketplace Analysis
# vendor feedback extraction by month

# load ------------------------------------------------------------------------

library(rvest)
library(magrittr)
library(tm)
library(tidyr)
library(dplyr)

# set directory

rm(list = ls())

pDir <- "~/GitHub/ag-Product/2014-01"
setwd(pDir)

# Vendor and Date extraction --------------------------------------------------
pList <- list.files(path = pDir, pattern = ".html", all.files = T, recursive = T)
products0114 <- data.frame()

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
  products0114 <- rbind(products0114, pTab)
}
)

#    user  system elapsed 
# 367.693   1.323 370.147

# safety
write.csv(products0114, file = "products0114-raw.csv", row.names = F)

# clean extracted data
products0114 <- products0114[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(products0114) <- c("list", "date", "vendor", "product", 
                            "price", "cat", "feedback", "shipping")

products0114$vendor <- gsub("/vendor/", "", products0114$vendor)
products0114$vendor <- gsub("#", "", products0114$vendor)

products0114 <- separate(products0114, shipping, c("from", "to"), sep = "Ships to ")
products0114$from <- gsub("Ships from ", "", products0114$from)
products0114$from[products0114$from == ""] <- NA # figure this out.

products0114$price <- gsub(" BTC", "", products0114$price)
products0114$price <- as.double(products0114$price)

write.csv(products0114, file = "products0114-c1.csv", row.names = F)
products0114 <- read.csv("products0114-c1.csv")

# extract subcategories -------------------------------------------------------

# subset out variables w/o a subcategory
levels(as.factor(products0114$cat))
p0114 <- subset(products0114, products0114$cat != "Forgeries" & products0114$cat != "Listings")

pList2 <- p0114$list
subcat <- data.frame()

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
#  88.923   1.227  90.139

# bind subcategories
bind0114 <- dplyr::left_join(products0114, subcat, by = "list")
is.na(bind0114$pTab2)

bind0114 <- bind0114[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
colnames(bind0114) <- c("list", "date", "vendor", "product", "price", 
                        "cat", "subcat", "feedback", "from", "to")

products0114 <- bind0114

# safety
write.csv(products0114, file = "p-2014-01.csv", row.names = F)
test <- read.csv("p-2014-01.csv")

# extract subsubcategories ----------------------------------------------------

# subset subsubcategories

which(products0114$list =="2014-01-01__ALDcahdCtn.html")
which(products0114$list =="2014-01-01__c2qU1VsyeS.html")
which(products0114$list =="2014-01-01__zVMJoA97ai.html")
products0114[15,7] <- "Methylone"
products0114[59,7] <- "Methylone"
products0114[1050,7] <- "Methylone"
products0114[4870, 7] <- "Methylone"

which(products0114$vendor == "drzheng")

which(products0114$list == "2014-01-16__pXkzBvwXfC.html")
which(products0114$list == "2014-01-26__pXkzBvwXfC.html")
which(products0114$list == "2014-01-16__tvVJErlrzG.html")
which(products0114$list == "2014-01-26__tvVJErlrzG.html")
which(products0114$list == "2014-01-16__TXCz5un5fp.html")
which(products0114$list == "2014-01-26__TXCz5un5fp.html")
products0114[3920, 7] <- "Ecstasy-MDMA"
products0114[6754, 7] <- "Ecstasy-MDMA"
products0114[4242, 7] <- "Ecstasy-MDMA"
products0114[7186, 7] <- "Ecstasy-MDMA"
products0114[4247, 7] <- "Ecstasy-MDMA"
products0114[7192, 7] <- "Ecstasy-MDMA"

levels(products0114$subcat)
products0114$subcat <- as.character(products0114$subcat)

drugs0114 <- subset(products0114, products0114$cat == "Drugs")
drugs0114 <- subset(drugs0114, drugs0114$subcat != "Other" & 
                      drugs0114$subcat != "Weight loss" &
                      drugs0114$subcat != "Benzos" &
                      drugs0114$subcat != "Prescription" &
                      drugs0114$subcat != "RCs" &
                      drugs0114$subcat != "Steroids" &
                      drugs0114$subcat != "Methylone" &
                      drugs0114$subcat != "Opioids" &
                      drugs0114$subcat != "Ecstasy-MDMA")

# 5744 --> 4264 obs --> 3968
levels(as.factor(drugs0114$subcat))

# extra subsubcategories
pList3 <- drugs0114$list
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
#  43.485   0.307  43.787 

# bind sub-subcategories
bind0114 <- dplyr::left_join(products0114, subcat2, by = "list")
is.na(bind0114$pTab3)

bind0114 <- bind0114[c(1, 2, 3, 4, 5, 6, 7, 11, 8, 9, 10)]
colnames(bind0114) <- c("list", "date", "vendor", "product", "price", 
                        "cat", "subcat", "subsubcat", "feedback", "from", "to")

products0114 <- bind0114

# final pre-arules
write.csv(products0114, file = "products-2014-01.csv", row.names = F)
test <- read.csv("products-2014-01.csv")

