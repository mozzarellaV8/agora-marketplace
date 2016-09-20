# Agora Marketplace Analysis: subcategory extraction

library(rvest)
library(magrittr)
library(tm)
library(tidyr)
library(dplyr)
library(data.table)

# SET DIRECTORY -------------------------------------------
getwd()
pDir <- "~/GitHub/ag-Product/2015-04-05"
setwd(pDir)

# SET FILE NAME -------------------------------------------
# p <- fread("/Users/pdpd/GitHub/ag-Product-Safety-2015/p0415.03-c2.csv", 
#            stringsAsFactors = F)
# p <- as.data.frame(p)

# subset --------------------------------------------------
levels(as.factor(p$cat))
p$cat <- as.character(p$cat)
p2 <- subset(p,  p$cat != "Listings" & p$cat != "Jewelry"
             & p$cat != "Electronics" & p$cat != "Other"
             & p$cat != "Chemicals")

# initialize  ---------------------------------------------
pList2 <- as.character(p2$list)
subcat <- data.frame(stringsAsFactors = F)

# extract -------------------------------------------------
system.time(
  for (i in 1:length(pList2)) {
    pLog2 <- read_html(pList2[i])
    
    pTab2 <- pLog2 %>%
      html_nodes(".topnav-element a") %>%
      extract2(2) %>%
      html_text() %>%
      as.character()
    pTab2 <- ifelse(length(nchar(pTab2)) != 0, pTab2, "none")
    
    pTab2 <- as.data.frame(pTab2)
    pTab2$list <- pList2[i]
    subcat <- rbind(subcat, pTab2)
  })

# bind ----------------------------------------------------
bind <- dplyr::left_join(p, subcat, by = "list")
is.na(bind$pTab2)

bind <- bind[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
colnames(bind) <- c("list", "date", "vendor", "product", "price", 
                    "cat", "subcat", "feedback", "from", "to")

p <- bind

# SET FILE NAME ####################################################
# SET FILE NAME ####################################################
# SET FILE NAME ####################################################
write.csv(p, file = "~/GitHub/ag-product-safety-2015/p-2015-04-05.csv", row.names = F)
