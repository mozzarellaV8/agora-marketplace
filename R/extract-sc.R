# Agora Marketplace Analysis: subcategory extraction

library(rvest)
library(magrittr)
library(tm)
library(tidyr)
library(dplyr)
library(data.table)

pDir <- "~/GitHub/ag-Product/2014/2014-12-01"
setwd(pDir)

p <- fread("~/GitHub/ag-product-safety/p-2014-12-01.csv", stringsAsFactors = F)
levels(as.factor(p$subcat))

# subset --------------------------------------------------
levels(as.factor(p$cat))

p$cat <- as.character(p$cat)
p <- subset(p,  p$cat != "Listings" | p$cat != "Jewelry"
                | p$cat != "Electronics" | p$cat != "Other")

# extract -------------------------------------------------
pList2 <- as.character(p$list)
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

# bind ----------------------------------------------------
bind <- dplyr::left_join(p, subcat, by = "list")
is.na(bind$pTab2)

bind <- bind[c(1, 2, 3, 4, 5, 6, 10, 7, 8, 9)]
colnames(bind) <- c("list", "date", "vendor", "product", "price", 
                           "cat", "subcat", "feedback", "from", "to")

p <- bind

# safety
write.csv(p, file = "p-2015-02-01.csv", row.names = F)