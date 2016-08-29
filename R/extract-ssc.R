# Agora Marketplace Analysis: sub-subcategory extraction
library(rvest)
library(magrittr)
library(tm)
library(tidyr)
library(dplyr)
library(data.table)

pDir <- "~/GitHub/ag-Product/2014/2014-11-08"
setwd(pDir)

p <- fread("~/GitHub/ag-product-safety/p-2014-11-08.csv", stringsAsFactors = F)
levels(as.factor(p$subcat))

# subset --------------------------------------------------
d <- subset(p, p$cat == "Drugs")
d <- subset(d, d$subcat == "Cannabis" |
              d$subcat == "Dissociatives" |
              d$subcat == "Ecstasy" |
              d$subcat == "Opioids" |
              d$subcat == "Psychedelics" |
              d$subcat == "Stimulants")

# extract -------------------------------------------------
pList3 <- d$list
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

# bind ----------------------------------------------------
p <- as.data.frame(p)
bind <- dplyr::left_join(p, subcat2, by = "list")
bind  <- bind[c(1, 2, 3, 4, 5, 6, 7, 11, 8, 9, 10)]
colnames(bind) <- c("list", "date", "vendor", "product", "price", 
                    "cat", "subcat", "subsubcat", "feedback", "from", "to")

p <- bind
p$vendor <- gsub("%7E", "", p$vendor)
p$vendor <- gsub("/user/", "", p$vendor)
p$feedback <- stripWhitespace(p$feedback)

write.csv(p, file = "products-2014-11-08.csv", row.names = F)
test <- read.csv("products-2014-11-08.csv")
levels(test$subsubcat)
