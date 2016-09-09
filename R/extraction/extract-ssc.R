# Agora Marketplace Analysis: sub-subcategory extraction

library(rvest)
library(magrittr)
library(tm)
library(tidyr)
library(dplyr)
library(data.table)

# CHECK DIRECTORY
getwd()
pDir <- "~/GitHub/ag-Product/2015-04-02"
setwd(pDir)

# CHECK FILE NAME -----------------------------------------
p <- fread("~/GitHub/ag-product-safety-2015/p-2015-04-02.csv", stringsAsFactors = F)
p <- as.data.frame(p)
levels(as.factor(p$subcat))

# subset --------------------------------------------------
d <- subset(p, p$cat == "Drugs")

d <- subset(d, d$subcat == "Cannabis" |
              d$subcat == "Dissociatives" |
              d$subcat == "Ecstasy" |
              d$subcat == "Opioids" |
              d$subcat == "Psychedelics" |
              d$subcat == "Stimulants")

# initialize  ---------------------------------------------
pList3 <- d$list
subcat2 <- data.frame()

# extract -------------------------------------------------
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
bind <- dplyr::left_join(p, subcat2, by = "list")
bind  <- bind[c(1, 2, 3, 4, 5, 6, 7, 11, 8, 9, 10)]
colnames(bind) <- c("list", "date", "vendor", "product", "price", 
                    "cat", "subcat", "subsubcat", "feedback", "from", "to")

p <- bind

# SET FILE OUTPUT NAME #####################################
# SET FILE OUTPUT NAME #####################################
# SET FILE OUTPUT NAME #####################################
write.csv(p, file = "products-2015-04-02.csv", row.names = F)
test <- fread("products-2015-04-02.csv")
levels(as.factor(test$subsubcat))

