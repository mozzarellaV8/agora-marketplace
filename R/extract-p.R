# Agora Marketplace Analysis
# product info extraction

# load ------------------------------------------------------------------------

library(rvest)
library(magrittr)
library(tm)
library(tidyr)
library(dplyr)

# high-level extraction -------------------------------------------------------

rm(list = ls())
pDir <- "~/GitHub/ag-Product/2015-01-01"
setwd(pDir)

# 18315
pList <- list.files(path = pDir, pattern = ".html", all.files = T, recursive = T)
p <- data.frame()

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
    p <- rbind(p, pTab)
  }
)

#        user  system elapsed 
#     863.045  13.085 900.043 

# safety
write.csv(p, file = "p-0115-01-raw.csv", row.names = F)
p <- read.csv("p-0115-01-raw.csv")

# clean extracted data
p <- p[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(p) <- c("list", "date", "vendor", "product", 
                        "price", "cat", "feedback", "shipping")

p$vendor <- gsub("/vendor/", "", p$vendor)
p$vendor <- gsub("/user/", "", p$vendor)
p$vendor <- gsub("#", "", p$vendor)
p$vendor <- gsub("%7E", "", p$vendor)

p$shipping <- as.character(p$shipping)
p$shipping <- stripWhitespace(p$shipping)
p$shipping[p$shipping == " "] <- NA
is.na(p$shipping)

p <- separate(p, shipping, c("from", "to"), sep = "To: ")
p$from <- gsub("From: ", "", p$from)

levels(as.factor(p$from)) # 53
levels(as.factor(p$to)) # 299

p$price <- gsub(" BTC", "", p$price)
p$price <- as.double(p$price)

write.csv(p, file = "p0115.01-c1.csv", row.names = F)
