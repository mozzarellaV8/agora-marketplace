# Agora Marketplace Analysis
# product info extraction

# load ------------------------------------------------------------------------

library(rvest)
library(magrittr)
library(tm)
library(tidyr)
library(dplyr)
library(data.table)

# high-level extraction -------------------------------------------------------

# SET THE DIRECTORY ###################
getwd()
pDir <- "~/GitHub/ag-Product/2014/2014-02-01"
setwd(pDir)

# initialize ------------------------------------------------------------------

pList <- list.files(path = pDir, pattern = ".html", all.files = T, recursive = T)
p <- data.frame()

# extract ---------------------------------------------------------------------

system.time(
  for (i in 1:length(pList)) {
    pLog <- read_html(pList[i])
    
    pTab <- pLog %>%
      html_nodes("a.gen-user-link ") %>% 
      html_attr("href") %>%
      as.character()
    pTab <- ifelse(length(nchar(pTab)) != 0 , pTab, "none")
      
    pTab <- as.data.frame(pTab)
    pTab$date <- sub(" *\\__.*", "", pList[i])
    pTab$product <- pLog %>%
      html_nodes("title") %>%
      html_text() %>%
      as.character()
    pTab$product <- ifelse(length(nchar(pTab$product)) != 0, pTab$product, "none")
      
    pTab$price <- pLog %>%
      html_nodes(".product-page-price") %>%
      html_text() %>%
      as.character()
    pTab$price <- ifelse(length(nchar(pTab$price)) != 0, pTab$price, "none")
      
    pTab$cat <- pLog %>%
      html_nodes(".topnav-element a") %>%
      extract2(1) %>%
      html_text() %>%
      as.character()
    pTab$cat <- ifelse(length(nchar(pTab$cat)) != 0, pTab$cat, "none")
      
    pTab$feedback <- pLog%>%
      html_nodes(".embedded-feedback-list") %>%
      html_text() %>%
      as.character()
    pTab$feedback <- ifelse(length(nchar(pTab$feedback)) != 0, pTab$feedback, "none")
      
    pTab$shipping <- pLog %>%
      html_nodes(".product-page-ships") %>%
      html_text() %>%
      as.character()
    pTab$shipping <- ifelse(length(nchar(pTab$shipping)) != 0, pTab$shipping, "none")
    
    pTab$list <- pList[i]
    p <- rbind(p, pTab)
    
  }
)

# lookup files stop the loop:
# pList[14444]  # A3K4weNDTY - 2015-06-11

# SET THE FILENAME ####################################
write.csv(p, file = "p-0214.01-raw.csv", row.names = F)

# cleanse extracted -----------------------------------------------------------

# column order and names
p <- p[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(p) <- c("list", "date", "vendor", "product", 
                 "price", "cat", "feedback", "shipping")

# vendor and price columns
p$vendor <- gsub("/vendor/", "", p$vendor)
p$vendor <- gsub("/user/", "", p$vendor)
p$vendor <- gsub("#", "", p$vendor)
p$vendor <- gsub("%7E", "", p$vendor)

p$price <- gsub(" BTC", "", p$price)
p$price <- as.double(p$price)

# be patient
p$feedback <- stripWhitespace(as.character(p$feedback))
p$shipping <- stripWhitespace(as.character(p$shipping))

# split 'shipping' column into 'from' and 'to'
# this info was  stored in one string within tags in html.
# check separator before running

levels(as.factor(p$shipping))

p <- separate(p, shipping, c("from", "to"), sep = "to")
p$from <- gsub("From:\\s", "", p$from)
p$from <- gsub("^\\s", "", p$from)
p$from <- gsub("\\s$", "", p$from)
p$from[p$from == ""] <- "No Info"
p$to[is.na(p$to) == T] <- "No Info"
p$to <- gsub("\\s$", "", p$to)
p$to <- gsub("^\\s+", "", p$to)
levels(as.factor(p$to))
levels(as.factor(p$from))

p$from <- gsub("^United\\sStates(.*)", "USA", p$from, ignore.case = T)
p$from <- gsub("^USA(.*)", "USA", p$from, ignore.case = T)
p$from <- gsub("^US(.*)", "USA", p$from, ignore.case = T)
p$from <- gsub("^The home of the Body Bags(.*)", "USA", p$from, ignore.case = T)
p$from <- gsub("^The United Snakes of Captivity(.*)", "USA", p$from, ignore.case = T)
p$from <- gsub("\\bLa\\sJolla\\b", "USA", p$from, ignore.case = T)
p$from <- gsub("\\bPacific\\sPalasades\\b", "USA", p$from, ignore.case = T)
p$from <- gsub("\\bPacific\\sPalisades\\b", "USA", p$from, ignore.case = T)
p$from <- gsub("\\bU.S.A.\\b", "USA", p$from, ignore.case = T)
p$from <- gsub("^West\\sof(.*)", "USA", p$from, ignore.case = T)
p$from <- gsub("^the\\sloins(.*)", "USA", p$from, ignore.case = T)
p$from <- gsub("^George\\swashington(.*)", "USA", p$from, ignore.case = T)
levels(as.factor(p$from))

p$from <- gsub("^Untied(.*)", "UK", p$from, ignore.case = T)
p$from <- gsub("^UK(.*)", "UK", p$from, ignore.case = T)
p$from <- gsub("\\bUnited\\sKingdom\\b", "UK", p$from, ignore.case = T)
p$from <- gsub("^China(.*)", "China", p$from, ignore.case = T)
p$from <- gsub("^Germany(.*)", "Germany", p$from, ignore.case = T)
p$from <- gsub("\\bWorld(.*)", "Worldwide", p$from, ignore.case = T)
p$from <- gsub("\\bShipping\\b", "Worldwide", p$from, ignore.case = T)
p$from <- gsub("^Unde(.*)", "Undeclared", p$from, ignore.case = T)
p$from <- gsub("\\bEurope\\b", "EU", p$from, ignore.case = T)
p$from <- gsub("\\bThe\\sNetherlands\\b", "Netherlands", p$from, ignore.case = T)
levels(as.factor(p$from))

p$from <- gsub("^Agora(.*)", "Agora", p$from, ignore.case = T)
p$from <- gsub("^my(.*)", "Internet", p$from, ignore.case = T)
p$from <- gsub("^me(.*)", "Internet", p$from, ignore.case = T)
p$from <- gsub("\\btorland\\b", "Torland", p$from, ignore.case = T)
p$from <- gsub("\\bCheqdropz\\b", "Czech Republic", p$from, ignore.case = T)
p$from <- gsub("Earth(.*)", "Earth", p$from, ignore.case = T)
p$from <- gsub("\\bMother\\sEarth\\b", "Earth", p$from, ignore.case = T)
levels(as.factor(p$from))

p$from <- gsub("^Ships", "", p$from, ignore.case = T)
p$from <- gsub("^\\sfrom(.*)", "", p$from, ignore.case = T)
p$from <- gsub("To:(.*)", "", p$from, ignore.case = T)

p$from <- gsub("\\s+$", "", p$from, ignore.case = T)
p$from[p$from == ""] <- "No Info"

levels(as.factor(p$from))

# SET THE FILENAME ################################
# SET THE FILENAME ################################
# SET THE FILENAME ################################
write.csv(p, file = "p0214.01-c1.csv", row.names = F)
