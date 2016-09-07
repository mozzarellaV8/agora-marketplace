# Agora Marketplace Analysis
# product info extraction

# load ------------------------------------------------------------------------

library(rvest)
library(magrittr)
library(tm)
library(tidyr)
library(dplyr)

# high-level extraction -------------------------------------------------------

# CHECK THE DIRECTORY
getwd()
pDir <- "~/GitHub/ag-Product/2015-03-03"
setwd(pDir)

# 24811
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

# pList[8215]  # N6Rf86kh7z - 2015-02-14
# pList[15831] # by3FNArKME - 2015-02-14
# pList[17438] # kVPcCCi1F3 - 2015-02-14
# pList[12971] # F5n4fc1XNr - 2015-03-02
# pList[13877] # k32NPhzF1B - 2015-03-03
# pList[19091] # UxxR1bU6xy - 2015-03-03
# pList[20725] # XYvJbgrEnx - 2015-03-03

# CHECK THE FILENAME
write.csv(p, file = "p-0315-03-raw.csv", row.names = F)
p <- read.csv("p-0315-03-raw.csv")

# clean extracted data --------------------------------------------------------
p <- p[c(8, 2, 1, 3, 4, 5, 6, 7)]
colnames(p) <- c("list", "date", "vendor", "product", 
                        "price", "cat", "feedback", "shipping")

p$vendor <- gsub("/vendor/", "", p$vendor)
p$vendor <- gsub("/user/", "", p$vendor)
p$vendor <- gsub("#", "", p$vendor)
p$vendor <- gsub("%7E", "", p$vendor)

p$price <- gsub(" BTC", "", p$price)
p$price <- as.double(p$price)

# be patient
p$feedback <- stripWhitespace(as.character(p$feedback))

p$shipping <- as.character(p$shipping)
p$shipping <- stripWhitespace(p$shipping)
p$shipping[p$shipping == " "] <- "No Info"
p$shipping[p$shipping == ""] <- "No Info"

# check separator before running
p <- separate(p, shipping, c("from", "to"), sep = "To: ")
p$from <- gsub("From:\\s", "", p$from)
p$from <- gsub("^\\s", "", p$from)
p$from <- gsub("\\s$", "", p$from)

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

levels(as.factor(p$from))
p$from <- gsub("^Agora(.*)", "Agora", p$from, ignore.case = T)
p$from <- gsub("^my(.*)", "Internet", p$from, ignore.case = T)
p$from <- gsub("^me(.*)", "Internet", p$from, ignore.case = T)
p$from <- gsub("\\btorland\\b", "Torland", p$from, ignore.case = T)

p$from <- gsub("\\bCheqdropz\\b", "Czech Republic", p$from, ignore.case = T)
p$from <- gsub("Earth(.*)", "Earth", p$from, ignore.case = T)
p$from <- gsub("\\bMother\\sEarth\\b", "Earth", p$from, ignore.case = T)

levels(as.factor(p$from))
levels(as.factor(p$to))

# CHECK THE FILENAME
write.csv(p, file = "p0315.03-c1.csv", row.names = F)
