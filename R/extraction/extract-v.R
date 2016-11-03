# Agora Marketplace Analysis
# extract vendor names and listings

# load ------------------------------------------------------------------------

# pkg <- c("rvest", "magrittr", "tm", "tidyr", "ggplot2", "dplyr", "plyr")
# install.packages(pkg)

library(data.table)
library(rvest)
library(magrittr)
library(tm)
library(tidyr)

##########################
# set vDir and write.csv for each separate folder, each extraction.
##########################

getwd()
vDir <- "~/GitHub/ag-Vendor/2015-01-d1"
setwd(vDir)

# extract vendor products via table, by month ---------------------------------

vList <- list.files(path = vDir, pattern = ".html", all.files = T, recursive = T)
v <- data.frame()

system.time(
  for (i in 1:length(vList)) {
    log <- read_html(vList[i])
    
    vTab <- log %>%
      html_nodes("table.products-list") %>%
      html_table(header = T, fill = T)
    
    vTab <- as.data.frame(vTab)
    vTab$date <- sub(" *\\__.*", "", vList[i])
    vTab$list <- vList[i]
    
    vTab$vendor <- log %>%
      html_nodes("#middlestuff strong") %>%
      extract2(1) %>%
      html_text()
    
    vTab$product <- log %>%
      html_nodes("#product-list a") %>%
      html_text()
    
    vTab$feedback <- log %>%
      html_nodes(".embedded-feedback-list") %>%
      html_text(trim = T)
    
    v <- rbind(v, vTab)
  }
)

nrow(v)/length(vList)

# CHECK THE FILENAME ############################################################
write.csv(v, file = "~/GitHub/agora-data/03-vendor/v-2014-12-d1.csv", row.names = F)
# v <- fread("~/GitHub/agora-data/03-vendor/v-2014-12-d1.csv")
# v <- as.data.frame(v)

nrow(v)/length(vList) #  18.81415

# clean extracted dataframe ---------------------------------------------------
v$Var.1 <- NULL
v <- v[c(5, 4, 6, 2, 7, 1, 8, 3)]
colnames(v) <- c("list", "date", "vendor", "price", "product", "description", "feedback", "shipping")

# be patient here
v$description <- stripWhitespace(v$description)
v$feedback <- stripWhitespace(v$feedback)

v$price <- gsub("\\sBTC", "", v$price)
v$price <- as.double(v$price)

v <- separate(v, shipping, into = c("from", "to"), sep = "To:")
levels(as.factor(v$from))
levels(as.factor(v$to))

v$to <- stripWhitespace(v$to)
v$to[is.na(v$to)] <- "No Info"
v$to <- gsub("^\\s", "", v$to)

levels(as.factor(v$from))
v$from <- stripWhitespace(v$from)
v$from <- gsub("\\s$", "", v$from)
v$from <- gsub("^From:\\s", "", v$from)
v$from[v$from == ""] <- "No Info"

levels(as.factor(v$from))
v$from <- gsub("^United\\sStates(.*)", "USA", v$from, ignore.case = T)
v$from <- gsub("^USA(.*)", "USA", v$from, ignore.case = T)
v$from <- gsub("^US(.*)", "USA", v$from, ignore.case = T)
v$from <- gsub("^The home of the Body Bags(.*)", "USA", v$from, ignore.case = T)
v$from <- gsub("^The United Snakes of Captivity(.*)", "USA", v$from, ignore.case = T)
v$from <- gsub("\\bLa\\sJolla\\b", "USA", v$from, ignore.case = T)
v$from <- gsub("\\bPacific\\sPalasades\\b", "USA", v$from, ignore.case = T)
v$from <- gsub("\\bPacific\\sPalisades\\b", "USA", v$from, ignore.case = T)
v$from <- gsub("\\bU.S.A.\\b", "USA", v$from, ignore.case = T)
v$from <- gsub("^West\\sof(.*)", "USA", v$from, ignore.case = T)
v$from <- gsub("^the\\sloins(.*)", "USA", v$from, ignore.case = T)
v$from <- gsub("^George\\swashington(.*)", "USA", v$from, ignore.case = T)

levels(as.factor(v$from))
v$from <- gsub("^Untied(.*)", "UK", v$from, ignore.case = T)
v$from <- gsub("^UK(.*)", "UK", v$from, ignore.case = T)
v$from <- gsub("\\bUnited\\sKingdom\\b", "UK", v$from, ignore.case = T)
v$from <- gsub("^China(.*)", "China", v$from, ignore.case = T)
v$from <- gsub("^Germany(.*)", "Germany", v$from, ignore.case = T)
v$from <- gsub("\\bMoldova,\\sRepublic\\sof", "Moldova", v$from, ignore.case = T)
v$from <- gsub("\\bWorld(.*)", "Worldwide", v$from, ignore.case = T)
v$from <- gsub("\\bShipping\\b", "Worldwide", v$from, ignore.case = T)
v$from <- gsub("^Unde(.*)", "Undeclared", v$from, ignore.case = T)

levels(as.factor(v$from))
v$from <- gsub("^Agora(.*)", "Agora", v$from, ignore.case = T)
v$from <- gsub("^my(.*)", "Internet", v$from, ignore.case = T)
v$from <- gsub("^me(.*)", "Internet", v$from, ignore.case = T)
v$from <- gsub("\\btorland\\b", "Torland", v$from, ignore.case = T)
v$from <- gsub("\\bCheqdropz\\b", "Czech Republic", v$from, ignore.case = T)
v$from <- gsub("Earth(.*)", "Earth", v$from, ignore.case = T)
v$from <- gsub("\\bMother\\sEarth\\b", "Earth", v$from, ignore.case = T)

levels(as.factor(v$from))
levels(as.factor(v$to))

# v$from <- gsub("\\bTorland\\b", "Agora/Internet/Torland", v$from, ignore.case = T)

# CHECK THE FILENAME ############################################################
write.csv(v, file = "~/GitHub/agora-data/03-vendor/v-2014-12-d1.csv", row.names = F)
test <- fread("~/GitHub/agora-data/03-vendor/v-2014-12-d1.csv")

