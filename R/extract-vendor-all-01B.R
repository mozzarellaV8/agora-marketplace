# Agora Marketplace Analysis
# extract vendor names and listings

# load ------------------------------------------------------------------------

rm(list = ls())

# moved all html files into one folder bc for loop isn't catching everything.
# the loop usually stops on 'date' - but could it be the shipping NAs that jam it?

pkg <- c("rvest", "magrittr", "tm", "tidyr", "ggplot2", "dplyr", "plyr")
install.packages(pkg)

library(rvest)
library(magrittr)
library(tm)
library(tidyr)

# extract vendor products via table, by month ---------------------------------

##
# January 2014 ----------------------------------------------------------------
##

# list of files to extract from by MONTH
jan2014Dir <- "~/GitHub/ag-Vendor/2014-01"
setwd(jan2014Dir)

vlist <- list.files(path = jan2014Dir, pattern = ".html", all.files = T, recursive = T)
vendorall <- data.frame()

system.time(
for (i in 1:length(vlist)) {
  log <- read_html(vlist[i])
  
  pTab <- log %>%
    html_nodes("table.products-list") %>%
    html_table(header = T)
  
  pTab <- as.data.frame(pTab)
  pTab$date <- sub(" *\\__.*", "", vlist[i])
  
  pTab$vendor <- log %>%
    html_nodes("#middlestuff strong") %>%
    extract2(1) %>%
    html_text()
  
  vendorall <- rbind(vendorall, pTab)
}
)

#    user  system elapsed 
#  18.115   0.098  18.211 

# 8533 obs. of 6 variables

# 0114 clean extracted dataframe ----------------------------------------------

vendorall$Var.1 <- NULL
vendorall <- vendorall[c(4, 5, 1, 2, 3)]
colnames(vendorall) <- c("date", "vendor", "name", "price", "shipping")

vendorall$vendor <- as.factor(vendorall$vendor)

vendorall <- separate(vendorall, shipping, into = c("from", "to"))
vendorall$from <- as.factor(vendorall$from)
vendorall$to <- as.factor(vendorall$to)

vendorall$name <- stripWhitespace(vendorall$name)
vendorall$name <- gsub(",", " ", vendorall$name)
vendorall$name <- as.factor(vendorall$name)

vendorall$price <- gsub(" BTC", "", vendorall$price)
vendorall$price <- as.double(vendorall$price)

write.csv(vendorall, file = "vendorall-2014-01.csv", row.names = F)
test <- read.csv("vendorall-2014-01.csv")

##
# February 2014 ---------------------------------------------------------------
##

rm(list = ls())

# list of files to extract from by MONTH
feb2014Dir <- "~/GitHub/ag-Vendor/2014-02"
setwd(feb2014Dir)

vlist <- list.files(path = feb2014Dir, pattern = ".html", all.files = T, recursive = T)
# 2440 files
vendorall_0214 <- data.frame()

system.time(
  for (i in 1:length(vlist)) {
    log <- read_html(vlist[i])
    
    pTab <- log %>%
      html_nodes("table.products-list") %>%
      html_table(header = T)
    
    pTab <- as.data.frame(pTab)
    pTab$date <- sub(" *\\__.*", "", vlist[i])
    
    pTab$vendor <- log %>%
      html_nodes("#middlestuff strong") %>%
      extract2(1) %>%
      html_text()
    
    vendorall_0214 <- rbind(vendorall_0214, pTab)
  }
)

#    user  system elapsed 
#  72.346   1.434  73.768 

# 32233 obs. of 6 variables

# 0214 clean extracted dataframe ----------------------------------------------

vendorall_0214$Var.1 <- NULL
vendorall_0214 <- vendorall_0214[c(4, 5, 1, 2, 3)]
colnames(vendorall_0214) <- c("date", "vendor", "name", "price", "shipping")

vendorall_0214$vendor <- as.factor(vendorall_0214$vendor)

vendorall_0214 <- separate(vendorall_0214, shipping, into = c("from", "to"))
vendorall_0214$from <- as.factor(vendorall_0214$from)
vendorall_0214$to <- as.factor(vendorall_0214$to)

vendorall_0214$name <- stripWhitespace(vendorall_0214$name)
vendorall_0214$name <- gsub(",", " ", vendorall_0214$name)
vendorall_0214$name <- as.factor(vendorall_0214$name)

vendorall_0214$price <- gsub(" BTC", "", vendorall_0214$price)
vendorall_0214$price <- as.double(vendorall_0214$price)

write.csv(vendorall_0214, file = "vendorall_0214.csv", row.names = F)
test <- read.csv("vendorall_0214.csv")

##
# March 2014 ---------------------------------------------------------------
##

rm(list = ls())

# list of files to extract from by MONTH
mar2014Dir <- "~/GitHub/ag-Vendor/2014-03"
setwd(mar2014Dir)

vlist <- list.files(path = mar2014Dir, pattern = ".html", all.files = T, recursive = T)
# 1740 files
vendorall_0314 <- data.frame()

system.time(
  for (i in 1:length(vlist)) {
    log <- read_html(vlist[i])
    
    pTab <- log %>%
      html_nodes("table.products-list") %>%
      html_table(header = T)
    
    pTab <- as.data.frame(pTab)
    pTab$date <- sub(" *\\__.*", "", vlist[i])
    
    pTab$vendor <- log %>%
      html_nodes("#middlestuff strong") %>%
      extract2(1) %>%
      html_text()
    
    vendorall_0314 <- rbind(vendorall_0314, pTab)
  }
)

#    user  system elapsed 
#  49.104   0.732  50.163 

# 22407 obs. of 6 variables

# 0314 clean extracted dataframe ----------------------------------------------

vendorall_0314$Var.1 <- NULL
vendorall_0314 <- vendorall_0314[c(4, 5, 1, 2, 3)]
colnames(vendorall_0314) <- c("date", "vendor", "name", "price", "shipping")

vendorall_0314$vendor <- as.factor(vendorall_0314$vendor)

vendorall_0314$shipping <- gsub("From: ", "", vendorall_0314$shipping)
vendorall_0314$shipping <- gsub("To: ", "", vendorall_0314$shipping)
vendorall_0314 <- separate(vendorall_0314, shipping, into = c("from", "to"))
vendorall_0314$from <- as.factor(vendorall_0314$from)
vendorall_0314$to <- as.factor(vendorall_0314$to)
vendorall_0314$from[vendorall_0314$from == ""] <- NA

vendorall_0314$name <- stripWhitespace(vendorall_0314$name)
vendorall_0314$name <- gsub(",", " ", vendorall_0314$name)
vendorall_0314$name <- as.factor(vendorall_0314$name)
# 9065 levels

vendorall_0314$price <- gsub(" BTC", "", vendorall_0314$price)
vendorall_0314$price <- as.double(vendorall_0314$price)

write.csv(vendorall_0314, file = "vendorall_0314.csv", row.names = F)
test <- read.csv("vendorall_0314.csv")

##
# April 2014 ---------------------------------------------------------------
##

rm(list = ls())

# list of files to extract from by MONTH
apr2014Dir <- "~/GitHub/ag-Vendor/2014-04"
setwd(apr2014Dir)

# 1664 files
vlist <- list.files(path = apr2014Dir, pattern = ".html", all.files = T, recursive = T)
vendorall_0414 <- data.frame()

system.time(
  for (i in 1:length(vlist)) {
    log <- read_html(vlist[i])
    
    pTab <- log %>%
      html_nodes("table.products-list") %>%
      html_table(header = T)
    
    pTab <- as.data.frame(pTab)
    pTab$date <- sub(" *\\__.*", "", vlist[i])
    
    pTab$vendor <- log %>%
      html_nodes("#middlestuff strong") %>%
      extract2(1) %>%
      html_text()
    
    vendorall_0414 <- rbind(vendorall_0414, pTab)
  }
)

# loop will stall if page has no product table.
# maybe could add if / else statement in loop, but for now
# check the next listing if so:
tail(vendorall_0414)

#    user  system elapsed 
# 51.036   0.771  52.443

# 27072 obs. of 6 variables

# 0414 clean extracted dataframe ----------------------------------------------

vendorall_0414$Var.1 <- NULL
vendorall_0414 <- vendorall_0414[c(4, 5, 1, 2, 3)]
colnames(vendorall_0414) <- c("date", "vendor", "name", "price", "shipping")

vendorall_0414$vendor <- as.factor(vendorall_0414$vendor)
# 873 levels

vendorall_0414$shipping <- gsub("From: ", "", vendorall_0414$shipping)
vendorall_0414$shipping <- gsub("To: ", "", vendorall_0414$shipping)

vendorall_0414 <- separate(vendorall_0414, shipping, into = c("from", "to"))

vendorall_0414$from <- as.factor(vendorall_0414$from)
vendorall_0414$to <- as.factor(vendorall_0414$to)
vendorall_0414$from[vendorall_0414$from == ""] <- NA

vendorall_0414$name <- stripWhitespace(vendorall_0414$name)
vendorall_0414$name <- gsub(",", " ", vendorall_0414$name)
vendorall_0414$name <- as.factor(vendorall_0414$name)
# 12988 levels

vendorall_0414$price <- gsub(" BTC", "", vendorall_0414$price)
vendorall_0414$price <- as.double(vendorall_0414$price)

write.csv(vendorall_0414, file = "vendorall_0414.csv", row.names = F)
test <- read.csv("vendorall_0414.csv")

##
# May 2014 ---------------------------------------------------------------
##

rm(list = ls())

# list of files to extract from by MONTH
may2014Dir <- "~/GitHub/ag-Vendor/2014-05"
setwd(may2014Dir)

# 3026 files
vlist <- list.files(path = may2014Dir, pattern = ".html", all.files = T, recursive = T)
vendorall_0514 <- data.frame()

system.time(
  for (i in 1:length(vlist)) {
    log <- read_html(vlist[i])
    
    pTab <- log %>%
      html_nodes("table.products-list") %>%
      html_table(header = T, fill = T)
    
    pTab <- as.data.frame(pTab)
    pTab$date <- sub(" *\\__.*", "", vlist[i])
    
    pTab$vendor <- log %>%
      html_nodes("#middlestuff strong") %>%
      extract2(1) %>%
      html_text()
    
    vendorall_0514 <- rbind(vendorall_0514, pTab)
  }
)

# loop will stall if page has no product table.
# maybe could add if / else statement in loop, but for now
# check the next listing if so:
tail(vendorall_0514)

#    user  system elapsed 
# 111.446   3.949 117.592

# 54329 obs. of 6 variables

# 0514 clean extracted dataframe ----------------------------------------------

vendorall_0514$Var.1 <- NULL
vendorall_0514 <- vendorall_0514[c(4, 5, 1, 2, 3)]
colnames(vendorall_0514) <- c("date", "vendor", "name", "price", "shipping")

vendorall_0514$vendor <- as.factor(vendorall_0514$vendor)
# 1026 levels

vendorall_0514$shipping <- gsub("From: ", "", vendorall_0514$shipping)
vendorall_0514$shipping <- gsub("To: ", "", vendorall_0514$shipping)

vendorall_0514 <- separate(vendorall_0514, shipping, into = c("from", "to"))
vendorall_0514$from[vendorall_0514$from == ""] <- NA

vendorall_0514$from <- as.factor(vendorall_0514$from)
vendorall_0514$to <- as.factor(vendorall_0514$to)

vendorall_0514$name <- stripWhitespace(vendorall_0514$name)
vendorall_0514$name <- gsub(",", " ", vendorall_0514$name)
vendorall_0514$name <- as.factor(vendorall_0514$name)
# 19941 levels

vendorall_0514$price <- gsub(" BTC", "", vendorall_0514$price)
vendorall_0514$price <- as.double(vendorall_0514$price)

write.csv(vendorall_0514, file = "vendorall_0514.csv", row.names = F)
test <- read.csv("vendorall_0514.csv")


##
# June 2014 ---------------------------------------------------------------
##

rm(list = ls())

# list of files to extract from by MONTH
june2014Dir <- "~/GitHub/ag-Vendor/2014-06"
setwd(june2014Dir)

# 1944 files
vlist <- list.files(path = june2014Dir, pattern = ".html", all.files = T, recursive = T)
vendorall_0614 <- data.frame()

system.time(
  for (i in 1:length(vlist)) {
    log <- read_html(vlist[i])
    
    pTab <- log %>%
      html_nodes("table.products-list") %>%
      html_table(header = T, fill = T)
    
    pTab <- as.data.frame(pTab)
    pTab$date <- sub(" *\\__.*", "", vlist[i])
    
    pTab$vendor <- log %>%
      html_nodes("#middlestuff strong") %>%
      extract2(1) %>%
      html_text()
    
    vendorall_0614 <- rbind(vendorall_0614, pTab)
  }
)

# if loop stalls:
tail(vendorall_0614)

#    user  system elapsed 
#  69.841   1.935  72.122

# 44694 obs. of 6 variables

# 0614 clean extracted dataframe ----------------------------------------------

vendorall_0614$Var.1 <- NULL
vendorall_0614 <- vendorall_0614[c(4, 5, 1, 2, 3)]
colnames(vendorall_0614) <- c("date", "vendor", "name", "price", "shipping")

vendorall_0614$vendor <- as.factor(vendorall_0614$vendor)
# 845 levels

vendorall_0614$shipping <- gsub("From: ", "", vendorall_0614$shipping)
vendorall_0614$shipping <- gsub("To: ", "", vendorall_0614$shipping)

vendorall_0614 <- separate(vendorall_0614, shipping, into = c("from", "to"))
vendorall_0614$from[vendorall_0614$from == ""] <- NA

vendorall_0614$from <- as.factor(vendorall_0614$from) # 29
vendorall_0614$to <- as.factor(vendorall_0614$to) # 83

vendorall_0614$name <- stripWhitespace(vendorall_0614$name)
vendorall_0614$name <- gsub(",", " ", vendorall_0614$name)
vendorall_0614$name <- as.factor(vendorall_0614$name)
# 15593 levels

vendorall_0614$price <- gsub(" BTC", "", vendorall_0614$price)
vendorall_0614$price <- as.double(vendorall_0614$price)

write.csv(vendorall_0614, file = "vendorall_0614.csv", row.names = F)
test <- read.csv("vendorall_0614.csv")

##
# July 2014 ---------------------------------------------------------------
##

rm(list = ls())

# list of files to extract from by MONTH
july2014Dir <- "~/GitHub/ag-Vendor/2014-07"
setwd(july2014Dir)

# 2586 files
vlist <- list.files(path = july2014Dir, pattern = ".html", all.files = T, recursive = T)
vendorall_0714 <- data.frame()

system.time(
  for (i in 1:length(vlist)) {
    log <- read_html(vlist[i])
    
    pTab <- log %>%
      html_nodes("table.products-list") %>%
      html_table(header = T, fill = T)
    
    pTab <- as.data.frame(pTab)
    pTab$date <- sub(" *\\__.*", "", vlist[i])
    
    pTab$vendor <- log %>%
      html_nodes("#middlestuff strong") %>%
      extract2(1) %>%
      html_text()
    
    vendorall_0714 <- rbind(vendorall_0714, pTab)
  }
)

# if loop stalls:
tail(vendorall_0714)

#    user  system elapsed 
#  92.899   3.277  97.266

# 52000 obs. of 6 variables

# 0714 clean extracted dataframe ----------------------------------------------

vendorall_0714$Var.1 <- NULL
vendorall_0714 <- vendorall_0714[c(4, 5, 1, 2, 3)]
colnames(vendorall_0714) <- c("date", "vendor", "name", "price", "shipping")

vendorall_0714$vendor <- as.factor(vendorall_0714$vendor)
# 1038 levels

vendorall_0714$shipping <- gsub("From: ", "", vendorall_0714$shipping)
vendorall_0714$shipping <- gsub("To: ", "", vendorall_0714$shipping)

vendorall_0714 <- separate(vendorall_0714, shipping, into = c("from", "to"))
vendorall_0714$from[vendorall_0714$from == ""] <- NA

vendorall_0714$from <- as.factor(vendorall_0714$from) # 34
vendorall_0714$to <- as.factor(vendorall_0714$to) # 91

vendorall_0714$name <- stripWhitespace(vendorall_0714$name)
vendorall_0714$name <- gsub(",", " ", vendorall_0714$name)
vendorall_0714$name <- as.factor(vendorall_0714$name)
# 17144 levels

vendorall_0714$price <- gsub(" BTC", "", vendorall_0714$price)
vendorall_0714$price <- as.double(vendorall_0714$price)

write.csv(vendorall_0714, file = "vendorall_0714.csv", row.names = F)
test <- read.csv("vendorall_0714.csv")
