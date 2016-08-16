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
  
  pTab$product <- log %>%
    html_nodes("#product-list a") %>%
    html_text()
  
  vendorall <- rbind(vendorall, pTab)
}
)

#     user  system elapsed 
#   27.251   0.239  28.177  

# 8533 obs. of 7 variables

# 0114 clean extracted dataframe ----------------------------------------------

vendorall$Var.1 <- NULL
vendorall <- vendorall[c(4, 5, 6, 1, 2, 3)]
colnames(vendorall) <- c("date", "vendor", "product", "description", "price", "shipping")

vendorall$vendor <- as.factor(vendorall$vendor) #262

vendorall <- separate(vendorall, shipping, into = c("from", "to"))
vendorall$from[vendorall$from == ""] <- NA

vendorall$from <- as.factor(vendorall$from) # 42
vendorall$to <- as.factor(vendorall$to) # 57

vendorall$description <- stripWhitespace(vendorall$description)
vendorall$description <- gsub(",", " ", vendorall$description)
vendorall$description <- as.factor(vendorall$description)
# 3256 levels

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
    
    pTab$product <- log %>%
      html_nodes("#product-list a") %>%
      html_text()
    
    vendorall_0214 <- rbind(vendorall_0214, pTab)
  }
)

#     user  system elapsed 
#  100.802   1.760 106.669 

# 32233 obs. of 7 variables

# 0214 clean extracted dataframe ----------------------------------------------

vendorall_0214$Var.1 <- NULL
vendorall_0214 <- vendorall_0214[c(4, 5, 6, 1, 2, 3)]
colnames(vendorall_0214) <- c("date", "vendor", "product", "description", "price", "shipping")

vendorall_0214$vendor <- as.factor(vendorall_0214$vendor) #735

vendorall_0214 <- separate(vendorall_0214, shipping, into = c("from", "to"), sep = "To:")
vendorall_0214$from <- gsub("From: ", "", vendorall_0214$from)
vendorall_0214$to <- as.factor(vendorall_0214$to)

vendorall_0214$description <- stripWhitespace(vendorall_0214$description)
vendorall_0214$description <- gsub(",", " ", vendorall_0214$description)
vendorall_0214$description <- as.factor(vendorall_0214$description)

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
# 1738 files
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
    
    pTab$product <- log %>%
      html_nodes("#product-list a") %>%
      html_text()
    
    vendorall_0314 <- rbind(vendorall_0314, pTab)
  }
)

#     user  system elapsed 
#   69.734   1.029  74.707

# 22407 obs. of 7 variables

# 0314 clean extracted dataframe ----------------------------------------------

vendorall_0314$Var.1 <- NULL
vendorall_0314 <- vendorall_0314[c(4, 5, 6, 1, 2, 3)]
colnames(vendorall_0314) <- c("date", "vendor", "product", "description", "price", "shipping")

vendorall_0314$vendor <- as.factor(vendorall_0314$vendor) # 702

vendorall_0314 <- separate(vendorall_0314, shipping, into = c("from", "to"), sep = "To: ")
vendorall_0314$from <- gsub("From: ", "", vendorall_0314$from)
vendorall_0314$from[vendorall_0314$from == ""] <- NA

vendorall_0314$from <- as.factor(vendorall_0314$from) # 80
vendorall_0314$to <- as.factor(vendorall_0314$to) # 190

vendorall_0314$description <- stripWhitespace(vendorall_0314$description)
vendorall_0314$description <- gsub(",", " ", vendorall_0314$description)
vendorall_0314$description <- as.factor(vendorall_0314$description)
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
    
    pTab$product <- log %>%
      html_nodes("#product-list a") %>%
      html_text()
    
    vendorall_0414 <- rbind(vendorall_0414, pTab)
  }
)

# loop will stall if page has no product table.
# maybe could add if / else statement in loop, but for now
# check the next listing if so:
tail(vendorall_0414)

#    user  system elapsed 
#  71.204   1.224  75.575

# 27072 obs. of 7 variables

# 0414 clean extracted dataframe ----------------------------------------------

vendorall_0414$Var.1 <- NULL
vendorall_0414 <- vendorall_0414[c(4, 5, 6, 1, 2, 3)]
colnames(vendorall_0414) <- c("date", "vendor", "product", "description", "price", "shipping")

vendorall_0414$vendor <- as.factor(vendorall_0414$vendor)
# 873 levels

vendorall_0414 <- separate(vendorall_0414, shipping, into = c("from", "to"), sep = "To: ")
vendorall_0414$from <- gsub("From: ", "", vendorall_0414$from)
vendorall_0414$from[vendorall_0414$from == ""] <- NA

vendorall_0414$from <- as.factor(vendorall_0414$from) # 89
vendorall_0414$to <- as.factor(vendorall_0414$to) # 266

vendorall_0414$description <- stripWhitespace(vendorall_0414$description)
vendorall_0414$description <- gsub(",", " ", vendorall_0414$description)
vendorall_0414$description <- as.factor(vendorall_0414$description)
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
    
    pTab$product <- log %>%
      html_nodes(".products-list-item a") %>%
      html_text()
    
    vendorall_0514 <- rbind(vendorall_0514, pTab)
  }
)

# loop will stall if page has no product table.
# maybe could add if / else statement in loop, but for now
# check the next listing if so:
tail(vendorall_0514)

#    user  system elapsed 
# 155.798   4.862 170.914 

# 54329 obs. of 6 variables
# vendor 'fake' was removed.

# 0514 clean extracted dataframe ----------------------------------------------

vendorall_0514$Var.1 <- NULL
vendorall_0514 <- vendorall_0514[c(4, 5, 6, 1, 2, 3)]
colnames(vendorall_0514) <- c("date", "vendor", "product", "description", "price", "shipping")

vendorall_0514$vendor <- as.factor(vendorall_0514$vendor)
# 1026 levels

vendorall_0514 <- separate(vendorall_0514, shipping, into = c("from", "to"), sep = "To: ")
vendorall_0514$from <- gsub("From: ", "", vendorall_0514$from)
vendorall_0514$from[vendorall_0514$from == ""] <- NA

vendorall_0514$from <- as.factor(vendorall_0514$from) # 92
vendorall_0514$to <- as.factor(vendorall_0514$to) #342

vendorall_0514$description <- stripWhitespace(vendorall_0514$description)
vendorall_0514$description <- gsub(",", " ", vendorall_0514$description)
vendorall_0514$description <- as.factor(vendorall_0514$description)
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
    
    pTab$product <- log %>%
      html_nodes("#product-list a") %>%
      html_text()
    
    vendorall_0614 <- rbind(vendorall_0614, pTab)
  }
)

# if loop stalls:
tail(vendorall_0614)

#    user  system elapsed 
#  94.487   2.552 100.840 

# 44694 obs. of 7 variables

# 0614 clean extracted dataframe ----------------------------------------------

vendorall_0614$Var.1 <- NULL
vendorall_0614 <- vendorall_0614[c(4, 5, 6, 1, 2, 3)]
colnames(vendorall_0614) <- c("date", "vendor", "product", "description", "price", "shipping")

vendorall_0614$vendor <- as.factor(vendorall_0614$vendor)
# 845 levels

vendorall_0614 <- separate(vendorall_0614, shipping, into = c("from", "to"), sep = "To: ")
vendorall_0614$from <- gsub("From: ", "", vendorall_0614$from)
vendorall_0614$from[vendorall_0614$from == ""] <- NA

vendorall_0614$from <- as.factor(vendorall_0614$from) # 92
vendorall_0614$to <- as.factor(vendorall_0614$to) # 314

vendorall_0614$description <- stripWhitespace(vendorall_0614$description)
vendorall_0614$description <- gsub(",", " ", vendorall_0614$description)
vendorall_0614$description <- as.factor(vendorall_0614$description)
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
    
    pTab$product <- log %>%
      html_nodes("#product-list a") %>%
      html_text()
    
    vendorall_0714 <- rbind(vendorall_0714, pTab)
  }
)

# if loop stalls:
tail(vendorall_0714)

#    user  system elapsed 
# 133.330   3.843 149.718 

# 52000 obs. of 7 variables

# 0714 clean extracted dataframe ----------------------------------------------

vendorall_0714$Var.1 <- NULL
vendorall_0714 <- vendorall_0714[c(4, 5, 6, 1, 2, 3)]
colnames(vendorall_0714) <- c("date", "vendor", "product", "description", "price", "shipping")

vendorall_0714$vendor <- as.factor(vendorall_0714$vendor)
# 1038 levels

vendorall_0714 <- separate(vendorall_0714, shipping, into = c("from", "to"), sep = "To:")
vendorall_0714$from <- gsub("From: ", "", vendorall_0714$from)
vendorall_0714$from[vendorall_0714$from == ""] <- NA

vendorall_0714$from <- as.factor(vendorall_0714$from) # 109
vendorall_0714$to <- as.factor(vendorall_0714$to) # 333

vendorall_0714$description <- stripWhitespace(vendorall_0714$description)
vendorall_0714$description <- gsub(",", " ", vendorall_0714$description)
vendorall_0714$description <- as.factor(vendorall_0714$description)
# 17144 levels

vendorall_0714$price <- gsub(" BTC", "", vendorall_0714$price)
vendorall_0714$price <- as.double(vendorall_0714$price)

write.csv(vendorall_0714, file = "vendorall_0714.csv", row.names = F)
test <- read.csv("vendorall_0714.csv")

##
# Aug 2014 ---------------------------------------------------------------
##
rm(list = ls())
aug2014Dir <- "~/GitHub/ag-Vendor/2014-08"
setwd(aug2014Dir)

# 2685 files
vlist <- list.files(path = aug2014Dir, pattern = ".html", all.files = T, recursive = T)
vendorall_0814 <- data.frame()

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
    
    pTab$product <- log %>%
      html_nodes("#product-list a") %>%
      html_text()
    
    vendorall_0814 <- rbind(vendorall_0814, pTab)
  }
)

# if loop stalls:
tail(vendorall_0814)

#      user  system elapsed 
#   146.786   4.828 155.452 

# 63206 obs. of 6 variables
# rage666666 removed

# 0814 clean extracted dataframe ----------------------------------------------

vendorall_0814$Var.1 <- NULL
vendorall_0814 <- vendorall_0814[c(4, 5, 6, 1, 2, 3)]
colnames(vendorall_0814) <- c("date", "vendor", "product", "description", "price", "shipping")

vendorall_0814$vendor <- as.factor(vendorall_0814$vendor)
# 1087 levels

vendorall_0814 <- separate(vendorall_0814, shipping, into = c("from", "to"), sep = "To:")
vendorall_0814$from <- gsub("From: ", "", vendorall_0814$from)
vendorall_0814$from[vendorall_0814$from == ""] <- NA

vendorall_0814$from <- as.factor(vendorall_0814$from) # 116
vendorall_0814$to <- as.factor(vendorall_0814$to) # 374

vendorall_0814$description <- stripWhitespace(vendorall_0814$description)
vendorall_0814$description <- gsub(",", " ", vendorall_0814$description)
vendorall_0814$description <- as.factor(vendorall_0814$description)
# 19412 levels

vendorall_0814$price <- gsub(" BTC", "", vendorall_0814$price)
vendorall_0814$price <- as.double(vendorall_0814$price)

write.csv(vendorall_0814, file = "vendorall_0814.csv", row.names = F)
test <- read.csv("vendorall_0814.csv")

##
# Sept 2014 ---------------------------------------------------------------
##

rm(list = ls())

# list of files to extract from by MONTH
sept2014Dir <- "~/GitHub/ag-Vendor/2014-09"
setwd(sept2014Dir)

# 4007 files
vlist <- list.files(path = sept2014Dir, pattern = ".html", all.files = T, recursive = T)
vendorall_0914 <- data.frame()

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
    
    pTab$product <- log %>%
      html_nodes("#product-list a") %>%
      html_text()
    
    vendorall_0914 <- rbind(vendorall_0914, pTab)
  }
)

# if loop stalls:
tail(vendorall_0914)

#      user  system elapsed 
#   168.069   7.335 177.533

# 77186 obs. of 7 variables

# 0914 clean extracted dataframe ----------------------------------------------

vendorall_0914$Var.1 <- NULL
vendorall_0914 <- vendorall_0914[c(4, 5, 6, 1, 2, 3)]
colnames(vendorall_0914) <- c("date", "vendor", "product", "description", "price", "shipping")

vendorall_0914$vendor <- as.factor(vendorall_0914$vendor)
# 1175 levels

vendorall_0914 <- separate(vendorall_0914, shipping, into = c("from", "to"), sep = "To:")
vendorall_0914$from <- gsub("From: ", "", vendorall_0914$from)
vendorall_0914$from[vendorall_0914$from == ""] <- NA

vendorall_0914$from <- as.factor(vendorall_0914$from) # 115
vendorall_0914$to <- as.factor(vendorall_0914$to) # 407

vendorall_0914$description <- stripWhitespace(vendorall_0914$description)
vendorall_0914$description <- gsub(",", " ", vendorall_0914$description)
vendorall_0914$description <- as.factor(vendorall_0914$description)
# 20809 levels

vendorall_0914$price <- gsub(" BTC", "", vendorall_0914$price)
vendorall_0914$price <- as.double(vendorall_0914$price)

write.csv(vendorall_0914, file = "vendorall_0914.csv", row.names = F)
test <- read.csv("vendorall_0914.csv")

##
# Oct 2014 01 -----------------------------------------------------------------
##

rm(list = ls())

# list of files to extract from by MONTH
oct2014Dir01 <- "~/GitHub/ag-Vendor/2014-10-01"
setwd(oct2014Dir01)

# 9385 total Oct 2010 files
# 3321 files in round 01
vlist <- list.files(path = oct2014Dir01, pattern = ".html", all.files = T, recursive = T)
vendorall_1014_01 <- data.frame()

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
    
    pTab$product <- log %>%
      html_nodes("#product-list a") %>%
      html_text()
    
    vendorall_1014_01 <- rbind(vendorall_1014_01, pTab)
  }
)

# if loop stalls:
tail(vendorall_1014_01)

#      user  system elapsed 
#   126.232   5.593 131.912 

# 64647 obs. of 6 variables

# 1014 clean extracted dataframe ----------------------------------------------

vendorall_1014_01$Var.1 <- NULL
vendorall_1014_01 <- vendorall_1014_01[c(4, 5, 1, 2, 3)]
colnames(vendorall_1014_01) <- c("date", "vendor", "product", "description", "price", "shipping")

vendorall_1014_01$vendor <- as.factor(vendorall_1014_01$vendor)
# 1143 levels

vendorall_1014_01 <- separate(vendorall_1014_01, shipping, into = c("from", "to"), sep = "To:")
vendorall_1014_01$from <- gsub("From: ", "", vendorall_1014_01$from)
vendorall_1014_01$from[vendorall_1014_01$from == ""] <- NA

vendorall_1014_01$from <- as.factor(vendorall_1014_01$from) # 111
vendorall_1014_01$to <- as.factor(vendorall_1014_01$to) # 379

vendorall_1014_01$name <- stripWhitespace(vendorall_1014_01$name)
vendorall_1014_01$name <- gsub(",", " ", vendorall_1014_01$name)
vendorall_1014_01$name <- as.factor(vendorall_1014_01$name)
# 19601 levels

vendorall_1014_01$price <- gsub(" BTC", "", vendorall_1014_01$price)
vendorall_1014_01$price <- as.double(vendorall_1014_01$price)

write.csv(vendorall_1014_01, file = "vendorall_1014_01.csv", row.names = F)
test <- read.csv("vendorall_1014_01.csv")

##
# Oct 2014 02 -----------------------------------------------------------------
##

rm(list = ls())

# list of files to extract from by MONTH
oct2014Dir02 <- "~/GitHub/ag-Vendor/2014-10-02"
setwd(oct2014Dir02)

# 9385 total Oct 2010 files
# 3032 files in round 01
vlist <- list.files(path = oct2014Dir02, pattern = ".html", all.files = T, recursive = T)
vendorall_1014_02 <- data.frame()

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
    
    pTab$product <- log %>%
      html_nodes("#product-list a") %>%
      html_text()
    
    vendorall_1014_02 <- rbind(vendorall_1014_02, pTab)
  }
)

# if loop stalls:
tail(vendorall_1014_02)

#      user  system elapsed 
#   114.655   4.103 119.198 

# 50946 obs. of 6 variables

# 1014-02 clean extracted dataframe -------------------------------------------

vendorall_1014_02$Var.1 <- NULL
vendorall_1014_02 <- vendorall_1014_02[c(4, 5, 1, 2, 3)]
colnames(vendorall_1014_02) <- c("date", "vendor", "product", "description", "price", "shipping")

vendorall_1014_02$vendor <- as.factor(vendorall_1014_02$vendor)
# 1158 levels

vendorall_1014_02 <- separate(vendorall_1014_02, shipping, into = c("from", "to"), sep = "To:")
vendorall_1014_02$from <- gsub("From: ", "", vendorall_1014_02$from)
vendorall_1014_02$from[vendorall_1014_02$from == ""] <- NA

vendorall_1014_02$from <- as.factor(vendorall_1014_02$from) # 103
vendorall_1014_02$to <- as.factor(vendorall_1014_02$to) # 386

vendorall_1014_02$name <- stripWhitespace(vendorall_1014_02$name)
vendorall_1014_02$name <- gsub(",", " ", vendorall_1014_02$name)
vendorall_1014_02$name <- as.factor(vendorall_1014_02$name)
# 18795 levels

vendorall_1014_02$price <- gsub(" BTC", "", vendorall_1014_02$price)
vendorall_1014_02$price <- as.double(vendorall_1014_02$price)

write.csv(vendorall_1014_02, file = "vendorall_1014_02.csv", row.names = F)
test <- read.csv("vendorall_1014_02.csv")

##
# Oct 2014 03 -----------------------------------------------------------------
##

rm(list = ls())

# list of files to extract from by MONTH
oct2014Dir03 <- "~/GitHub/ag-Vendor/2014-10-03"
setwd(oct2014Dir03)

# 9385 total Oct 2014 files
# 3032 files in round 01
vlist <- list.files(path = oct2014Dir03, pattern = ".html", all.files = T, recursive = T)
vendorall_1014_03 <- data.frame()

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
    
    pTab$product <- log %>%
      html_nodes("#product-list a") %>%
      html_text()
    
    vendorall_1014_03 <- rbind(vendorall_1014_03, pTab)
  }
)

# if loop stalls:
tail(vendorall_1014_03)

#      user  system elapsed 
#   111.163   3.948 115.566  

# 54110 obs. of 6 variables

# 1014-03 clean extracted dataframe -------------------------------------------

vendorall_1014_03$Var.1 <- NULL
vendorall_1014_03 <- vendorall_1014_03[c(4, 5, 1, 2, 3)]
colnames(vendorall_1014_03) <- c("date", "vendor", "product", "description", "price", "shipping")

vendorall_1014_03$vendor <- as.factor(vendorall_1014_03$vendor)
# 1152 levels

vendorall_1014_03 <- separate(vendorall_1014_03, shipping, into = c("from", "to"), sep = "To:")
vendorall_1014_03$from <- gsub("From: ", "", vendorall_1014_03$from)
vendorall_1014_03$from[vendorall_1014_03$from == ""] <- NA

vendorall_1014_03$from <- as.factor(vendorall_1014_03$from) # 104
vendorall_1014_03$to <- as.factor(vendorall_1014_03$to) # 394

vendorall_1014_03$name <- stripWhitespace(vendorall_1014_03$name)
vendorall_1014_03$name <- gsub(",", " ", vendorall_1014_03$name)
vendorall_1014_03$name <- as.factor(vendorall_1014_03$name)
# 19091 levels

vendorall_1014_03$price <- gsub(" BTC", "", vendorall_1014_03$price)
vendorall_1014_03$price <- as.double(vendorall_1014_03$price)

write.csv(vendorall_1014_03, file = "vendorall_1014_03.csv", row.names = F)
test <- read.csv("vendorall_1014_03.csv")

##
# Nov 2014 01 -----------------------------------------------------------------
##

rm(list = ls())

# list of files to extract from by MONTH
nov2014Dir01 <- "~/GitHub/ag-Vendor/2014-11-d1"
setwd(nov2014Dir01)

# 9385 total Nov 2014 files
# 4579 files in round 01
vlist <- list.files(path = nov2014Dir01, pattern = ".html", all.files = T, recursive = T)
vendorall_1114_01 <- data.frame()

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
    
    pTab$product <- log %>%
      html_nodes("#product-list a") %>%
      html_text()
    
    vendorall_1114_01 <- rbind(vendorall_1114_01, pTab)
  }
)

# if loop stalls:
tail(vendorall_1114_01)

#      user  system elapsed 
#   189.144   8.774 197.881 

# 86150 obs. of 6 variables

# 1114-01 clean extracted dataframe -------------------------------------------

vendorall_1114_01$Var.1 <- NULL
vendorall_1114_01 <- vendorall_1114_01[c(4, 5, 1, 2, 3)]
colnames(vendorall_1114_01) <- c("date", "vendor", "product", "description", "price", "shipping")

vendorall_1114_01$vendor <- as.factor(vendorall_1114_01$vendor)
# 1225 levels

vendorall_1114_01 <- separate(vendorall_1114_01, shipping, into = c("from", "to"), sep = "To:")
vendorall_1114_01$from <- gsub("From: ", "", vendorall_1114_01$from)
vendorall_1114_01$from[vendorall_1114_01$from == ""] <- NA

vendorall_1114_01$from <- as.factor(vendorall_1114_01$from) # 101
vendorall_1114_01$to <- as.factor(vendorall_1114_01$to) # 415

vendorall_1114_01$name <- stripWhitespace(vendorall_1114_01$name)
vendorall_1114_01$name <- gsub(",", " ", vendorall_1114_01$name)
vendorall_1114_01$name <- as.factor(vendorall_1114_01$name)
# 20778 levels

vendorall_1114_01$price <- gsub(" BTC", "", vendorall_1114_01$price)
vendorall_1114_01$price <- as.double(vendorall_1114_01$price)

write.csv(vendorall_1114_01, file = "vendorall_1114_01.csv", row.names = F)
test <- read.csv("vendorall_1114_01.csv")

##
# Nov 2014 02 -----------------------------------------------------------------
##

rm(list = ls())

# list of files to extract from by MONTH
nov2014Dir02 <- "~/GitHub/ag-Vendor/2014-11-d2"
setwd(nov2014Dir02)

# 9385 total Nov 2014 files
# 4479 files in round 02
vlist <- list.files(path = nov2014Dir02, pattern = ".html", all.files = T, recursive = T)
vendorall_1114_02 <- data.frame()

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
    
    pTab$product <- log %>%
      html_nodes("#product-list a") %>%
      html_text()
    
    vendorall_1114_02 <- rbind(vendorall_1114_02, pTab)
  }
)

# if loop stalls:
tail(vendorall_1114_02)

#      user  system elapsed 
#   211.553   8.967 220.475  

# 100290 obs. of 6 variables

# 1114-02 clean extracted dataframe -------------------------------------------

vendorall_1114_02$Var.1 <- NULL
vendorall_1114_02 <- vendorall_1114_02[c(4, 5, 1, 2, 3)]
colnames(vendorall_1114_02) <- c("date", "vendor", "product", "description", "price", "shipping")

vendorall_1114_02$vendor <- as.factor(vendorall_1114_02$vendor)
# 1056 levels

vendorall_1114_02 <- separate(vendorall_1114_02, shipping, into = c("from", "to"), sep = "To:")
vendorall_1114_02$from <- gsub("From: ", "", vendorall_1114_02$from)
vendorall_1114_02$from[vendorall_1114_02$from == ""] <- NA

vendorall_1114_02$from <- as.factor(vendorall_1114_02$from) # 101
vendorall_1114_02$to <- as.factor(vendorall_1114_02$to) # 379

vendorall_1114_02$name <- stripWhitespace(vendorall_1114_02$name)
vendorall_1114_02$name <- gsub(",", " ", vendorall_1114_02$name)
vendorall_1114_02$name <- as.factor(vendorall_1114_02$name)
# 21224 levels

vendorall_1114_02$price <- gsub(" BTC", "", vendorall_1114_02$price)
vendorall_1114_02$price <- as.double(vendorall_1114_02$price)

write.csv(vendorall_1114_02, file = "vendorall_1114_02.csv", row.names = F)
test <- read.csv("vendorall_1114_02.csv")

##
# Nov 2014 03 -----------------------------------------------------------------
##

rm(list = ls())

# list of files to extract from by MONTH
nov2014Dir03 <- "~/GitHub/ag-Vendor/2014-11-d3"
setwd(nov2014Dir03)

# 9385 total Nov 2014 files
# 4783 files in round 03
vlist <- list.files(path = nov2014Dir03, pattern = ".html", all.files = T, recursive = T)
vendorall_1114_03 <- data.frame()

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
    
    pTab$product <- log %>%
      html_nodes("#product-list a") %>%
      html_text()
    
    vendorall_1114_03 <- rbind(vendorall_1114_03, pTab)
  }
)

# if loop stalls:
tail(vendorall_1114_03)

#      user  system elapsed 
#   247.868  11.645 260.624  

# 125311 obs. of 6 variables

# 1114-03 clean extracted dataframe -------------------------------------------

vendorall_1114_03$Var.1 <- NULL
vendorall_1114_03 <- vendorall_1114_03[c(4, 5, 1, 2, 3)]
colnames(vendorall_1114_03) <- c("date", "vendor", "product", "description", "price", "shipping")

vendorall_1114_03$vendor <- as.factor(vendorall_1114_03$vendor)
# 1073 levels

vendorall_1114_03 <- separate(vendorall_1114_03, shipping, into = c("from", "to"), sep = "To:")
vendorall_1114_03$from <- gsub("From: ", "", vendorall_1114_03$from)
vendorall_1114_03$from[vendorall_1114_03$from == ""] <- NA

vendorall_1114_03$from <- as.factor(vendorall_1114_03$from) # 99
vendorall_1114_03$to <- as.factor(vendorall_1114_03$to) # 369

vendorall_1114_03$name <- stripWhitespace(vendorall_1114_03$name)
vendorall_1114_03$name <- gsub(",", " ", vendorall_1114_03$name)
vendorall_1114_03$name <- as.factor(vendorall_1114_03$name)
# 21123 levels

vendorall_1114_03$price <- gsub(" BTC", "", vendorall_1114_03$price)
vendorall_1114_03$price <- as.double(vendorall_1114_03$price)

write.csv(vendorall_1114_03, file = "vendorall_1114_03.csv", row.names = F)
test <- read.csv("vendorall_1114_03.csv")

##
# Dec 2014 01 -----------------------------------------------------------------
##

rm(list = ls())

# list of files to extract from by MONTH
dec2014Dir01 <- "~/GitHub/ag-Vendor/2014-12-d1"
setwd(dec2014Dir01)

# 11577 total Dec 2014 files
# 3782 files in round 01
vlist <- list.files(path = dec2014Dir01, pattern = ".html", all.files = T, recursive = T)
vendorall_1214_01 <- data.frame()

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
    
    pTab$product <- log %>%
      html_nodes("#product-list a") %>%
      html_text()
    
    vendorall_1214_01 <- rbind(vendorall_1214_01, pTab)
  }
)

# when loop stalls:
tail(vendorall_1214_01)

#      user  system elapsed 
#   170.908   6.786 177.820   

# 86093 obs. of 6 variables

# 1214-01 clean extracted dataframe -------------------------------------------

vendorall_1214_01$Var.1 <- NULL
vendorall_1214_01 <- vendorall_1214_01[c(4, 5, 1, 2, 3)]
colnames(vendorall_1214_01) <- c("date", "vendor", "product", "description", "price", "shipping")

vendorall_1214_01$vendor <- as.factor(vendorall_1214_01$vendor)
# 1052 levels

vendorall_1214_01 <- separate(vendorall_1214_01, shipping, into = c("from", "to"), sep = "To:")
vendorall_1214_01$from <- gsub("From: ", "", vendorall_1214_01$from)
vendorall_1214_01$from[vendorall_1214_01$from == ""] <- NA

vendorall_1214_01$from <- as.factor(vendorall_1214_01$from) # 106
vendorall_1214_01$to <- as.factor(vendorall_1214_01$to) # 375

vendorall_1214_01$name <- stripWhitespace(vendorall_1214_01$name)
vendorall_1214_01$name <- gsub(",", " ", vendorall_1214_01$name)
vendorall_1214_01$name <- as.factor(vendorall_1214_01$name)
# 20969 levels

vendorall_1214_01$price <- gsub(" BTC", "", vendorall_1214_01$price)
vendorall_1214_01$price <- as.double(vendorall_1214_01$price)

write.csv(vendorall_1214_01, file = "vendorall_1214_01.csv", row.names = F)
test <- read.csv("vendorall_1214_01.csv")

##
# Dec 2014 02 -----------------------------------------------------------------
##

rm(list = ls())

# list of files to extract from by MONTH
dec2014Dir02 <- "~/GitHub/ag-Vendor/2014-12-d2"
setwd(dec2014Dir02)

# 11577 total Dec 2014 files
# 3645 files in round 01
vlist <- list.files(path = dec2014Dir02, pattern = ".html", all.files = T, recursive = T)
vendorall_1214_02 <- data.frame()

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
    
    pTab$product <- log %>%
      html_nodes("#product-list a") %>%
      html_text()
    
    vendorall_1214_02 <- rbind(vendorall_1214_02, pTab)
  }
)

# when loop stalls:
tail(vendorall_1214_02)

#      user  system elapsed 
#   155.575   7.402 163.400  

# 86615 obs. of 6 variables

# 1214-02 clean extracted dataframe -------------------------------------------

vendorall_1214_02$Var.1 <- NULL
vendorall_1214_02 <- vendorall_1214_02[c(4, 5, 1, 2, 3)]
colnames(vendorall_1214_02) <- c("date", "vendor", "product", "description", "price", "shipping")

vendorall_1214_02$vendor <- as.factor(vendorall_1214_02$vendor)
# 1013 levels

vendorall_1214_02 <- separate(vendorall_1214_02, shipping, into = c("from", "to"), sep = "To:")
vendorall_1214_02$from <- gsub("From: ", "", vendorall_1214_02$from)
vendorall_1214_02$from[vendorall_1214_02$from == ""] <- NA

vendorall_1214_02$from <- as.factor(vendorall_1214_02$from) # 94
vendorall_1214_02$to <- as.factor(vendorall_1214_02$to) # 347

vendorall_1214_02$name <- stripWhitespace(vendorall_1214_02$name)
vendorall_1214_02$name <- gsub(",", " ", vendorall_1214_02$name)
vendorall_1214_02$name <- as.factor(vendorall_1214_02$name)
# 20911 levels

vendorall_1214_02$price <- gsub(" BTC", "", vendorall_1214_02$price)
vendorall_1214_02$price <- as.double(vendorall_1214_02$price)

write.csv(vendorall_1214_02, file = "vendorall_1214_02.csv", row.names = F)
test <- read.csv("vendorall_1214_02.csv")

##
# Dec 2014 03 -----------------------------------------------------------------
##

rm(list = ls())

# list of files to extract from by MONTH
dec2014Dir03 <- "~/GitHub/ag-Vendor/2014-12-d3"
setwd(dec2014Dir03)

# 11577 total Dec 2014 files
# 4089 files in round 01
vlist <- list.files(path = dec2014Dir03, pattern = ".html", all.files = T, recursive = T)
vendorall_1214_03 <- data.frame()

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
    
    pTab$product <- log %>%
      html_nodes("#product-list a") %>%
      html_text()
    
    vendorall_1214_03 <- rbind(vendorall_1214_03, pTab)
  }
)

# when loop stalls:
tail(vendorall_1214_03)

#      user  system elapsed 
#   218.456   8.004 226.470   

# 102381 obs. of 6 variables

# 1214-03 clean extracted dataframe -------------------------------------------

vendorall_1214_03$Var.1 <- NULL
vendorall_1214_03 <- vendorall_1214_03[c(4, 5, 1, 2, 3)]
colnames(vendorall_1214_03) <- c("date", "vendor", "product", "description", "price", "shipping")

vendorall_1214_03$vendor <- as.factor(vendorall_1214_03$vendor)
# 954 levels

vendorall_1214_03 <- separate(vendorall_1214_03, shipping, into = c("from", "to"), sep = "To:")
vendorall_1214_03$from <- gsub("From: ", "", vendorall_1214_03$from)
vendorall_1214_03$from[vendorall_1214_03$from == ""] <- NA

vendorall_1214_03$from <- as.factor(vendorall_1214_03$from) # 91
vendorall_1214_03$to <- as.factor(vendorall_1214_03$to) # 326

vendorall_1214_03$name <- stripWhitespace(vendorall_1214_03$name)
vendorall_1214_03$name <- gsub(",", " ", vendorall_1214_03$name)
vendorall_1214_03$name <- as.factor(vendorall_1214_03$name)
# 20249 levels

vendorall_1214_03$price <- gsub(" BTC", "", vendorall_1214_03$price)
vendorall_1214_03$price <- as.double(vendorall_1214_03$price)

write.csv(vendorall_1214_03, file = "vendorall_1214_03.csv", row.names = F)
test <- read.csv("vendorall_1214_03.csv")
