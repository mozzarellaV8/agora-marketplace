# Agora Marketplace Analysis
# 2015 data binding:
# individual y-m-d columns
# simple feedback scale column
# bitcoin-USD price 
# price of gold

# read and bind data ----------------------------------------------------------
#### up to 2015-04-03 as of Sept 10th, 2016
#### up to 2015-06-04 as of Sept 12th, 2016
#### done: Sept 14th, 2016

library(data.table)
library(ggplot2)
library(tidyr)
library(dplyr)
library(qdap)
library(zoo)
library(tm)

pDir <- "~/GitHub/agora-marketplace"
setwd(pDir)

pList <- list.files(path = pDir, pattern = ".csv", all.files = T,
                    full.names = T, recursive = T)

p15 <- data.frame()
system.time(
  for (i in 1:length(pList)) {
    temp <- read.csv(pList[i], stringsAsFactors = F)
    p15 <- rbind(p15, temp)
  }
)

#    user  system elapsed
#  92.438   4.379  99.682

p15$feedback <- stripWhitespace(as.character(p15$feedback))
p15$vendor <- gsub("%7E", "", p15$vendor)
levels(as.factor(p15$vendor)) # 1960 vendors ---> 2142 vendors

# 1133674 obs of 11 variables -->
# 1304852 obs of 11 variables
write.csv(p15, file = "~/GitHub/agora-data/ag15-04.csv", row.names = F)
rm(temp)

# cleanse data ----------------------------------------------------------------

# p15 <- fread("~/GitHub/agora-data/ag15-03.csv", stringsAsFactors = T)
# p15 <- as.data.frame(p15)

# date separation -------------------------------------------------------------

p15$fd <- p15$date
p15 <- separate(p15, fd, into = c("year", "month", "day"), by = "-")

# feedback --------------------------------------------------------------------

# strip leading/trailing whitespace 
p15$feedback <- as.character(p15$feedback)
p15$feedback <- sub("^\\s+", "", p15$feedback)
p15$feedback <- sub("\\s+$", "", p15$feedback)

# test regex
great <- grepl("Feedbacks:\\s5\\/5(.*)", p15$feedback)

# *simple* feedback scale - reduced to most recent.
p15$fb <- ifelse(grepl("Feedbacks:\\s5\\/5(.*)", p15$feedback) == T, 5, 
                 ifelse(grepl("Feedbacks:\\s4\\/5(.*)", p15$feedback) == T, 4,
                  ifelse(grepl("Feedbacks:\\s3\\/5(.*)", p15$feedback) == T, 3,
                   ifelse(grepl("Feedbacks:\\s2\\/5(.*)", p15$feedback) == T, 2, 
                    ifelse(grepl("Feedbacks:\\s1\\/5(.*)", p15$feedback) == T, 1, 
                     ifelse(grepl("Feedbacks:\\s0\\/5(.*)", p15$feedback) == T, 0,
                      ifelse(grepl("Feedbacks:\\sNo\\sfeedbacks\\sfound.", 
                                   p15$feedback) == T, "No FB", "incomplete")))))))

summary(as.factor(p15$fb))
#      0       1       2       3       4          5   incomplete    No FB 
#   8238    1859    1834    4546    6656     550548          6     559987 - 2nd bind
#   9405    2190    2104    5155    7509     631849          7     646633 - final bind

631849/nrow(p15) # 0.4842304
646633/nrow(p15) # 0.4955604

p15$fb[p15$fb == "No FB"] <- "-1"

hist(as.numeric(p15$fb))
# 5/5 feedback eclipses no feedback            -first bind
# this is essentially a binomial distribution - final bind

write.csv(p15, file = "~/GitHub/agora-data/ag15-04.csv", row.names = F)

# Bitcoin-USD Price -----------------------------------------------------------

# Bitcoin-USD
p15$date <- as.Date(p15$date)

bpi <- read.csv("data/BPI/bpi-Agora.csv")
colnames(bpi) <- c("date", "rate", "age")
bpi$age <- NULL
bpi$date <- as.Date(bpi$date)

p15 <- base::merge(p15, bpi, by = "date", all.x = T)
p15$usd <- p15$price * p15$rate

# gold oz rate
gpi <- read.csv("data/BPI/gpi.csv")
colnames(gpi) <- c("date", "gold.oz")
gpi$date <- as.Date(gpi$date)

# There are missing date values 
# need to be imputed from previous day's close
# na.locf from library(zoo)
p15 <- base::merge(p15, gpi, by = "date", all.x = T)

p15 <- as.data.frame(p15)
p15 <- p15[c("year", "month", "day", "date", "vendor", "product", "price", 
             "usd", "rate", "gold.oz", "cat", "subcat", "subsubcat", 
             "fb", "feedback", "from", "to", "list")]

# 1133674 obs of 18 variables
# 1304852 obs of 18 variables
# write.csv(p15, file = "~/GitHub/agora-data/ag15-04.csv", row.names = F)

# cleanse From field ----------------------------------------------------------
# stray $from levels in p15
levels(as.factor(p15$from))

# strip leading+trailing whitespace
p15$from <- sub("^\\s+", "", p15$from)
p15$from <- sub("\\s+$", "", p15$from)
levels(as.factor(p15$from)) # 106 levels
  
# missed in extraction
p15$from <- gsub("^United\\sStates(.*)", "USA", p15$from, ignore.case = T)
p15$from <- gsub("^USA(.*)", "USA", p15$from, ignore.case = T)
p15$from <- gsub("^US(.*)", "USA", p15$from, ignore.case = T)
p15$from <- gsub("^The home of the Body Bags(.*)", "USA", p15$from, ignore.case = T)
p15$from <- gsub("^The United Snakes of Captivity(.*)", "USA", p15$from, ignore.case = T)
p15$from <- gsub("\\bLa\\sJolla\\b", "USA", p15$from, ignore.case = T)
p15$from <- gsub("\\bPacific\\sPalasades\\b", "USA", p15$from, ignore.case = T)
p15$from <- gsub("\\bPacific\\sPalisades\\b", "USA", p15$from, ignore.case = T)
p15$from <- gsub("\\bU.S.A.\\b", "USA", p15$from, ignore.case = T)
p15$from <- gsub("^West\\sof(.*)", "USA", p15$from, ignore.case = T)
p15$from <- gsub("^the\\sloins(.*)", "USA", p15$from, ignore.case = T)
p15$from <- gsub("^George\\swashington(.*)", "USA", p15$from, ignore.case = T)
p15$from <- gsub("\\bPool Shark Acoustic Version\\b", "USA", p15$from, ignore.case = T)
levels(as.factor(p15$from)) # 97 levels

p15$from <- gsub("^Untied(.*)", "UK", p15$from, ignore.case = T)
p15$from <- gsub("^UK(.*)", "UK", p15$from, ignore.case = T)
p15$from <- gsub("\\bUnited\\sKingdom\\b", "UK", p15$from, ignore.case = T)
p15$from <- gsub("^China(.*)", "China", p15$from, ignore.case = T)
p15$from <- gsub("^Germany(.*)", "Germany", p15$from, ignore.case = T)
p15$from <- gsub("\\bWorld(.*)", "Worldwide", p15$from, ignore.case = T)
p15$from <- gsub("\\bShipping\\b", "Worldwide", p15$from, ignore.case = T)
p15$from <- gsub("^Unde(.*)", "Undeclared", p15$from, ignore.case = T)
p15$from <- gsub("\\bEurope\\b", "EU", p15$from, ignore.case = T)
p15$from <- gsub("\\bThe\\sNetherlands\\b", "Netherlands", p15$from, ignore.case = T)
p15$from <- gsub("Bangkok", "Thailand", p15$from, ignore.case = T)
levels(as.factor(p15$from)) # 79 levels

p15$from <- gsub("^Agora(.*)", "Agora", p15$from, ignore.case = T)
p15$from <- gsub("^my(.*)", "Internet", p15$from, ignore.case = T)
p15$from <- gsub("^me(.*)", "Internet", p15$from, ignore.case = T)
p15$from <- gsub("\\btorland\\b", "Torland", p15$from, ignore.case = T)
p15$from <- gsub("\\bCheqdropz\\b", "Czech Republic", p15$from, ignore.case = T)
p15$from <- gsub("Earth(.*)", "Earth", p15$from, ignore.case = T)
p15$from <- gsub("\\bMother\\sEarth\\b", "Earth", p15$from, ignore.case = T)
levels(as.factor(p15$from)) # 74 levels

p15$from[p15$from == "u"] <- "Worldwide"
p15$from[p15$from == ""] <- "No Info"
p15$from[p15$from == "no info"] <- "No Info"

levels(as.factor(p15$from)) # 66 levels --> 70 levels

write.csv(p15, file = "~/GitHub/agora-data/ag15-04.csv", row.names = F)

# bind 2014 + 2015 ------------------------------------------------------------
# p14 <- fread("~/GitHub/agora-data/ag14-01.csv", stringsAsFactors = T)
# p15 <- fread("~/GitHub/agora-data/ag15-02.csv", stringsAsFactors = T)

# fix the lowercase in p14
levels(as.factor(p14$from))

# function from help(chartr)
capwords <- function(s, strict = FALSE) {
    cap <- function(s) paste(toupper(substring(s, 1, 1)),
                  {s <- substring(s, 2); if(strict) tolower(s) else s},
                             sep = "", collapse = " " )
    sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
}

p14$from <- capwords(p14$from)
levels(as.factor(p14$from))

p14$from <- gsub("\\bUk\\b", "UK", p14$from)
p14$from <- gsub("\\bUsa\\b", "USA", p14$from)
p14$from <- gsub("\\bEu\\b", "EU", p14$from)
p14$from <- gsub("\\bUndeclared;)\\b", "Undeclared", p14$from)
levels(as.factor(p14$from))

levels(as.factor(p15$from)) # 56 levels
levels(as.factor(p14$from)) # 75 levels

agora <- rbind(p14, p15)
nrow(agora) # 1812775 obs of 18 variables

levels(as.factor(agora$from)) # 83 levels
levels(as.factor(agora$to))   # 1713 levels - very messy
agora$to <- stripWhitespace(as.character(agora$to))

write.csv(agora, file = "~/GitHub/agora-data/agora-00.csv", row.names = F)
agora <- fread("~/GitHub/agora-data/agora-00.csv", stringsAsFactors = T)
