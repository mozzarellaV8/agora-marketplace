# Agora Marketplace Analysis
# data binding 2014:
# individual y-m-d columns
# simple feedback scale column
# bitcoin-USD price 
# price of gold

# read and bind data ----------------------------------------------------------

library(data.table)
library(tidyr)
library(dplyr)
library(tm)

# initialize
pDir <- "~/GitHub/ag-p/p2014"
setwd(pDir)

pList <- list.files(path = pDir, pattern = ".csv", all.files = T,
                           full.names = T, recursive = T)
p14 <- data.frame()

# bind
system.time(
  for (i in 1:length(pList)) {
    temp <- read.csv(pList[i], stringsAsFactors = F)
    p14 <- rbind(p14, temp)
  }
)

#    user  system elapsed
#  57.765   3.400  63.833

p14$feedback <- stripWhitespace(p14$feedback)
p14$vendor <- gsub("%7E", "", p14$vendor) # tilde in vendorname
levels(as.factor(p14$vendor))             # 2473 --> 2284 one csv removed.

# 1018109 obs. of  11 variables
write.csv(p14, file = "~/GitHub/agora-data/ag14-01.csv", row.names = F)
pDir <- "~/GitHub/agora-marketplace"
setwd(pDir)

# date separation -------------------------------------------------------------
# p14 <- fread("~/GitHub/agora-data/ag01-2014.csv", stringsAsFactors = T)
# p14 <- as.data.frame(p14)

p14$fd <- p14$date
p14 <- separate(p14, fd, into = c("year", "month", "day"), by = "-")

# feedback --------------------------------------------------------------------

p14$feedback <- as.character(p14$feedback)
p14$feedback <- sub("^\\s+", "", p14$feedback)
p14$feedback <- sub("\\s+$", "", p14$feedback)

# test regex
great14 <- grepl("Feedbacks:\\s5\\/5(.*)", p14$feedback)
# there is a single blank space in front of 'Feedback'

# simple feedback scale - only the most recent
p14$fb <- ifelse(grepl("Feedbacks:\\s5\\/5(.*)", p14$feedback) == T, 5, 
            ifelse(grepl("Feedbacks:\\s4\\/5(.*)", p14$feedback) == T, 4,
              ifelse(grepl("Feedbacks:\\s3\\/5(.*)", p14$feedback) == T, 3,
                ifelse(grepl("Feedbacks:\\s2\\/5(.*)", p14$feedback) == T, 2, 
                  ifelse(grepl("Feedbacks:\\s1\\/5(.*)", p14$feedback) == T, 1, 
                    ifelse(grepl("Feedbacks:\\s0\\/5(.*)", p14$feedback) == T, 0,
                      ifelse(grepl("Feedbacks:\\sNo\\sfeedbacks\\sfound.", 
                          p14$feedback) == T, -1, "incomplete")))))))

summary(as.factor(p14$fb))
# 0      1      2     3      4        5        incomplete   -1 
# 8605   1799   1456  3720   6170     445550   7            550802

hist(as.numeric(p14$fb))
# no feedback and 5/5 dominate - binomial distribution

write.csv(p14, file = "~/GitHub/agora-data/ag14-01.csv", row.names = F)

# Bitcoin-USD Price -----------------------------------------------------------

# Bitcoin-USD
p14$date <- as.Date(p14$date)

bpi <- read.csv("data/BPI/bpi-Agora.csv")
colnames(bpi) <- c("date", "rate", "age")
bpi$age <- NULL
bpi$date <- as.Date(bpi$date)

p14 <- base::merge(p14, bpi, by = "date", all.x = T)
p14$usd <- p14$price * p14$rate

# gold oz rate
gpi <- read.csv("data/BPI/gpi.csv")
colnames(gpi) <- c("date", "gold.oz")
gpi$date <- as.Date(gpi$date)

p14 <- base::merge(p14, gpi, by = "date", all.x = T)

p14 <- as.data.frame(p14)
p14 <- p14[c("year", "month", "day", "date", "vendor", "product", "price", 
             "usd", "rate", "gold.oz", "cat", "subcat", "subsubcat", 
             "fb", "feedback", "from", "to", "list")]

# 1018109 obs of 18 variables
write.csv(p14, file = "~/GitHub/agora-data/ag14-01.csv", row.names = F)

# cleanse From field ----------------------------------------------------------
# stray $from levels in p14
levels(as.factor(p14$from)) # 406 smh

# strip leading+trailing whitespace
p14$from <- sub("^\\s+", "", p14$from)
p14$from <- sub("\\s+$", "", p14$from)
levels(as.factor(p14$from)) # 106 levels

# missed in extraction
p14$from <- gsub("^United\\sStates(.*)", "USA", p14$from, ignore.case = T)
p14$from <- gsub("^USA(.*)", "USA", p14$from, ignore.case = T)
p14$from <- gsub("^US(.*)", "USA", p14$from, ignore.case = T)
p14$from <- gsub("^The home of the Body Bags(.*)", "USA", p14$from, ignore.case = T)
p14$from <- gsub("^The United Snakes of Captivity(.*)", "USA", p14$from, ignore.case = T)
p14$from <- gsub("\\bLa\\sJolla\\b", "USA", p14$from, ignore.case = T)
p14$from <- gsub("\\bPacific\\sPalasades\\b", "USA", p14$from, ignore.case = T)
p14$from <- gsub("\\bPacific\\sPalisades\\b", "USA", p14$from, ignore.case = T)
p14$from <- gsub("\\bU.S.A.\\b", "USA", p14$from, ignore.case = T)
p14$from <- gsub("^West\\sof(.*)", "USA", p14$from, ignore.case = T)
p14$from <- gsub("^the\\sloins(.*)", "USA", p14$from, ignore.case = T)
p14$from <- gsub("^George\\swashington(.*)", "USA", p14$from, ignore.case = T)
p14$from <- gsub("\\bPool Shark Acoustic Version\\b", "USA", p14$from, ignore.case = T)
levels(as.factor(p14$from)) # 108 levels

p14$from <- gsub("^Untied(.*)", "UK", p14$from, ignore.case = T)
p14$from <- gsub("^UK(.*)", "UK", p14$from, ignore.case = T)
p14$from <- gsub("\\bUnited\\sKingdom\\b", "UK", p14$from, ignore.case = T)
p14$from <- gsub("^China(.*)", "China", p14$from, ignore.case = T)
p14$from <- gsub("^Germany(.*)", "Germany", p14$from, ignore.case = T)
p14$from <- gsub("\\bWorld(.*)", "Worldwide", p14$from, ignore.case = T)
p14$from <- gsub("\\bShipping\\b", "Worldwide", p14$from, ignore.case = T)
p14$from <- gsub("^Unde(.*)", "Undeclared", p14$from, ignore.case = T)
p14$from <- gsub("\\bEurope\\b", "EU", p14$from, ignore.case = T)
p14$from <- gsub("\\bThe\\sNetherlands\\b", "Netherlands", p14$from, ignore.case = T)
p14$from <- gsub("Bangkok", "Thailand", p14$from, ignore.case = T)
p14$from <- gsub("^Moldova(.*)", "Moldova", p14$from, ignore.case = T)

levels(as.factor(p14$from)) # 83 levels

p14$from <- gsub("^Agora(.*)", "Agora", p14$from, ignore.case = T)
p14$from <- gsub("^my(.*)", "Internet", p14$from, ignore.case = T)
p14$from <- gsub("^me(.*)", "Internet", p14$from, ignore.case = T)
p14$from <- gsub("\\btorland\\b", "Torland", p14$from, ignore.case = T)
p14$from <- gsub("\\bCheqdropz\\b", "Czech Republic", p14$from, ignore.case = T)
p14$from <- gsub("Earth(.*)", "Earth", p14$from, ignore.case = T)
p14$from <- gsub("\\bMother\\sEarth\\b", "Earth", p14$from, ignore.case = T)
levels(as.factor(p14$from)) # 77 levels

p14$from[p14$from == "u"] <- "Worldwide"
p14$from[p14$from == ""] <- "No Info"
p14$from[p14$from == "NULL"] <- "No Info"

levels(as.factor(p14$from)) # 66 levels --> 70 levels --> 75 levels

write.csv(p14, file = "~/GitHub/agora-data/ag14-01.csv", row.names = F)


# the bad location -----------------------------------------------------------
loc <- subset(p14, grepl("^From:(.*)", p14$from) == T)
summary(loc$date)
# bad extraction on these dates:
# "2014-02-02" "2014-02-05" "2014-02-05" "2014-02-05" "2014-02-05" "2014-02-10" 
# folder 2014-02-01

feb14 <- fread("~/GitHub/ag-p/products-2014-02-01.csv")
summary(as.factor(feb14$from))

# fix the lowercase in p14
# levels(as.factor(p14$from))

# function from help(chartr)
# capwords <- function(s, strict = FALSE) {
#   cap <- function(s) paste(toupper(substring(s, 1, 1)),
#                            {s <- substring(s, 2); if(strict) tolower(s) else s},
#                            sep = "", collapse = " " )
#   sapply(strsplit(s, split = " "), cap, USE.NAMES = !is.null(names(s)))
# }

# p14$from <- capwords(p14$from)
