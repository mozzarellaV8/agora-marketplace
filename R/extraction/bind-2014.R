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

pDir <- "~/GitHub/ag-p/p14"
pList <- list.files(path = pDir, pattern = ".csv", all.files = T,
                           full.names = T, recursive = T)

p14 <- data.frame()
system.time(
  for (i in 1:length(pList)) {
    temp <- read.csv(pList[i], stringsAsFactors = F)
    p14 <- rbind(p14, temp)
  }
)

#    user  system elapsed
#  60.263   2.891  64.025

p14$feedback <- stripWhitespace(p14$feedback)
p14$vendor <- gsub("%7E", "", p14$vendor) # tilde in vendorname
levels(as.factor(p14$vendor))

# 1018109 obs. of  11 variables
write.csv(p14, file = "~/GitHub/agora-data/ag01-2014.csv", row.names = F)

# date separation -------------------------------------------------------------
# p14 <- fread("~/GitHub/agora-data/ag07b-2014.csv", stringsAsFactors = T)
# p14 <- as.data.frame(p14)

p14$fd <- p14$date
p14 <- separate(p14, fd, into = c("year", "month", "day"), by = "-")

# feedback --------------------------------------------------------------------

p14$feedback <- as.character(p14$feedback)

# test regex
great14 <- grepl("Feedbacks:\\s5\\/5(.*)", p14$feedback)
# there is a single blank space in front of 'Feedback'

# simple feedback scale
p14$fb <- ifelse(grepl("Feedbacks:\\s5\\/5(.*)", p14$feedback) == T, 5, 
                 ifelse(grepl("Feedbacks:\\s4\\/5(.*)", p14$feedback) == T, 4,
                        ifelse(grepl("Feedbacks:\\s3\\/5(.*)", p14$feedback) == T, 3,
                               ifelse(grepl("Feedbacks:\\s2\\/5(.*)", p14$feedback) == T, 2, 
                                      ifelse(grepl("Feedbacks:\\s1\\/5(.*)", p14$feedback) == T, 1, 
                                             ifelse(grepl("Feedbacks:\\s0\\/5(.*)", p14$feedback) == T, 0,
                                                    ifelse(grepl("Feedbacks:\\sNo\\sfeedbacks\\sfound.", 
                                                                 p14$feedback) == T, "No FB", "incomplete")))))))

summary(as.factor(p14$fb))

p14$fb <- as.factor(p14$fb)
hist(as.numeric(p14$fb)) # no feedback and 5/5 dominate
# write.csv(p14, file = "~/GitHub/agora-data/ag07b-2014.csv", row.names = F)

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
gpi$date2 <- seq(as.Date())

# need to be imputed from previous day's close
p14 <- base::merge(p14, gpi, by = "date", all.x = T)

p14 <- as.data.frame(p14)
p14 <- p14[c("list", "date", "vendor", "product", "price", "usd", "rate", 
             "cat", "subcat", "subsubcat", "feedback", "fb", "from", "to",
             "year", "month", "day", "gold.oz")]


write.csv(p14, file = "~/GitHub/agora-data/ag14-01.csv", row.names = F)

# categorical tables ----------------------------------------------------------

# category table
c15 <- as.data.frame(table(p14$cat))
colnames(c15) <- c("category", "freq")
c15 <- c15[order(c15$freq, decreasing = T), ]
rownames(c15) <- NULL

# subcategory table
summary(p14$subcat)
sc15 <- as.data.frame(table(p14$subcat))
colnames(sc15) <- c("subcategory", "freq")
sc15 <- sc15[order(sc15$freq, decreasing = T), ]
rownames(sc15) <- NULL

# sub-subcategory (drug) table
summary(p14$subsubcat)
ssc15 <- as.data.frame(table(p14$subcat))
colnames(ssc15) <- c("drug", "freq")
ssc15 <- ssc15[order(ssc15$freq, decreasing = T), ]
rownames(ssc15) <- NULL



