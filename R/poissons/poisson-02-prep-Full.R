# Agora Marketplace Analysis
# Poisson Regression - Monthly, Weekly, Daily counts
# "How much is there? --> How much could there be?"

# load data -------------------------------------------------------------------

library(data.table)
library(dplyr)

agora <- fread("~/GitHub/agora-data/agora-01b.csv", stringsAsFactors = T)
agora$date <- as.Date(agora$date)

# get weekly counts -----------------------------------------------------------

# convert to numeric
agora$day <- as.numeric(agora$day)
agora$month <- as.numeric(agora$month)

# loop over months and count rows - 2014

w14 <- data.frame()

for (i in 1:12) {
  
  w1 <- subset(agora, agora$month == i & agora$year == "2014" & 
                      agora$day <= 7)
  w2 <- subset(agora, agora$month == i & agora$year == "2014" & 
                      agora$day <= 14 & agora$day > 7)
  w3 <- subset(agora, agora$month == i & agora$year == "2014" & 
                      agora$day <= 21 & agora$day > 14)
  w4 <- subset(agora, agora$month == i & agora$year == "2014" & 
                      agora$day <= 31 & agora$day > 21)
  
  wLog <- cbind(w = i, w1, w2, w3, w4)
  w14 <- rbind(w14, wLog)
}

write.csv(w14, file = "data/WeeklyCounts14.csv", row.names = F)
colnames(w14)[1] <- "month"

# stack counts + sort chronologically
w14 <- stack(w14, select = c(w1, w2, w3, w4))
w14$month <- c("01", "02", "03", "04", "05", "06", 
               "07", "08", "09", "10", "11", "12")
w14$m.w <- paste("2014", w14$month, w14$ind, sep = "-")
colnames(w14) <- c("count", "week", "month", "mw")
w14 <- w14[c(4, 1, 2, 3)]
w14 <- w14[order(w2$mw, decreasing = F), ]
rownames(w2) <- NULL

# 2015 weekly counts ----------------------------------------------------------

w15 <- data.frame()

for (i in 1:7) {
  
  w1 <- subset(agora, agora$month == i & agora$year == "2015" & 
                      agora$day <= 7)
  w2 <- subset(agora, agora$month == i & agora$year == "2015" & 
                      agora$day <= 14 & agora$day > 7)
  w3 <- subset(agora, agora$month == i & agora$year == "2015" & 
                      agora$day <= 21 & agora$day > 14)
  w4 <- subset(agora, agora$month == i & agora$year == "2015" & 
                      agora$day <= 31 & agora$day > 21)
  
  wLog <- cbind(w = i, w1 = w1, w2 = w2, w3 = w3, w4 = w4)
  w15 <- rbind(w15, wLog)
}

write.csv(w15, file = "data/WeeklyCounts15.csv", row.names = F)

# clean up 2015
w15 <- stack(w15, select = c(w1, w2, w3, w4))
w15$month <- c("01", "02", "03", "04", "05", "06", "07")
w15$mw <- paste("2015", w15$month, w15$ind, sep = "-")
colnames(w15) <- c("count", "week", "month", "mw")
w15 <- w15[c(4, 1, 2, 3)]
w15 <- w15[order(w3$mw, decreasing = F), ]
rownames(w3) <- NULL

# bind 2014-2015 weekly counts ------------------------------------------------

weekly <- rbind(w14, w15)
weekly$mw <- gsub("\\bw1\\b", "07", weekly$mw)
weekly$mw <- gsub("\\bw2\\b", "14", weekly$mw)
weekly$mw <- gsub("\\bw3\\b", "21", weekly$mw)
weekly$mw <- gsub("\\bw4\\b", "28", weekly$mw)

write.csv(weekly, file = "data/WeeklyCountsPop.csv", row.names = F)
wk <- weekly


