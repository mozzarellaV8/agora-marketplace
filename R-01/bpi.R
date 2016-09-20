# Agora Marketplace Analysis
# bitcoin price index data

# via coindesk.com ------------------------------------------------------------
bpi_coin <- read.csv("data/bpi/bpi-Coindesk.csv")
bpi_coin$Date <- as.Date(bpi_coin$Date)

bpiAg <- subset(bpi_coin, bpi_coin$Date > "2013-12-31")
bpiAg$btcAge <- rownames(bpiAg)
rownames(bpiAg) <- NULL
write.table(bpiAg, file = "data/bpi-Agora.csv", sep = ",", row.names = F)

# via investing.com -----------------------------------------------------------

bpi.inv <- read.csv("data/bpi/bpi-investing.csv")
bpi.inv$Date <- as.Date(bpi.inv$Date)



# compute daily high-low difference:
# bpi_DD <- data.frame()
# for (i in 1:length(bpi)) {
#   dd[i] <- bpii$High[i] - bpi$Low[i]
#  bpi_DD <- cbind(bpi, dd)
# }

# via blockchain.info ---------------------------------------------------------
# bpi_block <- read.csv("data/bpi/bpi-blockchain.csv")
# colnames(bpi_block) <- c("date", "price")
# bpi_block$date <- as.Date(bpi_block$date)

# Coindesk Total BTC History --------------------------------------------------

bpi <- bpi_coin
summary(bpi)

bpi <- na.omit(bpi)

par(mar = c(6, 6, 6, 6), family = "FranklinGothicSSK")
plot(bpi$Date, bpi$Close.Price, col = "#00000075",
  main = "Bitcoin Price Index (USD) 2010-07-18 :: 2016-08-02")
rug(bpi$Date, ticksize = 0.025, lwd = 0.1, col = "#000000")


quantile(bpi$Close.Price, na.rm = T)
#       0%      25%      50%      75%     100% 
#    0.050    5.900  119.125  386.210 1147.250
summary(bpi$Close.Price)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   0.05    5.90  119.10  213.50  386.20 1147.00

# Bind BPI to Agora Extraction ------------------------------------------------

library(data.table)
p14 <- fread("~/GitHub/agora-data/ag02-2014.csv", stringsAsFactors = T)
str(p14)

p14$date <- as.Date(p14$date)

bpi <- read.csv("data/BPI/bpi-Agora.csv")
colnames(bpi) <- c("date", "rate", "age")
bpi$age <- NULL
bpi$date <- as.Date(bpi$date)

# merge in BTC-USD exchanage rate
p14a <- base::merge(p14, bpi, by = "date")
p14a$usd <- p14a$price * p14a$rate

p14a <- as.data.frame(p14a)
p14a <- p14a[c(2, 1, 3, 4, 5, 13, 12, 6, 7, 8, 9, 10, 11)]
write.csv(p14a, file = "~/GitHub/agora-data/ag03-2014.csv", row.names = F)

# merge with vendor frame

v14 <- fread("~/GitHub/agora-data/v14-00c.csv")
v14$date <- as.Date(v14$date)

v14a <- base::merge(v14, bpi, by = "date")
v14a$usd <- v14a$price * v14a$rate
v14a <- as.data.frame(v14a)
v14a <- v14a[c(2, 1, 3, 4, 5, 12, 11, 6, 7, 8, 9, 10)]

write.csv(v14a, file = "~/GitHub/agora-data/v14-00d.csv", row.names = F)
