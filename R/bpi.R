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

bpi <- read.delim("data/bpi/bpi-investing.tsv")

bpi_DD <- data.frame()
for (i in 1:length(bpi)) {
  dd[i] <- bpii$High[i] - bpi$Low[i]
  bpi_DD <- cbind(bpi, dd)
}

# via blockchain.info ---------------------------------------------------------
# bpi_block <- read.csv("data/bpi/bpi-blockchain.csv")
# colnames(bpi_block) <- c("date", "price")
# bpi_block$date <- as.Date(bpi_block$date)

# Coindesk Total BTC History --------------------------------------------------

bpi <- bpi_coin
summary(bpi)

par(mar = c(6, 6, 6, 6), family = "FranklinGothicSSK")
plot(bpi$Date, bpi$Close.Price, 
  main = "Bitcoin Price Index (USD) 2010-07-18 :: 2016-08-02")

bpi <- na.omit(bpi)

quantile(bpi$Close.Price, na.rm = T)
#       0%      25%      50%      75%     100% 
#    0.050    5.900  119.125  386.210 1147.250
summary(bpi$Close.Price)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   0.05    5.90  119.10  213.50  386.20 1147.00
