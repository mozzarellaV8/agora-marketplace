# Agora Marketplace Analysis
# bitcoin price index data

# via coindesk.com ------------------------------------------------------------
bpi_coin <- read.csv("data/bpi/bpi-Coindesk.csv")
bpi_coin$Date <- as.Date(bpi_coin$Date)

bpiAg <- subset(bpi_coin, bpi_coin$Date > "2013-12-31")
bpiAg$btcAge <- rownames(bpiAg)
rownames(bpiAg) <- NULL
write.table(bpiAg, file = "data/bpi-Agora.csv", sep = ",", row.names = F)


# via blockchain.info ---------------------------------------------------------
bpi_block <- read.csv("data/bpi/bpi-blockchain.csv")
colnames(bpi_block) <- c("date", "price")
bpi_block$date <- as.Date(bpi_block$date)
