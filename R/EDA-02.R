# Agora Marketplace Analysis
# arules - 2014 data

# load data -------------------------------------------------------------------

library(data.table)

# all 2014 Agora market data
p14 <- fread("~/GitHub/agora-data/Agora2014.csv", stringsAsFactors = F)
p14$date <- as.Date(p14$date)

# subset for listings with feedback
fb <- subset(p14, p14$feedback != "\n    Feedbacks:\n    No feedbacks found.\n")
levels(as.factor(fb$from)) #64
levels(as.factor(fb$to))   #99

# bitcoin price data
bpi <- read.csv("data/bpi/bpi-Coindesk.csv")
bpi$Date <- as.Date(bpi$Date)
summary(bpi)

bpi <- na.omit(bpi)
colnames(bpi) <- c("Date", "Price")

# explore prices --------------------------------------------------------------

library(ggplot2)
library(scales)
library(RColorBrewer)

datebreaks <- seq(as.Date(bpi$Date)[1], as.Date(bpi$Date)[2208],
                  by = "2 month")

p1 <- ggplot(bpi, aes(Date, Close.Price, colour = Close.Price)) +
  geom_point(size = 2, shape = 1, alpha = 1) +
  scale_colour_gradient2(low = "steelblue4", mid = "white", high = "firebrick3") +
  theme_bw(base_size = 12, base_family = "FranklinGothicSSK") +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 08)) +
  theme(axis.text.y = element_text(size = 10)) +
  theme(axis.title.y = element_text(face = "italic")) +
  theme(plot.margin = unit(c(1.5, 1.5, 1.5, 1.5), "cm")) +
  theme(plot.title = element_text(size = 18, face = "bold", vjust = 3)) +
  labs(title = "Bitcoin Price Index (USD) 2010-07-18 :: 2016-08-02", 
       x = "", y = "price in USD")

p1 + scale_x_date(breaks = datebreaks)

# Bitcoin Lifetime with trends ------------------------------------------------

# plot entire Bitcoin lifetime
par(mar = c(6, 6, 6, 6), family = "FranklinGothicSSK")
plot(bpi$Date, bpi$Price, col = "#00000075",
     main = "Bitcoin Price Index (USD) 2010-07-18 :: 2016-08-02")
abline(a = 526.9241, b = 0, lty = 2, col = "#FF000075")
rug(bpi$Date, ticksize = 0.025, lwd = 0.1, col = "#000000")

# Subset Agora data date range
bpiAg <- bpi[bpi$Date >= "2014-01-01" & bpi$Date <= "2014-12-31", ]
plot(bpiAg, main = "Bitcoin Price Index 2014 (USD)")
abline(a = 526.9241, b = 0, lty = 2, col = "#FF000075")

summary(bpiAg)
bpiAg[bpiAg$Price == min(bpiAg$Price), ]
# 1627 2014-12-30 309.87

bpiAg[bpiAg$Price == max(bpiAg$Price), ]
# 1269 2014-01-06 951.39

bpiAg[bpiAg$Price == mean(bpiAg$Price), ]
mean(bpiAg$Price)
# [1] 526.9241

# subset Agora up/down trend intervals ----------------------------------------

# 1st event - 1st downturn ----------------------------------------------------

bpiAg04 <- bpiAg[bpiAg$Date >= "2014-04-01" & bpiAg$Date <= "2014-04-30", ]
summary(bpiAg04$Price)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#   360.8   441.2   456.9   460.2   486.3   529.2

bpiAg04[bpiAg04$Price == 360.84, ]
#            Date  Price
# 1363 2014-04-10 360.84

dt01 <- bpiAg[bpiAg$Date >="2014-01-01" & bpiAg$Date <= "2014-04-10", ]
dim(dt01)
# 100   2
# 100 day downward trend in pricing.

# start date
dt01[1, ]
#            Date  Price
# 1264 2014-01-01 770.44

# end date
dt01[100, ]
#            Date  Price
# 1363 2014-04-10 360.84

summary(dt01)
#       Date                Price      
# Min.   :2014-01-01   Min.   :360.8  
# 1st Qu.:2014-01-25   1st Qu.:575.2  
# Median :2014-02-19   Median :635.9  
# Mean   :2014-02-19   Mean   :678.8  
# 3rd Qu.:2014-03-16   3rd Qu.:842.4  
# Max.   :2014-04-10   Max.   :951.4

# 2nd event: 1st Rally --------------------------------------------------------

# identity peak value from range
bpiAgSummer <- bpiAg[bpiAg$Date >= "2014-04-11" & bpiAg$Price >= 600, ]
summary(bpiAgSummer)
#          Date                Price      
# Min.   :2014-05-30   Min.   :600.0  
# 1st Qu.:2014-06-09   1st Qu.:618.8  
# Median :2014-07-04   Median :627.6  
# Mean   :2014-06-28   Mean   :629.2  
# 3rd Qu.:2014-07-14   3rd Qu.:639.4  
# Max.   :2014-07-24   Max.   :665.7

bpiAgSummer[bpiAgSummer$Price == 665.73, ]
#            Date  Price
# 1417 2014-06-03 665.73

nrow(bpiAgSummer)
# [1] 41

# 3rd event - downturn 2 ------------------------------------------------------

dt02 <- bpiAg[bpiAg$Date >="2014-06-04" & bpiAg$Date <= "2014-12-01", ] 
summary(dt02)
# Date                Price      
# Min.   :2014-06-04   Min.   :319.6  
# 1st Qu.:2014-07-19   1st Qu.:377.1  
# Median :2014-09-02   Median :479.5  
# Mean   :2014-09-02   Mean   :485.7  
# 3rd Qu.:2014-10-17   3rd Qu.:591.0  
# Max.   :2014-12-01   Max.   :656.1 

dt02[dt02$Price == 309.]



# plot entire Bitcoin lifetime with Agora -------------------------------------
summary(bpi$Price)
# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.05    5.90  119.10  213.50  386.20 1147.00
quantile(bpi$Price)
# distribution of price values
# 0%      25%      50%      75%     100% 
# 0.050    5.900  119.125  386.210 1147.250

# colors:
# rally = #7AC5CD
# downturn =  
# Agora = #CD0000
# Bitcoing = #00000075

par(mar = c(6, 6, 6, 6), family = "FranklinGothicSSK")

plot(bpi$Date, bpi$Price, col = "#00000075",
     main = "Bitcoin Price Index (USD) 2010-07-18 :: 2016-08-02")
abline(a = 213.50, b = 0, lty = 2, col = "#FF000075")
abline(a = 300, b = 0, lty = 2, col = "#FF000050")
abline(a = 350, b = 0, lty = 2, col = "#FF0000")
abline(a = 400, b = 0, lty = 2, col = "#FF000025")
points(bpiAg$Date, bpiAg$Price, col = "#CD0000", pch = 19, cex = 0.8)
points(bpiAg$Date, bpiAg$Price, col = "#00000050", pch = 1, cex = 1.1)









