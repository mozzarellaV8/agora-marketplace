# Agora by Month
# Summary Plots

# load data -------------------------------------------------------------------

library(data.table)
agora <- fread("~/GitHub/agora-data/data/agora.csv")
str(agora)

agora$hash <- as.factor(agora$hash)
agora$Date <- as.Date(agora$Date)
agora$vendor_name <- as.factor(agora$vendor_name)
agora$description <- as.factor(agora$description)

# June 2014 -------------------------------------------------------------------

summary(agora)
summary(agora$vendor_name)

june14 <- subset(agora, grepl("2014-06", agora$Date))
summary(june14)

# look at vendor listing counts
juneVendors <- as.data.frame(table(june14$vendor_name))
colnames(juneVendors) <- c("vendor", "NumListings")
juneVendors <- juneVendors[order(juneVendors$NumListings, decreasing = T), ]

# fake, mssource, FREE, and captain kirk rounding out the 
# top 4 vendor by number listings.

# subset user captain kirk --------------------------------
kirk <- subset(agora, agora$vendor_name == "captainkirk")
# kirk is highly likely the book seller.

summary(kirk$usd)
#    Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
#  0.7243  0.9539  0.9849  0.9861  1.0180  1.2770 

length(unique(kirk$name)) # 444 unique listings
length(unique(kirk$hash)) # 1371
summary(as.factor(kirk$name))

kirkbooks <- as.data.frame(table(as.factor(kirk$name)))
captainkirk <- kirkbooks[order(kirkbooks$Freq, decreasing = T), ]
colnames(captainkirk) <- c("Title", "NumListings")
rownames(captainkirk) <- NULL
View(captainkirk)

write.table(captainkirk, file = "captainkirk-dnmBooks.csv", sep = ",",
            row.names = F)

# daily histograms -----------------------------------------
summary(june14$Date)
plot(june14$Date, june14$usd, ylim = c(0, 100000))
plot(june14$usd, june14$Date, xlim = c(0, 10000),
     cex = 2.0, col = "#00000020")


# July 2014 -------------------------------------------------------------------

library(ggplot2)

july2014 <- subset(agora, grepl("2014-07", agora$Date))
summary(july2014)
summary(july2014$usd)
#     Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
#        0       32       97     2151      343 14090000 

options(scipen = 999)


prices <- ggplot(july2014, aes(usd, as.factor(ship_from))) + 
  geom_point() + 
  theme_minimal(base_size = 10, base_family = "Arial Rounded MT Bold") +
  theme(plot.margin = unit(c(4, 4, 4, 4), "cm")) +
  labs(title = "Agora: July 2014 List Prices by Location",
       x = "USD", y = "location")

prices

product.prices <- ggplot(july2014, aes(usd, as.factor(name))) +
  geom_point() +
  theme_minimal(base_size = 10, base_family = "Arial Rounded MT Bold") +
  theme(plot.margin = unit(c(4, 4, 4, 4), "cm")) +
  labs(title = "Agora: July 2014 List Prices by Name",
       x = "USD", y = "location")

product.prices


grep("panama", july2014) # 8


# subset lower prices 
july2014B <- subset(july2014, july2014$usd <= 15000)

par(mar = c(8, 8, 8, 8), family = "Arial Rounded MT Bold")
plot(july2014B$usd, july2014B$Date, 
     main = "Agora Marketplace: July 2014 List Prices < $15000",
     xlab = "price in USD", ylab = "2014", cex = 2.0)

par(mar = c(8, 8, 8, 8), las = 1, family = "Arial Rounded MT Bold")
plot(july2014B$usd, july2014B$Date, 
     main = "Agora Marketplace: July 2014 List Prices < $5000",
     xlab = "price in USD", ylab = "2014", cex = 1.4,
     col = "#00000023", xlim = c(0, 5000))
axis(2, july2014B$Date)





