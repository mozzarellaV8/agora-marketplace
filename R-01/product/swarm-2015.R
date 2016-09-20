# Agora Marketplace Analysis
# 2015 data exploration
# summary statistics
# feedback histogram

# load  -----------------------------------------------------------------------

library(data.table)
library(ggplot2)
library(qdap)
library(zoo)
library(beeswarm)
library(extrafont)
library(colorRamps)

p15 <- fread("~/GitHub/agora-data/ag15-03b.csv", stringsAsFactors = T)
p15 <- as.data.frame(p15)

# feedback / fb ---------------------------------------------------------------

p15 <- subset(p15, p15$fb != "incomplete")
p15$fb <- as.integer(p15$fb)

p15$fb[p15$fb == "1"] <- -1
p15$fb[p15$fb == "2"] <- 0
p15$fb[p15$fb == "3"] <- 1
p15$fb[p15$fb == "4"] <- 2
p15$fb[p15$fb == "5"] <- 3
p15$fb[p15$fb == "6"] <- 4
p15$fb[p15$fb == "7"] <- 5

p15$fb <- factor(p15$fb)
levels(p15$fb)

# feedback distribution
p15$fb <- as.integer(p15$fb)
par(mfrow = c(1, 1), mar = c(8, 12, 8, 8), las = 1, family = "HersheySans")
hist(p15$fb, ylab = "", xlab = "feedback score",
     main = "Agora 2015: Distribution of feedback scores (n = 1133668)")
text(2, 400000, labels = "No Feedback, 0.4939603")
text(2, 420000, labels = "0/5: 0.007266")
text(2, 440000, labels = "1/5: 0.001639")
text(2, 460000, labels = "2/5: 0.001618")
text(2, 480000, labels = "3/5: 0.004011")
text(2, 500000, labels = "4/5: 0.005871")
text(2, 520000, labels = "5/5: 0.485634")
# looks binomial - bernoulli

summary(as.factor(p15$fb))
559987/nrow(p15)  # 0.4939603
8238/nrow(p15)    # 0.007266678
1859/nrow(p15)    # 0.00163981
1834/nrow(p15)    # 0.001617758
4546/nrow(p15)    # 0.004009992
6656/nrow(p15)    # 0.005871207
550548/nrow(p15)  # 0.4856342

fbp <- ggplot(p15, aes(x = fb)) + 
  geom_histogram(binwidth = 0.5, fill = "white", colour = "black", size = 0.50)

fbp + scale_x_continuous(labels = c("None", "0/5", "1/5", "2/5", "3/5", "4/5", "5/5"),
                         limits = c(-2, 6), breaks = c(-1:5)) +
  theme_minimal(base_size = 16, base_family = "FranklinGothicSSK") +
  theme(axis.text.x = element_text(size = 14.75)) +
  theme(axis.text.y = element_text(size = 14.75)) +
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm")) +
  labs(title = "Ag2015: Distribution of Feedback Scores (n = 1133668)",
       x = "", y = "")

# fb table
fb <- as.data.frame(table(p15$fb))
fb <- fb[-8, ]
colnames(fb) <- c("feedback", "count")

# distributions of Price ------------------------------------------------------

par(mfrow = c(2, 2), mar = c(8, 8, 6, 8), las = 1, family = "FranklinGothicSSK")
hist(p15$usd[p15$usd < 100000], breaks = 100, ylab = "", xlab = "",
     main = "Distribution of Prices (USD)", cex.main = 1.4,
     sub = "USD < 100000, n = 1132789", cex.sub = 1.1)
hist(p15$usd[p15$usd < 10000], breaks = 100, ylab = "", xlab = "",
     main = "Agora Marketplace 2015", sub = "USD < 10000, n = 1127925",
     cex.sub = 1.1, cex.main = 1.4)
hist(p15$usd[p15$usd < 1000], breaks = 100, ylab = "", xlab = "",
     main = "", sub = "USD < 1000, n = 1027963", cex.sub = 1.1)
hist(p15$usd[p15$usd < 500], breaks = 100, ylab = "", xlab = "",
     main = "", sub = "USD < 500, n = 950438", cex.sub = 1.1)

p500 <- subset(p15, p15$usd <= 500)     # 950438
p1000 <- subset(p15, p15$usd <= 1000)   # 1027963
p10k <- subset(p15, p15$usd <= 10000)   # 1127925
p100k <- subset(p15, p15$usd <= 100000) # 1132789

usp <- ggplot(p1000, aes(x = usd)) +
  geom_histogram(binwidth = 30, fill = "white", color = "black", size = 0.35)

usp + theme_grey(base_size = 14, base_family = "FranklinGothicSSK") +
  theme(panel.background = element_rect(fill = "grey92")) +
  theme(plot.title = element_text(margin = margin(c(0, 0, 25, 0)))) +
  theme(axis.text.x = element_text(size = 12.5, family = "Times", face = "italic")) +
  theme(axis.text.y = element_text(size = 12.5, family = "Times", face = "italic")) +
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm")) +
  labs(title = "Ag2015: Distribution of Prices (USD < $1000, n = 950438)",
       x = "", y = "")

# distribution of prices by category (<10000 USD) -----------------------------
# remove categories w/o subcategory
p15nc <- subset(p15, p15$cat != "Electronics" & p15$cat != "Jewelry"
                & p15$cat != "Listings" & p15$cat != "Other"
                & p15$cat != "Chemicals")

# 1094037 obs
p15nc$cat <- factor(p15nc$cat)
levels(p15nc$cat)

# under $100
p100 <- subset(p15nc, p15nc$usd <= 100.00)   # 605221 obs
p100s <- p100[sample(nrow(p100), 1000), ]    # 1000

# beeswarm n = 1000 - USD <= 1000 -----------------------------
# method = swarm, center, hex, square

p1k <- subset(p15nc, p15nc$usd <= 1000.00) # 990321
p1k$cat <- factor(p1k$cat)

p1ks <- p1k[sample(nrow(p1k), 1000), ]
p2k <- p1k[sample(nrow(p1k), 2000), ]

par(mfrow = c(1, 1), mar = c(8, 12, 8, 8), las = 1, family = "HersheySans", bty = "l")
bs1 <- beeswarm(usd ~ cat, data = p1ks, pch = 20, method = "swarm", cex = 1,
                ylab = "Price (USD)", 
                main = "Sample Distribution of Prices by Category (n = 1000, USD < 1,000)",
                col = c("red3", "gold1", "deepskyblue4", "lightblue3", "red1", 
                        "bisque1", "bisque3", "bisque4",  "darkorange3", "firebrick4"))

# each of 4 methods -----------------------------------------------------------
par(mfrow = c(1, 1), mar = c(6, 6, 6, 3), las = 1, family = "HersheySans", bty = "l")
bs1b <- beeswarm(usd ~ cat, data = p1ks, pch = 1, method = "center", priority = "descending",
                main = "Sample Distribution of Prices by Category (n = 1000, USD < 1,000)",
                col = c("red3", "gold1", "deepskyblue4", "lightblue3", "red1", 
                        "bisque1", "bisque3", "bisque4",  "darkorange3", "firebrick4"),
                cex = 1.2, ylab = "Price (USD)", xlab = "")
# points(bs1b$x, bs1b$y, pch = 20, cex = 0.75, col = "grey88")

bs1c <- beeswarm(usd ~ cat, data = p1ks, pch = 16, method = "hex", priority = "descending",
                 main = "Sample Distribution of Prices by Category (n = 1000, USD < 1,000)",
                 col = c("red3", "gold1", "deepskyblue4", "lightblue3", "red1", 
                         "bisque1", "bisque3", "bisque4",  "darkorange3", "firebrick4"),
                 cex = 0.80, ylab = "Price (USD)")
# points(bs1c$x, bs1c$y, pch = 1.3, col = "grey50")

bs1d <- beeswarm(usd ~ cat, data = p1ks, pch = 18, method = "square",
                 main = "Sample Distribution of Prices by Category (n = 1000, USD < 1,000)",
                 col = c("red3", "gold1", "deepskyblue4", "lightblue3", "red1", 
                         "bisque1", "bisque3", "bisque4",  "darkorange3", "firebrick4"),
                 cex = 1.2, ylab = "Price (USD)")

# bee n = 4000 - USD <= 1000 --------------------------------------------------
par(mar = c(6, 6, 6, 6), bty = "l", las = 1, family = "HersheySans")
bs2 <- beeswarm(usd ~ cat, data = p2k, pch = 18, method = "center", cex = 1.5,
                ylab = "Price (USD)", xlab = "", corral = "gutter", priority = "descending",
                main = "Sample Distributions of Prices by Category (n = 2000, USD < 1000)",
                col = c("red3", "gold1", "deepskyblue4", "lightblue3", "red1", 
                        "bisque1", "bisque3", "bisque4",  "darkorange3", "firebrick4"))

par(mar = c(6, 8, 6, 6), bty = "l", las = 1, family = "GillSans")
bs2b <- beeswarm(usd ~ cat, data = p2k, pch = 18, method = "center", cex = 1.75,
                ylab = "Price (USD)", xlab = "", corral = "gutter", priority = "descending",
                main = "Sample Distributions of Prices by Category (n = 2000, USD < 1000)",
                col = c("red3", "gold1", "deepskyblue4", "lightblue3", "red1", 
                        "bisque1", "bisque3", "bisque4",  "darkorange3", "firebrick4"),
                cex.main = 1.2, cex.axis = 1.1)

# bs plot 01 ------------------------------------------------------------------
bs2$x.orig <- factor(bs2$x.orig)

# print shape/text size  = 3.65 / 12.75
# web shape/text size    = 2.2 / 11.2

bs.plot <- ggplot(bs2, aes(x = x, y = y)) + 
  geom_point(aes(colour = x.orig), size = 2.8, shape = 18) +
  scale_colour_manual(values = c("red3", "gold1", "deepskyblue4", "lightblue3", 
                                 "red1", "bisque1", "bisque3", "bisque4",  
                                 "darkorange3", "firebrick4")) +
  scale_x_continuous(breaks = c(1:10), expand = c(0, 0.5),
                     labels = c("Counterfeits", "Data", "Drug paraphernalia",
                                "Drugs", "Forgeries", "Info/eBooks", 
                                "Information", "Services", "Tobacco", 
                                "Weapons"))

bs.plot <- bs.plot + theme_minimal(base_size = 12, base_family = "GillSans") +
  theme(plot.title = element_text(size = 12.75, margin = margin(c(0, 0, 20, 0)))) +
  theme(axis.text.x = element_text(size = 11.2, vjust = 0)) +
  theme(axis.text.y = element_text(size = 10.8)) +
  theme(plot.margin = unit(c(1.25, 1, 1.25, 0.75), "cm")) +
  theme(legend.position = "none") +
  labs(title = "Agora 2015: Sample Distribution, Prices ~ Category (USD < 1000, n = 2000)",
       x = "", y = "")

bs.plot

# plot(bs2$x, bs2$y)

# bs plot 02 ----------------------------------------------

# horizontal
bs.plot1 <- ggplot(bs2, aes(x = y, y = x)) + 
  geom_point(aes(colour = x.orig), size = 2.8, shape = 18) +
  scale_colour_manual(values = c("red3", "gold1", "deepskyblue4", "lightblue3", 
                                 "red1", "bisque1", "bisque3", "bisque4",  
                                 "darkorange3", "firebrick4")) +
  scale_y_continuous(breaks = c(1:10), expand = c(0, 0.5),
                     labels = c("Counterfeits", "Data", "Drug paraphernalia",
                                "Drugs", "Forgeries", "Info/eBooks", 
                                "Information", "Services", "Tobacco", "Weapons")) +
  theme_minimal(base_size = 12, base_family = "GillSans") +
  theme(axis.text.x = element_text(size = 11.5, vjust = 1)) +
  theme(axis.text.y = element_text(size = 11.5)) +
  theme(plot.margin = unit(c(1, 0.75, 0.25, 0.5), "cm")) +
  theme(legend.position = "none") +
  labs(title = "Agora 2015: Sample Distribution, Prices ~ Category (USD < 1000, n = 2000)",
       x = "", y = "")

bs.plot1

# bs plot 03 ----------------------------------------------

par(mfrow = c(1, 1), mar = c(6, 6, 6, 3), las = 1, family = "HersheySans", bty = "l")

bs1b <- beeswarm(usd ~ cat, data = p1ks, pch = 1, method = "center", 
                 priority = "density", corral = "random",
                 main = "Sample Distribution of Prices by Category (n = 1000, USD < 1,000)",
                 col = c("red3", "gold1", "deepskyblue4", "lightblue3", "red1", 
                         "bisque1", "bisque3", "bisque4",  "darkorange3", "firebrick4"),
                 cex = 1.2, ylab = "Price (USD)", xlab = "")

bs.plot2 <- ggplot(bs1b, aes(x = x, y = y)) + 
  geom_point(aes(colour = x.orig), size = 2.8, shape = 20) +
  scale_colour_manual(values = c("red3", "gold1", "deepskyblue4", "lightblue3", 
                                 "red1", "bisque1", "bisque3", "bisque4",  
                                 "darkorange3", "firebrick4")) +
  scale_x_continuous(breaks = c(1:10), expand = c(0, 0.5),
                     labels = c("Counterfeits", "Data", "Drug paraphernalia",
                                "Drugs", "Forgeries", "Info/eBooks", 
                                "Information",  "Services", 
                                "Tobacco", "Weapons")) +
  theme_minimal(base_size = 12, base_family = "GillSans") +
  theme(axis.text.x = element_text(size = 11.5, vjust = 1)) +
  theme(axis.text.y = element_text(size = 11.5)) +
  theme(plot.margin = unit(c(1, 0.75, 0.25, 0.5), "cm")) +
  theme(legend.position = "none") +
  labs(title = "Agora 2015: Sample Distribution, Prices ~ Category (USD < 1000, n = 1000)",
       x = "", y = "")

bs.plot2

# write.csv(bs1b, file = "data/swarm/bs1b-1000.csv", row.names = F)
# write.csv(bs2, file = "data/swarm/bs2-2000.csv", row.names = F)

# swarm - Price ~ Cat + Location ---------------------------------------------------


p1ks$from <- factor(p1ks$from)
levels(p1ks$from)

par(mar = c(8, 12, 8, 8), family = "HersheySans")
bs3 <- beeswarm(usd ~ from, data = p1ks, pch = 19, method = "center", 
                 priority = "density", corral = "none",
                 main = "Sample Distribution of Prices by Category (n = 1000, USD < 1,000)",
                 col = c("red3", "gold1", "deepskyblue4", "lightblue3", "red1", 
                         "bisque1", "bisque3", "bisque4",  "darkorange3", "firebrick4"),
                 cex.lab = 0.8, ylab = "", xlab = "price (USD)", horiz = T)
points(bs3$y, bs3$x, pch = 1, col = "#00000034")

# horizontal
bs.plot3 <- ggplot(bs3, aes(x = y, y = x)) + 
  geom_point(aes(colour = x.orig), size = 4, shape = 20) +
  geom_point(size = 4, shape = 1, colour = "#00000034") + 
  scale_colour_manual(values = primary.colors(29, steps = 2)) +
  scale_y_continuous(breaks = c(1:29), 
                     labels = c("Australia", "Austria", "Belgium", "Canada", 
                                "China", "Czech Republic", "Denmark", "EU", 
                                "France", "Germany", "Hong Kong", "India",
                                "Internet","Ireland","Italy","Netherlands", 
                                "No Info", "Norway", "Philippines", "Poland",
                                "Singapore","South Africa", "Spain", "Sweden", 
                                "Torland", "UK", "USA", "Undeclared","Worldwide")) +
  theme_minimal(base_size = 12, base_family = "GillSans") +
  theme(axis.text.x = element_text(size = 11.5, vjust = 1)) +
  theme(axis.text.y = element_text(size = 11.5)) +
  theme(plot.margin = unit(c(1, 0.75, 0.25, 0.5), "cm")) +
  theme(legend.position = "none") +
  labs(title = "Agora 2015: Sample Distribution, Prices ~ Location (USD < 1000, n = 2000)",
       x = "", y = "")

bs.plot3
