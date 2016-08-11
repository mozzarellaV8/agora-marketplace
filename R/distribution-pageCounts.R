# Agora MarketPlace Analysis
# Distribution of page counts by crawl date
# corrlation between number of products and vendors?

# load data -------------------------------------------------------------------

p2014 <- read.csv("data/crawl-distribution-2014.csv")
str(p2014)

p2014$date <- as.Date(p2014$date)
p2014$vendor <- as.integer(p2014$vendor)

summary(p2014)
#     date                  p             vendor      
# Min.   :2014-01-01   Min.   :    1   Min.   :  1.00  
# 1st Qu.:2014-06-01   1st Qu.: 2584   1st Qu.: 29.25  
# Median :2014-09-29   Median : 7894   Median : 55.50  
# Mean   :2014-08-20   Mean   : 9348   Mean   : 55.39  
# 3rd Qu.:2014-11-13   3rd Qu.:15554   3rd Qu.: 81.75  
# Max.   :2014-12-30   Max.   :22413   Max.   :107.00

# explore data ----------------------------------------------------------------

library(ggplot2)

p1 <- ggplot(p2014, aes(x = date, y = p)) + 
  geom_line(linetype = "solid", color = "red3") +
  geom_hline(yintercept = 9348, linetype = "dashed") +
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm")) +
  labs(title = "number of product pages by crawl date: 2014",
       y = "product pages")
p1

mean(p2014$vendor)

p2 <- ggplot(p2014, aes(x = date, y = vendor)) +
  geom_line(linetype = "solid", color = "steelblue3") +
  geom_hline(yintercept = 55.38596, linetype = "dashed", alpha = 0.75) +
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm")) +
  labs(title = "number of vendor pages by crawl date: 2014",
       y = "vendor pages")
p2

# might be worth throwing out the dates with no activity

p14 <- subset(p2014, p2014$p > 100 & p2014$vendor > 20)
View(p14)

p1.2 <- ggplot(p14, aes(x = date, y = p)) + 
  geom_line(linetype = "solid", color = "red3") +
  geom_hline(yintercept = 9348, linetype = "dashed") +
  geom_jitter() +
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm")) +
  labs(title = "number of product pages by crawl date: 2014",
       y = "product pages")

p1.2

p2.2 <- ggplot(p14, aes(x = date, y = vendor)) +
  geom_line(linetype = "solid", color = "steelblue3") +
  geom_hline(yintercept = 55.38596, linetype = "dashed", alpha = 0.75) +
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm")) +
  labs(title = "number of vendor pages by crawl date",
       y = "vendor pages")
p2.2

# correlation -----------------------------------------------------------------

library(corrplot)

pVend <- subset(p2014, select = c("p", "vendor"))
pCor <- cor(pVend, use = "everything")
pCor
#                p    vendor
# p      1.0000000 0.4237502
# vendor 0.4237502 1.0000000

corrplot(pCor, method = "ellipse", addshade = "all", addCoef.col = "black",
         tl.srt = 45, mar = c(8, 8, 12, 8))

lm.p14 <- lm(p ~ vendor, data = p2014)
summary(lm.p14)
# Coefficients:
#                 Estimate Std. Error t value  Pr(>|t|)    
#    (Intercept)  3979.04    1244.98   3.196   0.00181 ** 
#    vendor         96.94      19.58   4.951  2.63e-06 ***

#   Multiple R-squared:  0.1796,	Adjusted R-squared:  0.1722  

par(mar = c(8, 8, 8, 8), mfrow = c(2, 2))
plot(lm.p14)

# boxplot - histogram ---------------------------------------------------------

hist(p2014$p, breaks = 200)

library(car)

pDensity <- density(p2014$p, kernel = c("gaussian", "cosine"))

par(mar = c(8, 8, 8, 8), mfrow = c(1, 1))
plot(pDensity, col = "#CD0000")

par(mfrow = c(2, 2), mar = c(8, 8, 8, 8))
densityPlot(p2014$p, kernel = c("gaussian"), ylab = "gaussian density")
densityPlot(p2014$p, kernel = c("cosine"), ylab = "cosine density")
densityPlot(p2014$p, kernel = c("epanechnikov"), ylab = "epanechnikov density")
densityPlot(p2014$p, kernel = c("triangular"), ylab = "triangular density")

pKde02 <- ggplot(p2014, aes(x = p)) + 
  geom_density(fill = "red2", colour = NA, alpha = 0.2) +
  geom_line(stat = "density", adjust = 0.25, colour = "red") +
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm")) +
  labs(title = "density of product pages: 2014",
       x = "number of product pages")
pKde02

vKde01 <- ggplot(p2014, aes(x = vendor)) +
  geom_density(fill = "steelblue4", colour = NA, alpha = 0.2) +
  geom_line(stat = "density", adjust = 0.25, colour = "steelblue4") +
  theme(plot.margin = unit(c(2, 2, 2, 2), "cm")) +
  labs(title = "density of vendor pages: 2014",
       x = "num vendor pages")
vKde01

boxplot(p2014$p)
boxplot(p2014$vendor)

# violin
pVio <- ggplot(p2014, aes(x = vendor, y = p)) + geom_violin()
pVio

# wilkinson dot
pDot <- ggplot(p2014, aes(x = p)) + 
  geom_dotplot() +
  geom_rug()
pDot
