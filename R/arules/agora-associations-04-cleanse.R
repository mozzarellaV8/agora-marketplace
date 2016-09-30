# Agora Marketplace Analysis
# Association Rules - Agora Population
# Variable Preparation

# load data -------------------------------------------------------------------

library(data.table)
library(ggplot2)
library(anonymizer)

# population
a <- fread("~/GitHub/agora-data/agora-01b.csv", stringsAsFactors = T)
# 2322961 obs of 18 variables

# prepped data
ag <- fread("~/GitHub/agora-data/06-arules/ag-arules-20k.csv", stringsAsFactors = T)

# subset under 20k ------------------------------------------------------------
summary(ag$usd)
above <- subset(a, a$usd > 20000)
ag <- subset(a, a$usd <= 20000) # 2317353
ag <- as.data.frame(ag)

# aggregate categories --------------------------------------------------------

# subcategories only:
ag$sc <- paste(ag$subcat, ag$subsubcat, sep = "-")
ag$sc <- gsub("-NA$", "", ag$sc)
ag$sc <- gsub("Methylone", "RCs", ag$sc)
ag$sc <- factor(ag$sc)
levels(ag$sc)

# all categories:
ag$all.c <- paste(ag$cat, ag$sc, sep = ": ")
ag$all.c <- gsub("Drugs: Methylone", "Drugs: RCs", ag$all.c)
ag$all.c <- factor(ag$all.c)
levels(ag$all.c)

# prep - discretize prices -----------------------------------------------------
# but into cluster or interval?

ag$usd <- round(ag$usd, 2)
summary(ag$usd)
#  Min.  1st Qu.   Median     Mean  3rd Qu.     Max. 
# 0.00    24.28    84.97   426.40   290.20 20000.00

quantile(ag$usd)
#   0%      25%      50%      75%     100% 
# 0.00    24.28    84.97   290.19 20000.00 

# prep - plot price distributions ----------------------------------------------
par(mfrow = c(2, 2), mar = c(6, 6, 6, 6), family = "GillSans")
hist(ag$usd, breaks = 100, main = "n < $20,000", 
     xlab = "", ylab = "Frequency")
hist(ag$usd, breaks = 100, xlim = c(0, 5000), 
     main = "n < $5,000", xlab = "", ylab = "")
hist(ag$usd, breaks = 1000, xlim = c(0, 1000), 
     main = "n < $1,000", xlab = "price in USD", ylab = "Frequency")
hist(ag$usd, breaks = 10000, xlim = c(0, 200),
     main = "n < $200", xlab = "price in USD", ylab = "")

# plot under 200 to under 10 dollar
par(mfrow = c(2, 2), mar = c(6, 6, 6, 6), family = "GillSans")
hist(ag$usd, breaks = 25000, xlim = c(0, 200),
     main = "n < $200", xlab = "", ylab = "Frequency")
hist(ag$usd, breaks = 500000, xlim = c(0, 100), 
     main = "n < $100", xlab = "", ylab = "")
hist(ag$usd, breaks = 100000, xlim = c(0, 50), 
     main = "n < $50", xlab = "price in USD", ylab = "Frequency")
hist(ag$usd, breaks = 100000, xlim = c(0, 10),
     main = "n < 10", xlab = "price in USD", ylab = "")

# look at densities under $200
ag200 <- subset(ag$usd, ag$usd <= 200.00)
ag100 <- subset(ag$usd, ag$usd <= 100.00)
ag50 <- subset(ag$usd, ag$usd <= 50.000)
ag10 <- subset(ag$usd, ag$usd <= 10.000)

hist(ag200, breaks = 150, xlim = c(0, 200), main = "usd < $200", ylab = "")
hist(ag100, breaks = 150, xlim = c(0, 100), main = "usd < $100", ylab = "")
hist(ag50, breaks = 150, xlim = c(0, 50), main = "usd < $50", ylab = "")
hist(ag10, breaks = 100, xlim = c(0, 10), main = "usd < $10", ylab = "")

par(mfrow = c(2, 2), mar = c(5, 5, 5, 5), family = "GillSans")
plot(density(ag200), main = "usd < $200")
plot(density(ag100), main = "usd < $100", ylab = "")
plot(density(ag50), main = "usd < $50")
plot(density(ag10), main = "usd < $10", ylab = "")

# look at densities between 500-5000
ag5000 <- subset(ag$usd, ag$usd <= 5000 & ag$usd > 2000)
ag2000 <- subset(ag$usd, ag$usd > 1200 & ag$usd <= 2000)
ag1000 <- subset(ag$usd, ag$usd > 600 & ag$usd <= 1200)
ag600 <- subset(ag$usd, ag$usd > 200 & ag$usd <= 600)

par(mfrow = c(2, 2), mar = c(5, 5, 5, 5), family = "GillSans")
plot(density(ag5000), main = "$2000 < usd < $5000")
plot(density(ag2000), main = "$1200 < usd < $2000", ylab = "")
plot(density(ag1000), main = "$600 < usd < $1200")
plot(density(ag600), main = "$200 < usd < $600", ylab = "")

hist(ag5000, breaks = 200, xlim = c(2000, 5000), main = "$2000 < usd < $5000")
hist(ag2000, breaks = 200, xlim = c(1200, 2000), main = "$1200 < usd < $2000", ylab = "")
hist(ag1000, breaks = 150, xlim = c(600, 1200), main = "$600 < usd < $1200")
hist(ag600, breaks = 150, xlim = c(200, 600), main = "$200 < usd < $600", ylab = "")

# distributions between 5000-20000
par(mfrow = c(2, 2), mar = c(5, 5, 5, 5), las = 1, family = "GillSans")
hist(ag$usd, breaks = 1000, xlim = c(5000, 7500), ylim = c(0, 400),
     main = "$5000 < n < $7500", xlab = "", ylab = "Frequency")
hist(ag$usd, breaks = 1000, xlim = c(7500, 10000), ylim = c(0, 150),
     main = "$7500 < n < $10,000", xlab = "", ylab = "")
hist(ag$usd, breaks = 1000, xlim = c(10000, 15000), ylim = c(0, 150),
     main = "$10,000 < n < $15,000", xlab = "", ylab = "Frequency")
hist(ag$usd, breaks = 1000, xlim = c(15000, 20000), ylim = c(0, 30),
     main = "$15,000 < n < $20,000", xlab = "", ylab = "")

# heavy on the left/long tail - quick check of the log()
ag$log.usd <- log(ag$usd)

par(mfrow = c(1, 1), mar = c(6, 6, 6, 6), las = 1, family = "GillSans")
hist(ag$log.usd, main = "log(usd) Distribution of Prices, n = 2316650",
     breaks = 100, xlab = "", ylab = "")
axis(1, at = seq(-5, 10, 1))

summary(log.usd)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# -Inf   3.190   4.442    -Inf   5.671   9.903 
exp(4.25)
exp(seq(4, 5, 0.25))
# 54.59815  70.10541  90.01713 115.58428 148.41316

ggplot(ag, aes(x = log.usd)) + 
  geom_histogram(binwidth = 0.25, color = "black", alpha = 0, size = 0.5) +
  scale_x_continuous(breaks = seq(-5, 10, 1)) +
  theme_minimal(base_size = 16, base_family = "GillSans") +
  theme(axis.text.y = element_text(size = 14),
        axis.text.x = element_text(size = 14),
        panel.grid.major = element_line(color = "gray82"),
        plot.margin = unit(c(1, 1, 1, 1), "cm")) +
  labs(title = "log Distribution of Prices, n = 2316650",
       x = "", y = "")

# Warning message:
# Removed 703 rows containing non-finite values (stat_bin)
# so: n = nrow(ag)-703
nrow(ag) - 703



# prep - actually discretize --------------------------------------------------

# manually
ag$p <- ag$usd
ag$p <- ifelse(ag$p <= 10.00, "$0-$10",
               ifelse(ag$p > 10 & ag$p <= 100.00, "$10-$100",
                      ifelse(ag$p > 100 & ag$p <= 200, "$100-$200", 
                             ifelse(ag$p > 200 & ag$p <= 600.00, "$200-$600",
                                    ifelse(ag$p > 600 & ag$p <= 2000.00, "$600-$2000",
                                           ifelse(ag$p > 2000 & ag$p <= 10000, "$2000-10000",
                                                  ifelse(ag$p > 10000, "$10000-$20000", NA)))))))
            

ag$p <- factor(ag$p)  # 6 levels

summary(ag$p)
#  $0-10      $10-150 $10000-20000     $150-600  $2000-10000    $600-2000 
# 371235      1086166         7393       515111       106747       230701 

371235/nrow(ag)   # 0.1601979
1086166/nrow(ag)  # 0.4687098
7393/nrow(ag)     # 0.003190278
515111/nrow(ag)   # 0.2222842
106747/nrow(ag)   # 0.04606419
230701/nrow(ag)   # 0.09955367

ggplot(ag, aes(reorder(p), fill = p)) + 
  geom_bar(color = "gray45", size = 0.25) + coord_flip() +
  scale_fill_manual(values = c("#00688B32", "#00688B94", "#00688B02", 
                               "#00688B44", "#00688B10", "#00688B20"),
                    guide = F) +
  theme_minimal(base_size = 14, base_family = "GillSans") +
  theme(plot.margin = unit(c(0.25, 0.25, 0.25, 0.25), "cm"),
        panel.grid.major = element_line(color = "gray80"),
        axis.text.y = element_text(size = 12.75),
        axis.text.x = element_text(size = 12.75),
        legend.position = "none") +
  labs(title = "Distribution of Discretized Prices", 
       x = "", y = "", colour = "", fill = "")

# discretize using arules:
# ag$p <- ag$usd
# ag$p <- discretize(ag$p, method = "cluster", categories = 10)
# levels(ag$p)

# levels(ag$p) <- list("$0-136" = "[    0,  136)", "$136-381" = "[  136,  381)",
#                     "$381-797" = "[  381,  797)", "$797-1507" = "[  797, 1507)",
#                     "$1507-2560" = "[ 1507, 2560)", "$2560-$4100" = "[ 2560, 4100)",
#                     "$4100-6166" = "[ 4100, 6166)" , "$6166-8930" = "[ 6166, 8930)",
#                     "$8930-13131" = "[ 8930,13131)", "$13131-20000" = "[13131,20000]")

# prep - anonymize vendors ----------------------------------------------------

ag$v2 <- as.character(ag$vendor)
ag$v2 <- anonymize(ag$v2, .algo = "sha256", .seed = 12, 
                   .chars = letters[seq(from = 1, to = 26)])

nchar(ag$v2[234]) # 64

# verify same number of levels
length(levels(as.factor(ag$v2))) # 3183

ag$v3 <- abbreviate(ag$v2, minlength = 6, strict = F, method = "left.kept")
length(levels(as.factor(ag$v3))) # 3183
ag$v3 <- factor(ag$v3)
summary(ag$v3)

