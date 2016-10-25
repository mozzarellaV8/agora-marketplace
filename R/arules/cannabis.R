# Cannabis Class

# Agora Marketplace Analysis
# Association Rules - Agora Population
# Variable Preparation

# load data -------------------------------------------------------------------

library(data.table)
library(ggplot2)

# population with: character counts, subcat aggregated, all cats
a <- fread("~/GitHub/agora-data/agora-02.csv", stringsAsFactors = T)
# 2322961 obs of 18 variables
str(a)
summary(a$usd)

# investigate the price bins ----------------------------
a1 <- subset(a, a$usd <= 150)
nrow(a1) # 1457401

1457401/nrow(a) # 0.6273894 63% of listings $150 or less
summary(a1$usd)
#  Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 0.00    9.97   35.43   46.78   75.87  150.00

nrow(subset(a, a$usd > 150 & a$usd <= 600))   # 515111
nrow(subset(a, a$usd > 600 & a$usd <= 2000))  # 230701
nrow(subset(a, a$usd > 2000 & a$usd <= 10000)) # 106747
nrow(subset(a, a$usd > 10000 & a$usd <= 60000)) # 10259
nrow(subset(a, a$usd > 60000)) # 2742

cor(a$usd, a$price)
cov(a$usd, a$price)
var(a$usd)
var(a$price)

onedollar <- subset(a, a$usd <= 1) # 104040
summary(onedollar)
dollar.cats <- as.data.frame(table(onedollar$all.c))


# subset cannabis
cannabis <- subset(a, a$subcat == "Cannabis")



